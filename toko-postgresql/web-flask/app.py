import os
import json
from pathlib import Path
from functools import wraps
from datetime import date, datetime, timedelta, timezone
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

from flask import Flask, g, jsonify, redirect, render_template, request, session, url_for
from werkzeug.security import check_password_hash, generate_password_hash
from postgres_db import connect


APP_NAME = "MR. FAUZI ZAMI"
BASE_DIR = Path(__file__).resolve().parent
DATABASE_URL = os.environ.get(
    "DATABASE_URL", "postgresql://toko_app:toko_dev_password@localhost:5432/toko"
)
SCHEMA = BASE_DIR / "database" / "schema.sql"
LEGACY_STOK_MIN_ROWS = 9000
try:
    WIB = ZoneInfo("Asia/Jakarta")
except ZoneInfoNotFoundError:
    WIB = timezone(timedelta(hours=7), "WIB")


def create_app():
    app = Flask(__name__)
    app.config["SECRET_KEY"] = os.environ.get("TOKO_SECRET_KEY", "dev-toko-flask")
    app.config["DATABASE_URL"] = DATABASE_URL

    @app.before_request
    def ensure_database():
        init_db()

    @app.teardown_appcontext
    def close_db(error=None):
        db = g.pop("db", None)
        if db is not None:
            db.close()

    @app.template_filter("rupiah")
    def rupiah(value):
        return format_rupiah(value)

    @app.context_processor
    def inject_globals():
        store = store_settings()
        return {
            "app_name": store["name"],
            "store_name": store["name"],
            "store_city": store["city"],
            "cashier_name": session.get("cashier_name", "-"),
            "register_no": register_no(),
            "active_path": request.path,
        }

    @app.route("/logout")
    def logout():
        clear_cashier_session()
        session.pop("stock_logged_in", None)
        return redirect(url_for("kasir"))

    @app.route("/kasir/logout")
    def kasir_logout():
        clear_cashier_session()
        return redirect(url_for("kasir"))

    @app.route("/")
    def desktop():
        return redirect(url_for("kasir"))

    @app.route("/healthz")
    def healthz():
        return jsonify({"ok": True, "app": "toko-web-flask"})

    @app.route("/dashboard")
    @cashier_required
    def dashboard():
        db = get_db()
        product_count = db.execute("SELECT COUNT(*) AS count FROM products").fetchone()["count"]
        minus_count = db.execute("SELECT COUNT(*) AS count FROM products WHERE stock < 0").fetchone()["count"]
        return render_template(
            "dashboard.html",
            product_count=product_count,
            minus_count=minus_count,
        )

    @app.route("/kasir", methods=["GET", "POST"])
    def kasir():
        if request.method == "POST":
            code = request.form.get("cashier_code", "").strip()
            cashier = get_db().execute(
                "SELECT * FROM cashiers WHERE code = ? AND active = 1",
                (code,),
            ).fetchone()
            if not cashier:
                return render_template(
                    "kasir_login.html",
                    error="ID kasir tidak ditemukan.",
                    cashiers=cashier_map(),
                )
            password = request.form.get("password", "")
            if not cashier_password_matches(cashier, password):
                return render_template(
                    "kasir_login.html",
                    error="Password kasir salah.",
                    cashiers=cashier_map(),
                )

            session["cashier_id"] = cashier["id"]
            session["cashier_name"] = cashier["name"]
            session["register_no"] = request.form.get("register_no", "").strip() or "1"
            session["print_receipt"] = request.form.get("print_receipt", "")
            return redirect(url_for("kasir"))

        if "cashier_id" not in session:
            return render_template("kasir_login.html", error="", cashiers=cashier_map())

        db = get_db()
        store = store_settings()
        items = []
        subtotal = 0
        for item in items:
            item["line_total"] = int(item["qty"] * item["price"]) - int(item["discount"])
            subtotal += item["line_total"]

        total, rounding = round_total(subtotal)
        sale_no = next_sale_no(session["cashier_id"], register_no())
        server_now = datetime.now(WIB)
        return render_template(
            "kasir.html",
            items=items,
            subtotal=subtotal,
            total=total,
            rounding=rounding,
            sale_no=sale_no,
            paid=50000,
            change_amount=50000 - total,
            print_receipt=session.get("print_receipt", ""),
            today_label=clipper_date_label(server_now.date()),
            server_now=server_now.isoformat(),
            store=store,
        )

    @app.route("/kasir/kas", methods=["GET", "POST"])
    @cashier_required
    def kas_harian():
        db = get_db()
        trans_date = request.values.get("tanggal", date.today().isoformat())
        cashier_name = session.get("cashier_name", "")

        if request.method == "POST":
            account = db.execute(
                "SELECT * FROM cash_accounts WHERE code = ?",
                (request.form.get("account_code", "").strip(),),
            ).fetchone()
            if not account:
                return redirect(url_for("kas_harian", tanggal=trans_date, error="kode"))

            amount = parse_int(request.form.get("amount", "0"))
            db.execute(
                """
                INSERT INTO cash_transactions
                  (trans_date, cashier_name, account_code, account_name, description, amount, side, profit_loss)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    trans_date,
                    cashier_name,
                    account["code"],
                    account["name"],
                    request.form.get("description", "").strip(),
                    amount,
                    account["side"],
                    account["profit_loss"],
                ),
            )
            db.commit()
            return redirect(url_for("kas_harian", tanggal=trans_date))

        accounts = db.execute("SELECT * FROM cash_accounts ORDER BY code").fetchall()
        transactions = db.execute(
            """
            SELECT * FROM cash_transactions
            WHERE trans_date = ? AND cashier_name = ?
            ORDER BY id
            """,
            (trans_date, cashier_name),
        ).fetchall()
        error = request.args.get("error")
        return render_template(
            "kas_harian.html",
            accounts=accounts,
            transactions=transactions,
            trans_date=trans_date,
            date_label=iso_to_dmy(trans_date),
            error=error,
        )

    @app.route("/kasir/kas/<int:transaction_id>/edit", methods=["POST"])
    @cashier_required
    def kas_harian_edit(transaction_id):
        trans_date = request.form.get("tanggal", date.today().isoformat())
        get_db().execute(
            """
            UPDATE cash_transactions
            SET description = ?, amount = ?, updated_at = CURRENT_TIMESTAMP
            WHERE id = ? AND cashier_name = ?
            """,
            (
                request.form.get("description", "").strip(),
                parse_int(request.form.get("amount", "0")),
                transaction_id,
                session.get("cashier_name", ""),
            ),
        )
        get_db().commit()
        return redirect(url_for("kas_harian", tanggal=trans_date))

    @app.route("/kasir/kas/<int:transaction_id>/delete", methods=["POST"])
    @cashier_required
    def kas_harian_delete(transaction_id):
        trans_date = request.form.get("tanggal", date.today().isoformat())
        get_db().execute(
            "DELETE FROM cash_transactions WHERE id = ? AND cashier_name = ?",
            (transaction_id, session.get("cashier_name", "")),
        )
        get_db().commit()
        return redirect(url_for("kas_harian", tanggal=trans_date))

    @app.route("/kasir/kas/laporan")
    @cashier_required
    def laporan_kas_harian():
        db = get_db()
        trans_date = request.args.get("tanggal", date.today().isoformat())
        cashier_name = session.get("cashier_name", "")
        sale_total = db.execute(
            """
            SELECT COALESCE(SUM(total), 0) AS total
            FROM sales
            WHERE sale_date = ? AND cashier_id = ? AND status <> 'void'
            """,
            (trans_date, session.get("cashier_id")),
        ).fetchone()["total"]
        debit_rows = db.execute(
            """
            SELECT account_name, description, amount
            FROM cash_transactions
            WHERE trans_date = ? AND cashier_name = ? AND side = 'D'
            ORDER BY account_code, id
            """,
            (trans_date, cashier_name),
        ).fetchall()
        credit_rows = db.execute(
            """
            SELECT account_name, description, amount
            FROM cash_transactions
            WHERE trans_date = ? AND cashier_name = ? AND side = 'K'
            ORDER BY account_code, id
            """,
            (trans_date, cashier_name),
        ).fetchall()
        total_debit = sale_total + sum(row["amount"] for row in debit_rows)
        total_credit = sum(row["amount"] for row in credit_rows)
        return render_template(
            "laporan_kas.html",
            trans_date=trans_date,
            date_label=iso_to_dmy(trans_date),
            sale_total=sale_total,
            debit_rows=debit_rows,
            credit_rows=credit_rows,
            total_debit=total_debit,
            total_credit=total_credit,
            cash_balance=total_debit - total_credit,
        )

    @app.route("/kasir/kas/perkiraan", methods=["GET", "POST"])
    @cashier_required
    def perkiraan_kas():
        db = get_db()
        error = ""
        if request.method == "POST":
            code = request.form.get("code", "").strip().upper()
            name = request.form.get("name", "").strip().upper()
            side = request.form.get("side", "").strip().upper()
            profit_loss = request.form.get("profit_loss", "").strip().upper()
            if side not in {"D", "K"}:
                error = "D/K = Harus diisi dengan D=Debet atau K=Kredit"
            elif profit_loss not in {"Y", "T"}:
                error = "L/R = diisi Y=Masuk Lap.L/R atau T=Transaksi Kas saja"
            elif not code or not name:
                error = "Kode dan Pos Perkiraan wajib diisi"
            else:
                db.execute(
                    """
                    INSERT INTO cash_accounts (code, name, side, profit_loss)
                    VALUES (?, ?, ?, ?)
                    ON CONFLICT(code) DO UPDATE SET
                      name = excluded.name,
                      side = excluded.side,
                      profit_loss = excluded.profit_loss
                    """,
                    (code, name, side, profit_loss),
                )
                db.commit()
                return redirect(url_for("perkiraan_kas"))

        accounts = db.execute("SELECT * FROM cash_accounts ORDER BY code").fetchall()
        return render_template("perkiraan_kas.html", accounts=accounts, error=error)

    @app.route("/kasir/kas/perkiraan/<code>/delete", methods=["POST"])
    @cashier_required
    def perkiraan_kas_delete(code):
        db = get_db()
        used = db.execute(
            "SELECT COUNT(*) AS count FROM cash_transactions WHERE account_code = ?",
            (code,),
        ).fetchone()["count"]
        if used == 0:
            db.execute("DELETE FROM cash_accounts WHERE code = ?", (code,))
            db.commit()
        return redirect(url_for("perkiraan_kas"))

    @app.route("/blablabla", methods=["GET", "POST"])
    def stok_login():
        if request.method == "POST":
            password = request.form.get("password", "").strip()
            if password != "00":
                return render_template("stok_login.html", error="Password stok salah.")
            session["stock_logged_in"] = True
            return redirect(url_for("stok"))

        if "stock_logged_in" in session:
            return redirect(url_for("stok"))

        return render_template("stok_login.html", error="")

    @app.route("/stok", methods=["GET", "POST"])
    def stok():
        if "stock_logged_in" not in session:
            return redirect(url_for("stok_login"))

        return stok_barang_view()

    @app.route("/stok-barang", methods=["GET", "POST"])
    def stok_barang():
        if "stock_logged_in" not in session and "cashier_id" not in session:
            return redirect(url_for("stok_login"))
        return stok_barang_view()

    def stok_barang_view():
        db = get_db()
        if request.method == "POST":
            action = request.form.get("_action", "add")
            product_id = parse_int(request.form.get("product_id", 0))
            if action == "delete" and product_id:
                used_count = db.execute(
                    "SELECT COUNT(*) AS count FROM sale_items WHERE product_id = ?",
                    (product_id,),
                ).fetchone()["count"]
                if used_count:
                    db.execute(
                        """
                        UPDATE products
                        SET stock = 0, updated_at = CURRENT_TIMESTAMP
                        WHERE id = ?
                        """,
                        (product_id,),
                    )
                else:
                    db.execute("DELETE FROM products WHERE id = ?", (product_id,))
            else:
                values = (
                    request.form.get("barcode", "").strip(),
                    request.form.get("legacy_code", "").strip(),
                    request.form.get("group_code", "00").strip() or "00",
                    request.form.get("name", "").strip().upper(),
                    request.form.get("unit", "PCS").strip().upper() or "PCS",
                    parse_float(request.form.get("stock", "0")),
                    parse_int(request.form.get("cost_price", "0")),
                    parse_int(request.form.get("wholesale_price", "0")),
                    parse_int(request.form.get("retail_price", "0")),
                    request.form.get("supplier_code", "").strip(),
                    request.form.get("rack_code", "").strip(),
                )
                if action == "update" and product_id:
                    db.execute(
                        """
                        UPDATE products
                        SET barcode = ?, legacy_code = ?, group_code = ?, name = ?, unit = ?,
                            stock = ?, cost_price = ?, wholesale_price = ?, retail_price = ?,
                            supplier_code = ?, rack_code = ?, updated_at = CURRENT_TIMESTAMP
                        WHERE id = ?
                        """,
                        values + (product_id,),
                    )
                else:
                    db.execute(
                        """
                        INSERT INTO products
                          (barcode, legacy_code, group_code, name, unit, stock, cost_price,
                           wholesale_price, retail_price, supplier_code, rack_code)
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                        """,
                        values,
                    )
            db.commit()
            return redirect(url_for("stok_barang", show="barang"))

        q = request.args.get("q", "").strip()
        show_products = request.args.get("show", "") == "barang" or bool(q)
        if q:
            like = f"%{q}%"
            products = db.execute(
                """
                SELECT * FROM products
                WHERE barcode LIKE ? OR name LIKE ? OR group_code LIKE ?
                ORDER BY id DESC
                """,
                (like, like, like),
            ).fetchall()
        else:
            products = db.execute("SELECT * FROM products ORDER BY id DESC").fetchall()

        return render_template(
            "stok_barang.html",
            products=products,
            products_json=[product_to_stock_json(row) for row in products],
            q=q,
            show_products=show_products,
        )

    @app.route("/pelanggan", methods=["GET", "POST"])
    @stock_required
    def pelanggan():
        db = get_db()
        if request.method == "POST":
            code = request.form.get("code", "").strip()
            name = request.form.get("name", "").strip().upper()
            if code and name:
                db.execute(
                    """
                    INSERT INTO customers
                      (code, name, address, city, phone, discount, points, balance, notes)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT(code) DO UPDATE SET
                      name = excluded.name,
                      address = excluded.address,
                      city = excluded.city,
                      phone = excluded.phone,
                      discount = excluded.discount,
                      points = excluded.points,
                      balance = excluded.balance,
                      notes = excluded.notes,
                      updated_at = CURRENT_TIMESTAMP
                    """,
                    (
                        code,
                        name,
                        request.form.get("address", "").strip().upper(),
                        request.form.get("city", "").strip().upper(),
                        request.form.get("phone", "").strip(),
                        parse_float(request.form.get("discount", "0")),
                        parse_int(request.form.get("points", "0")),
                        parse_int(request.form.get("balance", "0")),
                        request.form.get("notes", "").strip(),
                    ),
                )
                db.commit()
            return redirect(url_for("pelanggan"))

        q = request.args.get("q", "").strip()
        if q:
            like = f"%{q}%"
            customers = db.execute(
                """
                SELECT * FROM customers
                WHERE code LIKE ? OR name LIKE ? OR address LIKE ? OR city LIKE ?
                ORDER BY code
                """,
                (like, like, like, like),
            ).fetchall()
        else:
            customers = db.execute("SELECT * FROM customers ORDER BY code").fetchall()
        return render_template("pelanggan.html", customers=customers, q=q)

    @app.route("/pelanggan/<int:customer_id>/delete", methods=["POST"])
    @stock_required
    def pelanggan_delete(customer_id):
        db = get_db()
        customer = db.execute("SELECT code FROM customers WHERE id = ?", (customer_id,)).fetchone()
        if not customer:
            return redirect(url_for("pelanggan"))
        usage = db.execute(
            """
            SELECT
              (SELECT COUNT(*) FROM sales WHERE member_code = ?) AS sales_count,
              (SELECT COUNT(*) FROM customer_point_ledger WHERE customer_code = ?) AS point_count,
              (SELECT COUNT(*) FROM customer_payments WHERE customer_code = ?) AS payment_count
            """,
            (customer["code"], customer["code"], customer["code"]),
        ).fetchone()
        if any(usage.values()):
            return redirect(url_for("pelanggan", error="Pelanggan yang memiliki transaksi, poin, atau piutang tidak dapat dihapus."))
        db.execute("DELETE FROM customers WHERE id = ?", (customer_id,))
        db.commit()
        return redirect(url_for("pelanggan"))

    @app.route("/pembelian")
    @stock_required
    def pembelian():
        rows = [
            {"name": "INDOMIE GORENG", "qty": 40, "price": 2850},
            {"name": "GULA PASIR / KG", "qty": 25, "price": 16500},
        ]
        for row in rows:
            row["total"] = row["qty"] * row["price"]
        return render_template("pembelian.html", rows=rows)

    @app.route("/laporan")
    @cashier_or_stock_required
    def laporan():
        return render_template("laporan.html")

    @app.route("/pengaturan")
    @cashier_or_stock_required
    def pengaturan():
        return render_template("pengaturan.html")

    @app.route("/api/kasir/<code>")
    def api_kasir(code):
        cashier = get_db().execute(
            "SELECT code, name FROM cashiers WHERE code = ? AND active = 1",
            (code.strip(),),
        ).fetchone()
        if not cashier:
            return jsonify({"found": False, "name": ""}), 404
        return jsonify({"found": True, "name": cashier["name"]})

    @app.route("/api/customers/<code>")
    @cashier_required
    def api_customer(code):
        customer = get_db().execute(
            """
            SELECT code, name, address, city, phone, discount, points, balance, notes
            FROM customers
            WHERE code = ?
            """,
            (code.strip(),),
        ).fetchone()
        if not customer:
            return jsonify({"found": False}), 404
        return jsonify({"found": True, "customer": customer_to_json(customer)})

    @app.route("/api/cashier/print-preference", methods=["POST"])
    @cashier_required
    def api_cashier_print_preference():
        payload = request.get_json(silent=True) or {}
        value = str(payload.get("print_receipt", "")).strip()
        if value not in {"", "1", "0"}:
            return jsonify({"ok": False, "error": "Pilihan print tidak valid."}), 400
        session["print_receipt"] = value
        labels = {"": "Tanya saat transaksi", "1": "Print struk", "0": "Download struk"}
        return jsonify({"ok": True, "print_receipt": value, "label": labels[value]})

    @app.route("/api/products/search")
    @cashier_required
    def api_products_search():
        q = request.args.get("q", "").strip()
        mode = request.args.get("mode", "code").strip()
        if not q:
            return jsonify({"items": []})

        db = get_db()
        like = f"%{q}%"
        prefix = f"{q}%"
        if mode == "name":
            rows = db.execute(
                """
                SELECT id, barcode, legacy_code, group_code, name, unit, stock, retail_price,
                       member_price, tier3_qty, tier3_price, tier4_qty, tier4_price, tier5_qty, tier5_price
                FROM products
                WHERE name LIKE ?
                ORDER BY
                  CASE WHEN name LIKE ? THEN 0 ELSE 1 END,
                  name
                LIMIT 12
                """,
                (like, prefix),
            ).fetchall()
        else:
            rows = db.execute(
                """
                SELECT id, barcode, legacy_code, group_code, name, unit, stock, retail_price,
                       member_price, tier3_qty, tier3_price, tier4_qty, tier4_price, tier5_qty, tier5_price
                FROM products
                WHERE barcode = ? OR legacy_code = ? OR barcode LIKE ? OR legacy_code LIKE ? OR name LIKE ?
                ORDER BY
                  CASE
                    WHEN barcode = ? OR legacy_code = ? THEN 0
                    WHEN barcode LIKE ? OR legacy_code LIKE ? THEN 1
                    ELSE 2
                  END,
                  name
                LIMIT 12
                """,
                (q, q, prefix, prefix, like, q, q, prefix, prefix),
            ).fetchall()

        return jsonify({"items": [product_to_json(row) for row in rows]})

    @app.route("/api/sales", methods=["POST"])
    @cashier_required
    def api_sales_create():
        payload = request.get_json(silent=True) or {}
        raw_items = payload.get("items") or []
        if not raw_items:
            return jsonify({"ok": False, "error": "Belum ada item transaksi."}), 400

        db = get_db()
        member = payload.get("member") or {}
        member_code = str(member.get("code", "")).strip()
        customer = None
        if member_code:
            customer = db.execute(
                "SELECT * FROM customers WHERE code = ? FOR UPDATE", (member_code,)
            ).fetchone()
            if not customer:
                return jsonify({"ok": False, "error": "Member tidak ditemukan."}), 400
        try:
            sale_items, subtotal = prepare_sale_items(db, raw_items, is_member=bool(customer))
        except ValueError as exc:
            return jsonify({"ok": False, "error": str(exc)}), 400

        member_discount = int(round(subtotal * parse_float(customer["discount"]))) if customer else 0
        member_discount = int(round(member_discount / 100))
        sale_discount = max(0, min(parse_int(payload.get("discount", 0)) + member_discount, subtotal))
        discounted_subtotal = subtotal - sale_discount
        total, rounding = round_total(discounted_subtotal)
        paid = parse_int(payload.get("paid", 0))
        if paid < total and not customer:
            return jsonify({"ok": False, "error": "Pembayaran kurang hanya boleh untuk member/piutang."}), 400
        donation = max(0, parse_int(payload.get("donation", 0)))
        donation = min(donation, max(0, paid - total))
        sale_paid = min(paid, total)
        outstanding = max(0, total - sale_paid)
        change_amount = max(0, paid - total - donation)
        member_name = customer["name"] if customer else str(member.get("name", "")).strip()
        member_address = customer_address(customer) if customer else str(member.get("address", "")).strip()
        point_redeemed = max(0, parse_int(payload.get("points_redeem", 0)))
        if point_redeemed and not customer:
            return jsonify({"ok": False, "error": "Poin hanya dapat digunakan oleh member."}), 400
        if customer and point_redeemed > int(customer["points"] or 0):
            return jsonify({"ok": False, "error": "Poin member tidak mencukupi."}), 400
        point_rate = max(1, parse_int(store_settings().get("point_earn_per", 100000)))
        point_earned = int(discounted_subtotal // point_rate) if customer else 0
        now = datetime.now(WIB)
        locked_cashier = db.execute(
            "SELECT last_note FROM cashiers WHERE id = ? FOR UPDATE", (session["cashier_id"],)
        ).fetchone()
        sale_no = next_sale_no_from_value(locked_cashier["last_note"], register_no())
        note_no = int(sale_no.split("-")[0])

        cursor = db.execute(
            """
            INSERT INTO sales
              (sale_no, register_no, cashier_id, sale_date, member_code, member_name,
               member_address, subtotal, discount, donation, rounding, total, paid,
               change_amount, print_receipt, status, point_earned, point_redeemed)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING id
            """,
            (
                sale_no,
                register_no(),
                session["cashier_id"],
                now.date().isoformat(),
                member_code or None,
                member_name or None,
                member_address or None,
                subtotal,
                sale_discount,
                donation,
                rounding,
                total,
                sale_paid,
                change_amount,
                1 if payload.get("receipt_action", "print") == "print" else 0,
                "credit" if outstanding else "paid",
                point_earned,
                point_redeemed,
            ),
        )
        sale_id = cursor.fetchone()["id"]

        for item in sale_items:
            product = item["product"]
            db.execute(
                """
                INSERT INTO sale_items
                  (sale_id, product_id, barcode, product_name, qty, unit, cost_price,
                   price, discount, subtotal)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    sale_id,
                    product["id"],
                    product["barcode"] or product["legacy_code"] or "",
                    product["name"],
                    item["qty"],
                    product["unit"] or "PCS",
                    product["cost_price"] or 0,
                    item["price"],
                    item["discount"],
                    item["subtotal"],
                ),
            )
            if product["id"]:
                db.execute(
                    "UPDATE products SET stock = stock - ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                    (item["qty"], product["id"]),
                )

        if customer:
            if outstanding:
                db.execute("UPDATE customers SET balance = balance + ? WHERE code = ?", (outstanding, member_code))
            if point_earned or point_redeemed:
                db.execute(
                    "UPDATE customers SET points = points + ? - ? WHERE code = ?",
                    (point_earned, point_redeemed, member_code),
                )
                db.execute(
                    """
                    INSERT INTO customer_point_ledger
                      (customer_code, sale_id, trans_date, points_in, points_out, description)
                    VALUES (?, ?, ?, ?, ?, ?)
                    """,
                    (member_code, sale_id, now.date().isoformat(), point_earned, point_redeemed, f"Penjualan {sale_no}"),
                )

        db.execute(
            "UPDATE cashiers SET last_note = ? WHERE id = ?",
            (note_no, session["cashier_id"]),
        )
        held_sale_id = parse_int(payload.get("held_sale_id", 0))
        if held_sale_id:
            db.execute(
                """
                DELETE FROM held_sales
                WHERE id = ? AND register_no = ? AND cashier_id = ?
                """,
                (held_sale_id, register_no(), session["cashier_id"]),
            )
        db.commit()

        receipt = build_receipt(
            sale_no=sale_no,
            cashier=session.get("cashier_name", "-"),
            register=register_no(),
            store=store_settings(),
            trans_time=now,
            member={"code": member_code, "name": member_name, "address": member_address},
            items=sale_items,
            subtotal=subtotal,
            discount=sale_discount,
            rounding=rounding,
            total=total,
            paid=sale_paid,
            change_amount=change_amount,
            donation=donation,
            outstanding=outstanding,
            point_earned=point_earned,
        )
        return jsonify(
            {
                "ok": True,
                "sale_id": sale_id,
                "sale_no": sale_no,
                "receipt": receipt,
                "next_sale_no": next_sale_no(session["cashier_id"], register_no()),
            }
        )

    @app.route("/api/held-sales", methods=["GET"])
    @cashier_required
    def api_held_sales_list():
        rows = get_db().execute(
            """
            SELECT id, cashier_name, member_code, member_name, subtotal, sale_discount,
                   total, item_count, created_at, updated_at
            FROM held_sales
            WHERE register_no = ? AND cashier_id = ?
            ORDER BY updated_at DESC, id DESC
            """,
            (register_no(), session["cashier_id"]),
        ).fetchall()
        return jsonify(
            {
                "items": [
                    {
                        "id": row["id"],
                        "cashier_name": row["cashier_name"],
                        "member_code": row["member_code"] or "",
                        "member_name": row["member_name"] or "",
                        "subtotal": row["subtotal"],
                        "discount": row["sale_discount"],
                        "total": row["total"],
                        "item_count": row["item_count"],
                        "created_at": row["created_at"],
                        "updated_at": row["updated_at"],
                    }
                    for row in rows
                ]
            }
        )

    @app.route("/api/held-sales", methods=["POST"])
    @cashier_required
    def api_held_sales_create():
        payload = request.get_json(silent=True) or {}
        raw_items = payload.get("items") or []
        if not raw_items:
            return jsonify({"ok": False, "error": "Belum ada item untuk ditunda."}), 400

        db = get_db()
        try:
            member_code = str((payload.get("member") or {}).get("code", "")).strip()
            sale_items, subtotal = prepare_sale_items(db, raw_items, is_member=bool(member_code))
        except ValueError as exc:
            return jsonify({"ok": False, "error": str(exc)}), 400

        sale_discount = max(0, min(parse_int(payload.get("discount", 0)), subtotal))
        total, _rounding = round_total(subtotal - sale_discount)
        member = payload.get("member") or {}
        member_code = str(member.get("code", "")).strip()
        member_name = str(member.get("name", "")).strip()
        member_address = str(member.get("address", "")).strip()
        item_count = sum(item["qty"] for item in sale_items)
        item_payload = [
            {
                "id": item["product"]["id"],
                "barcode": item["product"]["barcode"] or item["product"]["legacy_code"] or "",
                "legacy_code": item["product"]["legacy_code"] or "",
                "name": item["product"]["name"],
                "qty": item["qty"],
                "price": item["price"],
                "discount": item["discount"],
                "custom": item["product"]["id"] is None,
                "manual_price": bool(raw_items[index].get("manual_price")),
            }
            for index, item in enumerate(sale_items)
        ]
        cursor = db.execute(
            """
            INSERT INTO held_sales
              (register_no, cashier_id, cashier_name, member_code, member_name,
               member_address, sale_discount, items_json, subtotal, total, item_count)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING id
            """,
            (
                register_no(),
                session["cashier_id"],
                session.get("cashier_name", ""),
                member_code or None,
                member_name or None,
                member_address or None,
                sale_discount,
                json.dumps(item_payload),
                subtotal,
                total,
                item_count,
            ),
        )
        new_held_sale_id = cursor.fetchone()["id"]
        held_sale_id = parse_int(payload.get("held_sale_id", 0))
        if held_sale_id:
            db.execute(
                """
                DELETE FROM held_sales
                WHERE id = ? AND register_no = ? AND cashier_id = ? AND id <> ?
                """,
                (held_sale_id, register_no(), session["cashier_id"], new_held_sale_id),
            )
        db.commit()
        return jsonify({"ok": True, "held_sale_id": new_held_sale_id})

    @app.route("/api/held-sales/<int:held_sale_id>", methods=["GET"])
    @cashier_required
    def api_held_sale_detail(held_sale_id):
        row = get_db().execute(
            """
            SELECT id, member_code, member_name, member_address, sale_discount,
                   items_json, subtotal, total, item_count
            FROM held_sales
            WHERE id = ? AND register_no = ? AND cashier_id = ?
            """,
            (held_sale_id, register_no(), session["cashier_id"]),
        ).fetchone()
        if not row:
            return jsonify({"ok": False, "error": "Transaksi tunda tidak ditemukan."}), 404

        return jsonify(
            {
                "ok": True,
                "held_sale": {
                    "id": row["id"],
                    "member": {
                        "code": row["member_code"] or "",
                        "name": row["member_name"] or "",
                        "address": row["member_address"] or "",
                    },
                    "discount": row["sale_discount"],
                    "items": json.loads(row["items_json"] or "[]"),
                    "subtotal": row["subtotal"],
                    "total": row["total"],
                    "item_count": row["item_count"],
                },
            }
        )

    @app.route("/api/held-sales/<int:held_sale_id>", methods=["DELETE"])
    @cashier_required
    def api_held_sale_delete(held_sale_id):
        get_db().execute(
            """
            DELETE FROM held_sales
            WHERE id = ? AND register_no = ? AND cashier_id = ?
            """,
            (held_sale_id, register_no(), session["cashier_id"]),
        )
        get_db().commit()
        return jsonify({"ok": True})

    @app.route("/kasir/rekap")
    @cashier_required
    def rekap_kasir():
        trans_date = request.args.get("tanggal", date.today().isoformat())
        rows = get_db().execute(
            """
            SELECT s.id, s.sale_no, s.member_name, s.subtotal, s.discount, s.donation,
                   s.rounding, s.total, s.paid, s.change_amount, s.status, s.created_at,
                   COUNT(si.id) AS item_count
            FROM sales s
            LEFT JOIN sale_items si ON si.sale_id = s.id
            WHERE s.sale_date = ? AND s.cashier_id = ?
            GROUP BY s.id
            ORDER BY s.id DESC
            """,
            (trans_date, session["cashier_id"]),
        ).fetchall()
        summary = {
            "sales": sum(row["total"] for row in rows if row["status"] != "void"),
            "cash": sum(row["paid"] + row["donation"] for row in rows if row["status"] != "void"),
            "credit": sum(max(0, row["total"] - row["paid"]) for row in rows if row["status"] != "void"),
        }
        return render_template("rekap_kasir.html", rows=rows, summary=summary, trans_date=trans_date, date_label=iso_to_dmy(trans_date))

    @app.route("/api/sales/<int:sale_id>/receipt")
    @cashier_required
    def api_sale_receipt(sale_id):
        sale = get_db().execute(
            "SELECT * FROM sales WHERE id = ? AND cashier_id = ?", (sale_id, session["cashier_id"])
        ).fetchone()
        if not sale:
            return jsonify({"ok": False, "error": "Nota tidak ditemukan."}), 404
        return jsonify({"ok": True, "receipt": receipt_for_sale(get_db(), sale), "sale": sale_to_json(sale)})

    @app.route("/api/sales/<int:sale_id>/void", methods=["POST"])
    @cashier_required
    def api_sale_void(sale_id):
        db = get_db()
        sale = db.execute(
            "SELECT * FROM sales WHERE id = ? AND cashier_id = ? FOR UPDATE", (sale_id, session["cashier_id"])
        ).fetchone()
        if not sale:
            return jsonify({"ok": False, "error": "Nota tidak ditemukan."}), 404
        if sale["status"] == "void":
            return jsonify({"ok": False, "error": "Nota sudah dibatalkan."}), 400
        items = db.execute("SELECT product_id, qty FROM sale_items WHERE sale_id = ?", (sale_id,)).fetchall()
        for item in items:
            if item["product_id"]:
                db.execute("UPDATE products SET stock = stock + ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?", (item["qty"], item["product_id"]))
        if sale["member_code"]:
            outstanding = max(0, sale["total"] - sale["paid"])
            db.execute(
                "UPDATE customers SET balance = GREATEST(0, balance - ?), points = GREATEST(0, points - ? + ?) WHERE code = ?",
                (outstanding, sale["point_earned"], sale["point_redeemed"], sale["member_code"]),
            )
            if sale["point_earned"] or sale["point_redeemed"]:
                db.execute(
                    """INSERT INTO customer_point_ledger
                       (customer_code, sale_id, trans_date, points_in, points_out, description)
                       VALUES (?, ?, ?, ?, ?, ?)""",
                    (sale["member_code"], sale_id, date.today().isoformat(), sale["point_redeemed"], sale["point_earned"], f"Pembatalan {sale['sale_no']}"),
                )
        reason = str((request.get_json(silent=True) or {}).get("reason", "Dibatalkan kasir")).strip()[:200]
        db.execute(
            "UPDATE sales SET status = 'void', voided_at = CURRENT_TIMESTAMP, voided_by = ?, void_reason = ? WHERE id = ?",
            (session.get("cashier_name", ""), reason, sale_id),
        )
        db.commit()
        return jsonify({"ok": True})

    @app.route("/api/customer-payments", methods=["POST"])
    @cashier_required
    def api_customer_payment():
        payload = request.get_json(silent=True) or {}
        code = str(payload.get("customer_code", "")).strip()
        amount = parse_int(payload.get("amount", 0))
        if not code or amount <= 0:
            return jsonify({"ok": False, "error": "Kode pelanggan dan nominal pembayaran wajib diisi."}), 400
        db = get_db()
        customer = db.execute("SELECT * FROM customers WHERE code = ? FOR UPDATE", (code,)).fetchone()
        if not customer:
            return jsonify({"ok": False, "error": "Pelanggan tidak ditemukan."}), 404
        accepted = min(amount, int(customer["balance"] or 0))
        if accepted <= 0:
            return jsonify({"ok": False, "error": "Pelanggan tidak memiliki piutang."}), 400
        payment_date = date.today().isoformat()
        description = str(payload.get("description", "Pembayaran piutang")).strip()[:200]
        db.execute("UPDATE customers SET balance = balance - ? WHERE code = ?", (accepted, code))
        db.execute(
            "INSERT INTO customer_payments (customer_code, cashier_id, payment_date, amount, description) VALUES (?, ?, ?, ?, ?)",
            (code, session["cashier_id"], payment_date, accepted, description),
        )
        db.execute(
            """INSERT INTO cash_transactions
               (trans_date, cashier_name, account_code, account_name, description, amount, side, profit_loss)
               VALUES (?, ?, '10', 'PEMASUKAN', ?, ?, 'D', 'T')""",
            (payment_date, session.get("cashier_name", ""), f"Piutang {code}: {description}", accepted),
        )
        db.commit()
        return jsonify({"ok": True, "paid": accepted, "balance": int(customer["balance"]) - accepted})

    @app.route("/api/customers/<code>/points/withdraw", methods=["POST"])
    @cashier_required
    def api_points_withdraw(code):
        payload = request.get_json(silent=True) or {}
        points = parse_int(payload.get("points", 0))
        cash_value = parse_int(payload.get("cash_value", 0))
        if points <= 0 and cash_value <= 0:
            return jsonify({"ok": False, "error": "Jumlah poin atau nilai voucher wajib diisi."}), 400
        db = get_db()
        customer = db.execute("SELECT * FROM customers WHERE code = ? FOR UPDATE", (code,)).fetchone()
        if not customer:
            return jsonify({"ok": False, "error": "Pelanggan tidak ditemukan."}), 404
        if points > int(customer["points"] or 0):
            return jsonify({"ok": False, "error": "Poin pelanggan tidak mencukupi."}), 400
        description = str(payload.get("description", "Pengambilan poin")).strip()[:200]
        db.execute("UPDATE customers SET points = points - ? WHERE code = ?", (points, code))
        db.execute(
            """INSERT INTO customer_point_ledger
               (customer_code, trans_date, points_out, cash_value, description)
               VALUES (?, ?, ?, ?, ?)""",
            (code, date.today().isoformat(), points, cash_value, description),
        )
        db.commit()
        return jsonify({"ok": True, "points": int(customer["points"]) - points})

    return app


def get_db():
    if "db" not in g:
        g.db = connect(DATABASE_URL)
    return g.db


def init_db():
    db = get_db()
    db.executescript(SCHEMA.read_text(encoding="utf-8"))
    migrate_db(db)
    import_legacy_stock_if_needed(db)
    import_legacy_customers_if_needed(db)
    seed(db)
    ensure_default_cashier_passwords(db)


def migrate_db(db):
    ensure_columns(
        db,
        "sales",
        {
            "member_code": "TEXT",
            "member_name": "TEXT",
            "member_address": "TEXT",
            "point_earned": "INTEGER NOT NULL DEFAULT 0",
            "point_redeemed": "INTEGER NOT NULL DEFAULT 0",
            "voided_at": "TIMESTAMPTZ",
            "void_reason": "TEXT NOT NULL DEFAULT ''",
            "voided_by": "TEXT NOT NULL DEFAULT ''",
        },
    )
    ensure_columns(
        db,
        "products",
        {
            "member_price": "INTEGER NOT NULL DEFAULT 0",
            "tier3_qty": "REAL NOT NULL DEFAULT 0",
            "tier3_price": "INTEGER NOT NULL DEFAULT 0",
            "tier4_qty": "REAL NOT NULL DEFAULT 0",
            "tier4_price": "INTEGER NOT NULL DEFAULT 0",
            "tier5_qty": "REAL NOT NULL DEFAULT 0",
            "tier5_price": "INTEGER NOT NULL DEFAULT 0",
        },
    )


def ensure_columns(db, table, columns):
    existing = {
        row["column_name"]
        for row in db.execute(
            """
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema = 'public' AND table_name = ?
            """,
            (table,),
        )
    }
    for name, definition in columns.items():
        if name not in existing:
            db.execute(f"ALTER TABLE {table} ADD COLUMN {name} {definition}")


def import_legacy_stock_if_needed(db):
    product_count = db.execute("SELECT COUNT(*) AS count FROM products").fetchone()["count"]
    if product_count >= LEGACY_STOK_MIN_ROWS:
        return

    stok_path = legacy_stock_path()
    if not stok_path:
        return

    rows = read_legacy_stock(stok_path)
    if len(rows) < LEGACY_STOK_MIN_ROWS:
        return

    for row in rows:
        existing = None
        if row["legacy_code"]:
            existing = db.execute(
                "SELECT id FROM products WHERE legacy_code = ? ORDER BY id LIMIT 1",
                (row["legacy_code"],),
            ).fetchone()
        if existing is None and row["barcode"]:
            existing = db.execute(
                """
                SELECT id FROM products
                WHERE barcode = ? AND (legacy_code IS NULL OR legacy_code = '')
                ORDER BY id LIMIT 1
                """,
                (row["barcode"],),
            ).fetchone()

        values = (
            row["barcode"],
            row["legacy_code"],
            row["group_code"],
            row["name"],
            row["unit"],
            row["stock"],
            row["cost_price"],
            row["wholesale_price"],
            row["retail_price"],
            row["supplier_code"],
            row["member_price"],
            row["tier3_qty"], row["tier3_price"],
            row["tier4_qty"], row["tier4_price"],
            row["tier5_qty"], row["tier5_price"],
        )
        if existing:
            db.execute(
                """
                UPDATE products
                SET barcode = ?, legacy_code = ?, group_code = ?, name = ?, unit = ?,
                    stock = ?, cost_price = ?, wholesale_price = ?, retail_price = ?,
                    supplier_code = ?, member_price = ?, tier3_qty = ?, tier3_price = ?,
                    tier4_qty = ?, tier4_price = ?, tier5_qty = ?, tier5_price = ?,
                    updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                """,
                values + (existing["id"],),
            )
        else:
            db.execute(
                """
                INSERT INTO products
                  (barcode, legacy_code, group_code, name, unit, stock, cost_price,
                   wholesale_price, retail_price, supplier_code, member_price,
                   tier3_qty, tier3_price, tier4_qty, tier4_price, tier5_qty, tier5_price)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                values,
            )

    remove_unreferenced_duplicate_products(db)
    db.commit()


def import_legacy_customers_if_needed(db):
    customer_count = db.execute("SELECT COUNT(*) AS count FROM customers").fetchone()["count"]
    if customer_count > 0:
        return

    cust_path = legacy_customer_path()
    if not cust_path:
        return

    rows = read_legacy_customers(cust_path)
    for row in rows:
        db.execute(
            """
            INSERT INTO customers
              (code, name, address, city, phone, discount, points, balance, notes)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(code) DO UPDATE SET
              name = excluded.name,
              address = excluded.address,
              city = excluded.city,
              phone = excluded.phone,
              discount = excluded.discount,
              points = excluded.points,
              balance = excluded.balance,
              notes = excluded.notes,
              updated_at = CURRENT_TIMESTAMP
            """,
            (
                row["code"],
                row["name"],
                row["address"],
                row["city"],
                row["phone"],
                row["discount"],
                row["points"],
                row["balance"],
                row["notes"],
            ),
        )
    db.commit()


def legacy_stock_path():
    candidates = []
    env_path = os.environ.get("TOKO_STOK_DTA", "").strip()
    if env_path:
        candidates.append(Path(env_path))
    candidates.extend(
        [
            BASE_DIR.parent / "Toko" / "STOK.DTA",
            BASE_DIR.parent / "toko-harbour-build" / "STOK.DTA",
            BASE_DIR / "data" / "STOK.DTA",
        ]
    )
    for path in candidates:
        if path.is_file():
            return path
    return None


def legacy_customer_path():
    candidates = []
    env_path = os.environ.get("TOKO_CUST_DTA", "").strip()
    if env_path:
        candidates.append(Path(env_path))
    candidates.extend(
        [
            BASE_DIR.parent / "toko-harbour-build" / "CUST.DTA",
            BASE_DIR.parent / "Toko" / "Toko2_backup" / "CUST.DTA",
            BASE_DIR.parent / "Toko" / "CUST.DTA",
            BASE_DIR / "data" / "CUST.DTA",
        ]
    )
    available = [path for path in candidates if path.is_file()]
    if not available:
        return None
    return max(available, key=lambda path: legacy_record_count(path))


def legacy_record_count(path):
    data = path.read_bytes()
    if len(data) < 8:
        return 0
    return int.from_bytes(data[4:8], "little")


def read_legacy_stock(path):
    data = path.read_bytes()
    if len(data) < 32:
        return []

    record_count = int.from_bytes(data[4:8], "little")
    header_len = int.from_bytes(data[8:10], "little")
    record_len = int.from_bytes(data[10:12], "little")
    fields = dbf_fields(data, header_len)
    rows = []

    for index in range(record_count):
        start = header_len + (index * record_len)
        record = data[start : start + record_len]
        if len(record) < record_len or record[:1] == b"*":
            continue
        values = {field["name"]: dbf_value(record, field) for field in fields}
        name = values.get("NAMA_BRG", "").strip()
        legacy_code = values.get("KODE_BRG", "").strip()
        if not name or not legacy_code:
            continue
        rows.append(
            {
                "barcode": values.get("KODE_BAR", "").strip() or legacy_code,
                "legacy_code": legacy_code,
                "group_code": values.get("KODE_KLB", "").strip() or "00",
                "name": name,
                "unit": values.get("SATUAN", "").strip() or values.get("SATUAN_B", "").strip() or "PCS",
                "stock": dbf_number(values.get("STOK_K")),
                "cost_price": int(round(dbf_number(values.get("HARGA_B")))),
                "wholesale_price": int(round(dbf_number(values.get("HARGA_21")))),
                "retail_price": int(round(dbf_number(values.get("HARGA_2")))),
                "supplier_code": values.get("KODE_SPL", "").strip(),
                "member_price": int(round(dbf_number(values.get("HARGA_21")))),
                "tier3_qty": dbf_number(values.get("BATAS_3")),
                "tier3_price": int(round(dbf_number(values.get("HARGA_3")))),
                "tier4_qty": dbf_number(values.get("BATAS_4")),
                "tier4_price": int(round(dbf_number(values.get("HARGA_4")))),
                "tier5_qty": dbf_number(values.get("BATAS_5")),
                "tier5_price": int(round(dbf_number(values.get("HARGA_5")))),
            }
        )
    return rows


def read_legacy_customers(path):
    data = path.read_bytes()
    if len(data) < 32:
        return []

    record_count = int.from_bytes(data[4:8], "little")
    header_len = int.from_bytes(data[8:10], "little")
    record_len = int.from_bytes(data[10:12], "little")
    fields = dbf_fields(data, header_len)
    rows = []

    for index in range(record_count):
        start = header_len + (index * record_len)
        record = data[start : start + record_len]
        if len(record) < record_len or record[:1] == b"*":
            continue
        values = {field["name"]: dbf_value(record, field) for field in fields}
        code = values.get("KODE_LGN", "").strip()
        name = values.get("NAMA_LGN", "").strip()
        if not code or not name:
            continue
        rows.append(
            {
                "code": code,
                "name": name,
                "address": values.get("ALMT_LGN", "").strip(),
                "city": values.get("KOTA_LGN", "").strip(),
                "phone": values.get("TELP_LGN", "").strip(),
                "discount": dbf_number(values.get("DISKON")),
                "points": int(round(dbf_number(values.get("SO_POINT")))),
                "balance": int(round(dbf_number(values.get("RP_SALDO")))),
                "notes": values.get("KETERANGAN", "").strip(),
            }
        )
    return rows


def dbf_fields(data, header_len):
    fields = []
    offset = 1
    position = 32
    while position < header_len and data[position] != 0x0D:
        name = data[position : position + 11].split(b"\0", 1)[0].decode("ascii", "ignore")
        field_type = chr(data[position + 11])
        length = data[position + 16]
        decimal_count = data[position + 17]
        fields.append(
            {
                "name": name,
                "type": field_type,
                "length": length,
                "decimal_count": decimal_count,
                "offset": offset,
            }
        )
        offset += length
        position += 32
    return fields


def dbf_value(record, field):
    start = field["offset"]
    end = start + field["length"]
    raw = record[start:end]
    return raw.decode("cp437", "replace").strip()


def dbf_number(value):
    try:
        return float(str(value or "0").strip() or 0)
    except ValueError:
        return 0.0


def remove_unreferenced_duplicate_products(db):
    duplicates = db.execute(
        """
        SELECT legacy_code
        FROM products
        WHERE legacy_code IS NOT NULL AND legacy_code <> ''
        GROUP BY legacy_code
        HAVING COUNT(*) > 1
        """
    ).fetchall()
    for duplicate in duplicates:
        rows = db.execute(
            """
            SELECT id FROM products
            WHERE legacy_code = ?
            ORDER BY id
            """,
            (duplicate["legacy_code"],),
        ).fetchall()
        for row in rows[1:]:
            used = db.execute(
                "SELECT COUNT(*) AS count FROM sale_items WHERE product_id = ?",
                (row["id"],),
            ).fetchone()["count"]
            if used:
                continue
            db.execute("DELETE FROM products WHERE id = ?", (row["id"],))

    db.execute(
        """
        DELETE FROM products
        WHERE (legacy_code IS NULL OR legacy_code = '')
          AND id NOT IN (SELECT COALESCE(product_id, 0) FROM sale_items)
          AND EXISTS (
            SELECT 1 FROM products p2
            WHERE p2.id <> products.id
              AND p2.legacy_code IS NOT NULL
              AND p2.legacy_code <> ''
              AND (
                (products.barcode IS NOT NULL AND products.barcode <> '' AND p2.barcode = products.barcode)
                OR p2.name = products.name
              )
          )
        """
    )


def seed(db):
    settings = [
        ("store_name", "DUA PUTRA JAYA"),
        ("store_city", "KOTA TEGAL"),
        ("point_earn_per", "100000"),
    ]
    db.executemany(
        """
        INSERT INTO store_settings (key, value)
        VALUES (?, ?) ON CONFLICT DO NOTHING
        """,
        settings,
    )

    cashiers = [
        ("01", "1", "ROYANI", 63),
        ("02", "2", "MAKSUM", 26),
        ("03", "3", "RIZQIFAUZI", 3),
        ("04", "4", "ZAM-ZAMI", 0),
    ]
    db.executemany(
        """
        INSERT INTO cashiers
          (legacy_no, code, name, password, last_note)
        VALUES (?, ?, ?, '00', ?) ON CONFLICT DO NOTHING
        """,
        cashiers,
    )

    registers = [("1", "A", 58), ("2", "A", 58), ("3", "A", 50)]
    db.executemany(
        """
        INSERT INTO registers
          (register_no, mode, receipt_width)
        VALUES (?, ?, ?) ON CONFLICT DO NOTHING
        """,
        registers,
    )

    accounts = [
        ("10", "PEMASUKAN", "D", "T"),
        ("20", "PENGELUARAN", "K", "T"),
        ("30", "BIAYA ADM/UMUM", "K", "Y"),
        ("40", "BIAYA GAJI", "K", "Y"),
        ("50", "BIAYA LISTIK", "K", "Y"),
    ]
    db.executemany(
        """
        INSERT INTO cash_accounts
          (code, name, side, profit_loss)
        VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING
        """,
        accounts,
    )

    product_count = db.execute("SELECT COUNT(*) AS count FROM products").fetchone()["count"]
    if product_count == 0:
        products = [
            ("BN", "00", "BERAS SPHP / KG", "PCS", -606, 13500, 14000),
            ("8991906106311", "00", "APEL ROYAL", "PCS", -3, 15000, 15500),
            ("7118441200327", "11", "ABC SAMBAL ASLI 135ML", "PCS", 0, 6200, 6500),
            ("7118441200872", "11", "ABC SAMBAL EXT.PDS 135ML", "PCS", -358, 7500, 8000),
        ]
        db.executemany(
            """
            INSERT INTO products
              (barcode, group_code, name, unit, stock, cost_price, retail_price)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            products,
        )
    db.commit()


def ensure_default_cashier_passwords(db):
    rows = db.execute("SELECT id, code, name, password FROM cashiers").fetchall()
    for row in rows:
        stored = str(row["password"] or "")
        if stored and stored not in {"00", "0"}:
            continue
        db.execute(
            "UPDATE cashiers SET password = ? WHERE id = ?",
            (generate_password_hash(default_cashier_password(row)), row["id"]),
        )
    db.commit()


def store_settings():
    rows = get_db().execute("SELECT key, value FROM store_settings").fetchall()
    values = {row["key"]: row["value"] for row in rows}
    return {
        "name": values.get("store_name", "DUA PUTRA JAYA"),
        "city": values.get("store_city", "KOTA TEGAL"),
        "point_earn_per": values.get("point_earn_per", "100000"),
    }


def product_to_json(row):
    return {
        "id": row["id"],
        "barcode": row["barcode"] or row["legacy_code"] or "",
        "legacy_code": row["legacy_code"] or "",
        "group_code": row["group_code"] or "00",
        "name": row["name"],
        "unit": row["unit"] or "PCS",
        "stock": row["stock"],
        "price": row["retail_price"] or 0,
        "member_price": row.get("member_price", 0) or 0,
        "tier3_qty": row.get("tier3_qty", 0) or 0,
        "tier3_price": row.get("tier3_price", 0) or 0,
        "tier4_qty": row.get("tier4_qty", 0) or 0,
        "tier4_price": row.get("tier4_price", 0) or 0,
        "tier5_qty": row.get("tier5_qty", 0) or 0,
        "tier5_price": row.get("tier5_price", 0) or 0,
    }


def product_to_stock_json(row):
    return {
        "id": row["id"],
        "barcode": row["barcode"] or "",
        "legacy_code": row["legacy_code"] or "",
        "group_code": row["group_code"] or "00",
        "name": row["name"] or "",
        "unit": row["unit"] or "PCS",
        "stock": row["stock"] or 0,
        "cost_price": row["cost_price"] or 0,
        "wholesale_price": row["wholesale_price"] or 0,
        "retail_price": row["retail_price"] or 0,
        "supplier_code": row["supplier_code"] or "",
        "rack_code": row["rack_code"] or "",
    }


def customer_to_json(row):
    return {
        "code": row["code"],
        "name": row["name"],
        "address": row["address"] or "",
        "city": row["city"] or "",
        "phone": row["phone"] or "",
        "discount": row["discount"] or 0,
        "points": row["points"] or 0,
        "balance": row["balance"] or 0,
        "notes": row["notes"] or "",
    }


def sale_to_json(row):
    return {
        "id": row["id"],
        "sale_no": row["sale_no"],
        "total": row["total"],
        "paid": row["paid"],
        "donation": row["donation"],
        "status": row["status"],
        "member_code": row["member_code"] or "",
        "created_at": row["created_at"],
    }


def receipt_for_sale(db, sale):
    rows = db.execute(
        """
        SELECT si.*, p.legacy_code
        FROM sale_items si
        LEFT JOIN products p ON p.id = si.product_id
        WHERE si.sale_id = ? ORDER BY si.id
        """,
        (sale["id"],),
    ).fetchall()
    items = [
        {
            "product": {
                "name": row["product_name"],
                "barcode": row["barcode"] or row["legacy_code"] or "",
            },
            "qty": row["qty"],
            "price": row["price"],
            "discount": row["discount"],
            "subtotal": row["subtotal"],
        }
        for row in rows
    ]
    created_at = sale["created_at"]
    if isinstance(created_at, str):
        created_at = datetime.fromisoformat(created_at)
    return build_receipt(
        sale_no=sale["sale_no"],
        cashier=db.execute("SELECT name FROM cashiers WHERE id = ?", (sale["cashier_id"],)).fetchone()["name"],
        register=sale["register_no"],
        store=store_settings(),
        trans_time=created_at,
        member={"code": sale["member_code"] or "", "name": sale["member_name"] or "", "address": sale["member_address"] or ""},
        items=items,
        subtotal=sale["subtotal"],
        discount=sale["discount"],
        rounding=sale["rounding"],
        total=sale["total"],
        paid=sale["paid"],
        change_amount=sale["change_amount"],
        donation=sale["donation"],
        outstanding=max(0, sale["total"] - sale["paid"]),
        point_earned=sale["point_earned"],
    )


def sale_price(product, qty, is_member=False):
    price = int(product["member_price"] or 0) if is_member else int(product["retail_price"] or 0)
    if price <= 0:
        price = int(product["retail_price"] or 0)
    for quantity_field, price_field in (("tier3_qty", "tier3_price"), ("tier4_qty", "tier4_price"), ("tier5_qty", "tier5_price")):
        threshold = parse_float(product[quantity_field])
        tier_price = parse_int(product[price_field])
        if threshold > 0 and qty >= threshold and tier_price > 0:
            price = tier_price
    return price


def prepare_sale_items(db, raw_items, is_member=False):
    sale_items = []
    subtotal = 0
    for raw in raw_items:
        product_id = raw.get("id")
        qty = parse_float(raw.get("qty", 1))
        discount = parse_int(raw.get("discount", 0))
        if qty <= 0:
            raise ValueError("Item transaksi tidak valid.")

        if raw.get("custom"):
            name = str(raw.get("name", "")).strip().upper()
            price = parse_int(raw.get("price", 0))
            if not name or price < 0:
                raise ValueError("Nama dan harga item bebas wajib diisi.")
            product = {
                "id": None,
                "barcode": str(raw.get("barcode", "")).strip(),
                "legacy_code": "",
                "name": name,
                "unit": str(raw.get("unit", "PCS")).strip().upper() or "PCS",
                "cost_price": 0,
                "retail_price": price,
            }
            line_total = max(0, int(round(qty * price)) - discount)
            subtotal += line_total
            sale_items.append({"product": product, "qty": qty, "price": price, "discount": discount, "subtotal": line_total})
            continue

        if not product_id:
            raise ValueError("Produk transaksi tidak valid.")

        product = db.execute(
            """
            SELECT id, barcode, legacy_code, name, unit, cost_price, retail_price,
                   member_price, tier3_qty, tier3_price, tier4_qty, tier4_price,
                   tier5_qty, tier5_price
            FROM products
            WHERE id = ?
            """,
            (product_id,),
        ).fetchone()
        if not product:
            raise ValueError("Produk tidak ditemukan.")

        price = sale_price(product, qty, is_member)
        if raw.get("manual_price"):
            manual_price = parse_int(raw.get("price", 0))
            if manual_price < 0:
                raise ValueError("Harga manual tidak valid.")
            price = manual_price
        line_total = max(0, int(round(qty * price)) - discount)
        subtotal += line_total
        sale_items.append(
            {
                "product": product,
                "qty": qty,
                "price": price,
                "discount": discount,
                "subtotal": line_total,
            }
        )
    return sale_items, subtotal


def build_receipt(
    sale_no,
    cashier,
    register,
    store,
    trans_time,
    member,
    items,
    subtotal,
    discount,
    rounding,
    total,
    paid,
    change_amount,
    donation=0,
    outstanding=0,
    point_earned=0,
):
    width = 42
    lines = [
        store["name"].center(width),
        store["city"].center(width),
        "-" * width,
        f"Nota : {sale_no}",
        f"Kassa: {register}  Kasir: {cashier}",
        trans_time.strftime("%d-%m-%Y %H:%M:%S"),
    ]
    if member.get("code") or member.get("name"):
        lines.extend(
            [
                f"Member: {member.get('code', '')} {member.get('name', '')}".strip(),
                f"Almt  : {member.get('address', '')}".rstrip(),
            ]
        )
    lines.append("-" * width)
    for item in items:
        product = item["product"]
        name = product["name"][:width]
        qty_price = f"{format_rupiah(item['qty'])} x {format_rupiah(item['price'])}"
        amount = format_rupiah(item["subtotal"])
        lines.extend(
            [
                name,
                f"{qty_price:<28}{amount:>14}",
            ]
        )
        if item["discount"]:
            lines.append(f"Disc{'':<24}{format_rupiah(item['discount']):>14}")
    lines.extend(
        [
            "-" * width,
            f"{'Subtotal':<28}{format_rupiah(subtotal):>14}",
            f"{'Diskon':<28}{format_rupiah(discount):>14}",
            f"{'Pembulatan':<28}{format_rupiah(rounding):>14}",
            f"{'Total':<28}{format_rupiah(total):>14}",
            f"{'Bayar':<28}{format_rupiah(paid):>14}",
            f"{'Donasi':<28}{format_rupiah(donation):>14}" if donation else "",
            f"{'Piutang':<28}{format_rupiah(outstanding):>14}" if outstanding else "",
            f"{'Poin didapat':<28}{format_rupiah(point_earned):>14}" if point_earned else "",
            f"{'Kembali':<28}{format_rupiah(change_amount):>14}",
            "-" * width,
            "TERIMA KASIH".center(width),
        ]
    )
    return "\n".join(line for line in lines if line)


def cashier_required(view):
    @wraps(view)
    def wrapped_view(**kwargs):
        if "cashier_id" not in session:
            return redirect(url_for("kasir"))
        return view(**kwargs)

    return wrapped_view


def clear_cashier_session():
    session.pop("cashier_id", None)
    session.pop("cashier_name", None)
    session.pop("register_no", None)
    session.pop("print_receipt", None)


def stock_required(view):
    @wraps(view)
    def wrapped_view(**kwargs):
        if "stock_logged_in" not in session:
            return redirect(url_for("stok"))
        return view(**kwargs)

    return wrapped_view


def cashier_or_stock_required(view):
    @wraps(view)
    def wrapped_view(**kwargs):
        if "cashier_id" not in session and "stock_logged_in" not in session:
            return redirect(url_for("desktop"))
        return view(**kwargs)

    return wrapped_view


def cashier_map():
    rows = get_db().execute(
        "SELECT code, name FROM cashiers WHERE active = 1 ORDER BY code"
    ).fetchall()
    return {row["code"]: row["name"] for row in rows}


def default_cashier_password(cashier):
    name = "".join(char for char in str(cashier["name"] or "").lower() if char.isalnum())
    code = str(cashier["code"] or "").strip()
    return f"{name}000{code}"


def cashier_password_matches(cashier, candidate):
    stored = str(cashier["password"] or "")
    if stored.startswith(("scrypt:", "pbkdf2:", "argon2:")):
        return check_password_hash(stored, candidate)
    if stored in {"", "00", "0"}:
        return candidate == default_cashier_password(cashier)
    return candidate == stored


def register_no():
    if session.get("register_no"):
        return str(session["register_no"])
    from_env = os.environ.get("KASSA", "").strip()
    return from_env or "1"


def next_sale_no(cashier_id, current_register_no):
    row = get_db().execute(
        "SELECT last_note FROM cashiers WHERE id = ?",
        (cashier_id,),
    ).fetchone()
    return next_sale_no_from_value(row["last_note"] if row else 0, current_register_no)


def next_sale_no_from_value(last_note, current_register_no):
    next_no = int(last_note or 0) + 1
    if next_no > 99999:
        next_no = 1
    return f"{next_no:05d}-{current_register_no}"


def customer_address(customer):
    if not customer:
        return ""
    return " ".join(part for part in (customer["address"], customer["city"]) if part)


def round_total(total):
    rounded = int(round(total / 100) * 100)
    return rounded, rounded - total


def format_rupiah(value):
    try:
        number = float(value)
    except (TypeError, ValueError):
        number = 0
    return f"{number:,.0f}".replace(",", ".")


def parse_int(value):
    return int(str(value or "0").replace(".", "").replace(",", "") or 0)


def parse_float(value):
    if isinstance(value, (int, float)):
        return float(value)
    raw = str(value or "0").strip()
    if "," in raw:
        normalized = raw.replace(".", "").replace(",", ".")
    elif raw.count(".") == 1 and len(raw.rsplit(".", 1)[1]) != 3:
        normalized = raw
    else:
        normalized = raw.replace(".", "")
    try:
        return float(normalized)
    except ValueError:
        return 0.0


def iso_to_dmy(value):
    try:
        year, month, day = value.split("-")
    except ValueError:
        return date.today().strftime("%d-%m-%Y")
    return f"{day}-{month}-{year}"


def clipper_date_label(value):
    days = ["Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu"]
    return f"{days[value.weekday()]}, {value.strftime('%d-%m-%Y')}"


app = create_app()


if __name__ == "__main__":
    app.run(debug=True, port=5000)
