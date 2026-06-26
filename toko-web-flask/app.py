import os
import sqlite3
from pathlib import Path
from functools import wraps
from datetime import date, datetime, timedelta, timezone
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

from flask import Flask, g, jsonify, redirect, render_template, request, session, url_for


APP_NAME = "MR. FAUZI ZAMI"
BASE_DIR = Path(__file__).resolve().parent
DATABASE = Path(os.environ.get("TOKO_DATABASE", BASE_DIR / "storage" / "toko.sqlite"))
SCHEMA = BASE_DIR / "database" / "schema.sql"
try:
    WIB = ZoneInfo("Asia/Jakarta")
except ZoneInfoNotFoundError:
    WIB = timezone(timedelta(hours=7), "WIB")


def create_app():
    app = Flask(__name__)
    app.config["SECRET_KEY"] = os.environ.get("TOKO_SECRET_KEY", "dev-toko-flask")
    app.config["DATABASE"] = DATABASE

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
        return {
            "app_name": APP_NAME,
            "cashier_name": session.get("cashier_name", "-"),
            "register_no": register_no(),
            "active_path": request.path,
        }

    @app.route("/logout")
    def logout():
        session.pop("cashier_id", None)
        session.pop("cashier_name", None)
        session.pop("register_no", None)
        session.pop("print_receipt", None)
        session.pop("stock_logged_in", None)
        return redirect(url_for("desktop"))

    @app.route("/")
    def desktop():
        return render_template("desktop.html")

    @app.route("/healthz")
    def healthz():
        return jsonify({"ok": True, "app": "toko-web-flask"})

    @app.route("/dashboard")
    @cashier_required
    def dashboard():
        db = get_db()
        product_count = db.execute("SELECT COUNT(*) FROM products").fetchone()[0]
        minus_count = db.execute("SELECT COUNT(*) FROM products WHERE stock < 0").fetchone()[0]
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

            session["cashier_id"] = cashier["id"]
            session["cashier_name"] = cashier["name"]
            session["register_no"] = request.form.get("register_no", "").strip() or "1"
            session["print_receipt"] = request.form.get("print_receipt", "1")
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
            print_receipt=session.get("print_receipt", "1") == "1",
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
            SELECT COALESCE(SUM(total), 0)
            FROM sales
            WHERE sale_date = ? AND cashier_id = ?
            """,
            (trans_date, session.get("cashier_id")),
        ).fetchone()[0]
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
            "SELECT COUNT(*) FROM cash_transactions WHERE account_code = ?",
            (code,),
        ).fetchone()[0]
        if used == 0:
            db.execute("DELETE FROM cash_accounts WHERE code = ?", (code,))
            db.commit()
        return redirect(url_for("perkiraan_kas"))

    @app.route("/stok", methods=["GET", "POST"])
    def stok():
        if request.method == "POST" and "stock_logged_in" not in session:
            password = request.form.get("password", "").strip()
            if password != "00":
                return render_template("stok_login.html", error="Password stok salah.")
            session["stock_logged_in"] = True
            return redirect(url_for("stok"))

        if "stock_logged_in" not in session:
            return render_template("stok_login.html", error="")

        return stok_barang_view()

    @app.route("/stok-barang", methods=["GET", "POST"])
    def stok_barang():
        if "stock_logged_in" not in session and "cashier_id" not in session:
            return redirect(url_for("stok"))
        return stok_barang_view()

    def stok_barang_view():
        db = get_db()
        if request.method == "POST":
            db.execute(
                """
                INSERT INTO products
                  (barcode, group_code, name, unit, stock, cost_price, wholesale_price, retail_price)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    request.form.get("barcode", "").strip(),
                    request.form.get("group_code", "00").strip() or "00",
                    request.form.get("name", "").strip(),
                    request.form.get("unit", "PCS").strip() or "PCS",
                    parse_float(request.form.get("stock", "0")),
                    parse_int(request.form.get("cost_price", "0")),
                    parse_int(request.form.get("wholesale_price", "0")),
                    parse_int(request.form.get("retail_price", "0")),
                ),
            )
            db.commit()
            return redirect(url_for("stok_barang"))

        q = request.args.get("q", "").strip()
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

        return render_template("stok_barang.html", products=products, q=q)

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
                SELECT id, barcode, legacy_code, group_code, name, unit, stock, retail_price
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
                SELECT id, barcode, legacy_code, group_code, name, unit, stock, retail_price
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
        sale_items = []
        subtotal = 0
        for raw in raw_items:
            product_id = raw.get("id")
            qty = parse_float(raw.get("qty", 1))
            discount = parse_int(raw.get("discount", 0))
            if not product_id or qty <= 0:
                return jsonify({"ok": False, "error": "Item transaksi tidak valid."}), 400

            product = db.execute(
                """
                SELECT id, barcode, legacy_code, name, unit, cost_price, retail_price
                FROM products
                WHERE id = ?
                """,
                (product_id,),
            ).fetchone()
            if not product:
                return jsonify({"ok": False, "error": "Produk tidak ditemukan."}), 404

            price = int(product["retail_price"] or 0)
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

        total, rounding = round_total(subtotal)
        paid = parse_int(payload.get("paid", 0))
        if paid < total:
            return jsonify({"ok": False, "error": "Uang pembayaran kurang."}), 400

        member = payload.get("member") or {}
        member_code = str(member.get("code", "")).strip()
        member_name = str(member.get("name", "")).strip()
        member_address = str(member.get("address", "")).strip()
        now = datetime.now(WIB)
        sale_no = next_sale_no(session["cashier_id"], register_no())
        note_no = int(sale_no.split("-")[0])

        cursor = db.execute(
            """
            INSERT INTO sales
              (sale_no, register_no, cashier_id, sale_date, member_code, member_name,
               member_address, subtotal, discount, donation, rounding, total, paid,
               change_amount, print_receipt, status)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0, 0, ?, ?, ?, ?, ?, 'paid')
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
                rounding,
                total,
                paid,
                paid - total,
                1 if payload.get("receipt_action", "print") == "print" else 0,
            ),
        )
        sale_id = cursor.lastrowid

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
            db.execute(
                "UPDATE products SET stock = stock - ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                (item["qty"], product["id"]),
            )

        db.execute(
            "UPDATE cashiers SET last_note = ? WHERE id = ?",
            (note_no, session["cashier_id"]),
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
            rounding=rounding,
            total=total,
            paid=paid,
            change_amount=paid - total,
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

    return app


def get_db():
    if "db" not in g:
        g.db = sqlite3.connect(DATABASE)
        g.db.row_factory = sqlite3.Row
    return g.db


def init_db():
    DATABASE.parent.mkdir(parents=True, exist_ok=True)
    db = get_db()
    db.executescript(SCHEMA.read_text(encoding="utf-8"))
    migrate_db(db)
    seed(db)


def migrate_db(db):
    ensure_columns(
        db,
        "sales",
        {
            "member_code": "TEXT",
            "member_name": "TEXT",
            "member_address": "TEXT",
        },
    )


def ensure_columns(db, table, columns):
    existing = {row["name"] for row in db.execute(f"PRAGMA table_info({table})")}
    for name, definition in columns.items():
        if name not in existing:
            db.execute(f"ALTER TABLE {table} ADD COLUMN {name} {definition}")


def seed(db):
    settings = [
        ("store_name", "DUA PUTRA JAYA"),
        ("store_city", "KOTA TEGAL"),
    ]
    db.executemany(
        """
        INSERT OR IGNORE INTO store_settings (key, value)
        VALUES (?, ?)
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
        INSERT OR IGNORE INTO cashiers
          (legacy_no, code, name, password, last_note)
        VALUES (?, ?, ?, '00', ?)
        """,
        cashiers,
    )

    registers = [("1", "A", 58), ("2", "A", 58), ("3", "A", 50)]
    db.executemany(
        """
        INSERT OR IGNORE INTO registers
          (register_no, mode, receipt_width)
        VALUES (?, ?, ?)
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
        INSERT OR IGNORE INTO cash_accounts
          (code, name, side, profit_loss)
        VALUES (?, ?, ?, ?)
        """,
        accounts,
    )

    product_count = db.execute("SELECT COUNT(*) FROM products").fetchone()[0]
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


def store_settings():
    rows = get_db().execute("SELECT key, value FROM store_settings").fetchall()
    values = {row["key"]: row["value"] for row in rows}
    return {
        "name": values.get("store_name", "DUA PUTRA JAYA"),
        "city": values.get("store_city", "KOTA TEGAL"),
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
    }


def build_receipt(
    sale_no,
    cashier,
    register,
    store,
    trans_time,
    member,
    items,
    subtotal,
    rounding,
    total,
    paid,
    change_amount,
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
            f"{'Pembulatan':<28}{format_rupiah(rounding):>14}",
            f"{'Total':<28}{format_rupiah(total):>14}",
            f"{'Bayar':<28}{format_rupiah(paid):>14}",
            f"{'Kembali':<28}{format_rupiah(change_amount):>14}",
            "-" * width,
            "TERIMA KASIH".center(width),
        ]
    )
    return "\n".join(lines)


def cashier_required(view):
    @wraps(view)
    def wrapped_view(**kwargs):
        if "cashier_id" not in session:
            return redirect(url_for("kasir"))
        return view(**kwargs)

    return wrapped_view


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
    next_no = int(row["last_note"] if row else 0) + 1
    if next_no > 99999:
        next_no = 1
    return f"{next_no:05d}-{current_register_no}"


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
    normalized = str(value or "0").replace(".", "").replace(",", ".")
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
