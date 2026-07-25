"""Import legacy JUAL*.DTA cashier transactions into the PostgreSQL schema.

Run this only after products, customers, and cashiers have been migrated. The
script is idempotent: an existing sale_no is never imported a second time.
"""

import argparse
from collections import defaultdict
from datetime import datetime
import hashlib
from pathlib import Path
import sys

import psycopg
from psycopg.rows import dict_row

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from app import dbf_fields, dbf_number, dbf_value  # noqa: E402


def legacy_rows(path):
    data = path.read_bytes()
    header_len = int.from_bytes(data[8:10], "little")
    record_len = int.from_bytes(data[10:12], "little")
    record_count = int.from_bytes(data[4:8], "little")
    fields = dbf_fields(data, header_len)
    for index in range(record_count):
        record = data[header_len + index * record_len : header_len + (index + 1) * record_len]
        if len(record) < record_len or record[:1] == b"*":
            continue
        yield {field["name"]: dbf_value(record, field) for field in fields}


def sale_date(raw):
    raw = raw.strip()
    return f"{raw[:4]}-{raw[4:6]}-{raw[6:8]}" if len(raw) == 8 else datetime.today().date().isoformat()


def money(raw):
    return int(round(dbf_number(raw)))


def migrate(database_url, source_dir):
    source_dir = Path(source_dir)
    groups = defaultdict(list)
    seen_files = set()
    seen_rows = set()
    for path in source_dir.glob("JUAL*.DTA"):
        fingerprint = hashlib.sha256(path.read_bytes()).digest()
        if fingerprint in seen_files:
            continue
        seen_files.add(fingerprint)
        for row in legacy_rows(path):
            row_key = tuple(sorted(row.items()))
            if row_key in seen_rows:
                continue
            seen_rows.add(row_key)
            invoice = row.get("FAKTUR", "").strip()
            if not invoice:
                continue
            key = (sale_date(row.get("TANGGAL", "")), row.get("KASSA", "1").strip() or "1", row.get("KODE_KSR", "").strip(), invoice)
            groups[key].append(row)

    with psycopg.connect(database_url, row_factory=dict_row) as conn:
        for (trans_date, register_no, cashier_code, invoice), rows in groups.items():
            cashier = conn.execute("SELECT id FROM cashiers WHERE code = %s OR legacy_no = %s ORDER BY id LIMIT 1", (cashier_code, cashier_code)).fetchone()
            if not cashier:
                print(f"skip {invoice}: kasir {cashier_code or '-'} tidak ditemukan")
                continue
            sale_no = f"{invoice.zfill(5)}-{register_no}"
            if conn.execute("SELECT 1 FROM sales WHERE sale_no = %s", (sale_no,)).fetchone():
                continue
            first = rows[0]
            member_code = first.get("KODE_LGN", "").strip()
            customer = conn.execute("SELECT name, address, city FROM customers WHERE code = %s", (member_code,)).fetchone() if member_code else None
            subtotal = 0
            for row in rows:
                subtotal += max(0, int(round(dbf_number(row.get("JUMLAH")) * dbf_number(row.get("HARGA_2")))) - money(row.get("DISKON_RP")))
            discount = 0
            rounding = max(money(row.get("RP_BULAT")) for row in rows)
            donation = max(money(row.get("RP_DONASI")) for row in rows)
            paid = max(money(row.get("RP_BAYAR")) for row in rows)
            total = subtotal + rounding
            status = "credit" if any(row.get("ID_DATA", "").strip() == "K" for row in rows) or paid < total else "paid"
            sale = conn.execute(
                """INSERT INTO sales
                   (sale_no, register_no, cashier_id, sale_date, member_code, member_name, member_address,
                    subtotal, discount, donation, rounding, total, paid, change_amount, print_receipt, status)
                   VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,1,%s) RETURNING id""",
                (sale_no, register_no, cashier["id"], trans_date, member_code or None,
                 customer["name"] if customer else None,
                 " ".join(part for part in ((customer or {}).get("address", ""), (customer or {}).get("city", "")) if part) or None,
                 subtotal, discount, donation, rounding, total, min(paid, total), max(0, paid - total - donation), status),
            ).fetchone()
            for row in rows:
                code = row.get("KODE_BRG", "").strip()
                barcode = row.get("KODE_BAR", "").strip()
                product = conn.execute("SELECT id, cost_price FROM products WHERE legacy_code = %s OR barcode = %s ORDER BY id LIMIT 1", (code, barcode)).fetchone()
                qty = dbf_number(row.get("JUMLAH"))
                price = money(row.get("HARGA_2"))
                item_discount = money(row.get("DISKON_RP"))
                conn.execute(
                    """INSERT INTO sale_items
                       (sale_id, product_id, barcode, product_name, qty, unit, cost_price, price, discount, subtotal)
                       VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)""",
                    (sale["id"], product["id"] if product else None, barcode or code,
                     row.get("NAMA_BRG", "ITEM LAMA").strip() or "ITEM LAMA", row.get("JUMLAH", 0),
                     row.get("SATUAN", "PCS").strip() or "PCS", product["cost_price"] if product else money(row.get("HARGA")),
                     price, item_discount, max(0, int(round(qty * price)) - item_discount)),
                )
            if customer and total > paid:
                conn.execute("UPDATE customers SET balance = balance + %s WHERE code = %s", (total - paid, member_code))
        conn.commit()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("database_url")
    parser.add_argument("source_dir", type=Path)
    args = parser.parse_args()
    migrate(args.database_url, args.source_dir)
