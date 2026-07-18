"""One-time migration of the archived Flask SQLite database to PostgreSQL."""

import argparse
import sqlite3

import psycopg
from psycopg import sql


TABLES = [
    "cashiers",
    "registers",
    "store_settings",
    "products",
    "customers",
    "sales",
    "sale_items",
    "held_sales",
    "cash_accounts",
    "cash_transactions",
]


def migrate(sqlite_path, database_url):
    source = sqlite3.connect(sqlite_path)
    source.row_factory = sqlite3.Row
    target = psycopg.connect(database_url)

    try:
        with target.transaction():
            for table in reversed(TABLES):
                target.execute(
                    sql.SQL("TRUNCATE TABLE {} RESTART IDENTITY CASCADE").format(
                        sql.Identifier(table)
                    )
                )

            for table in TABLES:
                rows = source.execute(f'SELECT * FROM "{table}"').fetchall()
                if not rows:
                    print(f"{table}: 0")
                    continue
                columns = rows[0].keys()
                statement = sql.SQL("INSERT INTO {} ({}) VALUES ({})").format(
                    sql.Identifier(table),
                    sql.SQL(", ").join(map(sql.Identifier, columns)),
                    sql.SQL(", ").join(sql.Placeholder() for _ in columns),
                )
                target.cursor().executemany(
                    statement, [tuple(row[column] for column in columns) for row in rows]
                )
                print(f"{table}: {len(rows)}")

            for table in TABLES:
                if "id" not in {
                    row[1] for row in source.execute(f'PRAGMA table_info("{table}")')
                }:
                    continue
                target.execute(
                    sql.SQL(
                        "SELECT setval(pg_get_serial_sequence(%s, 'id'), "
                        "COALESCE((SELECT MAX(id) FROM {}), 1), "
                        "EXISTS (SELECT 1 FROM {}))"
                    ).format(sql.Identifier(table), sql.Identifier(table)),
                    (table,),
                )
    finally:
        source.close()
        target.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("sqlite_path")
    parser.add_argument("database_url")
    args = parser.parse_args()
    migrate(args.sqlite_path, args.database_url)
