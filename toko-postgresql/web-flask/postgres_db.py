import re

import psycopg
from psycopg.rows import dict_row


def _postgres_sql(sql):
    """Translate the app's DB-API qmark placeholders to PostgreSQL placeholders."""
    return re.sub(r"\?", "%s", sql)


class PostgresConnection:
    def __init__(self, dsn):
        self.connection = psycopg.connect(dsn, row_factory=dict_row)

    def execute(self, sql, params=None, **kwargs):
        return self.connection.execute(_postgres_sql(sql), params, **kwargs)

    def executemany(self, sql, params_seq):
        cursor = self.connection.cursor()
        cursor.executemany(_postgres_sql(sql), params_seq)
        return cursor

    def executescript(self, sql):
        # prepare=False selects PostgreSQL's simple-query protocol, which accepts
        # the multi-statement schema file.
        return self.connection.execute(sql, prepare=False)

    def commit(self):
        self.connection.commit()

    def rollback(self):
        self.connection.rollback()

    def close(self):
        self.connection.close()


def connect(dsn):
    return PostgresConnection(dsn)
