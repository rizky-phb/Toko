CREATE TABLE IF NOT EXISTS cashiers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  legacy_no TEXT NOT NULL UNIQUE,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  password TEXT NOT NULL DEFAULT '00',
  last_note INTEGER NOT NULL DEFAULT 0,
  active INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS registers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  register_no TEXT NOT NULL UNIQUE,
  mode TEXT NOT NULL DEFAULT 'A',
  print_enabled INTEGER NOT NULL DEFAULT 1,
  drawer_enabled INTEGER NOT NULL DEFAULT 1,
  receipt_width INTEGER NOT NULL DEFAULT 58
);

CREATE TABLE IF NOT EXISTS store_settings (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS products (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  barcode TEXT,
  legacy_code TEXT,
  group_code TEXT NOT NULL DEFAULT '00',
  name TEXT NOT NULL,
  unit TEXT NOT NULL DEFAULT 'PCS',
  stock REAL NOT NULL DEFAULT 0,
  cost_price INTEGER NOT NULL DEFAULT 0,
  wholesale_price INTEGER NOT NULL DEFAULT 0,
  retail_price INTEGER NOT NULL DEFAULT 0,
  supplier_code TEXT,
  rack_code TEXT,
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_products_legacy_code
  ON products (legacy_code);

CREATE INDEX IF NOT EXISTS idx_products_barcode
  ON products (barcode);

CREATE INDEX IF NOT EXISTS idx_products_name
  ON products (name);

CREATE TABLE IF NOT EXISTS sales (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  sale_no TEXT NOT NULL UNIQUE,
  register_no TEXT NOT NULL,
  cashier_id INTEGER NOT NULL,
  sale_date TEXT NOT NULL,
  member_code TEXT,
  member_name TEXT,
  member_address TEXT,
  subtotal INTEGER NOT NULL DEFAULT 0,
  discount INTEGER NOT NULL DEFAULT 0,
  donation INTEGER NOT NULL DEFAULT 0,
  rounding INTEGER NOT NULL DEFAULT 0,
  total INTEGER NOT NULL DEFAULT 0,
  paid INTEGER NOT NULL DEFAULT 0,
  change_amount INTEGER NOT NULL DEFAULT 0,
  print_receipt INTEGER NOT NULL DEFAULT 1,
  status TEXT NOT NULL DEFAULT 'paid',
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (cashier_id) REFERENCES cashiers(id)
);

CREATE TABLE IF NOT EXISTS sale_items (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  sale_id INTEGER NOT NULL,
  product_id INTEGER,
  barcode TEXT,
  product_name TEXT NOT NULL,
  qty REAL NOT NULL,
  unit TEXT NOT NULL DEFAULT 'PCS',
  cost_price INTEGER NOT NULL DEFAULT 0,
  price INTEGER NOT NULL DEFAULT 0,
  discount INTEGER NOT NULL DEFAULT 0,
  subtotal INTEGER NOT NULL DEFAULT 0,
  FOREIGN KEY (sale_id) REFERENCES sales(id),
  FOREIGN KEY (product_id) REFERENCES products(id)
);

CREATE TABLE IF NOT EXISTS cash_accounts (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  side TEXT NOT NULL CHECK (side IN ('D', 'K')),
  profit_loss TEXT NOT NULL DEFAULT 'T' CHECK (profit_loss IN ('Y', 'T'))
);

CREATE TABLE IF NOT EXISTS cash_transactions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  trans_date TEXT NOT NULL,
  cashier_name TEXT NOT NULL,
  account_code TEXT NOT NULL,
  account_name TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '',
  amount INTEGER NOT NULL DEFAULT 0,
  side TEXT NOT NULL CHECK (side IN ('D', 'K')),
  profit_loss TEXT NOT NULL DEFAULT 'T' CHECK (profit_loss IN ('Y', 'T')),
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (account_code) REFERENCES cash_accounts(code)
);

CREATE INDEX IF NOT EXISTS idx_cash_transactions_day_cashier
  ON cash_transactions (trans_date, cashier_name);

CREATE INDEX IF NOT EXISTS idx_cash_transactions_account_day
  ON cash_transactions (account_code, trans_date);
