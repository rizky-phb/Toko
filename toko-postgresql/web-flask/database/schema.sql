CREATE TABLE IF NOT EXISTS cashiers (
  id BIGSERIAL PRIMARY KEY,
  legacy_no TEXT NOT NULL UNIQUE,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  password TEXT NOT NULL DEFAULT '00',
  last_note INTEGER NOT NULL DEFAULT 0,
  active INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS registers (
  id BIGSERIAL PRIMARY KEY,
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
  id BIGSERIAL PRIMARY KEY,
  barcode TEXT,
  legacy_code TEXT,
  group_code TEXT NOT NULL DEFAULT '00',
  name TEXT NOT NULL,
  unit TEXT NOT NULL DEFAULT 'PCS',
  stock REAL NOT NULL DEFAULT 0,
  cost_price INTEGER NOT NULL DEFAULT 0,
  wholesale_price INTEGER NOT NULL DEFAULT 0,
  retail_price INTEGER NOT NULL DEFAULT 0,
  member_price INTEGER NOT NULL DEFAULT 0,
  tier3_qty REAL NOT NULL DEFAULT 0,
  tier3_price INTEGER NOT NULL DEFAULT 0,
  tier4_qty REAL NOT NULL DEFAULT 0,
  tier4_price INTEGER NOT NULL DEFAULT 0,
  tier5_qty REAL NOT NULL DEFAULT 0,
  tier5_price INTEGER NOT NULL DEFAULT 0,
  supplier_code TEXT,
  rack_code TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_products_legacy_code
  ON products (legacy_code);

CREATE INDEX IF NOT EXISTS idx_products_barcode
  ON products (barcode);

CREATE INDEX IF NOT EXISTS idx_products_name
  ON products (name);

CREATE TABLE IF NOT EXISTS customers (
  id BIGSERIAL PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  address TEXT NOT NULL DEFAULT '',
  city TEXT NOT NULL DEFAULT '',
  phone TEXT NOT NULL DEFAULT '',
  discount REAL NOT NULL DEFAULT 0,
  points INTEGER NOT NULL DEFAULT 0,
  balance INTEGER NOT NULL DEFAULT 0,
  notes TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_customers_name
  ON customers (name);

CREATE TABLE IF NOT EXISTS sales (
  id BIGSERIAL PRIMARY KEY,
  sale_no TEXT NOT NULL UNIQUE,
  register_no TEXT NOT NULL,
  cashier_id BIGINT NOT NULL,
  sale_date DATE NOT NULL,
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
  point_earned INTEGER NOT NULL DEFAULT 0,
  point_redeemed INTEGER NOT NULL DEFAULT 0,
  voided_at TIMESTAMPTZ,
  void_reason TEXT NOT NULL DEFAULT '',
  voided_by TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (cashier_id) REFERENCES cashiers(id)
);

CREATE TABLE IF NOT EXISTS sale_items (
  id BIGSERIAL PRIMARY KEY,
  sale_id BIGINT NOT NULL,
  product_id BIGINT,
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

CREATE TABLE IF NOT EXISTS held_sales (
  id BIGSERIAL PRIMARY KEY,
  register_no TEXT NOT NULL,
  cashier_id BIGINT NOT NULL,
  cashier_name TEXT NOT NULL DEFAULT '',
  member_code TEXT,
  member_name TEXT,
  member_address TEXT,
  sale_discount INTEGER NOT NULL DEFAULT 0,
  items_json TEXT NOT NULL,
  subtotal INTEGER NOT NULL DEFAULT 0,
  total INTEGER NOT NULL DEFAULT 0,
  item_count REAL NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (cashier_id) REFERENCES cashiers(id)
);

CREATE INDEX IF NOT EXISTS idx_held_sales_register_cashier
  ON held_sales (register_no, cashier_id, created_at);

CREATE TABLE IF NOT EXISTS cash_accounts (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  side TEXT NOT NULL CHECK (side IN ('D', 'K')),
  profit_loss TEXT NOT NULL DEFAULT 'T' CHECK (profit_loss IN ('Y', 'T'))
);

CREATE TABLE IF NOT EXISTS cash_transactions (
  id BIGSERIAL PRIMARY KEY,
  trans_date DATE NOT NULL,
  cashier_name TEXT NOT NULL,
  account_code TEXT NOT NULL,
  account_name TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '',
  amount INTEGER NOT NULL DEFAULT 0,
  side TEXT NOT NULL CHECK (side IN ('D', 'K')),
  profit_loss TEXT NOT NULL DEFAULT 'T' CHECK (profit_loss IN ('Y', 'T')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (account_code) REFERENCES cash_accounts(code)
);

CREATE INDEX IF NOT EXISTS idx_cash_transactions_day_cashier
  ON cash_transactions (trans_date, cashier_name);

CREATE INDEX IF NOT EXISTS idx_cash_transactions_account_day
  ON cash_transactions (account_code, trans_date);

CREATE TABLE IF NOT EXISTS customer_point_ledger (
  id BIGSERIAL PRIMARY KEY,
  customer_code TEXT NOT NULL,
  sale_id BIGINT,
  trans_date DATE NOT NULL,
  points_in INTEGER NOT NULL DEFAULT 0,
  points_out INTEGER NOT NULL DEFAULT 0,
  cash_value INTEGER NOT NULL DEFAULT 0,
  description TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (customer_code) REFERENCES customers(code),
  FOREIGN KEY (sale_id) REFERENCES sales(id)
);

CREATE INDEX IF NOT EXISTS idx_customer_point_ledger_customer
  ON customer_point_ledger (customer_code, trans_date, id);

CREATE TABLE IF NOT EXISTS customer_payments (
  id BIGSERIAL PRIMARY KEY,
  customer_code TEXT NOT NULL,
  cashier_id BIGINT NOT NULL,
  payment_date DATE NOT NULL,
  amount INTEGER NOT NULL CHECK (amount > 0),
  description TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (customer_code) REFERENCES customers(code),
  FOREIGN KEY (cashier_id) REFERENCES cashiers(id)
);
