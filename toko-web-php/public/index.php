<?php

session_start();

require_once __DIR__ . '/../src/Database.php';
require_once __DIR__ . '/../src/LegacyRules.php';

$root = dirname(__DIR__);
$db = new Database($root . '/storage/toko.sqlite');
$db->migrate($root . '/database/schema.sql');
$pdo = $db->pdo();

$path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH) ?: '/';

if ($path === '/logout') {
    session_destroy();
    header('Location: /login');
    exit;
}

if ($path === '/login' && $_SERVER['REQUEST_METHOD'] === 'POST') {
    $code = trim($_POST['code'] ?? '');
    $password = trim($_POST['password'] ?? '');

    $stmt = $pdo->prepare('SELECT * FROM cashiers WHERE code = ? AND password = ? AND active = 1');
    $stmt->execute([$code, $password]);
    $cashier = $stmt->fetch();

    if ($cashier) {
        $_SESSION['cashier_id'] = $cashier['id'];
        $_SESSION['cashier_name'] = $cashier['name'];
        $_SESSION['register_no'] = trim($_POST['register_no'] ?? '') ?: LegacyRules::registerNo();
        header('Location: /kasir');
        exit;
    }

    $_SESSION['error'] = 'Kode kasir atau password salah.';
    header('Location: /login');
    exit;
}

if ($path !== '/login' && empty($_SESSION['cashier_id'])) {
    header('Location: /login');
    exit;
}

if ($path === '/stok-barang' && $_SERVER['REQUEST_METHOD'] === 'POST') {
    $stmt = $pdo->prepare(
        'INSERT INTO products (barcode, group_code, name, unit, stock, cost_price, wholesale_price, retail_price) VALUES (?, ?, ?, ?, ?, ?, ?, ?)'
    );
    $stmt->execute([
        trim($_POST['barcode'] ?? ''),
        trim($_POST['group_code'] ?? '00') ?: '00',
        trim($_POST['name'] ?? ''),
        trim($_POST['unit'] ?? 'PCS') ?: 'PCS',
        (float) ($_POST['stock'] ?? 0),
        (int) ($_POST['cost_price'] ?? 0),
        (int) ($_POST['wholesale_price'] ?? 0),
        (int) ($_POST['retail_price'] ?? 0),
    ]);

    header('Location: /stok-barang');
    exit;
}

function render(string $title, string $body): void
{
    $cashier = $_SESSION['cashier_name'] ?? '-';
    $registerNo = LegacyRules::registerNo();
    echo <<<HTML
<!doctype html>
<html lang="id">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{$title} - Toko Web</title>
  <style>
    * { box-sizing: border-box; }
    body { margin:0; font:14px Consolas, "Courier New", monospace; background:#048d86; color:#061313; }
    header, footer { background:#071887; color:#fff; padding:7px 14px; display:flex; justify-content:space-between; }
    nav { background:#07958e; border-bottom:2px solid #111; display:flex; gap:4px; padding-left:24px; }
    nav a { color:#001817; text-decoration:none; padding:8px 16px; font-weight:700; }
    nav a.active, nav a:hover { background:#8d0606; color:#fff; }
    main { padding:18px 24px; }
    .panel { border:2px solid #031d1b; background:#078981; min-height:70vh; }
    .title { background:#8d0606; color:#fff; padding:6px 10px; display:flex; justify-content:space-between; font-weight:700; }
    .toolbar { display:flex; gap:8px; flex-wrap:wrap; padding:12px; background:#0a9f96; border-bottom:2px solid #031d1b; }
    input, select, button { font:inherit; padding:7px 9px; border:2px solid #063230; background:#eafffb; }
    button, .button { background:#071887; color:#fff; text-decoration:none; font-weight:700; cursor:pointer; }
    table { width:100%; border-collapse:collapse; background:#078981; }
    th, td { border-bottom:1px solid #064340; padding:7px 8px; white-space:nowrap; }
    th { background:#2bc2b7; text-align:left; border-bottom:2px solid #062625; }
    tr.selected td { background:#071887; color:#fff; }
    .pos { display:grid; grid-template-columns:1fr 340px; gap:14px; padding:14px; background:#032c30; color:#fff; min-height:70vh; }
    .pos-box { border:2px solid #3adbd1; background:#078981; }
    .total { background:#eafaf6; color:#061313; padding:14px; }
    .grand { background:#8d0606; color:#fff; text-align:right; padding:12px; font-size:34px; font-weight:800; margin-top:12px; }
    .login { max-width:420px; margin:60px auto; background:#078981; border:2px solid #031d1b; padding:18px; }
    .login label { display:block; margin-top:10px; font-weight:700; }
    .login input { width:100%; }
  </style>
</head>
<body>
<header><span>MR. FAUZI ZAMI</span><span>Kassa: {$registerNo} | Kasir: {$cashier}</span></header>
<nav>
  <a class="{$GLOBALS['path'] === '/' ? 'active' : ''}" href="/">Dashboard</a>
  <a class="{$GLOBALS['path'] === '/kasir' ? 'active' : ''}" href="/kasir">Kasir</a>
  <a class="{$GLOBALS['path'] === '/stok-barang' ? 'active' : ''}" href="/stok-barang">Stok Barang</a>
  <a href="/logout">Logout</a>
</nav>
<main>{$body}</main>
<footer><span>F2 Cari | F4 Tambah | F5 Edit | F8 Hapus | F10 Cetak</span><span>PHP Port Awal</span></footer>
</body>
</html>
HTML;
}

if ($path === '/login') {
    $error = $_SESSION['error'] ?? '';
    unset($_SESSION['error']);
    echo <<<HTML
<!doctype html>
<html lang="id">
<head><meta charset="utf-8"><title>Login Kasir</title></head>
<body style="font:14px Consolas;background:#048d86">
<form method="post" class="login" style="max-width:420px;margin:60px auto;background:#078981;border:2px solid #031d1b;padding:18px">
  <h2>Login Kasir</h2>
  <p style="color:#8d0606">{$error}</p>
  <label>Kode Kasir</label><input name="code" autofocus value="2">
  <label>Password</label><input name="password" type="password" value="00">
  <label>No Kassa</label><input name="register_no" value="1">
  <p><button style="padding:8px 14px">Masuk</button></p>
</form>
</body>
</html>
HTML;
    exit;
}

if ($path === '/stok-barang') {
    $products = $pdo->query('SELECT * FROM products ORDER BY id DESC')->fetchAll();
    $rows = '';
    foreach ($products as $p) {
        $rows .= '<tr><td>' . htmlspecialchars($p['barcode'] ?? '') . '</td><td>' . htmlspecialchars($p['group_code']) . '</td><td>' . htmlspecialchars($p['name']) . '</td><td>' . LegacyRules::formatRupiah($p['stock']) . '</td><td>' . htmlspecialchars($p['unit']) . '</td><td>' . LegacyRules::formatRupiah($p['cost_price']) . '</td><td>' . LegacyRules::formatRupiah($p['retail_price']) . '</td></tr>';
    }

    render('Stok Barang', <<<HTML
<section class="panel">
  <div class="title"><span>Stok Barang</span><span>Master Produk</span></div>
  <form method="post" class="toolbar">
    <input name="barcode" placeholder="Barcode">
    <input name="group_code" placeholder="Klp" value="00" size="4">
    <input name="name" placeholder="Nama barang" required size="34">
    <input name="stock" placeholder="Stok" value="0" size="6">
    <input name="unit" placeholder="Satuan" value="PCS" size="6">
    <input name="cost_price" placeholder="Harga beli" value="0" size="10">
    <input name="retail_price" placeholder="Harga ecer" value="0" size="10">
    <button>F4 Tambah</button>
  </form>
  <table>
    <thead><tr><th>Barcode</th><th>Klp</th><th>Nama Barang</th><th>Stok</th><th>Sat</th><th>Hrg.Beli</th><th>Hrg.Ecer</th></tr></thead>
    <tbody>{$rows}</tbody>
  </table>
</section>
HTML);
    exit;
}

if ($path === '/kasir') {
    $items = [
        ['BERAS SPHP / KG', 2, 14000, 0],
        ['ABC SAMBAL ASLI 135ML', 1, 6500, 0],
    ];
    $subtotal = 0;
    $rows = '';
    foreach ($items as [$name, $qty, $price, $discount]) {
        $line = (int) ($qty * $price) - $discount;
        $subtotal += $line;
        $rows .= '<tr><td>' . htmlspecialchars($name) . '</td><td>' . $qty . '</td><td>' . LegacyRules::formatRupiah($price) . '</td><td>' . LegacyRules::formatRupiah($discount) . '</td><td>' . LegacyRules::formatRupiah($line) . '</td></tr>';
    }
    [$total, $rounding] = LegacyRules::roundTotal($subtotal);
    $saleNo = LegacyRules::nextSaleNo($pdo, LegacyRules::registerNo(), (int) $_SESSION['cashier_id']);

    render('Kasir', <<<HTML
<section class="pos">
  <div class="pos-box">
    <div class="title"><span>KASIR / POS</span><span>Nota: {$saleNo}</span></div>
    <div class="toolbar"><input placeholder="Scan barcode / cari barang" size="44" autofocus><button>F2 Cari</button><button>Enter</button></div>
    <table><thead><tr><th>Nama Barang</th><th>Qty</th><th>Harga</th><th>Diskon</th><th>Jumlah</th></tr></thead><tbody>{$rows}</tbody></table>
  </div>
  <aside class="pos-box">
    <div class="total">
      <p>Subtotal <strong style="float:right">{$subtotal}</strong></p>
      <p>Pembulatan <strong style="float:right">{$rounding}</strong></p>
      <div class="grand">{$total}</div>
    </div>
    <div class="toolbar"><button>F9 Bayar</button><button>F10 Cetak</button><button>Batal</button></div>
  </aside>
</section>
HTML);
    exit;
}

render('Dashboard', <<<HTML
<section class="panel">
  <div class="title"><span>Dashboard</span><span>Port awal dari source PRG</span></div>
  <div class="toolbar">
    <a class="button" href="/kasir">Buka Kasir</a>
    <a class="button" href="/stok-barang">Buka Stok Barang</a>
  </div>
</section>
HTML);

