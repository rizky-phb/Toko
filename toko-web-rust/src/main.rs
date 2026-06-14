use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};

const APP_NAME: &str = "MR. FAUZI ZAMI";

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:3000")?;
    println!("Toko web berjalan di http://localhost:3000");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => handle_connection(stream),
            Err(err) => eprintln!("Koneksi gagal: {err}"),
        }
    }

    Ok(())
}

fn handle_connection(mut stream: TcpStream) {
    let mut buffer = [0; 2048];
    let bytes_read = stream.read(&mut buffer).unwrap_or(0);
    let request = String::from_utf8_lossy(&buffer[..bytes_read]);
    let path = request
        .lines()
        .next()
        .and_then(|line| line.split_whitespace().nth(1))
        .unwrap_or("/");

    let (status, body) = match path {
        "/" => ("200 OK", page("Dashboard", dashboard())),
        "/stok-barang" | "/barang" => ("200 OK", page("Stok Barang", barang())),
        "/kasir" => ("200 OK", page("Kasir", kasir())),
        "/pembelian" => ("200 OK", page("Pembelian", pembelian())),
        "/laporan" => ("200 OK", page("Laporan", laporan())),
        "/pengaturan" => ("200 OK", page("Pengaturan", pengaturan())),
        _ => ("404 Not Found", page("Tidak Ditemukan", not_found())),
    };

    let response = format!(
        "HTTP/1.1 {status}\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\n\r\n{body}",
        body.len()
    );

    let _ = stream.write_all(response.as_bytes());
}

fn page(title: &str, content: String) -> String {
    format!(
        r#"<!doctype html>
<html lang="id">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title} - {APP_NAME}</title>
  <style>
    :root {{
      --blue: #071887;
      --teal: #048d86;
      --teal-dark: #02645f;
      --red: #8d0606;
      --yellow: #fff04a;
      --ink: #061313;
      --line: #031d1b;
      --panel: #e7f4f0;
    }}

    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      color: var(--ink);
      background: #d9e8ef;
      font: 14px/1.35 "Consolas", "Courier New", monospace;
    }}

    .app {{
      min-height: 100vh;
      display: grid;
      grid-template-rows: auto auto 1fr auto;
      background:
        linear-gradient(135deg, rgba(255,255,255,.22), transparent 34%),
        var(--teal);
    }}

    .topbar, .statusbar {{
      background: var(--blue);
      color: white;
      padding: 5px 14px;
      display: flex;
      justify-content: space-between;
      gap: 16px;
      border-bottom: 3px solid #62d6d0;
    }}

    .menubar {{
      background: #07958e;
      border-bottom: 2px solid #111;
      display: flex;
      gap: 4px;
      padding: 0 26px;
    }}

    .menubar a {{
      color: #001817;
      text-decoration: none;
      padding: 7px 16px;
      min-width: 110px;
      text-align: center;
      font-weight: 700;
    }}

    .menubar a.active, .menubar a:hover {{
      background: var(--red);
      color: white;
    }}

    main {{
      padding: 20px 26px 18px;
      min-width: 0;
    }}

    .workarea {{
      border: 2px solid var(--line);
      min-height: calc(100vh - 148px);
      background:
        repeating-linear-gradient(115deg, rgba(0,0,0,.07) 0 1px, transparent 1px 4px),
        var(--teal);
    }}

    .section-title {{
      background: var(--red);
      color: white;
      padding: 4px 10px;
      display: flex;
      justify-content: space-between;
      border-bottom: 2px solid var(--line);
      font-weight: 700;
    }}

    .toolbar {{
      display: flex;
      gap: 10px;
      padding: 12px;
      border-bottom: 2px solid var(--line);
      background: #0a9f96;
      flex-wrap: wrap;
    }}

    input, select, button {{
      font: inherit;
      border: 2px solid #063230;
      background: #eafffb;
      padding: 7px 9px;
    }}

    button {{
      background: var(--blue);
      color: white;
      cursor: pointer;
      font-weight: 700;
    }}

    button.danger {{ background: var(--red); }}
    button.warn {{ background: #776900; color: white; }}

    table {{
      width: 100%;
      border-collapse: collapse;
      background: #078981;
    }}

    th, td {{
      border-bottom: 1px solid #064340;
      padding: 6px 8px;
      white-space: nowrap;
    }}

    th {{
      text-align: left;
      background: #2bc2b7;
      border-bottom: 2px solid #062625;
    }}

    tr.selected td {{
      background: var(--blue);
      color: white;
    }}

    .grid {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
      gap: 14px;
      padding: 14px;
    }}

    .tile {{
      border: 2px solid #052827;
      background: #dff8f3;
      padding: 16px;
      min-height: 96px;
    }}

    .tile strong {{
      display: block;
      font-size: 20px;
      margin-top: 8px;
    }}

    .form-grid {{
      display: grid;
      grid-template-columns: repeat(4, minmax(150px, 1fr));
      gap: 10px;
      padding: 14px;
      align-items: end;
    }}

    .field label {{
      display: block;
      font-weight: 700;
      margin-bottom: 4px;
    }}

    .field input, .field select {{ width: 100%; }}

    .totals {{
      margin-left: auto;
      width: min(360px, 100%);
      padding: 14px;
      background: #eafaf6;
      border-left: 2px solid #052827;
    }}

    .totals div {{
      display: flex;
      justify-content: space-between;
      padding: 5px 0;
    }}

    .total {{
      font-size: 24px;
      color: var(--red);
      font-weight: 800;
    }}

    .shortcut {{
      background: var(--red);
      color: white;
      padding: 5px 14px;
      display: flex;
      gap: 22px;
      overflow-x: auto;
      border-top: 2px solid #062625;
    }}

    @media (max-width: 760px) {{
      .menubar {{ overflow-x: auto; padding: 0; }}
      .menubar a {{ min-width: max-content; }}
      main {{ padding: 10px; }}
      .form-grid {{ grid-template-columns: 1fr; }}
      table {{ font-size: 12px; }}
    }}
  </style>
</head>
<body>
  <div class="app">
    <header class="topbar">
      <span>{APP_NAME}</span>
      <span>Minggu, 31-05-2026</span>
    </header>
    {nav}
    <main>{content}</main>
    <footer class="statusbar">
      <span>F2 Cari/Sortir &nbsp; F4 Tambah &nbsp; F5 Edit &nbsp; F8 Hapus &nbsp; F10 Cetak</span>
      <span>localhost:3000</span>
    </footer>
  </div>
</body>
</html>"#,
        nav = nav(title)
    )
}

fn nav(active: &str) -> String {
    let items = [
        ("Dashboard", "/"),
        ("Kasir", "/kasir"),
        ("Stok Barang", "/stok-barang"),
        ("Pembelian", "/pembelian"),
        ("Laporan", "/laporan"),
        ("Pengaturan", "/pengaturan"),
    ];

    let links = items
        .iter()
        .map(|(name, href)| {
            let class = if *name == active { "active" } else { "" };
            format!(r#"<a class="{class}" href="{href}">{name}</a>"#)
        })
        .collect::<Vec<_>>()
        .join("");

    format!(r#"<nav class="menubar">{links}</nav>"#)
}

fn dashboard() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Dashboard</span><span>Ringkasan Hari Ini</span></div>
  <div class="grid">
    <div class="tile">Penjualan Hari Ini<strong>Rp 0</strong></div>
    <div class="tile">Transaksi<strong>0 Nota</strong></div>
    <div class="tile">Barang Stok Minus<strong>0 Item</strong></div>
    <div class="tile">Margin<strong>Rp 0</strong></div>
  </div>
</section>"#
        .to_string()
}

fn barang() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Tabel Barang</span><span>Semua Kelompok Barang</span></div>
  <div class="toolbar">
    <input placeholder="Cari barcode / nama barang" size="34">
    <select><option>Semua Kelompok</option><option>Sembako</option><option>Minuman</option></select>
    <button>F2 Sortir</button>
    <button>F4 Tambah</button>
    <button>F5 Edit</button>
    <button class="danger">F8 Hapus</button>
  </div>
  <table>
    <thead>
      <tr><th>Barcode</th><th>Klp</th><th>Nama Barang</th><th>Sisa Stk</th><th>Sat</th><th>Hrg.Bakul</th><th>Hrg.Ecer</th></tr>
    </thead>
    <tbody>
      <tr class="selected"><td>BN</td><td>00</td><td>BERAS SPHP / KG</td><td>-606</td><td>PCS</td><td>13.500</td><td>14.000</td></tr>
      <tr><td>8991906106311</td><td>00</td><td>APEL ROYAL</td><td>-3</td><td>PCS</td><td>15.000</td><td>15.500</td></tr>
      <tr><td>7118441200327</td><td>11</td><td>ABC SAMBAL ASLI 135ML</td><td>0</td><td>PCS</td><td>6.200</td><td>6.500</td></tr>
      <tr><td>7118441200872</td><td>11</td><td>ABC SAMBAL EXT.PDS 135ML</td><td>-358</td><td>PCS</td><td>7.500</td><td>8.000</td></tr>
    </tbody>
  </table>
  <div class="shortcut">BN &nbsp; 00 &nbsp; BERAS SPHP / KG &nbsp; -606 PCS &nbsp; F6=Mutasi &nbsp; F10=Cetak</div>
</section>"#
        .to_string()
}

fn kasir() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Kasir</span><span>Nota: 000001 | Shift: Pagi</span></div>
  <div class="toolbar">
    <input placeholder="Scan barcode / cari barang" size="44" autofocus>
    <button>F2 Cari</button>
    <button>F9 Bayar</button>
    <button>F10 Cetak</button>
  </div>
  <table>
    <thead>
      <tr><th>Nama Barang</th><th>Qty</th><th>Harga</th><th>Diskon</th><th>Jumlah</th></tr>
    </thead>
    <tbody>
      <tr class="selected"><td>BERAS SPHP / KG</td><td>2</td><td>14.000</td><td>0</td><td>28.000</td></tr>
      <tr><td>ABC SAMBAL ASLI 135ML</td><td>1</td><td>6.500</td><td>0</td><td>6.500</td></tr>
    </tbody>
  </table>
  <div class="totals">
    <div><span>Subtotal</span><strong>34.500</strong></div>
    <div><span>Diskon</span><strong>0</strong></div>
    <div class="total"><span>Total</span><span>34.500</span></div>
    <div><span>Bayar</span><input value="50000"></div>
    <div><span>Kembali</span><strong>15.500</strong></div>
  </div>
  <div class="shortcut">F4=Qty &nbsp; F5=Diskon &nbsp; F8=Hapus Item &nbsp; F9=Bayar</div>
</section>"#
        .to_string()
}

fn pembelian() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Pembelian</span><span>Input Transaksi Pembelian</span></div>
  <div class="form-grid">
    <div class="field"><label>No Faktur</label><input value="PB-000001"></div>
    <div class="field"><label>Tanggal</label><input value="31-05-2026"></div>
    <div class="field"><label>Supplier</label><select><option>Supplier Umum</option></select></div>
    <div class="field"><label>Barang</label><input placeholder="Cari barang"></div>
  </div>
  <table>
    <thead><tr><th>Barang</th><th>Qty</th><th>Harga Beli</th><th>Total</th></tr></thead>
    <tbody>
      <tr class="selected"><td>INDOMIE GORENG</td><td>40</td><td>2.850</td><td>114.000</td></tr>
      <tr><td>GULA PASIR / KG</td><td>25</td><td>16.500</td><td>412.500</td></tr>
    </tbody>
  </table>
  <div class="shortcut">Simpan Pembelian &nbsp; Cetak &nbsp; Batal</div>
</section>"#
        .to_string()
}

fn laporan() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Laporan</span><span>Penjualan, Pembelian, Stok</span></div>
  <div class="grid">
    <div class="tile">Laporan Barang<strong>Stok & Harga</strong></div>
    <div class="tile">Laporan Penjualan<strong>Harian / Bulanan</strong></div>
    <div class="tile">Laporan Pembelian<strong>Supplier</strong></div>
    <div class="tile">Retur & Mutasi<strong>Audit Stok</strong></div>
  </div>
</section>"#
        .to_string()
}

fn pengaturan() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>Utility</span><span>Setup User, Password, Tanggal</span></div>
  <div class="grid">
    <div class="tile">Setup User<strong>Admin / Kasir</strong></div>
    <div class="tile">Pembulatan<strong>Aturan Rupiah</strong></div>
    <div class="tile">Ganti Password<strong>Keamanan</strong></div>
    <div class="tile">Printer Nota<strong>POS-80 / Epson</strong></div>
  </div>
</section>"#
        .to_string()
}

fn not_found() -> String {
    r#"<section class="workarea">
  <div class="section-title"><span>404</span><span>Halaman tidak ditemukan</span></div>
</section>"#
        .to_string()
}
