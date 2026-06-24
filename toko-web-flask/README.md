# Toko Web Flask

Port awal dari kodingan Harbour/Clipper toko lama menjadi aplikasi web Flask.

Fitur yang sudah dipindahkan sebagai fondasi:

- Landing page bergaya desktop Windows dengan shortcut `Kasir` dan `Stok`.
- Login kasir di `/kasir` memakai ID kasir, nomor kassa default `1`, dan pilihan cetak nota.
- Login stok di `/stok` memakai password `00`.
- Seed kasir memakai data dari `KASIR.DTA` lama.
- Nomor kassa mengikuti session, dengan fallback dari environment variable `KASSA`, lalu default `1`.
- Dashboard, kasir/POS, stok barang, pembelian, laporan, dan pengaturan.
- SQLite lokal untuk master kasir, kassa, produk, penjualan, dan item penjualan.
- Tambah dan cari stok barang dari halaman web.
- Tampilan mengikuti nuansa layar Harbour/DOS lama.

## Menjalankan

```powershell
cd C:\Users\User\Downloads\Toko\toko-web-flask
py -m venv .venv
.\.venv\Scripts\Activate.ps1
pip install -r requirements.txt
flask --app app run --debug --port 5000
```

Buka:

```text
http://localhost:5000/
```

Login awal:

- `1` / `00` = ROYANI
- `2` / `00` = MAKSUM
- `3` / `00` = RIZQIFAUZI
- `4` / `00` = ZAM-ZAMI

Untuk kasir cukup isi ID kasir, misalnya `1`. Untuk stok isi password `00`.

## Catatan

Ini belum konversi penuh semua file `.PRG`; ini fondasi Flask yang mengambil struktur dan aturan penting dari port Harbour/PHP sebelumnya agar modul berikutnya bisa dipindahkan bertahap.
