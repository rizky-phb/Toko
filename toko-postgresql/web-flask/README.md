# Toko Web Flask PostgreSQL

Versi PostgreSQL terpisah dari arsip SQLite/Harbour lama.

Fitur yang sudah dipindahkan sebagai fondasi:

- Landing page bergaya desktop Windows dengan shortcut `Kasir` dan `Stok`.
- Login kasir di `/kasir` memakai ID kasir, nomor kassa default `1`, dan pilihan cetak nota.
- Login stok di `/stok` memakai password `00`.
- Seed kasir memakai data dari `KASIR.DTA` lama.
- Nomor kassa mengikuti session, dengan fallback dari environment variable `KASSA`, lalu default `1`.
- Dashboard, kasir/POS, stok barang, pembelian, laporan, dan pengaturan.
- PostgreSQL untuk master kasir, kassa, produk, penjualan, dan item penjualan.
- Tambah dan cari stok barang dari halaman web.
- Tampilan mengikuti nuansa layar Harbour/DOS lama.

## Menjalankan

Set `DATABASE_URL`, install `requirements.txt`, lalu jalankan `python wsgi.py`.

Buka:

```text
http://localhost:8000/
```

Login awal:

- `1` / `00` = ROYANI
- `2` / `00` = MAKSUM
- `3` / `00` = RIZQIFAUZI
- `4` / `00` = ZAM-ZAMI

Untuk kasir cukup isi ID kasir, misalnya `1`. Untuk stok isi password `00`.

## Deploy

Lihat `DEPLOY.md` untuk deployment Windows Server 2019.

## Catatan

Ini belum konversi penuh semua file `.PRG`; ini fondasi Flask yang mengambil struktur dan aturan penting dari port Harbour/PHP sebelumnya agar modul berikutnya bisa dipindahkan bertahap.
