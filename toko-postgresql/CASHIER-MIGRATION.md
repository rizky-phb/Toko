# Migrasi modul kasir

Versi Flask PostgreSQL sudah mencakup alur kasir operasional berikut:

| Fungsi lama Harbour | Implementasi Flask PostgreSQL |
| --- | --- |
| Scan/cari barcode dan nama, qty `n*` | POS `/kasir`, pencarian produk dan scanner keyboard |
| Harga normal, member, grosir 3/4/5 | Harga otomatis berdasarkan member dan jumlah; harga manual saat koreksi item |
| Item bebas | F2: nama, harga, dan qty item bebas tanpa mengubah stok |
| Member/pelanggan | F1, pencarian kode/nama, alamat, diskon member, saldo piutang |
| Pembayaran, pembulatan, donasi | Hitung otomatis, F6 untuk donasi dari kembalian |
| Piutang dan pembayaran piutang | Transaksi kurang bayar untuk member, lalu pembayaran dari rekap |
| Poin pelanggan | Perolehan poin konfigurabel, penggunaan pada transaksi, F11 cek/ambil poin, buku mutasi poin |
| Tahan/transaksi gantung | F9 simpan dan pulihkan transaksi gantung |
| Cetak/download nota | F5 set preferensi, cetak/download setelah transaksi, cetak ulang dari rekap |
| Rekap dan pembatalan | F4/F10 rekap kasir per tanggal; pembatalan mengembalikan stok dan membalik poin/piutang |
| Nomor nota per kassa | Nomor nota dikunci pada data kasir saat transaksi dibuat |

## Migrasi riwayat JUAL.DTA

Setelah master produk, pelanggan, dan kasir tersedia, jalankan dari folder `web-flask`:

```powershell
python tools/migrate_legacy_cashier_to_postgres.py $env:DATABASE_URL ..\harbour-src
```

Importer membaca `JUAL*.DTA`, mengabaikan salinan file dan baris duplikat, lalu tidak mengimpor `sale_no` yang sudah ada. Lakukan backup PostgreSQL sebelum impor pertama dan uji dahulu di basis data salinan.

## Batasan desktop Harbour

Binary Harbour tetap dibangun untuk arsip dan penggunaan DBF lokal. Aplikasi web adalah kasir multi-pengguna yang memakai PostgreSQL; jangan arahkan kedua aplikasi ke transaksi aktif yang sama karena model databasenya berbeda.
