# Harbour build

- `cr-dbf-wvt.exe`: kasir Harbour dengan database DTA/NDX.
- `stok-dbf-wvt.exe`: modul stok Harbour dengan database DTA/NDX.
- `indek-dbf-wvt.exe`: utility indeks DTA/NDX.
- `pg-connection-test.exe`: pemeriksaan koneksi PostgreSQL melalui psqlODBC.

Nama `dbf` disengaja: executable operasional belum memakai PostgreSQL. Jangan
menghapus DTA/NDX atau menganggap build ini sudah berbagi tabel dengan Flask.

Untuk build ulang, jalankan `harbour-src\BUILD-ALL.bat` pada Windows yang
memiliki Harbour di `C:\hb30`.

## Tes PostgreSQL

Install PostgreSQL Unicode ODBC Driver 64-bit, lalu set:

```bat
set TOKO_PG_HOST=127.0.0.1
set TOKO_PG_DATABASE=toko
set TOKO_PG_USER=toko_app
set TOKO_PG_PASSWORD=PASSWORD
pg-connection-test.exe
```

Password hanya disimpan di environment proses pengujian, bukan di source.
