# Toko PostgreSQL

Versi baru dalam folder terpisah. Folder `toko-web-flask`, `toko-harbour`, dan
`toko-harbour-build` lama tetap menjadi arsip dan tidak diubah.

## Isi

- `web-flask`: Flask dengan PostgreSQL dan Waitress untuk Windows Server.
- `harbour-src`: salinan source Harbour serta fondasi koneksi PostgreSQL/ODBC.
- `harbour-build`: executable Harbour hasil build yang dapat direproduksi.

## Menjalankan Flask

1. Buat database dan user PostgreSQL.
2. Set `DATABASE_URL`, contoh:

   `postgresql://toko_app:PASSWORD@127.0.0.1:5432/toko`

3. Install dependency: `py -m pip install -r requirements.txt`.
4. Jalankan: `py wsgi.py`.
5. Periksa `http://127.0.0.1:8000/healthz`.

## Migrasi SQLite lama

Jalankan Flask sekali agar schema terbentuk, hentikan aplikasi, lalu:

`py tools/migrate_sqlite_to_postgres.py PATH_TOKO_SQLITE DATABASE_URL`

Migrator mengosongkan tabel target sebelum menyalin data. Selalu backup kedua
database sebelum menjalankannya di lingkungan produksi.

## Status Harbour

Flask sudah diarahkan ke PostgreSQL. Harbour belum sepenuhnya dikonversi; source
lama memiliki 44 modul dan memakai operasi DBF/NDX secara menyeluruh. Baca
`harbour-src/SQL-MIGRATION.md` sebelum mengubah executable produksi.

Build DBF-compatible dan utilitas tes koneksi PostgreSQL tersedia di
`harbour-build`. Jalankan `harbour-src/BUILD-ALL.bat` untuk build ulang.
