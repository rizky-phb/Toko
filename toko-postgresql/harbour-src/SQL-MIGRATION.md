# Migrasi Harbour ke PostgreSQL

Source dalam folder ini adalah salinan terpisah. Program produksi lama dan file
`.DTA/.NDX` tidak diubah.

Harbour dapat mengakses PostgreSQL melalui driver ODBC PostgreSQL (`psqlODBC`)
dan ADO Windows. Akan tetapi `USE`, `USELAN`, `USEIDX`, `SEEK`, `LOCATE`,
`APPEND BLANK`, `REPLACE`, alias/work-area, serta indeks NDX tidak otomatis
berubah menjadi SQL.

Urutan port yang aman:

1. Jadikan PostgreSQL milik Flask sebagai sumber data pusat.
2. Petakan tabel DTA ke schema SQL dan tentukan primary key yang stabil.
3. Ganti fungsi baca master (`STOK`, `CUST`, `KASIR`) dengan repository SQL.
4. Ganti penyimpanan transaksi dalam satu database transaction.
5. Port laporan dan utility/reindex terakhir.
6. Uji dua pengguna secara bersamaan sebelum melepas database DTA/NDX.

`sql/pg_connection.prg` hanya pemeriksaan koneksi. File itu belum membuat modul
Harbour lama aman memakai PostgreSQL.
