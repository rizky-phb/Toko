PATCH TEMPLATE NOTA THERMAL 80 MM

Perubahan:
1. CR.PRG: mode thermal lebar 80mm memakai layout 45 karakter.
2. Header dipusatkan dan menggunakan TOKO SEMBAKO DAN SNACK + aidi1/aidi2 + alamat + alm3 (nomor WA/telepon bila terisi).
3. Detail barang: nama barang satu baris, lalu qty x satuan x harga dengan subtotal di kanan.
4. Bagian total dibuat menyerupai nota contoh: itm., TOTAL, BAYAR, KEMBALI/KURANG.
5. Footer default: KOMPLAIN HARUS MEMBAWA NOTA / MAKSIMAL DALAM WAKTU 1 HARI / TERIMA KASIH jika promo1..promo4 kosong.
6. Default lebar roll untuk KASSA.SET baru diubah dari 5 (58mm) menjadi 8 (80mm). Pengaturan kasir yang sudah tersimpan tetap perlu dipilih menjadi 8 melalui menu SET PRINTER.

Catatan:
- Source ini belum dikompilasi menjadi EXE karena compiler Clipper/PLINK86/Harbour tidak tersedia di lingkungan kerja.
- Format angka mengikuti formatter yang sudah digunakan program; printer thermal harus diatur pada 80mm dan mode Thermal (T).
- Untuk posisi kanan subtotal, lebar efektif diasumsikan 45 karakter.
