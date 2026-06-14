# Toko Web PHP

Port awal dari source Clipper/dBase (`.PRG`) aplikasi toko lama.

Source lama yang sudah ditemukan:

- `CR.PRG`: kasir/POS.
- `CRX.PRG`: helper kasir, cetak nota, pencarian barang, pembulatan, tunda nota.
- `STOK.PRG`: menu stok, transaksi pembelian, laporan, utility.
- `INDEK.PRG`: re-index dan pembuatan file data bulanan/per-kassa.
- `MPB.PRG`: pembelian.
- `RTB.PRG`: retur pembelian.
- `BANT.PRG`: helper umum, pesan, hari, pembulatan, master data.

Catatan penting dari `CR.PRG`:

```clipper
Ksa = gete("KASSA")
Useidx('KASSA.SET')
Loca for no_kassa=ksa
```

Jadi nomor kassa aplikasi lama dibaca dari environment variable `KASSA`.
Di web baru ini, nomor kassa disimpan di session dan default ke `1`.

## Menjalankan

Butuh PHP dengan extension `pdo_sqlite`.

```powershell
cd C:\Users\User\Downloads\Toko\toko-web-php
php -S localhost:8080 -t public
```

Buka:

```text
http://localhost:8080/kasir
http://localhost:8080/stok-barang
```

## Login Awal

Seed user mengikuti `KASIR.DTA` lama:

- `1` / `00` = ROYANI
- `2` / `00` = MAKSUM
- `3` / `00` = RIZQIFAUZI
- `4` / `00` = ZAM-ZAMI

## Status Porting

Ini belum konversi penuh semua `.PRG`; ini fondasi web yang mulai mengambil logika dari source lama.

