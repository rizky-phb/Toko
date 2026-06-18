# PANDUAN COMPILE CA-CLIPPER → HARBOUR → EXE

## HASIL KONVERSI INI
- 42 file `.PRG` sudah dikonversi ke format Harbour
- Tambahan: `GETE_FIX.PRG` (wrapper fungsi GETE)
- File project: `master.hbp`

---

## LANGKAH 1: INSTALL HARBOUR

Download dari: https://github.com/harbour/core/releases

**Windows (64-bit):**
- Download `harbour-3.x.x-win64.exe`
- Install ke `C:\Harbour`
- Tambahkan `C:\Harbour\bin` ke PATH Windows

Atau pakai **HMG Extended** (lebih lengkap, sudah include hbmk2):
https://hmgextended.com/

---

## LANGKAH 2: STRUKTUR FOLDER

Siapkan folder kerja, misal `C:\POS_TOKO\`:
```
C:\POS_TOKO\
  ├── *.PRG          ← semua file PRG hasil konversi ini
  ├── *.DTA          ← semua file data dari zip asli
  ├── *.SET          ← semua file SET dari zip asli
  ├── *.NDX          ← file index DBF (jika ada)
  ├── GETE_FIX.PRG
  └── master.hbp
```

---

## LANGKAH 3: COMPILE

Buka Command Prompt, masuk ke folder:
```cmd
cd C:\POS_TOKO
hbmk2 master.hbp
```

Jika berhasil, akan muncul file `master.exe`.

**Untuk compile satu file saja (misal CR.PRG):**
```cmd
hbmk2 CR.PRG -lhbct
```

---

## LANGKAH 4: JALANKAN

```cmd
cd C:\POS_TOKO
master.exe
```

---

## HAL-HAL YANG PERLU DIPERHATIKAN

### ✅ Yang sudah dikonversi otomatis:
| CA-Clipper | Harbour |
|---|---|
| `SET SAFE OFF` | `SET SAFETY OFF` |
| `SET CONS OFF` | `SET CONSOLE OFF` |
| `SET CONF ON` | `SET CONFIRM ON` |
| `SET DELE ON` | `SET DELETED ON` |
| `SET FIXE ON` | `SET FIXED ON` |
| `SET CENT ON` | `SET CENTURY ON` |
| `SET DECI TO 0` | `SET DECIMALS TO 0` |
| `SET DATE ITAL` | `SET DATE ITALIAN` |
| `SET PROC TO x` | `SET PROCEDURE TO x` |
| `SET EXCL OFF` | `SET EXCLUSIVE OFF` |
| `SET DEVI TO PRIN` | `SET DEVICE TO PRINTER` |
| `STOR` | `STORE` |
| `SELE` | `SELECT` |
| `LOCA FOR` | `LOCATE FOR` |
| `APPE BLAN` | `APPEND BLANK` |
| `REPL` | `REPLACE` |
| `INDE ON` | `INDEX ON` |
| `CLEA` | `CLEAR` |
| `CLOS` | `CLOSE` |
| `RELE` | `RELEASE` |
| `PARA` | `PARAMETERS` |
| `RETU` | `RETURN` |
| `ENDD` | `ENDDO` |
| `ENDI` | `ENDIF` |
| `DO WHIL` | `DO WHILE` |
| `FOUN()` | `FOUND()` |
| `LTRI()` | `LTRIM()` |
| `RTRI()` | `RTRIM()` |
| `SPAC()` | `SPACE()` |
| `SUBS()` | `SUBSTR()` |
| `GETE()` | `GetEnv()` |
| `PROC` | `PROCEDURE` |
| `FUNC` | `FUNCTION` |

### ⚠️ Yang perlu perhatian manual:

1. **`SET STAT OFF` / `SET SCOR OFF`**  
   Sudah diubah jadi komentar `//` — tidak ada di Harbour, tidak masalah.

2. **`DbEdit()`** — fungsi browse interaktif  
   Memerlukan library **hbct**. Sudah ditambahkan `-lhbct` di `master.hbp`.

3. **`SET PROCEDURE TO`**  
   Di Harbour, cara modern pakai `REQUEST` atau gabungkan semua file di `.hbp`.  
   Cara lama masih bisa jalan jika file `.PRG` dikompile bersamaan.

4. **Macro substitution `&var`**  
   Masih valid di Harbour. Jika ada error, periksa variabel yang dimacro.

5. **RDD (Record Data Driver)**  
   File `.DTA` Anda adalah DBF. Harbour perlu `REQUEST DBFCDX` atau `-lhbrddbfcdx`.  
   Tambahkan di file utama (CR.PRG atau KEU.PRG):
   ```harbour
   REQUEST DBFCDX
   ```

6. **`GETE("KASSA")`**  
   Dulu di Clipper membaca variabel environment DOS.  
   Di Harbour diganti `GetEnv("KASSA")` — sudah dikonversi.  
   Pastikan variabel environment `KASSA` di-set sebelum jalankan program:
   ```cmd
   SET KASSA=1
   master.exe
   ```

---

## JIKA ADA ERROR SAAT COMPILE

Error umum dan solusinya:

| Error | Solusi |
|---|---|
| `Undefined function DBEDIT` | Tambahkan `-lhbct` di compile |
| `Undefined function GETE` | Pastikan `GETE_FIX.PRG` ikut dikompile |
| `Cannot open SET PROCEDURE file` | Gabungkan semua PRG dalam satu perintah hbmk2 |
| `RDD not found` | Tambahkan `REQUEST DBFCDX` di file utama |
| `Undefined function NETERR` | Fungsi ini ada di Harbour, pastikan versi ≥ 3.0 |

---

## KONTAK / REFERENSI
- Harbour official: https://harbour.github.io
- Forum Harbour: https://groups.google.com/g/harbour-users
- Docs: https://harbour.github.io/doc/
