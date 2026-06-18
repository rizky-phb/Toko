// ============================================================
// File    : TES3.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use stok.dta
SELECT 2
use jenis.dta
go top
DO WHILE .not. eof()
   klb =kode_klb
   nklb=nama_klb
   *
   SELECT 1
   set filter to kelompok=nklb
   REPLACE all kode_klb with klb for kelompok=nklb
   *
   SELECT 2
   skip
ENDDO
