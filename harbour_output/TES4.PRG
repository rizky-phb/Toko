// ============================================================
// File    : TES4.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use stok2.dta
INDEX ON kode_bar to stok.ndx
SELECT 2
use stok.dta
go top
DO WHILE .not. eof()
   kdb=kode_bar
   jml=stok_a
   *
   SELECT 1
   seek kdb
   if FOUND()
      REPLACE stok_a with jml
   ENDIF
   *
   SELECT 2
   skip
ENDDO
