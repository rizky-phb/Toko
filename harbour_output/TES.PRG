// ============================================================
// File    : TES.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use stokx.dbf
SELECT 2
use stok.dta
go top
DO WHILE .not. eof()
   kdb=kode_brg
   *
   SELECT 1
   LOCATE FOR kode_brg=kdb
   hj=harga_21
   *
   SELECT 2
   REPLACE harga_21 with hj
   skip
ENDDO
