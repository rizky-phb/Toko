// ============================================================
// File    : XXX.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use stok.dta
index on nama_brg to xxx
SELECT 2
use abc.dbf
DO WHILE .not. eof()
   nmb=nama_brg
   hj4=harga_4
   hj5=harga_5
   *
   SELECT 1
   seek nmb
   REPLACE harga_4 with hj4,harga_5 with hj5
   *
   SELECT 2
   skip
ENDDO
