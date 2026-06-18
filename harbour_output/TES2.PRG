// ============================================================
// File    : TES2.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use jenis.dta
INDEX ON nama_klb to kelompok.ndx
SELECT 2
use abc
go top
DO WHILE .not. eof()
   klb=kelompok
   TAKADA=.f.
   jml=0
   *
   SELECT 1
   seek klb
   if .not. FOUND()
      takada=.t.
   ENDIF
   *
   SELECT 2
   if takada
      REPLACE supl with '1'
   ENDIF
   skip
ENDDO
