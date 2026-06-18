// ============================================================
// File    : TES5.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SELECT 1
use stok.dta
INDEX ON kode_bar to stok.ndx
SELECT 2
use stok2.dta
DO WHILE .not. eof()
   Kdb=SPACE(7)
   Ksp=kode_spl
   Klb=kode_klb
   Kdb2=kode_bar
   Nmb=nama_brg
   St =satuan
   Hp =harga
   Hpb=harga_b
   Hj =harga_2
   Hj3 =harga_3
   Hj4 =harga_4
   Hj5 =harga_5
   Bts3=batas_3
   Bts4=batas_4
   Bts5=batas_5
   Stb=satuan_b
   Isi=isine_b
   Hjx =persen
   *
   SELECT 1
   seek kdb2
   if .not. FOUND()
      APPEND BLANK
      REPLACE kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdb2,;
           nama_brg with nmb,satuan with st,harga_b with hpb,harga with hp,harga_2 with hj
      REPLACE harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5,;
           satuan_b with stb,harga_b with hpb,isine_b with isi,persen with hjx
   else
      REPLACE harga with hp,harga_b with hpb,harga_2 with hj,harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5,persen with hjx
   ENDIF
   *
   SELECT 2
   skip
ENDDO

           


