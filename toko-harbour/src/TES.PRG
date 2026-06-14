sele 1
use stokx.dbf
sele 2
use stok.dta
go top
do whil .not. eof()
   kdb=kode_brg
   *
   sele 1
   loca for kode_brg=kdb
   hj=harga_21
   *
   sele 2
   repl harga_21 with hj
   skip
endd
