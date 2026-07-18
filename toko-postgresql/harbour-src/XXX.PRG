sele 1
use stok.dta
index on nama_brg to xxx
sele 2
use abc.dbf
do whil .not. eof()
   nmb=nama_brg
   hj4=harga_4
   hj5=harga_5
   *
   sele 1
   seek nmb
   repl harga_4 with hj4,harga_5 with hj5
   *
   sele 2
   skip
endd
