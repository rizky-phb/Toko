sele 1
use stok2.dta
inde on kode_bar to stok.ndx
sele 2
use stok.dta
go top
do whil .not. eof()
   kdb=kode_bar
   jml=stok_a
   *
   sele 1
   seek kdb
   if foun()
      repl stok_a with jml
   endi
   *
   sele 2
   skip
endd
