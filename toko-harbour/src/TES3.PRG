sele 1
use stok.dta
sele 2
use jenis.dta
go top
do whil .not. eof()
   klb =kode_klb
   nklb=nama_klb
   *
   sele 1
   set filter to kelompok=nklb
   repl all kode_klb with klb for kelompok=nklb
   *
   sele 2
   skip
endd
