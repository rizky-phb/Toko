sele 1
use jenis.dta
inde on nama_klb to kelompok.ndx
sele 2
use abc
go top
Do While .not. eof()
   klb=kelompok
   TAKADA=.f.
   jml=0
   *
   sele 1
   seek klb
   if .not. foun()
      takada=.t.
EndIf
   *
   sele 2
   if takada
      repl supl with '1'
EndIf
   skip
EndDo
