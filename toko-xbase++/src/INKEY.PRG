set talk off
set stat off
set scor off
clea
Do While .t.
   @10,10 say 'Tekan Sembarang Tombol'
   Wait''
   @11,10 say 'Character= '+chr(lastkey())+'  Nilai= '+ltri(str(lastkey()))+'  '
   If lastkey()=27
      Exit
EndIf
EndDo
Rele all
