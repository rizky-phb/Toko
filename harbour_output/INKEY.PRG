// ============================================================
// File    : INKEY.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SET TALK OFF
// SET STAT OFF  // Note: Not supported in Harbour
// SET SCOR OFF  // Note: Not supported in Harbour
CLEAR
DO WHILE .t.
   @10,10 say 'Tekan Sembarang Tombol'
   Wait''
   @11,10 say 'Character= '+chr(lastkey())+'  Nilai= '+LTRIM(str(lastkey()))+'  '
   If lastkey()=27
      Exit
   ENDIF
ENDDO
RELEASE all
