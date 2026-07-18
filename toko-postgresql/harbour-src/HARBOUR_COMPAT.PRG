* Compatibility helpers for Harbour builds.
* These procedures exist in large legacy modules, but CR only needs their
* keyboard side effects. Keeping them here avoids linking unrelated reports.

Procedure XIT
   Maju := .T.
   Ada  := .T.
   Keyboard Chr(27)
Return

Procedure XMUNDUR
   Mundur := .T.
   Keyboard Chr(27)
Return

Procedure XHAPUSI
   Hapusi := .T.
   Keyboard Chr(13)
Return

Procedure DAFKSR
   Set Key 28 To
   Save Screen To sdaf
   Set Color To w+/br
   @ 1,55 Clear To 22,79
   @ 1,55 To 22,79
   @ 1,55 Say Chr(218) + Replicate(gl,23) + Chr(191)
   @ 3,55 Say Chr(195)
   @ 3,79 Say Chr(180)
   Declare field[2], kop[2], bts[2]
   field[1] := "NOMER+tl+NAMA"
   kop[1]   := "No.Nama Kasir"
   bts[1]   := ""
   DbEdit(2,56,21,78,field,"fksr","",kop,gl,bts)
   Restore Screen From sdaf
   Set Color To /bg
   Set Cursor On
Return

Function FKSR(mode, pointer)
   Do Case
   Case mode < 4
      Return 1
   Case LastKey() == 27
      Return 0
   Case LastKey() == 13
      nsls := nama
      Return 0
   Otherwise
      Return 1
   EndCase
Return 0

Procedure DAFSLS
Return

Procedure DAFTKG
Return
