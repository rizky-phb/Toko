// ============================================================
// File    : X.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
SET TALK OFF
// SET SCOR OFF  // Note: Not supported in Harbour
CLEAR
SET PRINTER ON
SET DEVICE TO PRINTER
a=1
DO WHILE a<=60
   b=LTRIM(str(a))
   c='chr(27)+chr(33)+chr(&b)'
   ?&c
   ?'&c'
   a=a+1
ENDDO
SET PRINTER OFF
SET DEVICE TO SCREEN

*@prow()+1,0 say chr(27)+'@'
*@prow()+1,0 say chr(27)+'p'+'0'+chr(0)+chr(15)
*@prow()+1,0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'255'
*@prow()+1,0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
*@prow()+1,0 say chr(27)+'p'+chr(1)+chr(29)+chr(73)
*@prow()+1,0 say chr(16)+chr(4)+chr(1)
