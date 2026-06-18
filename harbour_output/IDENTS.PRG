// ============================================================
// File    : IDENTS.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
*** IDENTITAS
*Empunya='YONEV Computer(0283)3300984TEGALASIH JAYA-HARJOSARI LOR'
*Aidi1=subs(empunya,33,9)    &&& 70 + 51
*Aidi2=subs(empunya,43,13)
*---
*Empunya='YONEV Computer(0283)3300984TEGALBERKAH CALISTA-DUKUH AREN'
*Aidi1=subs(empunya,33,14)  &&& 70 + 34
*Aidi2=subs(empunya,48,10)
*---
*Empunya='YONEV Computer(0283)3300984TEGALTOKO AZAFA-KETUREN'
*Aidi1=subs(empunya,33,10)  &&& 71 + 35
*Aidi2=subs(empunya,44,7)
*---
*Empunya='YONEV Computer(0283)3300984TEGALTOKO INDAH-TUWEL'
*Aidi1=subs(empunya,33,10)  &&& 71 + 37
*Aidi2=subs(empunya,44,5)
*---
*Empunya='YONEV Computer(0283)3300984TEGALTOKO KHOIMAH-GROBOG WETAN'
*Aidi1=subs(empunya,33,12)  &&& 71 + 17
*Aidi2=subs(empunya,46,12)
*
*Empunya='YONEV Computer(0283)3300984TEGALMM.BERKAH-SLAWI'
*Aidi1=subs(empunya,33,9)    &&& 71 + 15
*Aidi2=subs(empunya,43,5)
*
*Empunya='YONEV Computer(0283)3300984TEGALDP MART-PENER'
*Aidi1=subs(empunya,33,7)    &&& 71 + 10
*Aidi2=subs(empunya,41,5)
*
*Empunya='YONEV Computer(0283)3300984TEGALSWADES BERSAHAJA-KAJEN'
*Aidi1=subs(empunya,33,16)    &&& 7 + 48
*Aidi2=subs(empunya,50,5)
*
*Empunya='YONEV-Computer-Tegal-0817289414TOKO TAUFIK-KEBANDINGAN'
*Aidi1=subs(empunya,32,11)    &&& 74 + 36
*Aidi2=subs(empunya,44,11)
*
*Empunya='YONEV-Computer-Tegal-0817289414ALHAS MART-KEDAWON'
*Aidi1=subs(empunya,32,10)    &&& 74 + 36
*Aidi2=subs(empunya,43,7)
*
*Empunya='YONEV-Computer-Tegal-0817289414PUTRA JAYA-KESAMIRAN'
*Aidi1=subs(empunya,32,10)    &&& 75 + 8
*Aidi2=subs(empunya,43,9)
*
Empunya='YONEV-Computer-Tegal-0817289414MR.FAUZI ZAMI-BANDUNG'
Aidi1=SUBSTR(empunya,32,13)    &&& 74 + 33
Aidi2=SUBSTR(empunya,46,7)
*
STORE 0 to tgl,tgl2
DO WHILE (tgl< Len(empunya))
   Tgl = tgl + 1
   Tgl2= tgl2+ asc(SUBSTR(empunya, tgl, 1))
ENDDO
*?aidi1
*?aidi2
*?tgl
*?tgl2
*?tgl2/tgl
*wait''                          
If (tgl2 # len(empunya) * 74 + 33)
   SET COLOR TO br+
   @09,32 CLEAR to 15,46
   @09,32 to 15,46 doub
   SET COLOR TO r
   @10,35 say '  ³\,/³'
   @11,35 say ' \ ë ë /'
   @12,35 say ' (  ­  )'
   @13,35 say '   \ /  '
   @14,35 say '//  ß  \\'
   SET COLOR TO r+
   @11,38 say 'ë'
   @11,40 say 'ë'
   @13,39 say 'Ü'
   SET COLOR TO r+*
   @14,39 say 'ß'
   @15,0  say ''
   @16,0  say ''
   Clear All
   RELEASE All
   Quit
ENDIF
RETURN
****
