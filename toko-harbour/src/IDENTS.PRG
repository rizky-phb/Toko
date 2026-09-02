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
Empunya='DUA PUTRA JAYA-KOTA TEGAL'
Aidi1='DUA PUTRA JAYA'
Aidi2='KOTA TEGAL'
Retu
*
Stor 0 to tgl,tgl2
Do whil (tgl< Len(empunya))
   Tgl = tgl + 1
   Tgl2= tgl2+ asc(subs(empunya, tgl, 1))
Endd
*?aidi1
*?aidi2
*?tgl
*?tgl2
*?tgl2/tgl
*wait''                          
If (tgl2 # len(empunya) * 74 + 33)
   Set colo to br+
   @09,32 clea to 15,46
   @09,32 to 15,46 doub
   Set colo to r
   @10,35 say '  ?\,/?'
   @11,35 say ' \ ‰ ‰ /'
   @12,35 say ' (  ?  )'
   @13,35 say '   \ /  '
   @14,35 say '//  á  \\'
   Set colo to r+
   @11,38 say '‰'
   @11,40 say '‰'
   @13,39 say 'š'
   Set colo to r+*
   @14,39 say 'á'
   @15,0  say ''
   @16,0  say ''
   Clear All
   Rele All
   Quit
Endi
Retu
****
