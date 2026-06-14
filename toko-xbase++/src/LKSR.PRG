Procedure LKSR
* LAPORAN PENDPTAN KASIR
Sele 1
*
Sele 2
*
Sele 3
Uselan('KASIR.DTA','','','','','')

Stor tglx to tgl,tgl2
Do While .t.
   Ksa=' '
   Nsls=spac(20)
   Sele 3
   Set filter to
   @1,0 clea to 24,79
   @2,24 say 'Kassa.:'get ksa pict '9'
   @3,24 say 'Kasir.:'get nsls pict '@!'
   @4,24 say 'Tanggal'get tgl
   @4,43 say 'sampai dg.'get tgl2
   Do pesan with 'ESC-Selesai    Kassa=0 (u/Perincian per Kassa)'
   Read
   @24,0
   If lastkey()=27
      Exit
EndIf
   If nsls#spac(20)
      Loca for nama_ksr=rtri(nsls)
      If .not. foun()
         Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
         Wait''
         Loop
EndIf
      Nsls=nama_ksr
EndIf
   Do bacafile
   Hal =1
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf

   Sele 1
   Uselan('&nf0','&nf3','','','','')
   Sele 2
   Uselan('RETURJN.DTA','RETURJN1.NDX','','','','')
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   @1,1 say 'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(37)+'Hal: '+tran(hal,'99')
   @2,0 say chr(213)+repl(gd,4)+chr(209)+repl(gd,21)+repl(chr(209)+repl(gd,12),4)+chr(184)
   @3,0 say tl+' No.'+tl+' Ks. Nama Kasir      '+tl+'     Rp.Jual'+tl+'    Rp.Retur'+tl+'    Rp.Pokok'+tl+'   Rp.Margin'+tl
   @4,0 say chr(195)+repl(gl,4)+chr(197)+repl(gl,21)+repl(chr(197)+repl(gl,12),4)+chr(180)
   B=5
   Stor 0 to hp,hj,rtr,jml,jml2,jml3,tot,tot2,tot3,n
   Tgl3=tgl
   If ksa='0' .and. nsls=spac(20)
      Sele 3
      Go top
Do While .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         For ch=1 to 5
             Ksa=ltri(str(ch))
             Do tampilksr
         Next
         @b,0   say tl+spac(4)+tl+spac(21)+repl(tl+spac(12),4)+tl
         @b+1,0 say chr(195)+repl(gl,4)+chr(194)+repl(gl,21)+repl(chr(197)+repl(gl,12),4)+chr(180)
         Set colo to w+
         @b,6 say '     J u m l a h --->'
         @b,28 say tran(jml2,'@e 9999,999,999')
         @b,41 say tran(jml3,'@e 9999,999,999')
         @b,54 say tran(jml ,'@e 9999,999,999')
         @b,67 say tran((jml2-jml3)-jml,'@e 9999,999,999')
         Set colo to g
         B=b+2
         Stor 0 to jml,jml2,jml3

         Sele 3
         Skip
EndDo
   Elseif ksa=' ' .and. nsls=spac(20)
      Sele 3
      Go top
Do While .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         Sele 1
         Set filter to kassa#' ' .and. nama_ksr=nsls
Do While tgl<=tgl2
            Seek dtos(tgl)
            If .not. foun()
               Tgl=tgl+1
            Elseif foun()
               N   =n+1
               @b,0 say tl+tran(n,'999.')+tl+' '+ksa+'   '+left(nsls,16)+repl(tl+spac(12),4)+tl
               Ada=.t.
               Exit
EndIf
EndDo
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
            Hp  =hp  + &brs14
            Hj  =hj  + &brs1
            If xOwner
               @b,28 say tran(hj,'@e 9999,999,999')
               @b,54 say tran(hp,'@e 9999,999,999')
               @b,67 say tran(hj-hp,'@e 9999,999,999')
            Else
               @b,28 say tran(hj,'@e 9999,999,999')
EndIf
            Skip
EndDo
         Tgl=tgl3

         Sele 2
         Set filter to nama_ksr2=nsls
Do While tgl<=tgl2
            Seek dtos(tgl)
            If foun()
               Exit
            Else
               Tgl=tgl+1
EndIf
EndDo
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
            Rtr=rtr + (jumlah*harga_2)
            If xOwner
               @b,41 say tran(rtr,'@e 9999,999,999')
               @b,67 say tran((hj-rtr)-hp,'@e 9999,999,999')
            Else
               @b,41 say tran(hj,'@e 9999,999,999')
EndIf
            Skip
EndDo

         Jml =jml +hp
         Jml2=jml2+hj
         Jml3=jml3+rtr
         Tot =tot +hp
         Tot2=tot2+hj
         Tot3=tot3+rtr
         Tgl =tgl3
         Stor 0 to hp,hj,rtr
         If ada
            Ada=.f.
            B=b+1
EndIf

         Sele 3
         Skip
EndDo
   Elseif ksa=' ' .and. nsls#spac(20)
      For ch=1 to 5
          Ksa=ltri(str(ch))
          Do tampilksr
      Next
   Elseif ksa#' ' .and. nsls=spac(20)
      Sele 3
      Go top
Do While .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         Do tampilksr
         Sele 3
         Skip
EndDo
   Elseif ksa#' ' .and. nsls#spac(20)
      Do tampilksr
EndIf
   @b,0 say chr(192)+repl(gl,4)+chr(193)+repl(gl,21)+repl(chr(193)+repl(gl,12),4)+chr(217)
   @b+1,0 clea to 24,79
   Set colo to w+
   If xOwner
      @b+1,19 say 'Total=Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e 9999,999,999 ')+tran(tot,'@e 9999,999,999 ')+tran((tot2-tot3)-tot,'@e 9999,999,999')
   Else
      @b+1,19 say 'Total=Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e 9999,999,999 ')
EndIf
   Set colo to g
   @24,0
   Do pesan3 with 'ESC-Selesai   F10-Cetak'
   If nlp=0
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Set prin on
   Set devi to prin
   Set marg to 10
   ?chr(27)+chr(33)+chr(4)
   @prow()+1,1 say 'PENDAPATAN KASIR   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @prow()+1,0 say '*'+repl('=',86)+'*'
   @prow()+1,0 say '| No.| Ks. Nama Kasir          |     Rp.Jual |    Rp.Retur |    Rp.Pokok |   Rp.Margin |'
   @prow()+1,0 say '|'+repl('-',86)+'|'
   B=5
   Stor 0 to hp,hj,tot,tot2,n
   Tgl3=tgl
   If ksa=' ' .and. nsls=spac(20)
      For ch=1 to 5
          Ksa=ltri(str(ch))
          Sele 3
          Go top
Do While .not. eof()
             Nsls=nama_ksr
             Ada =.f.
             Do tampilksr2
             Sele 3
             Skip
EndDo
      Next
   Elseif ksa=' ' .and. nsls#spac(20)
      For ch=1 to 5
          Ksa=ltri(str(ch))
          Do tampilksr2
      Next
   Elseif ksa#' ' .and. nsls=spac(20)
      Sele 3
      Go top
Do While .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         Do tampilksr2
         Sele 3
         Skip
EndDo
   Elseif ksa#' ' .and. nsls#spac(20)
      Do tampilksr2
EndIf
   @b,0 say '*'+repl('=',86)+'*'
   If xOwner
      @b+1,1 say '      Total ='+spac(17)+'Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e  9999,999,999 ')+tran(tot,'@e  9999,999,999 ')+tran((tot2-tot3)-tot,'@e  9999,999,999')
EndIf
   @b+4,0 say ' '
   Set prin off
   Set devi to scre
EndDo
Clos all
Return
Return

Procedure tampilksr
Sele 1
If nsls#spac(20)
   Set filter to kassa=ksa .and. nama_ksr=nsls
Elseif nsls=spac(20)
   Set filter to kassa=ksa
EndIf
Do While tgl<=tgl2
   Seek dtos(tgl)
   If .not. foun()
      Tgl=tgl+1
   Elseif foun()
      N   =n+1
      @b,0 say tl+tran(n,'999.')+tl+' '+ksa+' - '+left(nsls,16)+repl(tl+spac(12),4)+tl
      Ada=.t.
      Exit
EndIf
EndDo
Tgl=tgl3
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
   Hp  =hp  + &brs14
   Hj  =hj  + &brs1
   If xOwner
      @b,28 say tran(hj,'@e 9999,999,999')
      @b,54 say tran(hp,'@e 9999,999,999')
      @b,67 say tran(hj-hp,'@e 9999,999,999')
   Else
      @b,28 say tran(hj,'@e 9999,999,999')
EndIf
   Skip
EndDo

Tgl=tgl3
Sele 2
If nsls#spac(20)
   Set filter to kassa=ksa .and. nama_ksr2=nsls
Elseif nsls=spac(20)
   Set filter to kassa=ksa
EndIf
Do While tgl<=tgl2
   Seek dtos(tgl)
   If foun()
      Exit
   Else
      Tgl=tgl+1
EndIf
EndDo
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
   Rtr=rtr + (jumlah*harga_2)
   If xOwner
      @b,41 say tran(rtr,'@e 9999,999,999')
      @b,67 say tran((hj-rtr)-hp,'@e 9999,999,999')
   Else
      @b,41 say tran(hj,'@e 9999,999,999')
EndIf
   Skip
EndDo

Jml =jml +hp
Jml2=jml2+hj
Jml3=jml3+rtr
Tot =tot +hp
Tot2=tot2+hj
Tot3=tot3+rtr
Tgl =tgl3
Stor 0 to hp,hj,rtr
If ada
   Ada=.f.
   B=b+1
   If b>=22
      B=5
      Hal=hal+1
      Do pesan with 'Tekan ENTER Untuk Halaman Berikutnya'
      Wait''
      @b+1,0 
      @1,1 say 'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(25)+'Hal: '+tran(hal,'99')
      Do pesan with 'Tunggu, sedang dihitung'
EndIf
EndIf
Return
Procedure tampilksr2
Sele 1
If nsls#spac(20)
   Set filter to kassa=ksa .and. nama_ksr=nsls
Elseif nsls=spac(20)
   Set filter to kassa=ksa
EndIf
Do While tgl<=tgl2
   Seek dtos(tgl)
   If .not. foun()
      Tgl=tgl+1
   Elseif foun()
      Ada=.t.
      Exit
EndIf
EndDo
Tgl=tgl3
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
   Hp  =hp  + &brs14
   Hj  =hj  + &brs1
   Skip
EndDo
Tgl=tgl3

Sele 2
If nsls#spac(20)
   Set filter to kassa=ksa .and. nama_ksr2=nsls
Elseif nsls=spac(20)
   Set filter to kassa=ksa
EndIf
Do While tgl<=tgl2
   Seek dtos(tgl)
   If .not. foun()
      Tgl=tgl+1
   Elseif foun()
      Ada=.t.
      Exit
EndIf
EndDo
Tgl=tgl3
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
   Rtr=rtr + (jumlah*harga_2)
   Skip
EndDo
If hj#0 .or. hp#0
   N=n+1
   @b,0 say tran(n,'|999.| ')+ksa+' - '+nsls+'|'+tran(hj,'@e 9999,999,999 |')+tran(rtr,'@e 9999,999,999 |')+tran(hj,'@e 9999,999,999 |')+tran((hj-rtr)-hp,'@e 9999,999,999 |')
   B=b+1
EndIf
Tot =tot +hp
Tot2=tot2+hj
Tot3=tot3+rtr
Tgl =tgl3
Stor 0 to hp,hj,rtr
Return

* EOF LKSR
