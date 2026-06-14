* LAPORAN TRANSAKSI KAS/HARIAN
*-----------------------------
Procedure lkas0
Sele 1
Uselan('KAS.DTA','','','','','')
Go top
Tglso=tanggal
Jmlso=rp_total
Close
*
Uselan('BAYARP.DTA','BAYARP1.NDX','','','','')
Sele 2
Uselan('BAYARH.DTA','BAYARH1.NDX','','','','')
*
Stor tglx to tgl,tgl2
Do While .t.
   Tgl=tgl2
   Set cent on
   @1,0 clea to 24,79
   @2,30 say 'Per Tanggal:'get tgl
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
EndIf
   Tgl1=tglso
   Tgl2=tgl
   Jmlx=jmlso
   Tgl =ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))

   Set cent on
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung...'
   Stor 0 to jml,jml2,jml3

   Set alte on
   Set alte to BUKU-KAS.RTF
   ?' LAPORAN KAS HARIAN'
   ?' PER TANGGAL: '+dtoc(tgl)
   ?chr(213)+repl(gd,5)+chr(209)+repl(gd,35)+repl(chr(209)+repl(gd,11),2)+chr(209)+repl(gd,12)+chr(184)
   ?tl+'Tgl. '+tl+'         Keterangan/Transaksi      '+tl+'      Debet'+tl+'     Kredit'+tl+'       Saldo'+tl
   ?chr(192)+repl(gl,5)+chr(193)+repl(gl,35)+repl(chr(193)+repl(gl,11),2)+chr(193)+repl(gl,12)+chr(217)
   Awl =1
   Bln1=mont(tgl1)
   Bln2=mont(tgl2)
   Do pesan with 'Tunggu, sedang dihitung'
Do While bln1<=bln2
      If bln1#bln2
         Sele 1
         Sum jumlah_byr to jml  for mont(tanggal)=bln1

         Sele 2
         Sum jumlah_byr to jml2 for mont(tanggal)=bln1

         Sele 3
         Do bacaflkas
         If file('&nf0')
            Do bukaflkas
            Sum jumlah_byr to rp_total for mont(tanggal)=bln1 .and. id_pos='D'
EndIf
         Jmlx =(jmlso+jml)-(jml2+jml3)
         Stor 0 to jml,jml2,jml3
         *
      Elseif bln1=bln2
         ?' '+left(dtoc(tgl),2)+'/'+subs(dtoc(tgl),4,2)+'  Saldo Awal per '+dtoc(tgl)+spac(35)+tran(jmlx,'@e 999,999,999')
         *
Do While tgl<=tgl2
            Sele 1
            Seek dtos(tgl)
Do While .not. eof() .and. tanggal=tgl
               Jml =jumlah_byr
               Jmlx=jmlx+jml
               Kataph=rtri(jenis_byr)+' '+rtri(nama_lgn)
               If len(kataph)<35
                  Kataph=kataph+spac(35-len(kataph))
               Else
                  Kataph=left(kataph,35)
EndIf
               ?' '+left(dtoc(tgl),2)+'/'+subs(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e 999,999,999        -     ')+tran(jmlx,'@e 999,999,999')
               Skip
EndDo
            *
            Sele 2
            Seek dtos(tgl)
Do While .not. eof() .and. tanggal=tgl
               Jml =jumlah_byr
               Jmlx=jmlx-jml
               Kataph=rtri(jenis_byr)+' '+rtri(nama_spl)
               If len(kataph)<35
                  Kataph=kataph+spac(35-len(kataph))
               Else
                  Kataph=left(kataph,35)
EndIf
               ?' '+left(dtoc(tgl),2)+'/'+subs(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e        -    999,999,999  ')+tran(jmlx,'@e 999,999,999')
               Skip
EndDo
            *
            Sele 3
            Do bacaflkas
            If file('&nf0')
               Do bukaflkas
               Seek dtos(tgl)
Do While .not. eof() .and. tanggal=tgl
                  If uppe(left(nama_perk,5))#'MODAL'
                     Jml=rp_total
                     Idp=id_pos
                     Kataph=kode_perk+rtri(keterangan)
                     If len(kataph)<35
                        Kataph=kataph+spac(35-len(kataph))
                     Else
                        Kataph=left(kataph,35)
EndIf
                     If idp='D'
                        Jmlx=jmlx+jml
                        ?' '+left(dtoc(tgl),2)+'/'+subs(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e 999,999,999        -     ')+tran(jmlx,'@e 999,999,999')
                     Else
                        Jmlx=jmlx-jml
                        ?' '+left(dtoc(tgl),2)+'/'+subs(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e        -    999,999,999  ')+tran(jmlx,'@e 999,999,999')
EndIf
EndIf
                  Skip
EndDo
EndIf
            Tgl=tgl+1
EndDo
         *
EndIf
      Bln1=bln1+1
EndDo
   ?repl(gl,80)
   ?' '
   Set alte off
   Set alte to
   Set cent off
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('BUKU-KAS.RTF'),1,0,22,210,.t.,'',210)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If lastkey()=-9
      Do cp
      If cc#0
         Set prin on
         Set devi to prin
         Set marg to 5
         ?chr(27)+chr(33)+chr(1)
         Type BUKU-KAS.RTF to prin
         Eject
         Set prin off
         Set devi to scre
EndIf
EndIf
   Tgl=tgl2
   *
EndDo
Clos all
Return

* LAPORAN TRANSAKSI KAS1
*------------------------
Procedure lkas1
Sele 10
Uselan('POS.DTA','POS.NDX','','','','')
Sele 11
Uselan('SUBPOS.DTA','SUBPOS2.NDX','','','','')
Sele 3
*
Stor spac(5) to kpos,kspos
Stor tglx to tgl,tgl2
Do While .t.
   @1,0 clea to 24,79
   Sele 10
   Set filter to id_lr='Y'
   Loca for id_lr='Y'
   Set cent on
   Set key 28 to dafperk
   @2,22 say 'Pos...:'get kpos pict '@!'
   @3,22 say 'SubPos:'get kspos pict '@!'
   @4,22 say 'Tanggal'get tgl
   @4,41 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai   F1=Daftar Perkiraan'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Do pesan with 'Tunggu, sedang dihitung'
   If kpos#spac(5)
      Sele 10
      Seek kpos
      If .not. foun()
         Do pesan2 with 'Tidak ada Pos Perkiraan '+rtri(kpos)
         Loop
EndIf
      Npos=nama_pos
      If kspos#spac(5)
         Sele 11
         Seek kpos+kspos
         If .not. foun()
            Do pesan2 with 'Tidak ada Perkiraan '+rtri(kpos)+' - '+rtri(kspos)
            Loop
EndIf
         Nspos=nama_perk
EndIf
EndIf
   Tgl4=tgl
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   Do bacaflkas
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   *
   Do bukaflkas
   *
   Stor 1 to cf,hal,no,n
   Cfx =ltri(str(cf))
   Arg1='L-BYA&cfx'+'.RTF'
   Set cent off
   Set alte on
   Set alte to &arg1
   If kpos=spac(5)
      ?' LAPORAN TRANSAKSI KAS'
      ?' Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(10)+'Hal:'+tran(hal,'999')
      ?chr(213)+repl(gd,8)+chr(209)+repl(gd,32)+chr(209)+repl(gd,25)+chr(209)+repl(gd,10)+chr(184)
      ?tl+'Tanggal '+tl+'Nama Perkiraan'+spac(18)+tl+'Keterangan'+spac(15)+tl+'Jumlah Rp.'+tl
      ?chr(195)+repl(gl,8)+chr(197)+repl(gl,32)+chr(197)+repl(gl,25)+chr(197)+repl(gl,10)+chr(180)
      Stor 0 to totd,totk
Do While .t.
         Sele 3
         Set filter to id_lr='Y'
         Seek dtos(tgl)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2)
EndIf
         Nr=recno()
         Set cent off
Do While .not. eof()
            If (tanggal>=tgl .and. tanggal<=tgl2)
               If id_pos='D'
                  Totd=totd+rp_total
               Elseif id_pos='K'
                  Totk=totk+rp_total
EndIf
               ?tl+dtoc(tanggal)+tl+left(nama_perk,25)+kode_perk+' '+id_pos+tl+left(keterangan,25)+tl+tran(rp_total,'@e 99,999,999')+tl
               N=n+1
               If n>200
                  ?chr(192)+repl(gl,8)+chr(193)+repl(gl,32)+chr(193)+repl(gl,25)+chr(193)+repl(gl,10)+chr(217)
                  ?' '
                  N  =1
                  Hal=hal+1
                  Cf =cf+1
                  Cfx=ltri(str(cf))
                  Arg1='L-BYA&cfx'+'.RTF'
                  Set alte to &arg1
                  ?' LAPORAN TRANSAKSI KAS'
                  ?' Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(10)+'Hal:'+tran(hal,'999')
                  ?chr(213)+repl(gd,8)+chr(209)+repl(gd,32)+chr(209)+repl(gd,25)+chr(209)+repl(gd,10)+chr(184)
                  ?tl+'Tanggal '+tl+'Nama Perkiraan'+spac(18)+tl+'Keterangan'+spac(15)+tl+'Jumlah Rp.'+tl
                  ?chr(195)+repl(gl,8)+chr(197)+repl(gl,32)+chr(197)+repl(gl,25)+chr(197)+repl(gl,10)+chr(180)
EndIf   
EndIf
            Skip
EndDo
         If beda
            Set cent on
            Do cariflkas
            If tgl>tgl2
               Exit
EndIf
            If file('&nf0')
               Do bukaflkas
EndIf
         Elseif beda=.f.
            Exit
EndIf
EndDo
      ?chr(192)+repl(gl,8)+chr(193)+repl(gl,32)+chr(193)+repl(gl,25)+chr(193)+repl(gl,10)+chr(217)
      ?' Total Debet  Rp'+tran(totd,'@e 9,999,999,999')
      ?'       Kredit Rp'+tran(totk,'@e 9,999,999,999')
   Else
      If kpos#spac(5) .and. kspos=spac(5)
         ?spac(11)+rtri(npos)
         ?spac(11)+'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?spac(10)+chr(213)+repl(gd,4)+chr(209)+repl(gd,5)+chr(209)+repl(gd,30)+chr(209)+repl(gd,11)+chr(184)
         ?spac(10)+tl+' No.'+tl+'Kode '+tl+'Nama Perkiraan'+spac(16)+tl+' Jumlah Rp.'+tl
         ?spac(10)+chr(195)+repl(gl,4)+chr(197)+repl(gl,5)+chr(197)+repl(gl,30)+chr(197)+repl(gl,11)+chr(180)
         Y=1
         Stor 0 to jml,tot
         Sele 11
         Seek kpos
Do While .not. eof() .and. kode_pos=kpos
            Kspos=kode_perk
            Nspos=nama_perk
            Jml=0
            Z=ltri(str(y))
Do While .t.
               Sele 3
               Set order to 2
               Seek kpos+kspos
               Nr=recno()
Do While .not. eof() .and. ( kode_pos=kpos .and. kode_perk=kspos)
                  If (tanggal>=tgl .and. tanggal<=tgl2)
                     Jml=jml +rp_total
EndIf
                  Skip
EndDo
               If beda
                  Set cent on
                  Do cariflkas
                  If tgl>tgl2
                     Exit
EndIf
                  If file('&nf0')
                     Do bukaflkas
EndIf
               Elseif beda=.f.
                  Exit
EndIf
EndDo
            ?spac(10)+tl+tran(y,'999 ')+tl+kspos+tl+nspos+tl+tran(jml,'@e 999,999,999')+tl
            Tot=tot+jml
            Y  =y + 1
            Jml=0
            Tgl=tgl4
            Do bacaflkas
            Do bukaflkas
            *
            Sele 11
            Skip
EndDo
         ?spac(10)+chr(192)+repl(gl,4)+chr(193)+repl(gl,5)+chr(193)+repl(gl,30)+chr(193)+repl(gl,11)+chr(217)
         ?spac(44)+'Total=Rp '+tran(tot,'@e 999,999,999')
      Elseif kpos#spac(5) .and. kspos#spac(5)
         ?spac(11)+'POS ...: '+rtri(npos)
         ?spac(11)+'SUB POS: '+rtri(nspos)
         ?spac(11)+'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?spac(10)+chr(213)+repl(gd,4)+chr(209)+repl(gd,10)+chr(209)+repl(gd,30)+chr(209)+repl(gd,11)+chr(184)
         ?spac(10)+tl+' No.'+tl+'Tanggal   '+tl+'Keterangan'+spac(20)+tl+' Jumlah Rp.'+tl
         ?spac(10)+chr(195)+repl(gl,4)+chr(197)+repl(gl,10)+chr(197)+repl(gl,30)+chr(197)+repl(gl,11)+chr(180)
         Y=1
         Stor 0 to jml,tot
Do While .t.
            Sele 3
            Set order to 2
            Seek kpos+kspos
            Nr=recno()
            Y=1
Do While .not. eof() .and. ( kode_pos=kpos .and. kode_perk=kspos)
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  ?spac(10)+tl+tran(y,'999 ')+tl+dtoc(tanggal)+tl+keterangan+tl+tran(rp_total,'@e 999,999,999')+tl
                  Tot=tot+rp_total
                  Y  =y+1
EndIf
               Skip
EndDo
            If beda
               Set cent on
               Do cariflkas
               If tgl>tgl2
                  Exit
EndIf
               If file('&nf0')
                  Do bukaflkas
EndIf
            Elseif beda=.f.
               Exit
EndIf
EndDo
         ?spac(10)+chr(192)+repl(gl,4)+chr(193)+repl(gl,10)+chr(193)+repl(gl,30)+chr(193)+repl(gl,11)+chr(217)
         ?spac(49)+'Total=Rp '+tran(tot,'@e 999,999,999')
EndIf
EndIf
   Set alte off
   Set alte to
   Stor spac(5) to kpos,kspos
   Tgl=tgl4
   *
   Layar('L-BYA')
   *
   Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
   If lastkey()=-7
      Do hpstr2
      Loop
EndIf
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
   ?chr(27)+chr(33)+chr(1)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-BYA&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Set cent on
Clos all
Return

*** PROSEDUR HAPUS TRANSAKSI
Procedure hpstr2
Sele 3
@1,0 clea to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Pesanx='ENTER=Edit   F8=Hapus   F10=Cetak'
Do pesan with pesanx
Declare field[8],kop[8],bts[8],frm[8]
Field[1]='TANGGAL'
Field[2]='KODE_PERK'
Field[3]='left(NAMA_PERK,25)'
Field[4]='ID_POS'
Field[5]='left(KETERANGAN,26)'
Field[6]='RP_TOTAL'
Field[7]='left(NAMA_KSR,14)'
Kop[1]='Tanggal'
Kop[2]='Kode'
Kop[3]='Nama Perkiraan'
Kop[4]=' '
Kop[5]='Keterangan'
Kop[6]='Jumlah Rp.'
Kop[7]='Nama Kasir'
Frm[1]='99-99-9999'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]=''
Frm[6]='@e 99,999,999'
Frm[7]='@!'
Bts[1]=''+tl
Bts[2]=''+tl
Bts[3]=''+tl
Bts[4]=' '
Bts[5]=''+tl
Bts[6]=''+tl
Bts[7]=''+tl
Dbedit(2,0,21,79,field,'fhtr2',frm,kop,gl,bts)
*
Set curs on
Return

Function fhtr2
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
Return 1
Elseif lastkey()=27
Return 0
Elseif lastkey()=13
   B   =row()
   Ket =keterangan
   Jml =rp_total
   Set curs on
   @b,43 get ket pict '@S26'
   @b,70 get jml pict '@e 99,999,999'
   @24,0
   Read
   If lastkey()#27
      Rlok()
      Repl keterangan with ket,rp_total with jml
      Ul()
      Commit
EndIf
   Set curs off
   Do pesan with pesanx
Return 2
Elseif lastkey()=-7
   Set colo to gr+*
   @row(),col() say &fds
   Set colo to g
   Do pesan with ''
   Set curs on
   Do pesan with 'Dihapus? Y/T'
   If ulangi(.t.)
      Rlok()
      Dele
      Ul()
      Commit
      Skip
EndIf
   Do pesan with pesanx
   Set curs off
Return 2
Else
   Do pesan with pesanx
   Set curs off
Return 1
EndIf
Return 0

* EOF LKEU
