Procedure LPJ2
* LAPORAN PENJUALAN/KLP-ITEM
*----------------------------
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK4.NDX','','','','')
Sele 3
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
*
Stor tglx to tgl,tgl2
Ksp=spac(10)
Do While .t.
   Nmb=spac(30)
   Sele 3
   Set cent on
   Set filter to
   Set key 28 to daflgn
   @1,0 clea to 24,79
   @2,20 say 'Kode Plgn:'get ksp pict '@!'
   Do pesan with 'ESC=Selesai   F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Carikode=.f.
   Nsp='-'
   If ksp#spac(10)
      Set order to 1
      Seek ksp
      If .not. foun()
         Set order to 2
         Seek rtri(ksp)
         Do daflgn
EndIf
      If lastkey()=27
         Loop
EndIf
      Nsp=nama_lgn
EndIf
   *
   Sele 2
   @2,31
   @2,31 say rtri(nsp)+' ('+rtri(ksp)+')'
   @3,20 say 'Nama Brg.:'get nmb pict '@!'
   Do pesan with 'Masukan awalan Nama Barang'
   Read
   If lastkey()=27 .or. nmb=spac(30)
      Loop
EndIf
   Seek nmb
   If .not. foun()
      Carikode=.f.
      Seek rtri(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
EndIf
EndIf
   Klb=kode_klb
   Kdb=kode_bar
   Nmb=nama_brg
   @3,31 say nmb
   @4,20 say 'Tanggal..:'get tgl
   @4,42 say 'sampai'get tgl2
   Read
   If lastkey()=27
      Loop
EndIf
   Tgl4=tgl
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf2','','','','')
   Set filter to
   If ksp#spac(10)
      Set filter to kode_lgn=ksp
EndIf
   *
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   *
   Do koplpj2
   Stor 1 to y,n
   Stor 0 to stm,hp,hj,jml,tot,tot2
   Z=ltri(str(y))
Do While .t.
      Seek kdb
      St =satuan
Do While .not. eof() .and. kode_bar=kdb
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=&brs1
            Stm=stm+jumlah
            Hp =hp + &brs14
            Hj =hj + &brs1
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+left(nama_ksr,10)+tl+faktur+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(harga_2,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y=y+1
            N=n+1
            If n>50
               ?chr(192)+repl(gl,4)+chr(193)+repl(gl,16)+chr(193)+repl(gl,10)+chr(193)+repl(gl,5)+chr(193)+repl(gl,7)+chr(193)+repl(gl,9)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(217)
               ?''
               Hal=hal+1
               Cf =cf +1
               Cfx=ltri(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplpj2
               N=1
EndIf
EndIf
         Skip
EndDo
      If beda
         Do carifljual
         If tgl>tgl2
            Exit
EndIf
         If file('&nf0')
            Uselan('&nf0','&nf2','','','','')
            Set filter to kode_lgn=ksp
EndIf
         Set cent on
      Elseif beda=.f.
         Exit
EndIf
EndDo
   *
   If hj>0
      Tot2=tot2+ hp
      Tot =tot + hj
EndIf
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999,999 ')+st
   ?chr(192)+repl(gl,4)+chr(193)+repl(gl,16)+chr(193)+repl(gl,10)+chr(193)+repl(gl,5)+chr(193)+repl(gl,7)+chr(193)+repl(gl,9)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(217)
   ?' JUAL =Rp'+tran(tot, '@e 9,999,999,999')+'      Item=  '+pesanx
   If xOwner .or. Owner
      ?' POKOK=Rp'+tran(tot2,'@e 9,999,999,999')+'    Margin=Rp'+tran(tot-tot2,'@e 999,999,999')
EndIf
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Set prin on
   Set devi to prin
   Set marg to 5
   ?chr(27)+chr(33)+chr(1)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nsp,kdb,kdb2,nmb,nsp,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return
Return

Procedure koplpj2
?' LAPORAN PENJUALAN'
If ksp#spac(10)
   ?' PELANGGAN: '+rtri(nsp)+' ('+rtri(ksp)+')'
EndIf
?' NAMA BRG : '+rtri(nmb)+' ('+rtri(kdb)+')'
?' TANGGAL  : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(30)+'HAL:'+tran(hal,'999')
?chr(218)+repl(gl,4)+chr(194)+repl(gl,16)+chr(194)+repl(gl,10)+chr(194)+repl(gl,5)+chr(194)+repl(gl,7)+chr(194)+repl(gl,9)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)+chr(191)
?tl+' No.'+tl+'Tanggal    Jam  '+tl+'Kasir     '+tl+'Nota '+tl+'   Qty.'+tl+'   @Pokok'+tl+'    @Jual'+tl+'  Rp.Jumlah'+tl
?chr(195)+repl(gl,4)+chr(197)+repl(gl,16)+chr(197)+repl(gl,10)+chr(197)+repl(gl,5)+chr(197)+repl(gl,7)+chr(197)+repl(gl,9)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)+chr(180)
Return

* LAPORAN PENJUALAN/KLP-ITEM
*----------------------------
Procedure lpj21
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK3.NDX','','','','')
Sele 3
Uselan('JENIS.DTA','JENIS2.NDX','JENIS.NDX','','','')
*
Klb=spac(5)
Klp=.t.
Stor ' ' to jlap,jlap2,ket,flt
Stor tglx to tgl,tgl2
Do While .t.
   Sele 3
   Set cent on
   Set filter to
   Set key 28 to dafklb
   @1,0 clea to 23,79
   @2,24 say 'Kelompok:'get klb pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   @4,24 say '0 =Totalnya saja    1 =Terperinci..:'get jlap2 pict '!'
   Do pesan with 'ESC=Selesai     F1=Daftar_Kelompok_Brg'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Set order to 2
   Seek klb
   If .not. foun()
      Do pesan2 with 'Tidak ada Klp. '+rtri(klb)
      Loop
EndIf
   Nklb=nama_klb
   Tgl4=tgl
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf2','','','','')
   Set filter to
   *
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set cent off
   Set alte on
   Set alte to &arg1
   *
   If jlap2#'1'
      ?' LAPORAN PENJUALAN, KLP: '+rtri(nklb)
      ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?chr(218)+repl(gl,7)+chr(194)+repl(gl,28)+chr(194)+repl(gl,6)+repl(chr(194)+repl(gl,11),3)+chr(191)
      ?tl+'Kode   '+tl+'Nama Barang'+spac(17)+tl+'  Qty.'+tl+'    Rp.Jual'+tl+'   Rp.Pokok'+tl+'  Rp.Margin'+tl
      ?chr(195)+repl(gl,7)+chr(197)+repl(gl,28)+chr(197)+repl(gl,6)+repl(chr(197)+repl(gl,11),3)+chr(180)
      Y=1
      Stor 0 to jml,xjml,hp,hj,tot,tot2
      *
      Sele 2
      Seek klb
Do While .not. eof() .and. kode_klb=klb
         Kdb =kode_bar
         Nmb =nama_brg
         Z=ltri(str(y))
         *
Do While .t.
            Sele 1
            Seek kdb
Do While .not. eof() .and. kode_bar=kdb
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  xJml=xjml+jumlah
                  Hp =hp + &brs14
                  Hj =hj + &brs1
EndIf
               Skip
EndDo
            If beda
               Do carifljual
               If tgl>tgl2
                  Exit
EndIf
               If file('&nf0')
                  Uselan('&nf0','&nf2','','','','')
                  Set filter to
                  Do cekjlap
EndIf
            Elseif beda=.f.
               Exit
EndIf
EndDo
         *
         If hj#0
            Do fmtjml2
            ?tl+left(kdb,7)+tl+left(nmb,28)+tl+jml+tl+tran(hj,'@e 999,999,999')+tl+iif(xOwner,tran(hp,'@e 999,999,999')+tl+tran(hj-hp,'@e 999,999,999'),spac(11)+tl+spac(11))+tl
            Tot2=tot2+ hp
            Tot =tot + hj
            Y = y + 1
EndIf
         Stor 0 to jml,hp,hj,xjml
         Sele 2
         Skip
EndDo
      ?chr(192)+repl(gl,7)+chr(193)+repl(gl,28)+chr(193)+repl(gl,6)+repl(chr(193)+repl(gl,11),3)+chr(217)
      ?tran(y-1,'999 Item')+spac(28)+'Total Rp '+tran(tot,'@e 999,999,999 ')+iif(xOwner,tran(tot2,'@e 999,999,999 ')+tran(tot-tot2,'@e 999,999,999 '),'')
      *
   Else
      Do koplpj21
      Y=1
      Stor 0 to jml,xjml,hp,hj,tot,tot2
Do While tgl<=tgl2
         *
         Sele 2
         Seek klb
Do While .not. eof() .and. kode_klb=klb
            Kdb =kode_bar
            Nmb =nama_brg
            Z=ltri(str(y))
            *
            Sele 1
            Set filter to tanggal=tgl
            Seek kdb
            Tgl3=tanggal
            Tglan=dtoc(tanggal)
            Set cent off
Do While .not. eof() .and. kode_bar=kdb
               Fkt =faktur
               Ksp =kode_lgn
               xJml=jumlah
               Hp  =hp + &brs14
               Hj  =hj + &brs1
               Do fmtjml2
               If xjml#0
                  ?tl+tglan+tl+fkt+tl+iif(ksp#spac(10),right(rtri(ksp),5),spac(5))+tl+left(kdb,7)+tl+nama_brg+tl+jml+tl+tran(harga_2-diskon_rp,'@e 9,999,999')+tl+tran(&brs1,'@e 999,999,999')+tl+iif(xOwner,tran(&brs1-&brs14,'@e 999,999,999'),spac(11))+tl
                  Y =y +1
                  No=no+1
                  If no>175
                     ?chr(192)+repl(gl,8)+repl(chr(193)+repl(gl,5),2)+chr(193)+repl(gl,7)+chr(193)+repl(gl,30)+chr(193)+repl(gl,6)+chr(193)+repl(gl,9)+repl(chr(193)+repl(gl,11),2)+chr(217)
                     ?''
                     Hal=hal+1
                     Cf =cf+1
                     Cfx=ltri(str(cf))
                     Arg1='L-JUAL&cfx'+'.RTF'
                     Set cent on
                     Set alte to &arg1
                     Do koplpj21
                     No=1
                     Set cent off
EndIf
EndIf
               Skip
               If tanggal=tgl3
                  Tglan=spac(8)
               Elseif tanggal#tgl3
                  Tglan=dtoc(tanggal)
EndIf
EndDo
            Tot2=tot2+ hp
            Tot =tot + hj
            Stor 0 to jml,hp,hj,xjml
            Sele 2
            Skip
EndDo
         Tgl=tgl+1
EndDo
      ?chr(192)+repl(gl,8)+repl(chr(193)+repl(gl,5),2)+chr(193)+repl(gl,7)+chr(193)+repl(gl,30)+chr(193)+repl(gl,6)+chr(193)+repl(gl,9)+repl(chr(193)+repl(gl,11),2)+chr(217)
      ?tran(y-1,'9999 Item')+spac(55)+'Total Jual Rp '+tran(tot,'@e 999,999,999 ')+iif(xOwner,tran(tot-tot2,'@e 999,999,999'),'')
      If xOwner
         ?spac(69)+'Pokok Rp '+tran(tot2,'@e 999,999,999')
EndIf
      *
EndIf
   ?' '
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Set prin on
   Set devi to prin
   If jlap2='0'
      Set marg to 10
      ?chr(27)+chr(33)+chr(1)
   Else
      Set marg to 15
      ?chr(27)+chr(33)+chr(4)
EndIf
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nklb,kdb,kdb2,nmb,nsp,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return
Procedure koplpj21
?' LAPORAN PENJUALAN, KLP: '+rtri(nklb)
?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(34)+'HAL:'+tran(hal,'999')
?chr(218)+repl(gl,8)+repl(chr(194)+repl(gl,5),2)+chr(194)+repl(gl,7)+chr(194)+repl(gl,30)+chr(194)+repl(gl,6)+chr(194)+repl(gl,9)+repl(chr(194)+repl(gl,11),2)+chr(191)
?tl+'Tanggal '+tl+'Nota '+tl+'Plgn.'+tl+'Kode   '+tl+'Nama Barang'+spac(19)+tl+'  Qty.'+tl+'    @Jual'+tl+' Jumlah Rp.'+tl+'  Rp.Margin'+tl
?chr(195)+repl(gl,8)+repl(chr(197)+repl(gl,5),2)+chr(197)+repl(gl,7)+chr(197)+repl(gl,30)+chr(197)+repl(gl,6)+chr(197)+repl(gl,9)+repl(chr(197)+repl(gl,11),2)+chr(180)
Return

* LAPORAN PENJUALAN/KLP-TOTAL
*----------------------------
Procedure lpj23
Sele 1
*
Sele 2
Uselan('JENIS.DTA','JENIS.NDX','','','','')
*
Count to jml
Stor ' ' to jlap,ket
Stor tglx to tgl,tgl2
Do While .t.
   @1,0 clea to 23,79
   @2,21 say 'Tanggal.:'get tgl
   @2,42 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Exit
EndIf
   If mont(tgl)#mont(tgl2)
      Do pesan2 with 'Bulan Harus Sama'
      Loop
EndIf
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf2','','','','')
   *
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?' '+aidi1
   ?' '+repl('-',len(aidi1))
   ?' REKAP PENJUALAN, PER KELOMPOK BARANG'
   ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(218)+repl(gl,4)+chr(194)+repl(gl,31)+repl(chr(194)+repl(gl,13),3)+chr(191)
   ?tl+' No.'+tl+'Kelompok Barang'+spac(16)+tl+'      Rp.Jual'+tl+'     Rp.Pokok'+tl+'    Rp.Margin'+tl
   ?chr(195)+repl(gl,4)+chr(197)+repl(gl,31)+repl(chr(197)+repl(gl,13),3)+chr(180)
   Y=1
   Stor 0 to tot,totx,tot2,jml
   Sele 2
   Go top
Do While .not. eof()
      Klb =kode_klb
      Nklb=nama_klb
      Z=ltri(str(y))
      *
      Sele 1
      Set filter to kode_klb=klb
      Go top
      If .not. eof()
Do While .not. eof() .and. kode_klb=klb
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml =jml +&brs14
               Tot2=tot2+&brs1
               Totx=totx+&brs14
               Tot =tot +&brs1
EndIf
            Skip
EndDo
EndIf
      If tot2#0
         ?tl+tran(y,'999.')+tl+klb+left(nklb,26)+tl+tran(tot2,'@e 9,999,999,999')+tl+iif(xOwner,tran(jml,'@e 9,999,999,999')+tl+tran(tot2-jml,'@e 9,999,999,999'),spac(13)+tl+spac(13))+tl
         Y = y + 1
EndIf
      Stor 0 to tot2,jml
      Sele 2
      Skip
EndDo
   ?chr(192)+repl(gl,4)+chr(193)+repl(gl,31)+repl(chr(193)+repl(gl,13),3)+chr(217)
   ?spac(11)+'Total Rp'+spac(19)+tran(tot, '@e 9,999,999,999 ')+iif(xOwner,tran(totx,'@e 9,999,999,999 ')+tran(tot-totx,'@e 9,999,999,999'),'')
   ?' '
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
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
   ?chr(27)+chr(33)+chr(9)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

* LAPORAN PENJUALAN/TGL/TOTAL
*----------------------------
Procedure lpj3
Stor tglx to tgl,tgl2
Stor ' ' to jlap,ket
Tgl=ctod('01-'+ltri(str(mont(tglx)))+'-'+ltri(str(year(tglx))))
Do While .t.
   @1,0 clea to 23,79
   @2,24 say 'Tanggal.:'get tgl
   @2,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Exit
EndIf
   If mont(tgl)#mont(tgl2)
      Do pesan with 'Bulan Harus Sama'
      Wait''
      Loop
EndIf
   @1,0 clea to 23,79
   Stor 0 to tot,tot2,tdns,jml,jml2,jdns
   Do konvtgl
   *
   Tgl1=tgl
   Awl =1
   Bln1=mont(tgl)
   Bln2=mont(tgl2)
   Hr  =''
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+&nf0
      Loop
EndIf
   Sele 3
   Uselan('&nf0','&nf3','','','','')
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?spac(5)+aidi1
   ?spac(5)+repl('-',len(aidi1))
   ?spac(4)+' REKAP PENJUALAN PER HARI'
   ?spac(4)+' TANGGAL : '+dtoc(tgl1)+' s/d '+dtoc(tgl2)
   ?spac(4)+chr(218)+repl(gl,13)+repl(chr(194)+repl(gl,13),4)+chr(191)
   ?spac(4)+tl+'Tgl - Hari   '+tl+'     Rp.Jual '+tl+'    Rp.Pokok '+tl+'   Rp.Margin '+tl+'   Rp.Donasi '+tl
   ?spac(4)+chr(195)+repl(gl,13)+repl(chr(197)+repl(gl,13),4)+chr(180)
Do While tgl<=tgl2
      Do hari
      Do konvtgl
      Tgl3=tgl
      Hri1=hri
      Bln1=hri
      Stor 0 to jml,jml2,xby
      Seek dtos(tgl)
Do While .not. eof() .and. (tanggal=tgl)
         Jml2=jml2+ &brs1
         Jml =jml + &brs14
         Jdns=jdns+ rp_donasi
         Tot2=tot2+ &brs1
         Tot =tot + &brs14
         Tdns=tdns+ rp_donasi
         Skip
EndDo
      ?spac(4)+tl+' '+hri1+' - '+hr+' '+tl+tran(jml2,'@e 9999,999,999 ')+tl+iif(xOwner,tran(jml,'@e 9999,999,999 ')+tl+tran(jml2-jml,'@e 9999,999,999 '),spac(13)+tl+spac(13))+tl+tran(jdns,'@e 9999,999,999 ')+tl
      If jml2#0
         Awl =awl+1
EndIf
      Stor 0 to jml,jml2,jdns
      Tgl =tgl3
      Tgl =tgl+1
EndDo
   Awl=awl-1
   ?spac(4)+chr(195)+repl(gl,13)+repl(chr(197)+repl(gl,13),4)+chr(180)
   ?spac(4)+tl+'  Total   Rp.'+tl+tran(tot2,'@e 9999,999,999 ')+tl+iif(xOwner,tran(tot,'@e 9999,999,999 ')+tl+tran(tot2-tot,'@e 9999,999,999 '),spac(13)+tl+spac(13))+tl+tran(tdns,'@e 9999,999,999 ')+tl
   ?spac(4)+chr(195)+repl(gl,13)+repl(chr(197)+repl(gl,13),4)+chr(180)
   If awl#0
      ?spac(4)+tl+'  Rata2   Rp.'+tl+tran(tot2/awl,'@e 9999,999,999 ')+tl+iif(xOwner,tran(tot/awl,'@e 9999,999,999 ')+tl+tran((tot2-tot)/awl,'@e 9999,999,999 '),spac(13)+tl+spac(13))+tl+tran(tdns/awl,'@e 9999,999,999 ')+tl
EndIf
   ?spac(4)+chr(192)+repl(gl,13)+repl(chr(193)+repl(gl,13),4)+chr(217)
   ?' '
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Tgl=tgl1
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
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
   ?chr(27)+chr(33)+chr(9)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Set cent on
Clos all
Rele klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

* LAPORAN PENJUALAN/BLN GLOBAL
*------------------------------
Procedure lpj31
Tgl=tglx
Do konvtgl
Stor bln+'-'+thn to tgl,tgl2
Do While .t.
   Sele 2
   Set filter to
   @1,0 clea to 23,79
   @2,26 say 'Dari:   -     sampai   -'
   @2,32 get tgl pict '99-9999'
   @2,47 get tgl2 pict '99-9999'
   Do pesan with 'DiIsi Bulan-Tahun saja'
   Read
   @23,0
   If lastkey()=27
      Exit
EndIf
   Bln =left(tgl,2)
   Thn =right(tgl,4)
   Bln2=left(tgl2,2)
   Thn2=right(tgl2,4)
   x1  =val(bln)
   x2  =val(bln2)
   If thn#thn2 .or. x1>x2
      Do pesan2 with 'Tahun Harus Sama  -atau-  Koreksi Lagi Bulannya'
      Loop
EndIf
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?spac(4)+aidi1
   ?spac(4)+repl('-',len(aidi1))
   ?spac(4)+'REKAP PENJUALAN PER BULAN'
   ?spac(4)+'BULAN: '+(tgl)+' s/d '+(tgl2)
   ?spac(3)+chr(218)+repl(gl,4)+chr(194)+repl(gl,5)+repl(chr(194)+repl(gl,15),4)+chr(191)
   ?spac(3)+tl+' No.'+tl+' Bln '+tl+'       Rp.Jual '+tl+'      Rp.Pokok '+tl+'     Rp.Margin '+tl+'     Rp.Donasi '+tl
   ?spac(3)+chr(195)+repl(gl,4)+chr(197)+repl(gl,5)+repl(chr(197)+repl(gl,15),4)+chr(180)
   B=5
   Stor 0 to hp,hj,jdns,tot,tot2,tdns,n
Do While x1<=x2
      xBlnthn='thn+ltri(str(x1))'
      If len(ltri(str(x1)))=1
         xBlnthn="thn+'0'+ltri(str(x1))"
EndIf
      Nf1='J-'+&xblnthn+'.DTA'
      If file('&nf1')
         Uselan('&nf1','','','','','')
         Sum &brs14,&brs1,rp_donasi to hp,hj,jdns
EndIf
      If x1=1
         nBln='Jan'
      Elseif x1=2
         nBln='Feb'
      Elseif x1=3
         nBln='Mar'
      Elseif x1=4
         nBln='Apr'
      Elseif x1=5
         nBln='Mei'
      Elseif x1=6
         nBln='Jun'
      Elseif x1=7
         nBln='Jul'
      Elseif x1=8
         nBln='Ags'
      Elseif x1=9
         nBln='Sep'
      Elseif x1=10
         nBln='Okt'
      Elseif x1=11
         nBln='Nov'
      Elseif x1=12
         nBln='Des'
EndIf
      *
      N   =n+1
      ?spac(3)+tl+tran(n,'999 ')+tl+' '+nbln+' '+tl+tran(hj,'@e 99,999,999,999 ')+tl+iif(xOwner,tran(hp,'@e 99,999,999,999 ')+tl+tran(hj-hp,'@e 99,999,999,999 '),+spac(15)+tl+spac(15))+tl+tran(jdns,'@e 99,999,999,999 ')+tl
      Tot =tot +hj
      Tot2=tot2+hp
      Tdns=tdns+jdns
      Stor 0 to hp,hj,jdns
      B =b+1
      X1=x1+1
EndDo
   ?spac(3)+chr(192)+repl(gl,4)+chr(193)+repl(gl,5)+repl(chr(193)+repl(gl,15),4)+chr(217)
   ?spac(5)+'TOTAL =Rp '+tran(tot,'@e 99,999,999,999  ')+iif(xOwner,tran(tot2,'@e 99,999,999,999  ')+tran(tot-tot2,'@e 99,999,999,999  '),spac(32))+tran(tdns,'@e 99,999,999,999')
   ?''
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai     F10=Cetak'
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
   Type &arg1 to prin
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2,nbln,xblnthn,x1,x2
Tgl=tglx
Return

* LAPORAN PENDPTAN KASIR
*-----------------------
Procedure lpj32
Sele 1
*
Sele 2
*
Sele 3
Uselan('KASIR.DTA','','','','','')
*
Ksr='  '
Stor tglx to tgl,tgl2
Do While .t.
   Ada=.f.
   Nsls=spac(20)
   Sele 3
   Set filter to
   Set key 28 to dafksr
   @1,0 clea to 23,79
   @2,24 say 'Kasir.:'get nsls pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai dg.'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   If nsls#spac(20)
      Loca for nama=rtri(nsls)
      If .not. foun()
         Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
         Wait''
         Loop
EndIf
      Ksr =nomer
      Nsls=nama
      Ada =.t.
EndIf
   Do konvtgl
   Do bacafile
   Hal =1
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf3','','','','')
   Sele 2
   *Uselan('RETURJN.DTA','RETURJN1.NDX','','','','')
   *
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-KASIR.RTF
   ?spac(5)+aidi1
   ?spac(5)+repl('-',len(aidi1))
   ?spac(4)+' LAPORAN PENDAPATAN KASIR'
   ?spac(4)+' TANGGAL: '+dtoc(tgl)+' S/D '+dtoc(tgl2)
   ?spac(4)+repl(gl,67)
   ?spac(4)+' NO.  NAMA KASIR                 RP.JUAL     RP.MARGIN   RP.DONASI'
   ?spac(4)+repl(gl,67)
   Stor 0 to hp1,hp2,hj1,hj2,jdns,tot,tot2,tdns
   Stor ' ' to sip1,sip2
   Tgl3=tgl
   No  =1
   *
   Sele 3
   If ada
      Loca for nama=nsls
   Else
      Go top
EndIf
Do While .not. eof()
      Ksr =nomer
      Nsls=nama
      *
      Sele 1
      Set filter to kode_ksr=ksr
Do While tgl<=tgl2
         Seek dtos(tgl)
         If foun()
            Exit
         Elseif .not. foun()
            Tgl=tgl+1
EndIf
EndDo
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_ksr=ksr
         Hp1  =hp1 + &brs14
         Hj1  =hj1 + &brs1
         Jdns =jdns+ rp_donasi
         Sip1='1'
         Skip
EndDo
      If hj1#0
         ?spac(4)+tran(no,'999.  ')+nsls+tran(hj1,'@e 99,999,999,999')+iif(xOwner,tran(hj1-hp1,'@e 99,999,999,999'),spac(14))+tran(jdns,'@e 9999,999,999')
         No=no+1
         Tot =tot +hp1
         Tot2=tot2+hj1
         Tdns=tdns+jdns
EndIf
      Stor 0 to hp1,hp2,hj1,hj2,jdns
      Tgl=tgl3
      *
      Sele 3
      If ada
         Exit
EndIf
      Skip
EndDo
   ?spac(4)+repl(gl,67)
   ?spac(10)+'TOTAL RP'+spac(12)+tran(tot2,'@e 99,999,999,999')+iif(xOwner,tran(tot2-tot,'@e 99,999,999,999'),spac(14))+tran(tDNS,'@e 9999,999,999')
   ?spac(4)+repl(gl,67)
   ?''
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   Set colo to w/b
   Memoedit(Memoread('L-KASIR.RTF'),1,0,23,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai     F10=Cetak'
   If lastkey()=27
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
   Type L-KASIR.RTF to print
   Set prin off
   Set devi to scre
EndDo
Clos all
Return

* LAPORAN PENJUALAN/TGL/NOTA
*---------------------------
Procedure lpj4
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
Sele 3
Uselan('KASIR.DTA','','','','','')

Nsls=spac(20)
Fkt=spac(5)
Stor tglx to tgl,tgl2
Do While .t.
   Sele 3
   Set cent on
   Set filter to
   Set key 28 to dafksr
   @1,0 clea to 23,79
   @2,29 say 'Tanggal.:'get tgl
   @3,29 say 'Nama Ksr:'get nsls pict '@!'
   @4,29 say 'No.Nota.:'get fkt pict '@!'
   Do pesan with 'ESC=Selesai   F1=Daftar Kasir'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Loca for nama=nsls
   Nsls=nama
   Ksr =nomer
   Stor 0 to tot,tot2
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf3','&nf1','&nf2','','')
   Seek dtos(tgl)+ksr+fkt
   If .not. foun()
      Do pesan2 with 'Tidak ada data Tgl:'+dtoc(tgl)+'  Ksr:'+rtri(nsls)+'  Nota:'+rtri(fkt)
      Loop
EndIf
   Nr=recno()
   Set cent off
   @1,0 clea to 23,79
   @1,1 say '*Ksr: '+rtri(nsls)+'  *Tgl: '+dtoc(tgl)+'  *Nota: '+fkt+' / '+kode_lks
   @2,0 say repl(gl,5)+chr(194)+repl(gl,5)+chr(194)+repl(gl,6)+chr(194)+repl(gl,30)+chr(194)+repl(gl,11)+chr(194)+repl(gl,8)+chr(194)+repl(gl,9)
   @3,0 say 'Tgl. '+tl+'Plgn '+tl+'Kode  '+tl+'Nama Barang'+spac(19)+tl+' Jml.Brg.  '+tl+'  @Harga'+tl+'Jumlah Rp'
   @4,0 say repl(gl,5)+chr(193)+repl(gl,5)+chr(193)+repl(gl,6)+chr(193)+repl(gl,30)+chr(193)+repl(gl,11)+chr(193)+repl(gl,8)+chr(193)+repl(gl,9)
   B=5
Do While .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls .and. faktur=fkt)
      Do case
         Case jumlah<0
            Jml=tran(jumlah,'@e  9,999 ')
         Case jumlah=0
            Jml=tran(jumlah,'@e  9,999 ')
         Case jumlah>0 .and. jumlah<1
            Jml=tran(jumlah,'@e 999.99 ')
         Case jumlah>=1
            If mod(jumlah,int(jumlah))>0
               Jml=tran(jumlah,'@e 999.99 ')
            Else
               Jml=tran(jumlah,'@e  9,999 ')
EndIf
      Endc
      Jhg=&brs1
      Tot=tot+jhg
      Ksp=kode_lgn
      If ksp=spac(10)
         Ksp='CASH '
      Else
         Ksp=right(rtri(ksp),5)
EndIf
      @b,0 say left(dtoc(tanggal),5)+' '+ksp+' '+left(kode_brg,7)+left(nama_brg,30)+jml+satuan+tran(harga_2,'@e 9,999,999')+tran(jhg,'@e 99,999,999')
      @b,0 say left(dtoc(tanggal),5)+' '+ksp+' '+left(kode_brg,7)+left(nama_brg,30)+jml+satuan+tran(harga_2,'@e 9,999,999')+tran(jhg,'@e 99,999,999')
      B=b+1
      If b>21
         @b,0 to b,79
         Do pesan with '[Pgln]-Lanjut'
         Wait''
         If lastkey()=27
            Exit
EndIf
         B =5
EndIf
      Skip
EndDo
   @b,0 to b,79
   @b+1,0 clea to 23,79
   @b+1,60 say 'TOTAL= Rp'+tran(tot, '@e 999,999,999')
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Nota   F8=Hapus/Item Brg'
   If lastkey()=-3
      Do pesan with 'Nota diatas akan DiHapus? Y/T'
      Yt=' '
Do While .not. yt$'YT'
         @24,55 get yt pict '!'
         Read
EndDo
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Go nr
Do While .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls .and. faktur=fkt)
            Kdb=kode_bar
            Jml=jumlah
            Rlok()
            Dele
            Repl keterangan with left(time(),5)+' '+gete("KASSA")+'ADM '+dtoc(tglx)
            Ul()
            *
            Sele 2
            Seek kdb
            Rlok()
            Repl stok_k with (stok_k-jml)
            Ul()
            *
            Sele 1
            Skip
EndDo
         Loop
EndIf
   Elseif lastkey()=-7 .and. xOwner
      Hut=.t.
      Do hpj
      Loop
EndIf
EndDo
Set curs off
Clos all
Rele klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

* LAPORAN TOTAL NO.NOTA
*-----------------------
Procedure lpj41
Sele 1
Uselan('KASIR.DTA','','','','','')
Sele 2
*
Sele 3
*
Nsls=spac(20)
Stor ' ' to hri,bln,thn,jlap,ket,flt,ksr
Stor tglx to tgl,tgl2
Do While .t.
   Sele 1
   Set cent on
   Set key 28 to dafksr
   @1,0 clea to 23,79
   @2,24 say 'Nm.Kasir:'get nsls pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Kasir'
   Read
   @24,0
   Set key 28 to
   If lastkey()=27
      Exit
EndIf
   If nsls#spac(20)
      Loca for nama=nsls
      If .not. foun()
         Do pesan2 with 'Tidak ada kasir '+rtri(nsls)
         Loop
EndIf
      Ksr=nomer
EndIf
   @1,0 clea to 23,79
   Tgl4=tgl
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   Do konvtgl
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Do bukafljual
   If nsls#spac(20)
      Set filter to nama_ksr=nsls
EndIf
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
EndIf
   *
   Nr=recno()
   Stor 0 to b,no,tot,tot2,tots,tots2,tdns,jml1,jml2,jdns
   Stor 1 to cf,hal,no,n
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplnota
   *
Do While .t.
      Set cent off
      xJam=jam
      Mj  =kassa
      Fkt =faktur
      Sls =nama_ksr
      Tgl3=tanggal
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jml1 =jml1+&brs1
         Jdns =jdns+rp_donasi
         Tot  =tot +&brs1
         Tots =tots+&brs1
         Jml2 =jml2+&brs14
         Tot2 =tot2+&brs14
         Tots2=tots2+&brs14
         Tdns =tdns+rp_donasi
         Skip
         If faktur#fkt
            Kata1="tran(jml1,'@e 9999,999,999')+tl+iif(xOwner,tran(jml2,'@e 9999,999,999')+tl+tran(jml1-jml2,'@e 9999,999,999'),spac(12)+tl+spac(12))+tl+tran(jdns,'@e 9999,999,999')+tl"
            ?tl+tran(no,'9999.')+tl+left(dtoc(tgl3),2)+'/'+subs(dtoc(tgl3),4,2)+' '+xjam+tl+right(fkt,5)+'-'+mj+'-'+left(sls,12)+tl+&kata1
            No=no+1
            N =n +1
            Mj  =kassa
            xJam=jam
            Fkt =faktur
            Sls =nama_ksr
            Tgl3=tanggal
            Stor 0 to jml1,jml2,jdns
            If n>50
               ?chr(195)+repl(gl,38)+repl(chr(197)+repl(gl,12),4)+chr(180)
               ?tl+spac(18)+'Jumlah hal.ini =  Rp'+tl+tran(tots,'@e 9999,999,999')+tl+iif(xOwner,tran(tots2,'@e 9999,999,999')+tl+tran(tots-tots2,'@e 9999,999,999'),spac(12)+tl+spac(12))+tl+spac(12)+tl
               ?tl+spac(18)+'Sub Total      =  Rp'+tl+tran(tot ,'@e 9999,999,999')+tl+iif(xOwner,tran(tot2 ,'@e 9999,999,999')+tl+tran(tot-tot2  ,'@e 9999,999,999'),spac(12)+tl+spac(12))+tl+spac(12)+tl
               ?chr(192)+repl(gl,38)+repl(chr(193)+repl(gl,12),4)+chr(217)
               ?''
               Stor 0 to n,tots,tots2
               Hal=hal+1
               Cf =cf +1
               Cfx=ltri(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplnota
EndIf
EndIf
EndDo
      If beda
         Set cent on
         Do carifljual
         If tgl>tgl2
            Exit
EndIf
         If file('&nf0')
            Do bukafljual
EndIf
      Elseif beda=.f.
         Exit
EndIf
EndDo
   ?chr(195)+repl(gl,38)+repl(chr(197)+repl(gl,12),4)+chr(180)
   ?tl+spac(18)+'Jumlah hal.ini =  Rp'+tl+tran(tots,'@e 9999,999,999')+tl+iif(xOwner,tran(tots2,'@e 9999,999,999')+tl+tran(tots-tots2,'@e 9999,999,999'),spac(12)+tl+spac(12))+tl+spac(12)+tl
   ?tl+spac(18)+'Grand Total    =  Rp'+tl+tran(tot ,'@e 9999,999,999')+tl+iif(xOwner,tran(tot2 ,'@e 9999,999,999')+tl+tran(tot-tot2  ,'@e 9999,999,999'),spac(12)+tl+spac(12))+tl+tran(tdns,'@e 9999,999,999')+tl
   ?chr(192)+repl(gl,38)+repl(chr(193)+repl(gl,12),4)+chr(217)
   ?''
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Set prin on
   Set devi to prin
   Set marg to 20
   ?chr(27)+chr(33)+chr(4)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   Set prin off
   Set devi to scre
EndDo
Set curs off
Clos all
Rele nsls,tgl2,b,no,nr,tot,tot2,jhg,jml,jml1,jml2
Return
Procedure koplnota
?' LAPORAN PENJUALAN PER NO.NOTA'
If nsls#spac(20)
   ?' KASIR  : '+rtri(nsls)
EndIf
?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' // Hal:'+tran(hal,'999')
?chr(218)+repl(gl,5)+chr(194)+repl(gl,11)+chr(194)+repl(gl,20)+repl(chr(194)+repl(gl,12),4)+chr(191)
?tl+'  No.'+tl+'Tgl.  Jam  '+tl+'Nota-Ks-Kasir       '+tl+'     Rp Jual'+tl+'    Rp Pokok'+tl+'   Rp Margin'+tl+'   Rp Donasi'+tl
?chr(195)+repl(gl,5)+chr(197)+repl(gl,11)+chr(197)+repl(gl,20)+repl(chr(197)+repl(gl,12),4)+chr(180)
Return

* LAPORAN PENJUALAN (TERLARIS)
*-----------------------------
Procedure lpj99
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK5.NDX','STOK2.NDX','','','')
Sele 7
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
*
Stor tglx to tgl,tgl2
Do While .t.
   Sele 2
   Set order to 1
   Set filter to
   *
   Sele 7
   Set filter to
   Set curs on
   Set cent on
   Set key 28 to dafspl
   Set colo to /bg
   Jlap=' '
   Ksp =spac(5)
   @1,0 clea to 23,79
   @2,24 say 'Supplier:'get ksp pict'@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai     F1=Daftar_Supplier'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Ket2  =''
   Syarat='kode_bar#spac(13)'
   If ksp#spac(5)
      @5,24 say 'Tampilkan yg Tidak Ada Trx? Y/T :'get jlap pict '!'
      Do pesan with 'ESC=Ulangi'
      Read
      If lastkey()=27
         Loop
EndIf
      *
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada kode '+rtri(ksp)
         Loop
EndIf
      Nsp =nama_spl
      Ket2=rtri(nsp)+' ('+rtri(ksp)+')'
      Syarat='kode_spl=ksp'
      *
      Sele 2
      Set order to 2
      Set filter to &syarat
EndIf
   *
   Tgl4=tgl
   Stor .f. to beda,bedafile
   If mont(tgl)#mont(tgl2)
      Beda=.t.
EndIf
   Do konvtgl
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
EndIf
   *
   Sele 1
   Uselan('&nf0','&nf2','','','','')
   *
   Sele 3
   Useidx('TOP-RANK.DTA')
   Copy stru to TOP-RANK.TMP
   Close
   *
   Useidx('TOP-RANK.TMP')
   *
   Ada=.f.
   Awl=1
   *
   @1,0 clea to 23,79
Do While .t.
      Sele 2
      Do pesan with 'Tunggu, sedang dicari... Rec: '+ltri(str(reccount()))+'-          File: '+nf0
      Set colo to w+/r
      *
      Go top
Do While .not. eof() .and. &syarat
         Klb =kode_klb
         Kdb =kode_bar
         *Kdby=iif(len(rtri(kdb))>=9,left(kdb,3)+'x'+right(rtri(kdb),4),left(kdb,8))
         Kdby=kode_bar
         Nmb =nama_brg
         St  =satuan
         Stk =(stok_a+stok_m)-stok_k
         Ksp =kode_spl
         Hp  =harga
         Stor 0 to jml,jhg,jhp,jdi
         @24,44 say ltri(str(awl))
         *
         Sele 1
         Seek kdb
         If foun()
Do While .not. eof() .and. kode_bar=kdb
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  Jml=jml+jumlah
                  Jhg=jhg+(jumlah*harga_2)
                  Jhp=jhp+(jumlah*harga)
                  Jdi=jdi+(jumlah*diskon_rp)
                  If kode_spl#spac(5)
                     Ksp=kode_spl
EndIf
EndIf
               Skip
EndDo
         Elseif .not. foun()
            Sele 2
            Skip
            Loop
EndIf
         *
         If jml#0 .or. jlap='Y'
            Ada=.t.
            Sele 3
            If bedafile
               Loca for kode_brg=kdby
               If foun()
                  Repl jumlah with (jumlah+jml),harga with (harga+jhp),harga_2 with (harga_2+jhg),diskon with (diskon+jdi),margin with (margin+(jhg-jhp))
               Elseif .not. foun()
                  Appe blan
                  Repl kode_spl with ksp,kode_klb with klb,kode_brg with kdby,nama_brg with nmb,satuan with st,harga_bl with hp,;
                       sisa with stk,jumlah with jml,harga with jhp,harga_2 with jhg,diskon with jdi,margin with (jhg-jhp)
EndIf
            Elseif bedafile=.f.
               Appe blan
               Repl kode_spl with ksp,kode_klb with klb,kode_brg with kdby,nama_brg with nmb,satuan with st,harga_bl with hp,;
                    sisa with stk,jumlah with jml,harga with jhp,harga_2 with jhg,diskon with jdi,margin with (jhg-jhp)
EndIf
EndIf
         *
         Awl=awl+1
         Sele 2
         Skip
EndDo
      Awl=1
      If beda
         Bedafile=.t.
         Do carifljual
         Do konvtgl
         If tgl>tgl2
            Exit
EndIf
         If file('&nf0')
            Sele 1
            Uselan('&nf0','&nf2','','','','')
            Set filter to &syarat
EndIf
      Elseif beda=.f.
         Exit
EndIf
EndDo
   If ada
      Sele 6
      Close
      *
      Sele 5
      Close
      *
      Sele 4
      Close
      *
      Sele 3
      Sort on jumlah   /D to TOP-QTY.TMP
      Sort on harga_2  /D to TOP-RPH.TMP
      Sort on margin   /D to TOP-MRG.TMP
      Sort on nama_brg,jumlah to TOP-NMB.TMP
      Close
      *
      Pesanx='T=Top_Ranking     F1=Cari_Nama    F10=Cetak'
      Sele 3
      Useidx('TOP-QTY.TMP')
      Sele 4
      Useidx('TOP-RPH.TMP')
      Sele 5
      Useidx('TOP-MRG.TMP')
      Sele 6
      Useidx('TOP-NMB.TMP')
      *
      Set cent off
      Set colo to /bg
      Tgl=tgl4
      Ket='TOP ITEM BRG,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0 clea to 23,79
      @1,1 say ket+',  '+ket2
      @2,0 to 2,79 doub
      @22,0 to 22,79
Do While .t.
         Declare field[11],bts[11],frm[11],kop[11]
         Do pesan with pesanx
         Field[1]='KODE_BRG'
         Field[2]='NAMA_BRG'
         Field[3]='SATUAN'
         Field[4]='JUMLAH'
         Field[5]='SISA'
         Field[6]='HARGA_2'
         Field[7]='MARGIN'
         Field[8]='KODE_SPL'
         Field[9]='KODE_KLB'
         Kop[1]='Kode'
         Kop[2]='Nama Barang'
         Kop[3]='Sat.'
         Kop[4]='   Qty.'
         Kop[5]='   Sisa'
         Kop[6]=' Total Jual'
         Kop[7]='     Margin'
         Kop[8]='Supl.'
         Kop[9]='Klp.'
         Frm[1]=''
         Frm[2]='@!'
         Frm[3]='@!'
         Frm[4]='@e 999,999'
         Frm[5]='@e 999,999'
         Frm[6]='@e 999,999,999'
         Frm[7]='@e 999,999,999'
         Frm[8]='@!'
         Frm[9]='@!'
         Bts[1]=' '
         Bts[2]=' '
         Stor tl to bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9],bts[10]
         Dbedit(3,0,21,79,field,'flpj99',frm,kop,gl,bts)
         *
         If lastkey()=27
            Exit
EndIf
EndDo
   Elseif .not. ada
      Tone(1500,3)
      Do pesan with 'Tidak Ada Data Penjualan'
      Wait''
EndIf
EndDo
Clos all
Rele klb,ksp,jlap,ket,nklb,kdb,st,nmb,tgl2,jml1,jml2,jhg1,jhg2,jhp1,jhp2,awl
Return

Function flpj99
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
      @22,1 say repl(gl,15)
      Set colo to gr+/b
      @22,1 say ltri(str(recno()))+'//'+ltri(str(reccount()))
      Set colo to /bg
Return 1
Elseif lastkey()=27
Return 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=spac(30)
      Set curs on
      Do pesan with ''
      Set colo to w+/r
      @24,18 say 'Urut Nama Brg:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#spac(30)
         Set colo to /bg
         Loca for nama_brg=rtri(nmb)
         If .not. foun()
           Go nr
EndIf
EndIf
      Do pesan with pesanx
      Set colo to /bg
      Set curs off
Return 1
Elseif uppe(chr(lastkey()))='T' .and. fds='NAMA_BRG'
      Sele 6
      Ket='TOP ITEM BRG,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)
Return 0
Elseif uppe(chr(lastkey()))='T' .and. fds='JUMLAH'
      Sele 3
      Ket='TOP QTY,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)
Return 0
Elseif uppe(chr(lastkey()))='T' .and. fds='HARGA_2'
      Sele 4
      Ket='TOP RUPIAH,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)+chr(4)
Return 0
Elseif uppe(chr(lastkey()))='T' .and. fds='MARGIN'
      Sele 5
      Ket='TOP MARGIN,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)+chr(4)+chr(4)
Return 0
Elseif lastkey()=-9
      Go top
      Set curs on
      Pil='1'
      Do pesan with '1=Printer   2=File : '
      @24,50 get pil pict '!'
      Read
      If pil='2'
         No =1
         Nf='C:\TOPRANK.CSV'
         Do pesan with 'Tunggu, sedang proses '+nf
         Set alte on
         Set alte to &nf
         ?'   ;    ;LAPORAN PENJUALAN; ; ; ; ;'
         ?'   ;    ;TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'; ; ; ; ;'
         ?'NO.;KODE;NAMA BARANG;QTY;SISA;SAT;JUMLAH RP'
Do While .not. eof()
            Jml=tran(jumlah,'@e 999,999')
            Stk=tran(sisa,'@e 999,999')
            ?ltri(str(no))+';'+' '+alltrim(kode_brg)+';'+rtri(nama_brg)+';'+ltri(jml)+';'+ltri(stk)+';'+rtri(satuan)+';'+ltri(str(harga_2))
            No=no+1
            Skip
EndDo
         ?''
         Set alte off
         Set alte to
         Do pesan with 'Export selesai..., nama file: '+subs(nf,4,12)+' , di drive C:\'
         Wait''
         Set colo to /bg
         Set curs off
         Do pesan with pesanx
Return 2
EndIf
      *
      Stor 1 to n,no,hal
      Stor 0 to jml,jml2,tot,tot2,totd,totd2
      Do cp
      If cc=0
Return 0
EndIf
      Set prin on
      Set devi to prin
      Set marg to 5
      ?chr(27)+chr(33)+chr(4)
      ?' REKAP PENJUALAN BARANG'
      ?' '+ket
      ?' '+ket2+'  / Hal:'+tran(hal,'999')
      ?'/'+repl('=',122)+'\'
      ?'| No.| Supp  | Klp. | Kode          | Nama Barang                   | Sat. |  Qty.  | Total Jual |     Diskon | Netto Jual |'
      ?'|'+repl('-',122)+'|'
Do While .not. eof()
         ?tran(no,'|999 | ')+kode_spl+' | '+kode_klb+'| '+kode_brg+' | '+left(nama_brg,30)+'| '+satuan+'|'+tran(jumlah,'@e 999,999 |')+tran(harga_2,'@e 999,999,999 |')+tran(diskon,'@e 999,999,999 |')+tran(harga_2-diskon,'@e 999,999,999 |')
         Jml  =jml  +jumlah
         Jml2 =jml2 +jumlah
         Tot  =tot  +harga_2
         Tot2 =tot2 +harga_2
         Totd =totd +diskon
         Totd2=totd2+diskon
         No=no+1
         N =n+1
         If n>50
            ?'|'+repl('-',122)+'|'
            ?'|'+spac(37)+'Jumlah Hal ini'+spac(23)+'|'+tran(jml2,'@e 999,999 |')+tran(tot2,'@e 999,999,999 |')+tran(totd2,'@e 999,999,999 |')+tran(tot2-totd2,'@e 999,999,999 |')
            ?'|'+spac(37)+'Sub Total   Rp'+spac(23)+'|'+tran(jml ,'@e 999,999 |')+tran(tot ,'@e 999,999,999 |')+tran(totd ,'@e 999,999,999 |')+tran(tot-totd  ,'@e 999,999,999 |')
            ?'|'+repl('-',122)+'|'
            ?''
            Eject
            Set prin off
            Set devi to scre
            Do pesan3 with gtkertas
            If nlp=0
               Cc=0
               Exit
EndIf
            Do cp
            If cc=0
               Exit
EndIf
            Stor 0 to jml2,tot2,totd2
            N   =1
            Hal =hal+1
            Set prin on
            Set devi to prin
            Set marg to 5
            ?chr(27)+chr(33)+chr(4)
            ?' REKAP PENJUALAN BARANG'
            ?' '+ket
            ?' '+ket2+'  / Hal:'+tran(hal,'999')
            ?'/'+repl('=',122)+'\'
            ?'| No.| Supp  | Klp. | Kode          | Nama Barang                   | Sat. |  Qty.  | Total Jual |     Diskon | Netto Jual |'
            ?'|'+repl('-',122)+'|'
EndIf
         Skip
EndDo
      ?'|'+repl('-',122)+'|'
      ?'|'+spac(37)+'Jumlah Hal ini'+spac(23)+'|'+tran(jml2,'@e 999,999 |')+tran(tot2,'@e 999,999,999 |')+tran(totd2,'@e 999,999,999 |')+tran(tot2-totd2,'@e 999,999,999 |')
      ?'|'+spac(37)+'Grand Total Rp'+spac(23)+'|'+tran(jml ,'@e 999,999 |')+tran(tot ,'@e 999,999,999 |')+tran(totd ,'@e 999,999,999 |')+tran(tot-totd  ,'@e 999,999,999 |')
      ?'\'+repl('=',122)+'/'
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
Return 1
Else
      Set curs off
      Do pesan with pesanx
Return 1
EndIf
Return 0

* EOF LPJ2
