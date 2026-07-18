* LAPORAN RINCI PENJUALAN/TKG/SLS
Sele 1
If klp .and. hut=.f.
   Uselan('SALES.DTA','','','','','')
Else
   Uselan('TUKANG.DTA','TUKANG.NDX','TUKANG2.NDX','','','')
Endi

Sele 2
Uselan('STOK.DTA','STOK.NDX','','','','')
Sele 3
*
Sele 4
*
Sele 5
Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')

Stor spac(5) to ksp,sls,sls2,tkg,tkg2,fkt
Stor ' ' to hri,bln,thn,nsp,nsls,ntkg
Stor tglx to tgl,tgl2
Do whil .t.
   Sele 1
   Set cent on
   @1,0 clea to 24,79
   If klp .and. hut=.f.
      Set key 28 to dafsls
      Do pesan with 'F1=Daftar Salesman'
      @2,24 say 'Sales.:'get sls pict '@!'
      @3,24 say 'Tanggal'get tgl
      @3,43 say 'sampai'get tgl2
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      Endi
      Nsls='-'
      If sls#spac(5)
         Loca for kode_sls=sls
         If .not. foun()
            Do pesan2 with 'Tidak ada Salesman '+rtri(sls)
            Loop
         Endi
         Nsls=nama_sls
      Endi
   Else
      Set key 28 to daftkg
      Do pesan with 'F1=Daftar Tukang'
      @2,24 say 'Tukang:'get tkg pict '@!'
      @3,24 say 'Tanggal'get tgl
      @3,43 say 'sampai'get tgl2
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      Endi
      Nsp='-'
      If tkg#spac(5)
         Set order to 1
         Seek tkg
         If .not. foun()
            Do pesan2 with 'Tidak ada Tukang '+rtri(tkg)
            Loop
         Endi
         Ntkg=nama_tkg
      Endi
   Endi
   Stor 0 to no,tot,tot2,totx,jhg,jml1,jml2
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   Endi
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   Endi

   Sele 4
   Uselan('PEMBLTAN.DTA','PEMBLTAN.NDX','','','','')
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
   Endi
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jml1=jml1+ nilai_b
      Jml2=jml2+ nilai_a
      Skip
   Endd
   Close

   Do bukafjual1

   Stor 1 to cf,hal,jml
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpj11
   Set cent off

   Do whil .t.
      Set cent off
      Nr=recno()
      Fkt =faktur
      Nsls=kode_lgn
      Nsp =kode_tkg
      Nklb=nama_ksr
      Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         If diskon=0
            ?dtoc(tanggal)+'  '+subs(nama_brg,1,30)+jml+satuan+tran(harga_2,'@e  9,999,999       ')+tran(&brs1,'@e 999,999,999')
         Else
            ?dtoc(tanggal)+'  '+subs(nama_brg,1,30)+jml+satuan+tran(harga_2,'@e  9,999,999 ')+tran(diskon_rp,'@e 99999 ')+tran(&brs1,'@e 999,999,999')
         Endi
         Skip
         If faktur#fkt
            ?spac(68)+repl(gl,12)
            ?' Plgn..:'+nsls+' / '+nsp+'   Fkt: '+fkt+'   Ksr:'+left(nklb,16)+' Jumlah Rp.='+tran(jhg,'@e 999,999,999')
            ?repl(gl,80)
            Fkt =faktur
            Nsls=kode_lgn
            Nsp =kode_tkg
            Nklb=nama_ksr
            Jhg =0
            No  =no+1
            If no>=20
               ?spac(50)+'SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
               ?''
               Stor 0 to no,totx
               Hal=hal+1
               Cf =cf+1
               Cfx=ltri(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplpj11
            Endi
         Endi
      Endd
      If beda
         Set cent on
         Do carifljual
         If tgl>tgl2
            Exit
         Endi
         If file('&nf0')
            Do bukafjual1
         Endi
      Elseif beda=.f.
         Exit
      Endi

   Endd
   ?repl(gl,80)
   If xOwner
      ?spac(30)+'POKOK  Rp'+tran(tot2    ,'@e 999,999,999')+'      SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
      ?spac(30)+'Margin Rp'+tran(tot-tot2,'@e 999,999,999')+'      T O T A L Rp'+tran(tot, '@e  999,999,999')
   Else
      ?spac(56)+'SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
      ?spac(56)+'T O T A L Rp'+tran(tot, '@e  999,999,999')
   Endi
   If jml1#0 .or. jml2#0
       ?spac(35)+'Pembulatan keBawah='+tran(jml1,'@e 9999,999')+' , keAtas='+tran(jml2,'@e 9999,999')
   Endi   
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner ) .and. beda=.f.
      Do hpj
      Loop
   Elseif lastkey()=27
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot,tot2
   Tgl3=tgl
   *
   Do koplpj112
   Go nr
   Set cent off
   Fkt =faktur
   Ksp =kode_lgn
   Tgl =tanggal

   Sele 5
   Seek dtos(tgl)+ksp+fkt
   Nsp=nama_lgn

   Sele 3
   Do whil .not. eof() .and. (tanggal>=tgl3 .and. tanggal<=tgl2)
      Do fmtjml
      Tgl =tanggal
      Jhg =&brs1
      Tot =tot +jhg
      Tot2=tot2+jhg
      ?' '+dtoc(tanggal)+'  '+left(nama_ksr,10)+' '+faktur+'  '+left(kode_brg,7)+'    '+nama_brg+jml+satuan+tran(harga_2,'@e 9,999,999 ')+tran(diskon_rp,'@e  99,999 ')+tran(jhg,'@e 999,999,999')
      No=no+1
      Skip
      If faktur#fkt
         ?' '+repl('-',47)+'Nama: '+left(nsp,25)+' Kode: '+ksp+'  Jumlah='+tran(tot2,'@e 999,999,999')
         ?''
         No=no+2
         Fkt =faktur
         Ksp =kode_lgn
         Sele 5
         Seek dtos(tgl)+ksp+fkt
         Nsp=nama_lgn

         Sele 3
         Tot2=0
      Endi
      If no>55
         ?repl('-',99)+'bersambung...'
         ?''
         Eject
         Set prin off
         Set devi to scre
         Do pesan3 with gtkertas
         If nlp=0
            Cc=0
            Exit
         Endi
         Do cp
         If cc=0
            Exit
         Endi
         No =1
         Hal=hal+1
         Do koplpj112
      Endi
   Endd
   If cc=0
      Loop
   Endi
   ?repl('-',112)
   ?' T o t a l     '+spac(82)+'Rp.'+tran(tot,'@e 999,999,999')
   ?repl('=',112)
   ?''
   Tgl=tgl3
   Eject
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Rele ksp,sls,sls2,tkg,tkg2,fkt,hri,bln,thn,nsp,nsls,ntkg,tot,tot2,jhg
Retu

Proc koplpj11
If klp .and. hut=.f.
   ?' Sales: '+nsls+'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
Else
   ?' Tkng.: '+ntkg +'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
Endi
?repl(gd,9)+chr(209)+repl(gd,30)+chr(209)+repl(gd,12)+chr(209)+repl(gd,8)+chr(209)+repl(gd,6)+chr(209)+repl(gd,10)
?'Tanggal  '+tl+'Nama Barang'+spac(19)+tl+'  Qty.Sat.  '+tl+'  @Harga'+tl+'Dis.Rp'+tl+'    Jumlah'
?repl(gl,9)+chr(193)+repl(gl,30)+chr(193)+repl(gl,12)+chr(193)+repl(gl,8)+chr(193)+repl(gl,6)+chr(193)+repl(gl,10)
Retu

Proc koplpj112
Set prin on
Set devi to prin
Set marg to 10
?chr(27)+chr(33)+chr(4)
If klp .and. hut=.f.
   ?' LAPORAN PENJUALAN PER SALES'
   If sls#spac(5)
      ?' SALESMAN: '+rtri(nsls)+' - '+sls
   Endi
Else
   ?' LAPORAN PENJUALAN PER TUKANG'
   If tkg#spac(5)
      ?' TUKANG   : '+rtri(ntkg)+' - '+tkg
   Endi
Endi
If tgl=tgl2
   ?' TANGGAL: '+dtoc(tgl3)+spac(86)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl3)+' s/d '+dtoc(tgl2)+spac(73)+'Hal:'+tran(hal,'999')
Endi
?repl('=',112)
?' Tanggal   Kasir      Nota   Kode       Nama Barang                      Jml.Brg.    @Harga  Dis.Rp   Jumlah Rp.'
?repl('-',112)
No=1
Retu

Proc bukafjual1
Sele 3
Uselan('&nf0','&nf1','&nf2','&nf3','','')
If klp .and. hut=.f.
   Set filter to kode_sls#spac(5)
   If sls#spac(5)
      Set filter to kode_sls=sls
   Endi
Else
   Set filter to kode_tkg#spac(5)
   If tkg#spac(5)
      Set filter to kode_tkg=tkg
   Endi
Endi
Seek dtos(tgl)
If .not. foun()
   Loca for (tanggal>=tgl .and. tanggal<=tgl2)
Endi
Retu

* EOF LPJ13
