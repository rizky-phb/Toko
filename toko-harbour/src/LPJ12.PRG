* LAPORAN PENJUALAN/KREDIT
Sele 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
Sele 2
Uselan('STOK.DTA','STOK.NDX5','','','','')
Sele 3
*
Sele 4
*
Sele 5
Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
*
Stor spac(10) to ksp,fkt
Stor ' ' to hri,bln,thn,jlap
Stor tglx to tgl,tgl2
Do whil .t.
   Sele 1
   Set cent on
   Set key 28 to daflgn
   @1,0 clea to 23,79
   @2,24 say 'Plgn....:'get ksp pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   Endi
   Tgl4=tgl
   Stor '-' to nsls,nsp,jab
   If ksp#spac(10)
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada Pelanggan '+rtri(ksp)
         Loop
      Endi
      Nsp=nama_lgn
      Jab=almt_lgn
   Endi
   Stor 0 to no,tot,tot2,totx,totr,jhg,jml1,jml2
   @1,0 clea to 23,79
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
   *
   Do bukafjual2
   *
   Stor 1 to cf,hal,jml
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpj12
   Set cent off
   *
   Do whil .t.
      Set cent off
      Nr=recno()
      Tglz=tanggal
      Fkt =faktur
      Nsls=kode_lgn
      Nklb=nama_ksr
      *
      Do namalgn
      ?chr(254)+' '+rtri(nsp)+' / '+rtri(jab)+' / '+rtri(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
      No=no+1
      *
      Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Kdb =kode_brg
         Nmb =nama_brg
         St  =satuan
         Kata="kdb+' '+iif(jumlah_rtr=0,left(nama_brg,30)+jml+satuan,left(nama_brg,28)+jml+left(satuan,3)+'+'+tran(jumlah_rtr,'999'))+tran(harga_2,'@e  9,999,999 ')+iif(diskon_rp>0,tran(diskon_rp,'@e 999,999 '),spac(8))+tran(&brs1,'@e 999,999,999')"
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         No  =no+1
         ?&kata
         Skip
         If faktur#fkt .or. nama_ksr#nklb .or. kode_lgn#nsls
            If jhg>0
               ?spac(68)+repl(gl,12)
               ?spac(42)+'Ksr:'+left(nklb,10)+'  Jumlah Rp.='+tran(jhg,'@e 999,999,999')
               ?repl(gl,80)
               No=no+3
            Endi
            Tglz=tanggal
            Fkt =faktur
            Nsls=kode_lgn
            Nklb=nama_ksr
            *
            Do namalgn
            ?chr(254)+' '+rtri(nsp)+' / '+rtri(jab)+' / '+rtri(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
            *
            No=no+1
            Jhg =0
         Endi
         If no>200
            ?repl(gl,80)
            ?spac(53)+'SUB TOTAL Rp'+tran(totx,'@e 999,999,999,999')
            ?''
            Stor 0 to no,totx
            Hal=hal+1
            Cf =cf+1
            Cfx=ltri(str(cf))
            Arg1='L-JUAL&cfx'+'.RTF'
            Set alte to &arg1
            Do koplpj12
         Endi
      Endd
      If beda
         Set cent on
         Do carifljual
         If tgl>tgl2
            Exit
         Endi
         If file('&nf0')
            Do bukafjual2
         Endi
      Elseif beda=.f.
         Exit
      Endi

   Endd
   ?repl(gl,80)
   If xOwner
      ?spac(27)+'POKOK  Rp'+tran(tot2    ,'@e 99,999,999,999')+'       JUMLAH Rp'+tran(tot+totr,'@e 99,999,999,999')
      ?spac(27)+'Margin Rp'+tran(tot-tot2,'@e 99,999,999,999')+'       RETUR  Rp'+tran(totr    ,'@e 99,999,999,999')
      ?spac(27)+'TOTAL  Rp'+tran(tot     ,'@e 99,999,999,999')
   Endi
   ?' '
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai     F4=Hapus/Tgl     F8=Koreksi     F10=Cetak'
   If lastkey()=27
      Loop
   Elseif lastkey()=-7 .and. ( Owner .or. xOwner ) .and. beda=.f.
      Do hpj
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      Do whil .not. yt$'YT'
         @23,65 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp
         Ul()
         Commit
         Do pesan with 'Faktur Penjualan tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
         Yt=' '
         Do whil .not. yt$'YT'
            @23,69 get yt pict '!'
            Read
         Endd
         If yt='Y'
            Sele 5
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp
            Ul()
            Commit
         Endi
      Endi
      Loop
   Endi
   Sambung='Y'
   Do pesan with 'Pake kertas bersambung? Y/T :  '
   @23,54 get sambung pict '!'
   Read
   Do cp
   If cc=0
      Loop
   Endi
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot,tot2
   Do koplpj122
   Go nr
   Do whil .t.
      Set cent off
      Nr=recno()
      Tglz=tanggal
      Fkt =faktur
      Nsls=kode_lgn
      Nklb=nama_ksr
      *
      Do namalgn
      ?'* '+rtri(nsp)+' / '+rtri(jab)+' / '+rtri(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
      No=no+1
      *
      Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Kdb =kode_brg
         Nmb =nama_brg
         St  =satuan
         Kata="kdb+' '+iif(jumlah_rtr=0,left(nama_brg,30)+jml+satuan,left(nama_brg,28)+jml+left(satuan,3)+'+'+tran(jumlah_rtr,'999'))+tran(harga_2,'@e  9,999,999 ')+iif(diskon_rp>0,tran(diskon_rp,'@e 999,999 '),spac(8))+tran(&brs1,'@e 999,999,999')"
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         No  =no+1
         ?&kata
         Skip
         If faktur#fkt .or. nama_ksr#nklb .or. kode_lgn#nsls
            If jhg>0
               ?spac(42)+'Ksr:'+left(nklb,10)+'  Jumlah Rp.='+tran(jhg,'@e 999,999,999')
               ?' '
               No=no+2
            Endi
            Tglz=tanggal
            Fkt =faktur
            Nsls=kode_lgn
            Nklb=nama_ksr
            *
            Do namalgn
            ?'* '+rtri(nsp)+' / '+rtri(jab)+' / '+rtri(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
            *
            No=no+1
            Jhg =0
         Endi
         If no>55
            ?'--bersambung...'
            ?''
            If sambung='Y'
               Eject
            Else
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
            Endi
            No =1
            Hal=hal+1
            Do koplpj122
         Endi
      Endd
      If beda
         Set cent on
         Do carifljual
         If tgl>tgl2
            Exit
         Endi
         If file('&nf0')
            Do bukafjual2
         Endi
      Elseif beda=.f.
         Exit
      Endi
   Endd
   If cc=0
      Loop
   Endi
   ?repl('-',80)
   ?spac(50)+'Total Rp.'+tran(tot,'@e 99,999,999,999')
   ?repl('=',80)
   ?''
   Tgl=tgl4
   Eject
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Retu

Proc koplpj12
?' Plgn: '+nsp+'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
?repl(gl,7)+chr(194)+repl(gl,30)+chr(194)+repl(gl,12)+chr(194)+repl(gl,8)+chr(194)+repl(gl,8)+chr(194)+repl(gl,10)
?'Kode   '+tl+'Nama Barang'+spac(19)+tl+'  Qty.Sat.  '+tl+'  @Harga'+tl+' Dis.Rp '+tl+'    Jumlah'
?repl(gl,7)+chr(193)+repl(gl,30)+chr(193)+repl(gl,12)+chr(193)+repl(gl,8)+chr(193)+repl(gl,8)+chr(193)+repl(gl,10)
Retu

Proc koplpj122
Set prin on
Set devi to prin
Set marg to 5
?chr(27)+chr(33)+chr(1)
?' LAPORAN PENJUALAN'
If ksp#spac(10)
   ?' PLGN...: '+rtri(nsp)+' - '+ksp
Endi
If tgl=tgl2
   ?' TANGGAL: '+dtoc(tgl)+spac(86)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(40)+'Hal:'+tran(hal,'999')
Endi
?repl('=',80)
?'Kode    Nama Barang                      Jml.Brg.     @Harga  Dis.Rp   Jumlah Rp'
?repl('-',80)
No=1
Retu

Proc bukafjual2
Sele 3
Uselan('&nf0','&nf1','&nf2','&nf3','','')
Do cekjlap2
Seek dtos(tgl)
If .not. foun()
   Loca for (tanggal>=tgl .and. tanggal<=tgl2)
Endi
Retu

Proc cekjlap2
If ksp#spac(10)
   Set filter to kode_lgn=ksp
Elseif ksp=spac(10)
   Set filter to kode_lgn#spac(10)
Endi
Retu

Proc namalgn
xSele=select()
Sele 1
Seek nsls
Nsp=left(nama_lgn,25)
Jab=left(almt_lgn,25)
Sele(xsele)
Retu

* EOF LPJ12
