* LAPORAN PENJUALAN/TGL
Sele 1
Uselan('KASIR.DTA','','','','','')
Sele 2
Uselan('STOK.DTA','STOK.NDX','','','','')
Sele 3
*
Nsls=spac(20)
Stor ' ' to hri,bln,thn
Stor tglx to tgl,tgl2
Do whil .t.
   Ksr='  '
   Sele 1
   Set cent on
   Set key 28 to dafksr
   @1,0 clea to 24,79
   @2,24 say 'Nm.Ksr:'get nsls pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai dg.'get tgl2
   Do pesan with 'ESC=Selesai   F1=Daftar Kasir'
   Read
   @24,0
   Set key 28 to
   If lastkey()=27
      Exit
   Endi
   If nsls#spac(20)
      Loca for nama=nsls
      If .not. foun()
         Do pesan2 with 'Tidak ada kasir '+rtri(nsls)
         Loop
      Endi
      Ksr=nomer
   Endi
   @1,0 clea to 24,79
   Tgl4=tgl
   Stor 0 to b,no,tot,tot2,totr,tdns,jhg,jml,jml1,jml2,jdns
   Stor .f. to beda,xSambung
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   Endi
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   Endi
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Do bukafljual
   *
   Nr=recno()
   Stor 1 to cf,hal,no
   Cfx =ltri(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpj
   *
   Do whil .t.
      Set cent off
      Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Fkt=right(faktur,3)+'-'+kassa+'-'+left(nama_ksr,6)
         ?left(dtoc(tanggal),2)+'/'+subs(dtoc(tanggal),4,2)+tl+fkt+tl+left(nama_brg,28)+jml+satuan+tran(harga_2,'@e 99,999,999')+tl+tran(&brs1,'@e 99,999,999')
         No=no+1
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Tot2=tot2+&brs14
         Totr=totr+(jumlah_rtr* (harga_2- ((harga_2*(diskon/100))+diskon_rp)))
         Tdns=Tdns+rp_donasi
         Jml1=jml1+rp_bulat
         If no>200
            ?repl(gl,5)+chr(193)+repl(gl,12)+chr(193)+repl(gl,51)+chr(193)+repl(gl,10)
            ?spac(59)+'JUAL =Rp'+tran(tot,'@e 9,999,999,999')
            If xOwner
               ?spac(35)+'Margin=Rp'+tran(tot-tot2,'@e 999,999,999')+'    POKOK=Rp'+tran(tot2,'@e 9,999,999,999')
            Endi
            ?spac(35)+'Donasi     ='+tran(tdns,'@e 9999,999')
            No =1
            Hal=hal+1
            Cf =cf+1
            Cfx=ltri(str(cf))
            Arg1='L-JUAL&cfx'+'.RTF'
            Set alte to &arg1
            Do koplpj
         Endi
         Skip
      Endd
      If beda
         Set cent on
         Do carifljual
         If tgl>tgl2
            Exit
         Endi
         If file('&nf0')
            Do bukafljual
         Endi
      Elseif beda=.f.
         Exit
      Endi
      *
   Endd
   ?repl(gl,5)+chr(193)+repl(gl,12)+chr(193)+repl(gl,50)+chr(193)+repl(gl,11)
   If Owner
      ?spac(58)+'JUMLAH Rp'+tran(tot+totr,'@e 9,999,999,999')
   Elseif xOwner
      ?iif(jml1#0,'Pembulatan ='+tran(jml1,'@e 9999,999'),spac(20))+spac(9)+'POKOK  Rp'+tran(tot2    ,'@e 9,999,999,999')+'       JUMLAH Rp'+tran(tot+totr,'@e 9,999,999,999')
      ?spac(29)+'Margin Rp'+tran(tot-tot2,'@e 9,999,999,999')+'       RETUR  Rp'+tran(totr    ,'@e 9,999,999,999')
      ?spac(29)+'Donasi    '+tran(tdns   ,'@e    9,999,999')+'       TOTAL  Rp'+tran(tot     ,'@e 9,999,999,999')
   Endi
   ?' '
   Set alte off
   Set alte to
   @24,0
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai     '+iif(ksr#'  ','F4=Hapus/Tgl     ','')+'F8=Koreksi     F10=Cetak'
   *
   Go nr   &&&DITARUH DISINI UNT.CETAK YG TERPUTUS
   *
   If lastkey()=27
      Loop
   Elseif lastkey()=-7 .and. ( Owner .or. xOwner ) .and. beda=.f.
      Do hpj
      If xsambung=.f.
         Loop
      Endi
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Transaksi Ksr:'+rtri(nsls)+'  Tgl.'+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      Do whil .not. yt$'YT'
         @24,78 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls .and. kode_lgn=spac(5)
         Ul()
         Commit
      Endi
      Loop
   Endi
   *
   Stor 1 to n,no,hal,nhal,b
   Stor 0 to jhg,jml1,tot,tot2,totx,tdns
   If xSambung
      Do pesan with 'Mulai Hal:      Sub Total:         '
      @24,32 get hal  pict '999'
      @24,48 get tot2 pict '9999999999'
      Read
   Endi
   Sambung='Y'
   Do pesan with 'Mulai Hal:'+tran(hal,'999')+'   Sub Total Rp'+tran(tot2,'@e 99,999,999,999')+'     Pake kertas bersambung? Y/T : '
   @24,78 get sambung pict '!'
   Read
   If lastkey()=27
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Do koplpj1
   Set cent off
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Do fmtjml
      Jhg =&brs1
      Tot =tot +jhg
      Tot2=tot2+jhg
      Totx=totx+&brs14
      Tdns=Tdns+rp_donasi
      Jml1=jml1+rp_bulat
      ?' '+dtoc(tanggal)+'  '+jam+'  '+faktur+'  '+left(nama_ksr,10)+' '+kode_brg+'    '+nama_brg+jml+satuan+tran(harga_2,'@e 9,999,999 ')+tran(jhg,'@e 999,999,999')
      Nrz=recno()
      B  =b+1
      N  =n+1
      No =no+1
      If no>50
         ?repl('-',111)
         ?spac(77)+'Jumlah Hal ini  Rp'+tran(tot ,'@e 999,999,999,999')
         ?spac(77)+'Sub Total       Rp'+tran(tot2,'@e 999,999,999,999')
         ?repl('-',112)
         ?'#'+ltri(tran(n,'@e 999,999'))
         ?' '
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
         Hal =hal +1
         nHal=nHal+1
         If nHal>25
            Set prin off
            Set devi to scre
            Do pesan with 'Tunggu sampai selesai.     Tekan ENTER=untuk lanjut halaman '+ltri(str(hal))+' dst...'
            Wait''
            If lastkey()=27
               Exit
            Endi
            nHal=1
         Endi
         Tot=0
         Do koplpj1
      Endi
      Skip
   Endd
   If cc=0
      Loop
   Endi
   ?repl('-',111)
   ?iif(xSambung,spac(31),' Total Margin Rp'+tran(tot2-totx,'@e 999,999,999,999'))+spac(46)+'Jumlah Hal ini  Rp'+tran(tot ,'@e 999,999,999,999')
   ?iif(xSambung,spac(31),'       Donasi Rp'+tran(tdns     ,'@e 999,999,999,999'))+spac(46)+'Total Penjualan Rp'+tran(tot2,'@e 999,999,999,999')
   ?repl('=',111)
   ?' '
   Eject
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Rele nsls,tgl2,b,no,nr,tot,tot2,jhg,jml,jml1,jml2
Retu

Proc koplpj1
Set prin on
Set devi to prin
Set marg to 15
?chr(27)+chr(33)+chr(4)
?' LAPORAN PENJUALAN TUNAI'
If nsls#spac(5)
   ?' KASIR.: '+nsls
Endi
If tgl=tgl2
   ?' TANGAL: '+dtoc(tgl)+spac(86)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl)+' S/D '+dtoc(tgl2)+spac(72)+'Hal:'+tran(hal,'999')
Endi
?repl('=',111)
?' Tanggal   Jam    Nota   Kasir      Kode       Nama Barang                      Jml.Brg.    @Harga   Jumlah Rp.'
?repl('-',111)
No=1
Retu

Proc koplpj
?' Ksr: '+rtri(nsls)+'    Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' // Hal:'+tran(hal,'999')
?repl(gl,5)+chr(194)+repl(gl,12)+chr(194)+repl(gl,28)+chr(194)+repl(gl,10)+chr(194)+repl(gl,10)+chr(194)+repl(gl,10)
?'Tgl. '+tl+'Nota-Ks-Ksr.'+tl+'Nama Barang'+spac(17)+tl+'Jml.Barang'+tl+'    @Harga'+tl+' Jumlah Rp'
?repl(gl,5)+chr(197)+repl(gl,12)+chr(197)+repl(gl,28)+chr(193)+repl(gl,10)+chr(193)+repl(gl,10)+chr(197)+repl(gl,10)
Retu

Proc hpj
@1,0 clea to 23,79
@1,0 to 1,79 doub
@23,0 to 23,79
Katax='ESC=Selesai   ENTER=Edit   F1=No_Urut   F5=CariNota   F8=Hapus   F10=Cetak'
If hut
   Katax='ESC=Selesai   F8=Hapus'
Endi
Do pesan with katax
Declare field[16],kop[16],frm[16],bts[16]
Field[1]='RECNO()'
Field[2]='TANGGAL'
Field[3]='JAM'
Field[4]='FAKTUR'
Field[5]='left(NAMA_KSR,10)'
Field[6]='left(NAMA_BRG,30)'
Field[7]='JUMLAH'
Field[8]='left(SATUAN,4)'
Field[9]='HARGA_2'
Field[10]='HARGA'
Field[11]='DISKON'
Field[12]='KODE_BRG'
Field[13]='KODE_BAR'
Field[14]='KODE_LGN'
Field[15]='KASSA'
Field[16]='KETERANGAN'
Kop[1]='NoUrut'
Kop[2]='Tanggal'
Kop[3]='Jam'
Kop[4]='Nota'
Kop[5]='Kasir'
Kop[6]='Nama Barang'
Kop[7]=' Qty.'
Kop[8]='Sat.'
Kop[9]='   @Harga'
Kop[10]='   @Pokok'
Kop[11]='@Dis.%'
Kop[12]='KodeBrg'
Kop[13]='BarCode'
Kop[14]='Member'
Kop[15]='?'
Kop[16]='Keterangan'
Frm[1]='999999'
Frm[2]='99-99-99'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@!'
Frm[7]='@e 99999'
Frm[8]='@!'
Frm[9]='@e 9,999,999'
Frm[10]='@e 9,999,999'
Frm[11]='@e 99.99 '
Frm[12]='@!'
Frm[13]='@!'
Frm[14]='@!'
Frm[15]='@!'
Frm[16]='@!'
Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9],bts[10],;
           bts[11],bts[12],bts[13],bts[14],bts[15],bts[16]
Go nr
Dbedit(2,0,22,79,field,'fhpj',frm,kop,gl,bts)
*
Rele field[15],kop[15],frm[15],bts[15]
Set curs on
Retu

Function fhpj
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      If tanggal<tgl .or. tanggal>tgl2
         Do case
            Case lastkey()=18  &&&-PgUp
               Tone(700,2)
               Skip 18
            Case lastkey()=3   &&&-Pgln
               Tone(700,2)
               Skip -18
            Case lastkey()=5   &&&-Up
               Tone(700,2)
               Skip 1
            Case lastkey()=24   &&&-Dn
               Tone(700,2)
               Skip -1
         Endc
      Endi
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=28
      Set curs on
      xNo=0
      Do pesan with 'Loncat ke No_Urut #           ( lihat di bagian bawah kertas #??? )'
      @24,25 get xno pict '999999'
      Read
      If lastkey()#27 .or. xno>0
         Skip xno
         N=xno
      Endi
      Do pesan with katax
      Set curs off
      Retu 1
   Case lastkey()=-4
      Set curs on
      Fkt=spac(5)
      Do pesan with 'No.Nota:     '
      @24,41 get fkt pict '@!'
      Read
      If lastkey()#27 .or. fkt#spac(5)
         Seek (thn+bln+hri)+nsls+fkt
      Endi
      Do pesan with katax
      Set curs off
      Retu 1
   Case lastkey()=-7
      Kdb=kode_brg
      Jml=jumlah
      Hj =harga_2
      *
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Yt=' '
      Do whil .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
      Endd
      If yt='Y'
         If .not. dele()
            Rlok()
            Repl keterangan with dtos(tglx)+left(time(),5)+ksa
            Dele
            Ul()
            Commit
            Skip
            *
            Sele 2
            Set order to 1
            Seek kdb
            Rlok()
            Repl stok_k with (stok_k-jml)
            Ul()
            Commit
            *
            If hut
               Sele 5
               Seek (thn+bln+hri)+ksp+fkt
               Rlok()
               Repl rp_total with rp_total-(jml*hj)
               Ul()
               Commit
            Endi

            Sele 3
         Endi
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Case lastkey()=-9
      Nr=recno()
      Set colo to w+/b
      @row(),0 say tran(recno(),'999999')+tl+dtoc(tanggal)+tl+faktur+tl+left(nama_ksr,8)+tl+left(nama_brg,29)+tl+;
                   tran(jumlah,'9999')+tl+tran(jumlah_rtr,'9999')+tl+tran(harga_2,'@e 9,999,999')
      Set colo to g
      Do pesan with 'Cetak Mulai Record ini? Y/T :'
      Set curs on
      Yt=' '
      Do whil .not. yt$'YT'
         @24,55 get yt pict '!'
         Read
      Endd
      If yt='Y'
         xSambung=.t.
      Endi
      Do pesan with katax
      Set curs off
      Retu 0
   Case lastkey()=13
      Kdb=kode_brg
      Set curs on
      If field[pointer]='JUMLAH'
         Totx=jumlah
         Rlok()
         @row(),col() get &fds pict '@e 9999.99'
         Read
         Ul()
         Commit
         Jml=jumlah

         Sele 2
         Seek kdb
         Rlok()
         Repl stok_k with (stok_k-(totx+jml))
         Ul()
         Commit

         Sele 3
      Elseif left(field[pointer],5)='HARGA'
         Rlok()
         @row(),col() get &fds pict '@e 9,999,999'
         Read
         Ul()
         Commit
      Elseif field[pointer]='KODE_KLB' .or. field[pointer]='KODE_BRG'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         Commit
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

Proc carifljual
xBln=mont(tgl)+1
xThn=year(tgl)
If xBln=13
   xBln=1
   xThn=year(tgl)+1
Endi
Tgl=ctod('01-'+ltri(str(xBln))+'-'+ltri(str(xThn)))
Set cent on
Do bacafile
Set cent off
Retu

Proc bukafljual
Sele 3
Uselan('&nf0','&nf3','&nf1','&nf2','','')
If nsls=spac(20)
   Set filter to id_data=' '
Else
   Set filter to id_data=' ' .and. kode_ksr=ksr
Endi
Seek dtos(tgl)
If .not. foun()
   Loca for (tanggal>=tgl .and. tanggal<=tgl2)
Endi
Retu

Func tampiljlap
Para brs
@brs,24   say '0 =Kredit   1 =Tunai   2 =Semua ...:'get jlap pict '!'
Retu(.T.)

Proc cekjlap
Ket='(TUNAI & KREDIT)'
Flt="id_data#'~'"
If jlap='0'
   Flt="id_data='K'"
   Ket='(KREDIT)'
Elseif jlap='1'
   Flt="id_data=' '"
   Ket='(TUNAI)'
Endi
Set filter to &flt
Retu

Func Layar
Para arg1
Stor .f. to maju,mundur
Hal=1
Do whil hal<=cf
   Cfx =ltri(str(hal))
   Arg2='&arg1&cfx' + '.RTF'
   Do pesan with &gtlayar
   Set colo to w/b
   Set key 13 to xit
   Set key  8 to xmundur
   Memoedit(Memoread('&arg2'),1,0,23,210,.t.,'',210)
   Set key 13 to
   Set key  8 to
   If lastkey()=27     .and. ( maju=.f. .and. mundur=.f. )
      Exit
   Elseif lastkey()=27 .and. mundur=.t.
      Mundur=.f.
      Hal=hal-1
      If hal<1
         Hal=1
      Endi
      Loop
   Endi
   Maju=.f.
   Hal =hal+1
Endd
Retu(.T.)

Proc xit
Maju=.t.
Ada =.t.
Keyboard chr(27)
Retu

Proc xmundur
Mundur=.t.
Keyboard chr(27)
Retu

Proc endexport
?' '
Set alte off
Set alte to
Do pesan with 'Export selesai..., nama file: '+subs(arg1,4,12)+' , di drive D:\'
Wait''
Retu

* EOF LPJ1
