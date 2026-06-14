Procedure LPB1
* LAPORAN PEMBELIAN/TGL
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
Sele 3
Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
Sele 4
*
Sele 5
*
Nklb='-'
Stor ' ' to hri,bln,thn
Stor tglx to tgl,tgl2
Do While .t.
   Ksp=spac(5)
   Sele 1
   Set filter to
   Set cent on
   Set key 28 to dafspl
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 24,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai   F1=Daftar Supplier'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Nklb='-'
   If ksp#spac(5)
      Set order to 1
      Seek ksp
      If .not. foun()
         Set order to 2
         Seek rtri(ksp)
         Do dafspl
EndIf
      If lastkey()=27
         Loop
EndIf
      Nklb=nama_spl
EndIf
   Stor 0 to no,tot,tot2,hbnet
   Do konvtgl
   Sele 3
   Set filter to
   If ksp#spac(5)
      Set filter to kode_spl=ksp
EndIf
   Seek (thn+bln+hri)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
EndIf
   Nr=recno()
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='L-BELI&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpb
   Set cent off
   Fkt =faktur
   Nsls=''
   Nsp =nama_spl
   Lks =lokasi
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      If jumlah<0
            Jml=tran(jumlah,'@e  99,999 ')
      Elseif jumlah=0
            Jml=tran(jumlah,'@e  99,999 ')
      Elseif jumlah>0 .and. jumlah<1
            Jml=tran(jumlah,'@e 9999.99 ')
      Elseif jumlah>=1
            If mod(jumlah,int(jumlah))>0
               Jml=tran(jumlah,'@e 9999.99 ')
            Else
               Jml=tran(jumlah,'@e  99,999 ')
EndIf
EndIf
      Hitdis()
      Jhg =hbnet
      Tot =tot +jhg
      Tot2=tot2+jhg
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+' '+left(nama_brg,27)+jml+satuan+tran(harga,'@e  9,999,999 ')+tran(jhg,'@e 999,999,999')
      Skip
      If faktur#fkt .or. nama_spl#nsp
         ?spac(68)+repl(gl,12)
         ?spac(10)+'Supplier: '+left(nsp,20)+' No.Fkt:'+fkt+' Jumlah Rp='+tran(tot2,'@e 999,999,999')
         ?repl(gl,80)
         Fkt =faktur
         Nsp =nama_spl
         Lks =lokasi
         Tot2=0
         No  =no+1
         If no>=10
            No =0
            Hal=hal+1
            Cf =cf +1
            Cfx=ltri(str(cf))
            Arg1='L-BELI&cfx'+'.RTF'
            Set alte to &arg1
            Do koplpb
EndIf
EndIf
EndDo
   If xOwner
      ?spac(61)+'TOTAL=Rp'+tran(tot,'@e 999,999,999')
EndIf
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-BELI')
   *
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. xOwner
      Do hpb
      Loop
   Elseif lastkey()=-3 .and. xOwner
      Set cent on
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
Do While .not. yt$'YT'
         @24,66 get yt pict '!'
         Read
EndDo
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
         Ul()
         Commit
         Do pesan with 'Faktur Pembelian tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
         Yt=' '
Do While .not. yt$'YT'
            @24,70 get yt pict '!'
            Read
EndDo
         If yt='Y'
            Sele 5
            Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','','','')
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
            Ul()
            Commit
            Close
EndIf
EndIf
      Loop
   Elseif lastkey()=27
      Loop
EndIf
   If xOwner
      Do cp
      If cc=0
         Loop
EndIf
   Else
      Loop
EndIf
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot,tot2
   Do koplpb1
   Go nr
   Set cent off
   Fkt =faktur
   Nsls=nama_spl
   Lks =lokasi
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      If jumlah<0
         Jml=tran(jumlah,' 999,999 ')
      Elseif jumlah=0
         Jml=tran(jumlah,' 999,999 ')
      Elseif jumlah>0 .and. jumlah<1
         Jml=tran(jumlah,'@e  9999.99 ')
      Elseif jumlah>=1
         If mod(jumlah,int(jumlah))>0
            Jml=tran(jumlah,'@e  9999.99 ')
         Else
            Jml=tran(jumlah,' 999,999 ')
EndIf
EndIf
      Hitdis()
      Jhg =hbnet   &&((jumlah*harga)-((jumlah*harga)*(diskon/100)))
      Tot =tot +jhg
      Tot2=tot2+jhg
      ?' '+dtoc(tanggal)+'  '+left(kode_bar,13)+'  '+nama_brg+tran(isine_b,' 9999  ')+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(diskon_1,'@e 99,99 ')+tran(jhg,'@e 999,999,999')
      B =b+1
      No=no+1
      Skip
      If faktur#fkt .or. nama_spl#nsls
         ?spac(102)+repl('-',12)
         ?spac(26)+'Supp: '+nsls+'    No.Fkt: '+fkt+'                  Jumlah Rp.'+tran(tot2,'@e 9999,999,999')
         ?repl('-',114)
         B =b+3
         No=no+3
         Fkt =faktur
         Nsls=nama_spl
         Lks =lokasi
         Tot2=0
EndIf
      If no>55
         ?repl('-',114)
         ?spac(91)+'bersambung...'
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
         No =1
         Hal=hal+1
         Do koplpb1
EndIf
EndDo
   If cc=0
      Loop
EndIf
   ?' TOTAL PEMBELIAN'+spac(81)+'Rp '+tran(tot,'@e 9,999,999,999')
   ?repl('=',114)
   ?' '
   Eject
   Set prin off
   Set devi to scre
EndDo
Set curs off
Clos all
Return
Return

Procedure koplpb
?' Spl: '+rtri(nklb)+'-'+rtri(ksp)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(26)+'Hal:'+tran(hal,'999')
?repl(gl,9)+chr(194)+repl(gl,7)+chr(194)+repl(gl,27)+chr(194)+repl(gl,12)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)
?'Tanggal  '+tl+'Kode   '+tl+'Nama Barang'+spac(16)+tl+'   Jml.Brg. '+tl+'   @Harga'+tl+'     Jumlah'
?repl(gl,9)+chr(193)+repl(gl,7)+chr(193)+repl(gl,27)+chr(193)+repl(gl,12)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)
Return
Procedure koplpb1
Set prin on
Set devi to prin
?chr(27)+chr(33)+chr(4)
Set marg to 10
?' LAPORAN PEMBELIAN'
If ksp#spac(5)
   ?' Supl.: '+rtri(nklb)+' - '+ksp
EndIf
If tgl=tgl2
   ?' Tgl..: '+dtoc(tgl)+spac(81)+'Hal:'+tran(hal,'999')
Else
   ?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+spac(65)+'Hal:'+tran(hal,'999')
EndIf
?repl('=',114)
?' Tanggal   Barcode        Nama Barang                               Isi    Jml.Brg.       @Harga  Dis%   Rp.Jumlah'
?repl('-',114)
No=1
Return
Procedure hpb
@1,0 clea to 24,79
@1,0 say repl(gl,80)
@22,0 to 22,79
Katax='ESC=Selesai   F5=Edit   F8=Hapus'
Do pesan with katax
Declare field[10],kop[10],frm[10],bts[10]
Field[1]='TANGGAL'
Field[2]='JAM'
Field[3]='left(FAKTUR,7)'
Field[4]='left(NAMA_SPL,18)'
Field[5]='left(NAMA_BRG,30)'
Field[6]='JUMLAH'
Field[7]='SATUAN'
Field[8]='HARGA'
Field[9]='KODE_BRG'
Field[10]='LOKASI'
Kop[1]='Tanggal'
Kop[2]='Jam'
Kop[3]='No.Fkt'
Kop[4]='Nama Supplier'
Kop[5]='Nama Barang'
Kop[6]='Qty.'
Kop[7]='St.'
Kop[8]=' @Hg.Beli'
Kop[9]='Kode Brg.'
Kop[10]='Lks'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 9999.99'
Frm[7]='@!'
Frm[8]='@e 9,999,999'
Frm[9]='@!'
Frm[10]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Bts[7]=tl
Bts[8]=tl
Bts[9]=tl
Bts[10]=tl
Go nr
Dbedit(2,0,21,79,field,'fhpbx',frm,kop,gl,bts)
*
Set curs on
Return

Function fhpbx
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
EndIf
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-7
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Ht=' '
Do While .not. ht$'YT'
         @24,47 get ht pict '!'
         Read
EndDo
      If ht='Y'
         Tgl=tanggal
         Ksp=kode_spl
         Fkt=faktur
         Kdb=kode_bar
         Jml=jumlah
         Hp =harga
         Lks=lokasi
         Isi=1   &&isine_b
         Do konvtgl
         If .not. dele()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
            *
            Sele 2
            Seek kdb
            Rlok()
            If hut
               If lks='G'
                  Repl stok_mg with stok_mg-(jml*isi)
               Else
                  Repl stok_m with stok_m-(jml*isi)
EndIf
            Elseif hut=.f.
               Repl stok_m with stok_m+(jml*isi)
EndIf
            Ul()
            Commit
            *
            If hut .and. ksp#'TUNAI'
               Sele 5
               Uselan('FAKTURH.DTA','FAKTURH1.NDX','','','','')
               Seek (thn+bln+hri)+ksp+fkt
               Rlok()
               Repl rp_total with rp_total-(jml*hp)
               Ul()
               Commit
               Close
EndIf
            *
            Sele 3
EndIf
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Case lastkey()=-4 .and. fds='JUMLAH' .or. fds='HARGA'
      Kdb=kode_bar
      Hp =harga
      Lks=lokasi
      Isi=1
      Stor jumlah to jml,jml2
      Do konvtgl
      Set curs on
      Set colo to gr+*
      If fds='JUMLAH'
         @row(),col() get jml pict '@e 9999.99'
         Do pesan with 'Masukan Jumlah Barang'
      Else
         @row(),col() get hp pict '@e 9,999,999'
         Do pesan with 'Masukan Harga Beli'
EndIf
      Read
      *
      If lastkey()#27
         Rlok()
         Repl jumlah with jml,harga with hp
         Ul()
         Commit
         *
         Sele 2
         Seek kdb
         Rlok()
         If hut
            If lks='G'
               Repl stok_mg with (stok_mg-(jml2*isi))+(jml*isi)
            Else
               Repl stok_m with (stok_m-(jml2*isi))+(jml*isi)
EndIf
            Repl harga with hp,harga_b with hp*isine_b
         Elseif hut=.f.
            Repl stok_m with stok_m+(jml*isi)
EndIf
         Ul()
         Commit
         *
         Sele 3
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Case lastkey()=-4 .and. fds='left(FAKTUR,7)'
      Fkt=faktur
      Do konvtgl
      Set curs on
      Set colo to gr+*
      @row(),col() get fkt pict '@!'
      Do pesan with 'Masukan No.Faktur'
      Read
      *
      If lastkey()#27
         Rlok()
         Repl faktur with fkt
         Ul()
         Commit
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Othe
Return 1
Endc
Return 0

* LAPORAN PEMBELIAN/ITEM
*-----------------------
Procedure lpb2
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK4.NDX','','','','')

Stor ' ' to jlap,ket
Stor tglx to tgl,tgl2
Nmb=spac(30)
Carikode=.f.
Do While .t.
   Sele 2
   Set cent on
   Set filter to
   @1,0 clea to 24,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
EndIf
   If nmb=spac(30)
      Loop
EndIf
   Seek nmb
   If .not. foun()
      Seek rtri(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
EndIf
EndIf
   Klb=kode_klb
   Kdb=kode_brg
   Nmb=nama_brg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @2,34 say nmb
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Loop
EndIf
   Sele 1
   Uselan('BELIIN.DTA','BELIIN2.NDX','','','','')
   *
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set cent off
   Set alte on
   Set alte to L-BELI.RTF
   ?' LAPORAN PEMBELIAN PER ITEM BARANG'
   ?' NAMA BRG: '+rtri(nmb)+'   KODE: '+kdb
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(218)+repl(gl,4)+chr(194)+repl(gl,14)+chr(194)+repl(gl,17)+chr(194)+repl(gl,10)+chr(194)+repl(gl,7)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)+chr(191)
   ?tl+' No.'+tl+'Tanggal       '+tl+'Nama Supplier    '+tl+'No.Faktur '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+repl(gl,4)+chr(197)+repl(gl,14)+chr(197)+repl(gl,17)+chr(197)+repl(gl,10)+chr(197)+repl(gl,7)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)+chr(180)
   Y=1
   Stor 0 to stm,hp,hj,jml,tot,tot2
   Z=ltri(str(y))
Do While .t.
      Seek kdb
      St =satuan
Do While .not. eof() .and. kode_brg=kdb
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=(jumlah*harga)
            Stm=stm+jumlah
            Tot=tot + jhg
            Jumlah_rtr=0
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+left(nama_spl,17)+tl+left(faktur,10)+tl+left(jml,6)+iif(lokasi='G',lokasi,' ')+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y = y + 1
EndIf
         Skip
EndDo
      Exit
EndDo
   Nmb =spac(30)
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999 ')+st
   ?chr(192)+repl(gl,4)+chr(193)+repl(gl,14)+chr(193)+repl(gl,17)+chr(193)+repl(gl,10)+chr(193)+repl(gl,7)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(217)
   ?spac(18)+'TOTAL ='+spac(24)+pesanx+'  Rp'+tran(tot, '@e 9,999,999,999')
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('L-BELI.RTF'),1,0,22,240,.t.,'',240)
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
   Set marg to 15
   ?chr(27)+chr(33)+chr(4)
   Type L-BELI.RTF to prin
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

* EOF LPB1
