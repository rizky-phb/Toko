Procedure MPO
* PO
Sele 1
*
Sele 2
*
Sele 3
*
Sele 4
*
Sele 5
*
Sele 6
*
Sele 7
*
Sele 8
Uselan('DATA.SET','','','','','')
Cpo1=ket_po1
Cpo2=ket_po2
Cpo3=ket_po3
Npjk=npwp
Close
*
Stor .f. to ada,tambah,hapus,baru,diedit,isilagi,adaitem
Stor 0 to nrs,nrf,nr,jdt,xhal
Stor tglx to tgl,tgl2
Tgl2=ctod('  -  -    ')
Stor spac(5) to ksp,klb,nmb,fkt,nsp
Do While .t.
   Stor 0 to b,jml,hp,hb,hj,hj22,hj24,dis,pjk,dis1,pjk1,pjg,stk,pru,bts3,bts4,bts5,hj3,hj4,hj5,jwk,nn
   Stor 0 to jhg,tot,dis2,dis22,ppn2
   Sambung='Y'
   Flt    =' '
   *
   Set colo to /bg
   @1,0 clea to 24,79
   @21,2 to 21,77
   @1,2 say repl(repl(gl,10)+chr(194),2)+repl(repl(gl,5)+chr(194),2)+repl(gl,30)+chr(194)+repl(gl,11)
   @2,2 say 'Tanggal   '+tl+'Tgl.Kirim '+tl+'Nomor'+tl+'Kode '+tl+'Nama Supplier'+spac(17)+tl+'   Rp Total'
   @3,2 say repl(repl(gl,10)+chr(197),2)+repl(repl(gl,5)+chr(197),2)+repl(gl,30)+chr(197)+repl(gl,11)
   Do pesan with 'Tunggu, sedang dihitung...'
   *
   Sele 7
   Uselan('FAKTURO.DTA','FAKTURO1.NDX','FAKTURO2.NDX','','','')
   Sum rp_total to tot2
   If adaitem=.f.
      Go bott
      If recno()>17
         Skip -17
      Else
         Go top
EndIf
   Else
      Go nrf
EndIf
   Nrf=recno()
   Tgl=tglx
   Hbl=subs(dtoc(tgl),4,2)+right(dtoc(tgl),2)
   Set colo to gr+/b
   @22,0
   @22,49 say 'Total Order Rp ='+tran(tot2,'@e 9,999,999,999')
   Pesanx='F4=Tambah   F5=Edit   F8=Hapus   T=Top'
   Do pesan with pesanx
   Declare field[8],bts[8],frm[8]
   Field[1]='TANGGAL'
   Field[2]='TANGGAL_3'
   Field[3]='FAKTUR'
   Field[4]='KODE_SPL'
   Field[5]='NAMA_SPL'
   Field[6]='RP_TOTAL'
   Frm[1]='99-99-9999'
   Frm[2]='99-99-9999'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@!'
   Frm[6]='@e 999,999,999'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl
   Bts[4]=tl
   Bts[5]=tl
   Bts[6]=tl
   Dbedit(4,2,20,77,field,'ffkto',frm,'','',bts)
   *
   If lastkey()=27 .and. (.not. tambah)
      Exit
EndIf
   Tambah=.f.
EndDo
Set curs off
Clos all
Rele ada,batal,adadis,ubahjml,tambah,hapus,baru,tgl2,fkt,ksp,klb,lks,slhtbrg
Rele kdb,nmb,jml,isi,st,stb,tot,tot2,dis,dis1,dis2,ppn2,ppn22,hj,hp,hpb,byr
Rele hp,hb,hj,dis,pjk,dis1,pjk1,pjg,stk,hj22,hj24,hj3,hj4,hj5,bts3,bts4,bts5,pr,pr2
Return

Return

Function ffkto
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-3 .or. lastkey()=22
      Save scre to sdafx
      Set curs on
      Stor tglx to tgl,tgl2
      Tgl2=ctod('  -  -    ')
      Stor spac(5) to ksp,klb
      Stor .f. to diEdit,adapo
      Tambah=.t.
      Sele 8
      Uselan('DATA.SET','','','','','')
      Nn =no_order+1
      Do xnota
      Close
      *
      Do mpox
      *
      If adapo
         Sele 8
         Uselan('DATA.SET','','','','','')
         Rlok()
         Repl no_order with nn
         Ul()
         Commit
         Close
         *
         Nn=nn+1
         If nn>99999
            Nn=1
EndIf
         Do xnota
EndIf
      *
      Rest scre from sdafx
   Case lastkey()=-4
      Save scre to sdafx
      Set curs on
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      Nsp =nama_spl
      Nrf =recno()
      Stor .t. to diEdit,tambah,adaitem
      Do mpox
      Rest scre from sdafx
   Case lastkey()=-5
      Tgl=tanggal
      *
      Set colo to gr+*/b
      @row(),col() say &fds
      Yt=' '
      Set curs on
      Set colo to /w
      Do pesan with 'Proses? Y/T :'
Do While .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
         If lastkey()=27
            Exit
EndIf
EndDo
      If yt='Y' .and. lastkey()#27
         Go top
Do While .not. eof()
            Ksp=kode_spl
            Fkt=faktur
            Nfo='PO\'+right(rtri(ksp),3)+fkt+'.DTA'
            Nfx='TEMP\'+right(rtri(ksp),3)+hbl+'.TMP'
            *
            Do pesan with 'Tunggu, sedang proses...'
            Copy file ORDER.DTA to &nfx
            *
            Sele 4
            Useidx('&nfx')
            Appe from &nfo
            Copy to &nfo
            Close
            *
            Sele 7
            Skip
EndDo
EndIf
      Set curs off
Return 0
   Case lastkey()=-7
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      Nfo='PO\'+right(rtri(ksp),3)+fkt+'.DTA'
      *
      Set colo to gr+*/b
      @row(),col() say &fds
      Yt=' '
      Set curs on
      Set colo to /w
      Do pesan with 'Benar mau diHapus? Y/T :'
Do While .not. yt$'YT'
         @24,53 get yt pict '!'
         Read
EndDo
      If yt='Y'
         If .not. deleted()
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            Commit
            Skip
            *
            *Dele file &nfo
            *
EndIf
EndIf
      Sele 3
      Close
      *
      Sele 4
      Close
      *
      Do pesan with pesanx
      Set curs off
      Sele 7
Return 2
   Case lastkey()=133
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      Tot =0
      B   =6
      No  =1
      Save scre to sdaf
      Set colo to bg+/b
      @4,8 say dtoc(tanggal)+tl+dtoc(tanggal_3)+tl+faktur+tl+kode_spl+tl+left(nama_spl,30)
      Set colo to /w
      @5,0 to 5,79 doub
      @6,0 clea to 22,79
      @22,0 to 22,79
      Sele 4
      Uselan('ORDERIN.DTA','ORDERIN1.NDX','','','','')
      Seek dtos(tgl)+ksp+fkt
Do While .not. eof() .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
         Jml =jumlah
         Hp  =(jml*harga)
         Tot =tot+jhg
         *
         @b,0 say tran(no,'999. ')+left(nama_brg,25)+' '+left(kode_brg,7)+tran(jumlah,'@e  9999.99 ')+left(satuan,4)+tran(harga,'@e 9,999,999 ')+tran(diskon,'@e 99.99 ')+tran(pajak,'99=')+tran(jhg,'@e 99,999,999')
         B =b+1
         No=no+1
         If b>21
            Do pesan with 'Pgln=Lanjut'
            Wait''
            Set colo to w
            B=6
EndIf
         Skip
EndDo
      @b,0 clea to 19,79
      Do pesan with 'ESC=Selesai'
      Wait''
      Rest scre from sdaf
      Do pesan with pesanx
      Sele 7
Return 1
   Case lastkey()=84 .or. lastkey()=116
      If fds='KODE_SPL' .or. fds='NAMA_SPL'
         Set order to 2
      Else
         Set order to 1
EndIf
Return 2
   Othe
Return 1
Endc
Return 0
Procedure mpox
Stor 0 to jml,tot,tot2,dis,ppn,dis1,hj,hp,hpb
Stor .f. to hapus,batal,ada,adadis,ubahjml
Do While .t.
   Set colo to /w
   @1,0 clea to 24,79
   @1,1  say 'No.Urut: '
   @2,1  say 'Tanggal: '
   @1,40 say 'Kd.Supl: '
   If diEdit=.f.
      Sele 1
      Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
      *
      @1,1  say 'No.Urut: '+fkt
      @2,1  say 'Tanggal:'get tgl
      @1,40 say 'Kd.Supl:'get ksp pict '@!'
      Do pesan with 'ESC=Selesai'
      Read
      If lastkey()=27
         Exit
EndIf
      If ksp=spac(5)
         Do dafspl
EndIf
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
      Set order to 1
      Nsp  =nama_spl
      Set colo to w
      @1,10 say fkt
      @2,10 say tgl
      @1,49 say ksp
      @2,49 say nsp
      Yt='Y'
      Do pesan with 'Data Benar? Y/T: '
      @24,48 get yt pict '!'
      Read
      If yt#'Y'
         Loop
EndIf
      *
      Sele 7
      Uselan('FAKTURO.DTA','FAKTURO1.NDX','FAKTURO2.NDX','','','')
      Seek dtos(tgl)+ksp+fkt
      If foun()
         Tot  =0
      Elseif .not. foun()
         Flok()
         Appe blan
         Repl tanggal with tgl,tanggal_2 with dtos(tgl),tanggal_3 with tgl2,faktur with fkt,;
              kode_spl with ksp,nama_spl with nsp
         Ul()
EndIf
      Nrf  =recno()
      Adapo=.t.
   Else
      Set colo to w
      @1,10 say fkt
      @2,10 say tgl
      @1,49 say ksp
      @2,49 say nsp
EndIf
   Hbl=subs(dtoc(tgl),4,2)+right(dtoc(tgl),2)
   Nfo='PO\'+right(rtri(ksp),3)+fkt+'.DTA'
   Nfx='TEMP\'+right(rtri(ksp),3)+hbl+'.TMP'
   *
   Sele 3
   Uselan('STOK.DTA','STOK2.NDX','STOK.NDX','','','')
   *
   If .not. file('&nfo')
      Do pesan with 'Tunggu, sedang proses...'
      Set filter to kode_spl=ksp
      Go top
      Copy to &nfx
      *
      Sele 5
      Useidx('&nfx')
      Repl all stok_a with ((STOK_A+STOK_M+(STOK_AJ)) - (STOK_K+STOK_RB+STOK_KR))
      Close
      *
      Uselan('ORDER.DTA','','','','','')
      Copy stru to &nfo
      *
      Useidx('&nfo')
      Appe from &nfx
      Repl all tanggal with tgl,faktur with fkt,kode_spl with ksp
      Commit
      Sort on nama_brg to &nfx
      Close
      *
      Copy file &nfx to &nfo
      *
EndIf
   Sele 5
   Useidx('&nfo')
   Sum (jumlah*harga) to tot
   *
   Sele 7
   Go nrf
   Rlok()
   Repl rp_total with tot
   Ul()
   Commit
   *
   Sele 5
   Go top
   Set colo to w+/r
   @3,0 to 3,79
   @3,0 say 'Nama Barang'
   @3,31 say 'Isi'+chr(194)
   @3,35 say 'SatB'+gl+chr(194)
   @3,42 say 'Sisa'+chr(194)
   @3,48 say 'Jual'+chr(194)
   @3,53 say 'Order'+chr(194)
   @3,63 say 'Harga'
   @3,71 say 'Jumlah Rp'
   Set colo to w+/b
   @22,57 say 'Jumlah= Rp'+tran(tot,'@e 9,999,999,999')
   Set colo to /w
   @21,0 to 21,79
   Pesanx='ENTER=Edit     F1=Filter     F4=Tambah Item     F8=Hapus     F9/F10=Cetak'
   Do pesan with pesanx
   Declare field[9],bts[9],frm[9]
   Field[1]='left(NAMA_BRG,29)'
   Field[2]='ISINE_B'
   Field[3]='SATUAN'
   Field[4]='STOK_A'
   Field[5]='STOK_K+STOK_KR'
   Field[6]='JUMLAH'
   Field[7]='HARGA'
   Field[8]='(JUMLAH*HARGA)'
   Frm[1]='@!'
   Frm[2]='@e 9999'
   Frm[3]='@!'
   Frm[4]='@e 99999'
   Frm[5]='@e 99999'
   Frm[6]='@e 99999'
   Frm[7]='@e 9,999,999'
   Frm[8]='@e 999,999,999'
   Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9]
   Keyboard chr(4)+chr(4)+chr(4)+chr(4)+chr(4)
   Dbedit(4,0,20,79,field,'fisipo',frm,'','',bts)
   *
   If lastkey()=27 .and. (.not. tambah)
      Exit
EndIf
   Ksp=spac(5)
   Tgl2=ctod('  -  -    ')
   Ada=.f.
   *
   Sele 3
   Commit
   Close
   *
   Sele 4
   Commit
   Close
   *
   Exit
EndDo
Return

***KOLOM JUAL/BELI
Procedure kolompo
Do While b<=16
   @b,0 say spac(13)+tl+spac(28)+tl+spac(12)+tl+spac(12)+tl+spac(11)
   B =b+1                         
EndDo
@b,0 say repl(gl,13)+chr(193)+repl(gl,28)+chr(193)+repl(gl,12)+chr(193)+repl(gl,12)+chr(193)+repl(gl,11)
B=4
Return

Function fisipo
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Set colo to w+/b
   @22,0
   @22,1  say ltri(str(recno()))+'/'+ltri(str(reccount()))
   @22,10 say rtri(nama_brg)+'  /Jual: '+ltri(tran(stok_k,'@e 999,999'))+' + '+ltri(tran(stok_kr,'@e 999,999'))
   @22,57 say 'Jumlah= Rp'+tran(tot,'@e 9,999,999,999')
   Do pesan with pesanx
Return 1
Elseif lastkey()=27
Return 0
Elseif lastkey()=28
   If flt=' '
      Flt='jumlah#0'
      Set filter to &flt
      Loca for &flt
   Else
      Set filter to
      Flt=' '
EndIf
Return 2
Elseif lastkey()=13 .and. (fds='JUMLAH' .or. fds='HARGA')
   B   =row()
   Kl  =col()
   Isi =isine_b
   Jml =jumlah
   Hp  =harga
   Tot =tot-(jumlah*harga)
   Set curs on
   If fds='JUMLAH'
      @b,kl get jml pict '@e  9999'
      Do pesan with 'Masukan Jumlah Order Barangnya'
   Else
      @b,kl get hp  pict '@e 9,999,999'
      Do pesan with 'Masukan Perkiraan Harganya'
EndIf
   Read
   Tot =tot+(jml*hp)
   *
   If lastkey()#27
      Repl jumlah with jml,harga with hp
      Commit
      *
      Sele 7
      Go nrf
      Rlok()
      Repl rp_total with tot
      Ul()
      Commit
      *
      Sele 5
EndIf
   Do pesan with pesanx
   Set curs off
Return 2
Elseif lastkey()=13 .and. (fds='KODE_BAR' .or. fds='left(NAMA_BRG,32)')
   B   =row()
   Kl  =col()
   Kdb =kode_brg
   Kdbx=kode_bar
   Nmb =nama_brg
   Set curs on
   If fds='KODE_BAR'
      @b,kl get kdbx pict '@!'
      Do pesan with 'Masukan BarCode-nya'
   Else
      @b,kl get nmb pict '@!'
      Do pesan with 'Masukan Nama Barang-nya'
EndIf
   Read
   If lastkey()#27
      Repl kode_bar with kdbx,nama_brg with nmb
      *
      Sele 3
      Set order to 2
      Seek kdb
      Rlok()
      Repl kode_bar with kdbx,nama_brg with nmb
      Ul()
      Commit
      *
      Sele  5
EndIf
   Do pesan with pesanx
   Set curs off
Return 2
Elseif lastkey()=13 .and. fds='STOK_MAX'
   B   =row()
   Kl  =col()
   Stm =stok_max
   Set curs on
   @b,kl get stm pict '@e 99,999'
   Do pesan with 'Masukan Stok Maximal'
   Read
   If lastkey()#27
      Repl stok_max with stm
EndIf
   Do pesan with pesanx
   Set curs off
Return 2
Elseif (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57) .or. lastkey()=32
   Nr=recno()
   Nmb=uppe(chr(lastkey()))+spac(29)
   Do pesan with 'Cari: '+spac(30)
   Set curs on
   @24,28 get nmb pict '@!'
   Keyboard chr(4)
   Read
   Set curs off
   Set colo to /w
   Nmb=rtri(nmb)
   If lastkey()#27
      If left(nmb,1)=' '
         Nmb=alltrim(nmb)
         If flt=' '
            Set filter to AT(nmb,nama_brg)>0
         Else
            Set filter to AT(nmb,nama_brg)>0 .and. &flt
EndIf
         Go top
         @1,50
         Do pesan with pesanx+spac(16)+'Cari: ?'+nmb
      Else
         Set filter to &flt
         Loca for nama_brg=nmb
         If .not. foun()
            Loca for kode_bar=nmb
            If .not. foun()
               Go nr
EndIf
EndIf
         Do pesan with pesanx+spac(16)+'Cari: '+nmb
EndIf
EndIf
Return 2
Elseif lastkey()=-3
   Tgl=tanggal
   Ksp=kode_spl
   Fkt=faktur
   Nfo='PO\'+right(rtri(ksp),3)+fkt+'.DTA'
   Nfx='TEMP\'+right(rtri(ksp),3)+hbl+'.TMP'
   *
   Set colo to gr+*/b
   @row(),col() say &fds
   Yt=' '
   Set curs on
   Set colo to /w
   Do pesan with 'Akan menambah item Barang? Y/T :'
Do While .not. yt$'YT'
      @24,57 get yt pict '!'
      Read
      If lastkey()=27
         Exit
EndIf
EndDo
   If yt='Y' .and. lastkey()#27
      Do pesan with 'Tunggu, sedang proses...'
      Sele 3
      Uselan('STOK.DTA','STOK2.NDX','','','','')
      Set filter to kode_spl=ksp
      Go top
      Copy to &nfx
      Close
      *
      Sele 4
      Useidx('&nfx')
      Go top
Do While .not. eof()
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Stm =stok_max
         Jml =stok_a
         Sao =stok_aord
         Hp  =harga
         Dis1=diskon_1
         Dis2=diskon_2
         Dis3=diskon_3
         *Jl1 =jual0107
         *Jl2 =jual0814
         *Jl3 =jual1521
         *Jl4 =jual2231
         *
         Sele 5
         Loca for kode_brg=kdb
         If .not. foun()
            Appe blan
            Repl tanggal with tgl,faktur with fkt,kode_spl with ksp,kode_klb with klb,;
                 kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
                 stok_max with stm,stok_a with jml,stok_aord with sao,harga with hp
            Repl diskon_1 with dis1,diskon_2 with dis2,diskon_2 with dis3
                 *jual0107 with jl1,jual0814 with jl2,jual1521 with jl3,jual2231 with jl4
EndIf
         *
         Sele 4
         Skip
EndDo
      Close
      *
      Sele 5
      Commit
      Close
      *
      Sele 5
      Useidx('&nfo')
      Sort on nama_brg to &nfx
      Close
      Copy file &nfx to &nfo
EndIf
   Set curs off
Return 0
Elseif lastkey()=-7
   Set colo to gr+*/b
   @row(),col() say &fds
   Yt=' '
   Set curs on
   Set colo to /w
   Do pesan with 'DiHapus? Y/T'
Do While .not. yt$'YT'
      @24,47 get yt pict '!'
      Read
EndDo
   If yt='Y'
      If .not. deleted()
         Rlok()
         Dele
         Ul()
         Commit
         Skip
         *
EndIf
EndIf
   Set curs off
Return 2
Elseif lastkey()=-8
   Tglz=tanggal
   Fkt =faktur
   Set curs on
   Do pesan with 'Pake Kertas Bersambung? Y/T: '
   @24,54 get sambung pict '!'
   Read
   Set filter to jumlah#0
   Count to jdt
   Go top
   Nr=recno()
   Stor 1 to n,no,hal
   Do cp
   If cc=0
Return 0
EndIf
   Do cekhal
   Do koppo
Do While .not. eof()
      Kata1="tran(no,'999. | ')+kode_bar+' | '+nama_brg+'|'+tran(stok_max,'99999 |')+tran(stok_aord,'99999 |')+tran(stok_a,'99999 |')+tran(jumlah,'99999 |')"
      *Kata2="tran(jual0107,'99999 |')+tran(jual0814,'99999 |')+tran(jual1521,'99999 |')+tran(jual2231,'99999 |')+tran(harga,'@e 9,999,999 |')+spac(10)+'|'"
      Kata2="'      |      |      |      |')+tran(harga,'@e 9,999,999 |')+spac(10)+'|'"
      @b,0 say &kata1 + &kata2
      B =b+1
      No=no+1
      N =n+1
      If n>60
         @b,0 say repl(gl,156)
         @b+1,0 say ' '
         If sambung#'Y'
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
         Else
            Eject
EndIf
         N  =1
         Hal=hal+1
         Do koppo
EndIf
      Skip
EndDo
   @b,0 say repl(gl,156)
   @b+2,0 say ' Slawi, '+dtoc(tglx)
   @b+3,0 say ' Mengetahui,'
   @b+7,0 say ' ---------------                         ----------------'
   @b+8,0 say '     Manager                                 Supplier'
   @b+9,0 say ' '
   Set prin off
   Set devi to scre
   Set filter to
   Set curs off
   Do pesan with pesanx
   Go nr
Return 1
Elseif lastkey()=-9
   Tglz=tanggal
   Fkt =faktur
   Set curs on
   Do pesan with 'Pake Kertas Bersambung? Y/T: '
   @24,54 get sambung pict '!'
   Read
   Set filter to jumlah#0
   Coun to jdt
   Go top
   Nr=recno()
   Stor 1 to n,no,hal
   Do cp
   If cc=0
Return 0
EndIf
   Do cekhal
   Do koppo2
Do While .not. eof()
      @b,0 say tran(no,'999. | ')+kode_bar+' | '+left(nama_brg,35)+'| '+tran(jumlah,'99999 ')+satuan+'|'
      B =b+1
      No=no+1
      N =n+1
      If n>60
         @b,0 say repl('-',96)
         @b+1,0 say ' '
         If sambung#'Y'
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
         Else
            Eject
EndIf
         N  =1
         Hal=hal+1
         Do koppo2
EndIf
      Skip
EndDo
   @b,0 say repl('=',96)
   @b+2,0 say ' '+aidi2+', '+dtoc(tglx)
   @b+3,0 say ' Mengetahui,'
   @b+7,0 say ' ---------------                         ----------------'
   @b+8,0 say '     Manager                                 Supplier'
   B=b+9
   @b+1,1 say cpo1
   @b+2,1 say cpo2
   @b+3,1 say cpo3
   @b+4,0 say ' '
   Set prin off
   Set devi to scre
   Set filter to
   Set curs off
   Do pesan with pesanx
   Go nr
Return 1
Else
   Set curs off
Return 1
EndIf
Return 0
Procedure koppo
Set prin on
Set devi to prin
Set marg to 0
?chr(27)+chr(33)+chr(5)
*@prow()+1,1 say aidi1+spac(26)+'FORM ORDER BARANG'
*@prow()+1,1 say repl('-',12)+spac(26)+repl('-',17)
@prow()+1,1 say aidi1+spac(22)+'FORM ORDER BARANG'
@prow()+1,1 say repl('-',16)+spac(22)+repl('-',17)
@prow()+1,1 say 'NPWP: '+npjk+spac(7)+'Supplier: '+rtri(nsp)+'  ('+rtri(ksp)+')'
@prow()+1,1 say spac(38)             +'Tanggal : '+dtoc(tglz)+' / No: '+fkt+spac(72)+'Hal: '+ltri(str(hal))+'/'+ltri(str(xHal))
@prow()+1,0 say repl(gl,156)
@prow()+1,0 say ' NO. | PLU /         | NAMA BARANG                             |  QTY |  QTY | SISA | ORDER'+chr(27)+'-1'+'|    ESTIMASI PENJUALAN     |'+chr(27)+'-0'+'    HG.   |    HG.   | KETERANGAN'
@prow()+1,0 say '     | BARCODE       |                                         |  MAX | AWAL | STOCK|  PCS | 1-7  | 8-14 |15-21 |22-31 |   POKOK  |   JUAL   |'
@prow()+1,0 say repl(gl,156)
B=prow()+1
Return
Procedure koppo2
Set prin on
Set devi to prin
Set marg to 0
?chr(27)+chr(33)+chr(1)
@prow()+1,1 say aidi1+spac(29)+'FORM ORDER BARANG'
@prow()+1,1 say repl('-',9)+spac(29)+repl('-',17)
@prow()+1,1 say 'NPWP: '+npjk+spac(7)+'Supplier: '+rtri(nsp)+'  ('+rtri(ksp)+')'
@prow()+1,1 say spac(38)             +'Tanggal : '+dtoc(tglz)+' / No: '+fkt+spac(12)+'Hal: '+ltri(str(hal))+'/'+ltri(str(xHal))
@prow()+1,0 say repl(gl,96)
@prow()+1,0 say ' NO. | PLU / BarCode | NAMA BARANG                        |  QTY_ORDER | KETERANGAN'
@prow()+1,0 say repl(gl,96)
B=prow()+1
Return
Procedure cekhal
xHal =1
If jdt>60
   xHal=mod(jdt,60)
   If xHal>0
      xHal=((jdt-mod(jdt,60))/60)+1
EndIf
EndIf
Return

* EOF MPO
