Procedure LST2
* DAFTAR STOK
Sele 1
If klp
   Uselan('JENIS.DTA','JENIS.NDX','','','','')
Elseif spl
   Uselan('SUPP.DTA','SUPP.NDX','','','','')
EndIf
Sele 2
*
Sele 3
Uselan('STOK.DTA','STOK.NDX','STOK2.NDX','STOK3.NDX','','')
Ikdb=' '
Lks ='T'
Stor spac(5) to ksp,klb
Set cent off
Do While .t.
   @1,0 clea to 24,79
   Ket="Semua"
   Stk='((STOK_A+STOK_M) - (STOK_K+STOK_RB))'
   Lks='T'
   *
   Sele 1
   If klp
      Set key 28 to dafklb
      @2,29 say 'Jenis Barang:'get klb pict '@!'
      Do pesan with 'F1=Kelompok Brg'
      Read
      Set key 28 to
      If lastkey()=27 .or. klb=spac(5)
         Exit
EndIf
   Elseif spl
      Set key 28 to dafspl
      @2,29 say 'Kode Suplier:'get ksp pict '@!'
      Do pesan with 'F1=Daftar Supplier'
      Read
      Set key 28 to
      If lastkey()=27 .or. ksp=spac(5)
         Exit
EndIf
EndIf
   Nsp='-'
   Nsp='-'
   Katax='Kode Brg     '+tl+'Nama Barang'+spac(19)+tl+'Jml.Barang '+tl+'     @Harga'+tl+' Jumlah Rp.'
   Stor 0 to no,tot,tot2,jml2,jml3
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Sele 1
   If klp
      If klb#spac(5)
         Seek klb
         If .not. foun()
            Do pesan2 with 'Tidak ada kode '+rtri(klb)
            Loop
EndIf
         Nsp=nama_klb
         Ket='JENIS: '+rtri(nsp)+' ('+rtri(klb)+')'
EndIf
      If lastkey()=27
         Loop
EndIf
      Syarat="kode_klb=klb"
      Sele 3
      Set order to 3
      Seek klb
      Nr=recno()
   Elseif spl
      If ksp#spac(5)
         Set order to 1
         Seek ksp
         If .not. foun()
            Set order to 2
            Seek rtri(ksp)
            Do dafspl
EndIf
         Nsp=nama_spl
         Ket='SUPPLIER: '+rtri(nsp)+' ('+rtri(ksp)+')'
EndIf
      If lastkey()=27
         Loop
EndIf
      Syarat="kode_spl=ksp"
      Sele 3
      Set order to 2
      Seek ksp
      Nr=recno()
EndIf
   Sele 3
   Stkx=((STOK_A+STOK_M) - (STOK_K+STOK_RB))
   *
   Sum &stk*harga to tot2
   Go nr
Do While .not. eof() .and. &syarat
      Tot=tot+(&stk*harga)
      Skip
EndDo
   Stor '' to kdb,kdb2,nmb,isi,st,stb,hp,hp2,hpb,hj,hj21,hj2,hj3,hj4,hj5,ppn,pjk,;
              bts3,bts4,bts5,jml,jml1,jml2,jml3,hjx,hjy,rpx,rpy,tgl0,tgl1,tgl2,tgl3,tglrb
   Stor 0  to jml,jml1,jml2,jml3,jmlrb
   Stor .f. to keluar,adadis,hapus,xUdahada,hapusi,beli
   @1,0 clea to 24,79
   @1,1 say ket
   @2,0 say repl(gl,13)+chr(194)+repl(gl,30)+chr(194)+repl(gl,11)+chr(194)+repl(gl,11)+chr(194)+repl(gl,11)
   @3,0 say katax
   @4,0 say repl(gl,13)+chr(197)+repl(gl,30)+chr(197)+repl(gl,11)+chr(197)+repl(gl,11)+chr(197)+repl(gl,11)
   @21,0 say repl(gl,13)+chr(193)+repl(gl,30)+chr(193)+repl(gl,11)+chr(193)+repl(gl,11)+chr(193)+repl(gl,11)
   @22,1  say 'Total Semua Brng=Rp.'+tran(tot2,'@e 9999,999,999')
   @22,57 say 'Jumlah =Rp.'+tran(tot,'@e 9999,999,999')
   *
   Set filter to &syarat
   Go nr
   Pesanx='F10=Cetak'
   Do pesan with pesanx
   Declare field[7],kop[7],bts[7],frm[7]
   Field[1]='left(KODE_BAR,13)'
   Field[2]='left(NAMA_BRG,30)'
   Field[3]='&STK'
   Field[4]='left(SATUAN,3)'
   Field[5]='HARGA'
   Field[6]='&STK*HARGA'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 999,999'
   Frm[4]='@!'
   Frm[5]='@e 999,999,999'
   Frm[6]='@e 999,999,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=' '
   Bts[5]=''+tl
   Bts[6]=''+tl
   Dbedit(5,0,20,79,field,'flst12',frm,'','',bts)
   *
   If lastkey()#-9
      Loop
EndIf
   Kdb=kode_brg
   Sambung='Y'
   Stor 1 to no,hal,b,n
   Stor 0 to tot,tot2
   Do mulaictk
   If lastkey()=27
Return
EndIf
   Yt=' '
   Do pesan with 'Stok Nol ikut diCetak? Y/T : '
   @24,54 get yt pict '!'
   Read
   If lastkey()=27
      Loop
EndIf
   Set filter to ((STOK_A+STOK_M) - (STOK_K+STOK_RB))#0 .and. &syarat
   If yt='Y'
      Set filter to ((STOK_A+STOK_M) - (STOK_K+STOK_RB))#9999999 .and. &syarat
EndIf
   Go top
   Do cp
   If cc=0
      Exit
EndIf
   Do koplst12
Do While .not. eof() .and. &syarat
      Stkx=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
      Jml=tran(stkx,'@e 99,999 ')
      If stkx#0
         If (stkx>0 .and. stkx<1)
            Jml=tran(stkx,'@e 999.99 ')
         Elseif mod(stkx,int(stkx))>0 
            Jml=tran(stkx,'@e 999.99 ')
EndIf
EndIf
      Tot =tot +(stkx*harga)
      Tot2=tot2+(stkx*harga)
      ?tran(no,'|9999 | ')+kode_bar+' | '+left(nama_brg,30)+'| '+jml+' | '+satuan+'| '+;
       tran(harga,'@e 9,999,999.99 |')+tran(stkx*harga,'@e 999,999,999 |')
      No=no+1
      N =n+1
      If n>50
         ?'|'+repl('-',98)+'|'
         ?'|             Jumlah Hal Ini'+spac(58)+tran(tot,'@e 9999,999,999 |')
         ?'|'+repl('-',98)+'|'
         ?'|             SUB TOTAL  Rp.'+spac(58)+tran(tot2,'@e 9999,999,999 |')
         ?'*'+repl('=',98)+'*'
         ?''
         If sambung='T'
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
         Tot=0
         Hal=hal+1
         Do koplst12
EndIf
      Skip
EndDo
   If cc=0
      Loop
EndIf
   ?'|'+repl('-',98)+'|'
   ?'|             Jumlah Hal Ini'+spac(58)+tran(tot,'@e 9999,999,999 |')
   ?'|'+repl('-',98)+'|'
   ?'|             T O T A L  Rp.'+spac(58)+tran(tot2,'@e 9999,999,999 |')
   ?'*'+repl('=',98)+'*'
   ?''
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele kdb,nmb,isi,st,ksp,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,bts3,bts4,bts5,jml,;
     jml1,jml2,jml3,nr,hjx,hjy,rpx,rpy,tgl2,tgl3,keluar,adadis,hapus,xUdahada,hapusi
Set curs off
Set cent on
Return

Return

Function flst12
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=13 .and. spl
      @24,0
      Nr=recno()
      Kdb=kode_brg
      Klb=kode_klb
      Kdp=kode_dep
      Sele 2
      Uselan('JENIS.DTA','JENIS.NDX','','','','')
      *
      Sele 3
      Set order to 1
      Set curs on
      Do lkdb
      Set curs off
      If klp
         Set order to 3
      Elseif spl
         Set order to 2
EndIf
      Do pesan with pesanx
Return 2
   Case lastkey()=-9
Return 0
   Othe
Return 1
Endc
Return 0
Procedure koplst12
Set prin on
Set devi to prin
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR STOK'
?' '+ket
?' TANGGAL: '+dtoc(tglx)+spac(66)+'Hal:'+tran(hal,'999')
?'*'+repl('=',98)+'*'
?'| No. | Barcode       | Nama Barang                   |    Sisa | Sat. |      @Pokok |   Jumlah Rp.|'
?'|'+repl('-',98)+'|'
N=1
Return

* BARANG/KLP-TOTAL
*----------------------------
Procedure lst13
Sele 1
Uselan('STOK.DTA','STOK3.NDX','','','','')
Sele 2
Uselan('JENIS.DTA','JENIS.NDX','','','','')

Count to jml
Stor ' ' to lks,ket
Stor tglx to tgl,tgl2
Bln=subs(dtoc(tgl),4,2)
Thn=right(dtoc(tgl),2)
Do While .t.
   Ada=.f.
   @1,0 clea to 24,79
   Set colo to bg+/b
   @2,27 say '  -  '
   Set colo to /bg
   @2,18 say 'Bln-Thn:'
   @3,18 say 'Lokasi.: T=Toko    G=Gudang   S=Semua --->'
   @2,27 get bln
   @2,30 get thn
   @3,60 get lks pict '!'
   Do pesan with 'Bln-Thn = diisi Bulan dan Tahun'
   Read
   @24,0
   If lastkey()=27
      Exit
EndIf
   Sele 1
   Nf1='STO_'+bln+thn+'.DTA'
   If file('&nf1')
      Uselan('&nf1','','','','','')
      Ada=.t.
   Elseif .not. file('&nf1')
      Do pesan with 'File mungkin belum diPosting.    Tekan ENTER=Hitung Stok Bulan Sekarang'
      Wait''
      If lastkey()=27
         Loop
EndIf
      Uselan('STOK.DTA','STOK3.NDX','','','','')
EndIf
   Ket='TOKO & GUDANG'
   If lks='T'
      Ket='TOKO         '
   Elseif lks='G'
      Ket='GUDANG       '
EndIf
   *
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-STOK.RTF
   ?' LAPORAN PERSEDIAAN BARANG'
   ?' BLN/THN.: '+bln+'-'+thn
   ?' LOKASI  : '+ket+spac(18)+'TGL.CETAK: '+dtoc(tglx)
   ?'/'+repl('=',91)+'\'
   ?'| No.| Kelompok Barang'+spac(20)+'|   Item /     Stok |    Jumlah Rp.|    Margin Rp |'
   ?'|'+repl('-',91)+'|'
   Y=1
   Stor 0 to tothp,tothj,jml1,jml2,jmla,jmlb
   Sele 2
   Go top
Do While .not. eof()
      Klb =kode_klb
      Nklb=nama_klb
      Z   =ltri(str(y))
      Stor 0 to jml1,jml2,hj,hp
      *
      Sele 1
      If ada
         Loca for kode_klb=klb
      Else
         Seek klb
EndIf
      If foun()
Do While .not. eof() .and. kode_klb=klb
            Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
            *
            Jmla =jmla+1
            Jml1 =jml1+1
            Jml2 =jml2+&stk
            Jmlb =jmlb+&stk
            Hp   =hp + ( &stk * harga )
            Hj   =hj + ( &stk * harga_2 )
            Tothp=tothp + ( &stk * harga)
            Tothj=tothj + ( &stk * harga_2 )
            Skip
EndDo
EndIf
      If jml#0
         ?tran(y,'|999 | ')+nklb+' '+left(klb,4)+'|'+tran(jml1,'@e 999,999 /')+tran(jml2,'@e 9,999,999 |')+tran(hp,'@e 9,999,999,999 |')+tran(hj-hp,'@e 9,999,999,999 |')
         Y = y + 1
EndIf
      Sele 2
      Skip
EndDo
   ?'|'+repl('-',91)+'|'
   ?'|'+spac(34)+'Total = '+tran(jmla,'@e 999,999 /')+tran(jmlb,'@e 9,999,999 |')+tran(tothp, '@e 9,999,999,999 |')+tran(tothj-tothp, '@e 9,999,999,999 |')
   ?'\'+repl('=',91)+'/'
   ?' '
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('L-STOK.RTF'),1,0,22,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
EndIf
   Pil='1'
   Do pesan with '1=Printer   2=File : '
   @24,50 get pil pict '!'
   Read
   If pil='2'
      No =1
      Nf='C:\DAFSTOK2.TXT'
      Do pesan with 'Tunggu, sedang proses '+nf
      Set order to 4
      Go top
      Set alte on
      Set alte to &nf
      ?'   ;DAFTAR PERSEDIAAN BARANG; ; ;'
      ?'   ;BLN/THN.: '+bln+'-'+thn
      ?'   ;LOKASI  : '+ket+'; ; ;TGL.CETAK: '+dtoc(tglx)
      ?'NO.;NAMA KELOMPOK;KODE;ITEM;JUMLAH RP'
      Y=1
      Stor 0 to tot,totx,tot2,jml,jml1,jml2
      Sele 2
      Go top
Do While .not. eof()
         Klb =kode_klb
         Nklb=nama_klb
         Z   =ltri(str(y))
         Jml1=0
         *
         Sele 1
         If ada
            Loca for kode_klb=klb
         Else
            Seek klb
EndIf
         If foun()
Do While .not. eof() .and. kode_klb=klb
               Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
               *
               Jml1=jml1+1
               Jml2=jml2+1
               Jml =jml + ( &stk * harga)
               Tot =tot + ( &stk * harga)
               Skip
EndDo
EndIf
         If jml#0
            ?ltri(str(y))+';'+rtri(nklb)+';'+rtri(klb)+';'+ltri(str(jml1))+';'+ltri(str(jml))
            Y = y + 1
EndIf
         Stor 0 to tot2,jml,jml1
         Sele 2
         Skip
EndDo
      ?''
      Set alte off
      Set alte to
      Do pesan with 'Export selesai..., nama file: '+subs(nf,4,12)+' , di drive C:\'
      Wait''
      Loop
   Else
      Do cp
      If cc=0
         Loop
EndIf
      Set prin on
      Set devi to prin
      Set marg to 5
      ?chr(27)+chr(33)+chr(1)
      Type L-STOK.RTF to prin
      Set prin off
      Set devi to scre
EndIf
EndDo
Clos all
Rele klb,lks,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

* STOK MINIMAL
*--------------
Procedure lst14
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
*
Sele 3
Uselan('STOK.DTA','STOK2.NDX','STOK3.NDX','','','')
Ksp=spac(5)
Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
Do While .t.
   Sele 1
   Set key 28 to dafspl
   @1,0 clea to 24,79
   @2,30 say 'Supl:'get ksp pict '@!'
   Do pesan with 'F1=Daftar Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
EndIf
   If ksp#spac(5)
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada Supplier '+rtri(ksp)
         Loop
EndIf
      Nklb=nama_spl

      Sele 3
      Set order to 1
      Set filter to kode_spl=ksp .and. &stk <= stok_min .and. stok_min#0
      Loca for &stk <= stok_min .and. stok_min#0
      If .not. foun()
         Do pesan2 with 'Tidak Ada Data'
         Loop
EndIf
      Kata="'Supl: '+rtri(nklb)+'  ('+rtri(ksp)+')'"
   Else
      Sele 3
      Set order to 2
      Set filter to &stk <= stok_min .and. stok_min#0
      Loca for &stk <= stok_min .and. stok_min#0
      If .not. foun()
         Do pesan2 with 'Tidak Ada Data'
         Loop
EndIf
      Kata="'Semua Barang'"
EndIf

   Nr=recno()
   Stor '' to kdb,nmb,isi,st,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,;
              bts3,bts4,bts5,jml,jml1,jml2,jml3,hjx,hjy,rpx,rpy,tgl2,tgl3
   Stor .f. to keluar,adadis,hapus,xUdahada,hapusi
   @1,0 clea to 24,79
   @1,1 say  &kata
   @2,0 say  repl(gl,8)+chr(194)+repl(gl,30)+chr(194)+repl(gl,7)+chr(194)+repl(gl,14)+chr(194)+repl(gl,10)+chr(194)+repl(gl,6)
   @3,0 say  'Kode Brg'+tl+'Nama Barang'+spac(19)+tl+'Minimal'+tl+'  Sisa Stok   '+tl+'    @Harga'+tl+' Supl.'
   @4,0 say  repl(gl,8)+chr(197)+repl(gl,30)+chr(197)+repl(gl,7)+chr(197)+repl(gl,14)+chr(197)+repl(gl,10)+chr(197)+repl(gl,6)
   @22,0 say repl(gl,8)+chr(193)+repl(gl,30)+chr(193)+repl(gl,7)+chr(193)+repl(gl,14)+chr(193)+repl(gl,10)+chr(193)+repl(gl,6)
   Pesanx='F9=Order   F10=Cetak'
   Do pesan with pesanx
   Declare field[7],kop[7],bts[7],frm[7]
   Field[1]='KODE_BRG'
   Field[2]='left(NAMA_BRG,30)'
   Field[3]='STOK_MIN'
   Field[4]='&STK'
   Field[5]='SATUAN'
   Field[6]='HARGA'
   Field[7]='KODE_SPL'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 9999.99'
   Frm[4]='@e 99999.99'
   Frm[5]='@!'
   Frm[6]='@e 99,999,999'
   Frm[7]='@!'
   Bts[1]=''+tl
   Bts[2]=' '+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=' '
   Bts[6]=''+tl
   Bts[7]=tl+' '
   Dbedit(5,0,21,79,field,'flst14',frm,'','',bts)
   *
EndDo
Clos all
Rele kdb,nmb,isi,st,ksp,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,bts3,bts4,bts5,jml,;
     jml1,jml2,jml3,nr,hjx,hjy,rpx,rpy,tgl2,tgl3,keluar,adadis,hapus,xUdahada,hapusi,stk
Set curs off
Return

Function flst14
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-8
      Nr=recno()
      Do morder

      Sele 3
      Set curs off
      Do pesan with pesanx
Return 1
   Case lastkey()=-9
      Do cp
      If cc=0
         Do pesan with pesanx
Return 1
EndIf
      Stor 1 to no,hal,b,n
      Stor 0 to tot,tot2
      Do koplst14
      Go nr
Do While .not. eof()
         Stkx=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
         If &stk <= stok_min .and. stok_min#0
            ?tran(no,'|9999 | ')+kode_brg+' | '+nama_brg+'| '+tran(stok_min,'@e 9999.99 |')+;
             tran(stkx,'@e 999999.99 ')+satuan+' | '+tran(harga,'@e 9,999,999.99 |')
            No=no+1
            N =n+1
            If n>55
               ?'|'+repl('-',89)+'|'
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
               Hal=hal+1
               Do koplst14
EndIf
EndIf
         Skip
EndDo
      If cc=0
         Do pesan with pesanx
Return 1
EndIf
      ?'*'+repl('=',89)+'*'
      ?''
      Eject
      Set prin off
      Set devi to scre
      Do pesan with pesanx
Return 1
   Othe
Return 1
Endc
Return 0
Procedure koplst14
Set prin on
Set devi to prin
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR STOK MINIMAL'
?' TANGGAL: '+dtoc(tglx)+spac(63)+'Hal:'+tran(hal,'999')
?'*'+repl('=',89)+'*'
?'| No. | Kode Brg| Nama Barang                   | Minimal |   Sisa Stok    |       @Harga |'
?'|'+repl('-',89)+'|'
N=1
Return
Procedure morder
Warna=setcolor()
Nsp=' '
Tgl=tglx
Klb=kode_klb
Kdb=kode_brg
Nmb=nama_brg
St =satuan
Hp =harga_bl/isine_b
B  =row()
If b>=21
   B=17
EndIf
Jml=0
Ada=.f.
Do konvtgl
Set colo to gr+*
@row(),col() say &fds
Save scre to smorder

Sele 1
Do pesan with 'F1=Daftar Supplier'
Set key 28 to dafspl
Set curs on
Set colo to w+/b,gr+/b
@b+1,8 clea to b+3,73
@b+1,8 to b+3,73
@b+1,10 say 'Order ke:'get ksp pict '@!'
@b+2,10 say rtri(nmb)+' ('+rtri(kdb)+')'
Read
Set key 28 to
Set colo to w+/b
Seek ksp
If foun()
   Nsp=nama_spl
   @b+1,20 say rtri(nsp)+' ('+rtri(ksp)+')'
EndIf
Sele 4
Uselan('ORDERIN.DTA','ORDERIN1.NDX','','','','')
Seek dtos(tgl)+ksp+kdb
If foun()
   Jml=jumlah
   Ada=.t.
   Set colo to w+*/b
   @b+3,33 say 'Data Sudah Ada'
   Set colo to w+/b
EndIf
@b+2,54 say 'Jumlah:'get jml pict '@e 99999'
@b+2,68 say st
Read

Sele 4
If ada
   Rlok()
   Repl jumlah with jml,harga with hp
Elseif (.not. ada) .and. jml#0
   Flok()
   Appe blan
   Repl tanggal with tgl,tanggal_2 with dtos(tanggal),faktur with '0'+(hri+bln),;
        kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,;
        nama_brg with nmb,satuan with st,jumlah with jml,harga with hp
EndIf
Ul()
Commit
Close
Set colo to (warna)
Rest scre from smorder
Return

* DAFTAR STOK
*------------
Procedure lstlalu
Sele 1
Uselan('JENIS.DTA','JENIS.NDX','','','','')
Sele 2
*
Sele 3
*
Set cent on
Tgl=tglx
Stor spac(5) to ksp,klb,kdb
Bln=subs(dtoc(tgl),4,2)
Thn=right(dtoc(tgl),2)
Hri=bln
Hr =thn
Lks='T'
Do While .t.
   @1,0 clea to 24,79
   Sele 1
   Set key 28 to dafklb
   Set colo to bg+/b
   @3,43 say '  -  '
   Set colo to /bg
   @2,33 say 'Klp Brg.:'get klb pict '@!'
   @3,33 say 'Bln-Thn.:'
   @4,33 say 'Lokasi..:    [ T/G/S ]'
   @3,43 get bln
   @3,46 get thn
   @4,43 get lks pict '!'
   Do pesan with 'ESC=Selesai   F1=Kelompok Brg'
   Read
   Set key 28 to
   If lastkey()=27 .or. klb=spac(5)
      Exit
EndIf
   Sele 2
   Nf1='STO_'+bln+thn+'.DTA'
   If file('&nf1')
      Uselan('&nf1','','','','','')
   Elseif .not. file('&nf1')
      Do pesan2 with 'Tidak Ada File Stok Barang '+bln+'-'+thn+'   -atau-   Belum diPosting?'
      Loop
EndIf
   Do pesan with '+Hitung Seluruh Barang? Y/T'
   Yt=' '
   @24,54 get yt pict '!'
   Read
   Nklb='-'
   Sambung='T'
   Stor 0 to no,tot,tot2,jml2,xhal
   If klb#spac(5)
      Sele 1
      Seek klb
      If .not. foun()
         Do pesan2 with 'Tidak ada kode '+rtri(klb)
         Loop
EndIf
      Nklb=nama_klb
      Ket =' '
      Syarat='kode_klb=klb'
EndIf
   Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
   Ket2=rtri(nklb)+' ('+rtri(klb)+')   BLN: '+bln+'-'+thn+' / TOKO'
   Do pesan with 'Tunggu, sedang dihitung'

   Sele 2
   If yt='Y'
      Sele 2
      Sum harga*&stk to jml2
EndIf
   Loca for kode_klb=klb
   Nr=recno()
Do While .not. eof() .and. &syarat
      Tot=tot+(harga*&stk)
      Skip
EndDo
   Go nr
   @1,0 clea to 24,79
   @2,0 to 21,79
   @1,1 say ket2
   @2,0 say chr(218)+repl(gl,7)+chr(194)+repl(gl,30)+chr(194)+repl(gl,15)+chr(194)+repl(gl,10)+chr(194)+repl(gl,12)+chr(191)
   @3,0 say tl+'Kode   '+tl+'Nama Barang'+spac(19)+tl+'  Jml.Barang   '+tl+'    @Harga'+tl+'  Jumlah Rp.'+tl
   @4,0 say chr(195)+repl(gl,7)+chr(197)+repl(gl,30)+chr(197)+repl(gl,15)+chr(197)+repl(gl,10)+chr(197)+repl(gl,12)+chr(180)
   If yt='Y'
      @22,1  say 'Total Semua Brng=Rp.'+tran(jml2,'@e 9999,999,999')
EndIf
   @22,56 say 'Jumlah =Rp.'+tran(tot,'@e 9999,999,999')
   Katax='F10=Cetak'
   Do pesan with katax
   Declare field[14],kop[14],bts[14],frm[14]
   Field[1]='left(KODE_BRG,8)'
   Field[2]='NAMA_BRG'
   Field[3]='&STK'
   Field[4]='SATUAN'
   Field[5]='HARGA'
   Field[6]='HARGA*&STK'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 999999.99'
   Frm[4]='@!'
   Frm[5]='@e 99,999,999'
   Frm[6]='@e 9999,999,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=' '
   Bts[5]=''+tl
   Bts[6]=''+tl
   Dbedit(5,1,20,78,field,'flstl',frm,'','',bts)
   *
EndDo
Clos all
Rele klb,nklb,sambung,no,tot,tot2,jml2,xhal,ket2
Set curs off
Return

Function flstl
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-4
      Set curs on
      If fds='&STK' .and. lks#'S'
         Kdb =kode_bar
         Jml4=stok_rb
         Jml1=stok_a
         Jml2=stok_m
         Jml3=stok_k
         B=row()
         Klm=col()+13
         Save scre to smkdb
         Set curs on
         Set colo to w
         @b,klm-1 clea to b+5,klm+17
         @b,klm-1 to b+5,klm+17
         @b,klm-4 say '>>>'
         @b+1,klm say 'Awal..:'get jml1 pict '@e 999999.99'
         @b+2,klm say 'Masuk.:'get jml2 pict '@e 999999.99'
         @b+3,klm say 'Keluar:'get jml3 pict '@e 999999.99'
         @b+4,klm say 'ReturB:'get jml4 pict '@e 999999.99'
         Tone(1500,3)
         Tone(2000,6)
         Do pesan with 'Perhatian !  Meng-Edit Stok Bln Lalu,  Stok yg Sekarang HARUS di-Cek lagi !'
         Read
         Rest scre from smkdb
         If lastkey()#27
            Rlok()
            Repl stok_a with jml1,stok_m with jml2,stok_k with jml3,stok_rb with jml4
            Jml2=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
            Ul()
            Commit
            *
            Sele 3
            Uselan('STOK.DTA','STOK5.NDX','','','','')
            Seek kdb
            Rlok()
            If lks='T'
               Repl stok_a with jml2
            Else
               Repl stok_ag with jml2
EndIf
            Ul()
            Commit
            Close
            *
            Sele 2
EndIf
      Elseif fds='HARGA'
         Rlok()
         @row(),col() get &fds pict '@e 99,999,999'
         Read
         Ul()
         Commit
EndIf
      Set curs off
      Do pesan with katax
Return 1
   Case lastkey()=-9
      Nr=recno()
      Set curs on
      Kdb=kode_brg
      Do ctkstok
      Go nr
      Do pesan with katax
      Set curs off
Return 1
   Othe
Return 1
Endc
Return 0

* EOF LST2
