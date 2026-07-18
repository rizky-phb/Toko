* TRANSAKSI KAS
*--------------
Proc inkas
Do bacaflkas
*
Sele 10
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
If nsls=aidi1+spac(10)
   Set filter to id_lr='Y'
   Loca for id_lr='Y'
Endi
Sele 11
Uselan('&nf0','&nf1','&nf2','','','')
*
Tgl2=tgl
Stor 0 to jml,tot
Adadis=.f.
Do whil .t.
   Set colo to w+/r
   @0,0 say spac(14)+'TRANSAKSI KAS HARIAN, TGL.: '+dtoc(tgl)+', KASIR: '+nsls
   Set colo to bg+/b
   @1,0 clea to 23,79
   @1,0 say chr(218)+repl(gl,10)+chr(194)+repl(gl,5)+chr(194)+repl(gl,24)+chr(194)+repl(gl,25)+chr(194)+repl(gl,10)+chr(191)
   @2,0 say tl+'Tanggal   '+tl+'Kode '+tl+'Nama Perkiraan'+spac(10)+tl+'Keterangan'+spac(15)+tl+'Jumlah Rp.'+tl
   @3,0 say chr(195)+repl(gl,10)+chr(197)+repl(gl,5)+chr(197)+repl(gl,24)+chr(197)+repl(gl,25)+chr(197)+repl(gl,10)+chr(180)
   Do kolomtr
   B =4
   Disi=.f.
   xBln=mont(tgl)
   Stor ' ' to yt,irl
   Do whil .t.
      Stor spac(5) to kpos
      Ket  =spac(29)
      Sele 10
      Set key 5 to xadadis
      @b,1 get tgl
      @b,12 get kpos pict '@!'
      Do pesankas with chr(24)+'=Edit/Hapus   ESC=Selesai'
      Read
      Set key 5 to
      If lastkey()=27
         Exit
      Endi
      If xBln#mont(tgl)
         xBln=mont(tgl)
         Do bacaflkas
         Sele 11
         Uselan('&nf0','&nf1','&nf2','','','')
      Endi
      If adadis &&.and. xOwner
         Adadis=.f.
         Do hpstr
         Loop
      Endi
      @24,0
      Sele 10
      Seek kpos
      If .not. foun() .or. kpos=spac(5)
         Do dafperk
         If lastkey()=27
            Loop
         Endi
      Endi
      Nspos=nama_pos
      Ipos =id_pos
      Irl  =id_lr
      *
      @b,12 say kpos
      @b,18 say subs(nspos,1,23)
      @b,41 say ipos
      @b,43 get ket pict '@S25'
      @b,69 get jml pict '@e   99999999'
      Read
      If lastkey()=27
         Loop
      Endi
      Yt='Y'
      Do pesankas with 'Data Benar? Y/T : '
      @24,49 get yt pict '!'
      Read
      If yt#'Y' .or. lastkey()=27
         Loop
      Endi
      Set colo to bg+/b
      @b,1 say dtoc(tgl)+tl+kpos+tl+subs(nspos,1,22)+' '+ipos+tl+left(ket,25)+tl+tran(jml,'@e 99,999,999')
      *
      Sele 11
      Flok()
      Appe blan
      Repl tanggal with tgl,nama_ksr with nsls,kode_pos with kpos,;
           nama_pos with nspos,keterangan with ket,rp_total with jml,id_pos with ipos,id_lr with irl
      Ul()
      Commit
      *
      B=b+1
      Jml =0
      Kpos=spac(5)
      Ket =spac(29)
      If b>22
         Scroll(4,0,22,79,1)
         B=22
      Endi
   Endd
   Exit
Endd
Set curs off
Tgl=tgl2
Rele kpos,npos,ket,ipos,tambah
*
Sele 10
Close
*
Sele 11
Close
Retu

***KOLOM TRANSAKSI
Proc kolomtr
B=4
Do whil b<=22
   @b,0 say tl+spac(10)+tl+spac(5)+tl+spac(24)+tl+spac(25)+tl+spac(10)+tl
   B=b+1
Endd
@b,0 say chr(192)+repl(gl,10)+chr(193)+repl(gl,5)+chr(193)+repl(gl,24)+chr(193)+repl(gl,25)+chr(193)+repl(gl,10)+chr(217)
B=4
Retu

*** PROSEDUR HAPUS TRANSAKSI
Proc hpstr
Set colo to bg+/b
Sele 11
Go bott &&Seek dtos(tgl)
Nr=recno()
Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
Declare field[8],bts[8],frm[8]
Field[1]='TANGGAL'
Field[2]='KODE_POS'
Field[3]='left(NAMA_POS,22)'
Field[4]='ID_POS'
Field[5]='left(KETERANGAN,25)'
Field[6]='RP_TOTAL'
Frm[1]='99-99-9999'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]=''
Frm[6]='@e 99,999,999'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=' '
Bts[5]=tl
Bts[6]=tl
Dbedit(4,1,20,78,field,'fhptr',frm,'','',bts)
*
Do kolomtr
Tot=0
Go nr
Do whil .not. eof()
   @b,1 say dtoc(tanggal)+tl+kode_pos+tl+subs(nama_pos,1,22)+' '+id_pos+tl+left(keterangan,25)+tl+tran(rp_total,'@e 99,999,999')
   B  =b+1
   If b>20
      B=4
      Do kolomtr
   Endi
   Skip
Endd
Retu

Function fhptr
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=13
   B   =row()
   Ket =keterangan
   Jml =rp_total
   Set curs on
   @b,43 get ket pict '@S25'
   @b,69 get jml pict '@e 99,999,999'
   Read
   If lastkey()#27
      Rlok()
      Repl keterangan with ket,rp_total with jml
      Ul()
      Commit
   Endi
   Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
   Set curs off
   Set curs off
   Retu 1
Elseif lastkey()=-7
   Set colo to gr+/b
   @row(),col() say &fds
   Set colo to /bg
   Do pesankas with ''
   Set curs on
   Do pesankas with 'Dihapus? Y/T'
   If ulangi(.t.)
      Rlok()
      Dele
      Ul()
      Commit
      Skip
   Endi
   Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
   Set curs off
   Retu 2
Else
   Set curs off
   Retu 1
Endi
Retu 0

* LAPORAN TRANSAKSI KAS1
*------------------------
Proc lkas
Do whil .t.
   @1,0 clea to 24,79
   Do pesan with 'ESC=Menu Selanjutnya'
   Set colo to /bg
   If hut
      Nsls=spac(20)
      Adadis=.f.
      *
      Sele 6
      Uselan('KASIR.DTA','','','','','')
      Set key 28 to dafksr
      Set colo to /bg
      @2,24 say 'Nama Kasir:'get nsls pict '@!'
      @3,24 say 'Tanggal...:'get tgl
      Do pesan with 'ESC=Selesai   F1=Daftar Kasir'
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      Endi
      If nsls#spac(20)
         Loca for nama=nsls
         If .not. foun()
            Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
            Wait''
            Loop
         Endi
         Nsls=nama
         Ksr =nomer
      Endi
      Do pesan with 'ESC=Menu Selanjutnya'
   Endi
   Do bacafile
   Stor 0 to hj,hj2,dis1,rtr,jml1,jml2,totd,totk,totb
   Sele 12
   If .not. file('&nf0')
      Do pesan2 with &takada
      Sele 10
      Close
      Sele 11
      Close
      Exit
   Endi
   Uselan('&nf0','&nf3','','','','')
   Seek dtos(tgl)+ksr
   Do whil .not. eof() .and. ( tanggal=tgl .and. nama_ksr=nsls )
      If left(nama_brg,6)='Diskon'
         Dis1=dis1+abs(harga_2)
      Else
         Hj  =hj  +&brs1
      Endi
      Skip
   Endd
   Close
   *
   Do bacaflkas
   *
   Sele 10
   Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
   Sele 11
   Uselan('&nf0','&nf2','','','','')
   Set filter to nama_ksr=nsls .and. id_pos='D'
   *
   Set alte on
   Set alte to ARUS-KAS.RTF
   ?'    LAPORAN TRANSAKSI KAS HARIAN, TGL.: '+dtoc(tgl)+',  KSR: '+nsls
   ?'   '+repl(gl,74)
   ?'    PEMASUKAN :'
   ?'    - Total Penjualan Barang      '+spac(13)+tran(hj,'@e 999,999,999')
   Totd=hj
   Y=1
   *
   Sele 10
   Set filter to id_pos='D'
   Loca for id_pos='D'
   Do whil .not. eof()
      Kpos =kode_pos
      Nspos=nama_pos
      Ipos =id_pos
      *
      Sele 11
      Seek kpos+dtos(tgl)
      If .not. foun()
         Seek kpos
      Endi
      Do whil .not. eof() .and. kode_pos=kpos
         If tanggal=tgl
            ?'    - '+left(nama_pos,30)+spac(11)+tran(rp_total,'@e 999,999,999')
            Totd=totd+rp_total
         Endi
         Skip
      Endd
      Y=y+1
      Sele 10
      Skip
   Endd
   ?spac(44)+repl(gl,14)
   ?'      Jumlah Pemasukan'   +spac(37)+'Rp '+tran(totd,'@e 999,999,999')
   ?' '
   ?'    PENGELUARAN :'
   Sele 11
   Set filter to nama_ksr=nsls .and. id_pos='K'
   *
   Sele 10
   Set filter to id_pos='K'
   Loca for id_pos='K'
   Do whil .not. eof()
      Kpos =kode_pos
      Nspos=nama_pos
      Ipos =id_pos
      *
      Sele 11
      Seek kpos+dtos(tgl)
      If .not. foun()
         Seek kpos
      Endi
      Do whil .not. eof() .and. kode_pos=kpos
         If tanggal=tgl
            ?'    - '+left(nama_pos,20)+' '+left(keterangan,20)+tran(rp_total,'@e 999,999,999')
            Totk=totk+rp_total
         Endi
         Skip
      Endd
      Y=y+1
      Sele 10
      Skip
   Endd
   ?spac(44)+repl(gl,14)
   ?'      Jumlah Pengeluaran'+spac(35)+'Rp '+tran(totk,'@e 999,999,999')
   ?spac(59)+repl(gl,14)
   ?'      Sisa Uang Kas     '+spac(35)+'Rp '+tran(totd-totk,'@e 999,999,999')
   ?spac(59)+repl(gl,14)
   ?' '
   Set alte off
   Set alte to
   Memoedit(Memoread('ARUS-KAS.RTF'),1,0,22,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If lastkey()=27
      If hut
         Loop
      Endi
      Exit
   Endi
   Do cp
   If lastkey()=27
      Loop
   Endi
   Set prin on
   Set devi to prin
   Set marg to 5
   ?chr(27)+chr(33)+chr(1)
   Type ARUS-KAS.RTF to prin
   Eject
   Set prin off
   Set devi to scre
   If hut
      Loop
   Endi
   Exit
Endd
Set cent on
Rele totd,totk,dis1,hj,hj2,jml1,jml2,ipos,kpos,npos
Sele 10
Close
Sele 11
Close
Retu

* DAFTAR PERKIRAAN
*------------------
Proc prk
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
Kpos=spac(5)
Do whil .t.
   Ada=.f.
   Set colo to 
   @1,0 clea to 23,79
   Set colo to /bg
   @1,17 clea to 23,62
   @1,17 to 23,62
   @1,17 say chr(218)+repl(gl,5)+chr(194)+repl(gl,30)+chr(194)+gl+gl+gl+chr(194)+gl+gl+gl+chr(191)
   @2,17 say tl+'Kode '+tl+'Pos Perkiraan'+spac(17)+tl+'D/K'+tl+'L/R'+tl
   @3,17 say chr(195)+repl(gl,5)+chr(197)+repl(gl,30)+chr(197)+gl+gl+gl+chr(197)+gl+gl+gl+chr(180)
   Pesanx='F4=Tambah     F5=Edit     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   Declare field[5],bts[5],frm[5]
   Field[1]='KODE_POS'
   Field[2]='NAMA_POS'
   Field[3]='ID_POS'
   Field[4]='ID_LR'
   Frm[1]='@!'
   Frm[2]=''
   Frm[3]='@!'
   Frm[4]='@!'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl+' '
   Bts[4]=' '+tl+' '
   Dbedit(4,18,22,61,field,'fpos',frm,'','',bts)
   *
   If ada
      Ada=.f.
      Loop
   Endi
   If lastkey()=27
      Exit
   Endi
Endd
Set curs off
Close
Rele kpos,npos,ipos,irl
Retu

Function fpos
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=-3
   B=row()+1
   Go bott
   If eof()
      B=b-1
   Endi
   Set curs on
   Do whil .t.
      Kpos=spac(5)
      Npos=spac(30)
      Stor ' ' to ipos,irl
      Tambah=.t.
      @b,17 say tl+spac(5)+tl+spac(30)+tl+'   '+tl+'   '+tl
      Do pesan with 'ESC=Selesai'
      @b,18 get kpos pict '@!'
      Read
      @24,0
      If kpos=spac(5) .or. lastkey()=27
         Exit
      Endi
      Loca for kode_pos=kpos
      If foun()
         Kpos=kode_pos
         Npos=nama_pos
         Ipos=id_pos
         Irl =id_lr
         Tambah=.f.
      Endi
      @b,24 get npos pict '@!'
      @b,56 get ipos pict '@!'
      @b,60 get irl pict '@!'
      Read
      If ipos#'D' .and. ipos#'K'
         Do pesan2 with 'D/K = Harus diisi dengan D=Debet   atau   K=Kredit'
         Loop
      Endi
      If irl#'Y' .and. irl#'T'
         Do pesan2 with 'L/R = diisi dengan Y=Masuk ke Lap.L/R   atau   T=untuk Transaksi Kas saja'
         Loop
      Endi
      Do pesan with 'Koreksi? Y/T'
      If ulangi()
         Loop
      Endi
      If tambah
         Flok()
         Appe blan
         Repl kode_pos with kpos,nama_pos with npos,id_pos with ipos,id_lr with irl
      Else
         Rlok()
         Repl kode_pos with kpos,nama_pos with npos,id_pos with ipos,id_lr with irl
      Endi
      Ul()
      Commit
      B=b+1
      If b>22
         B=4
      Endi
   Endd
   Do pesan with pesanx
   Set curs off
   Retu 2
Elseif lastkey()=-4
   Set curs on
   If field[pointer]='KODE_POS'
      Do pesan2 with 'Kode pos perkiraan tidak bisa diganti'
      Set curs off
      Do pesan with pesanx
      Retu 1
   Elseif fds='ID_POS'
      Kpos=kode_pos
      Ipos=id_pos
      Rlok()
      @row(),col() get &fds pict '@!'
      Read
      Ul()
      Commit
   Else
      Rlok()
      @row(),col() get &fds pict '@!'
      Read
      Ul()
      Commit
   Endi
   Set curs off
   Do pesan with pesanx
   Retu 1
Elseif lastkey()=-7
   Yt=' '
   Set colo to gr+*
   @row(),col() say &fds
   Set colo to g
   Do pesan with ''
   Set curs on
   Do pesan with 'Dihapus? Y/T : '
   @24,49 get yt pict '!'
   Read
   If yt='Y'
      Rlok()
      Dele
      Ul()
      Commit
   Endi
   Do pesan with pesanx
   Set curs off
   Retu 2
Elseif lastkey()=-9
   Go top
   No=1
   Set curs on
   Do cp
   If cc=0
      Retu 0
   Endi
   Set prin on
   Set devi to prin
   Set marg to 10
   ?chr(27)+chr(33)+chr(1)
   ?' DAFTAR POS PERKIRAAN'
   ?repl('-',45)
   Do whil .not. eof()
      ?tran(no,'999  ')+kode_pos+' '+nama_pos+' '+id_po
      No=no+1
      Skip
   Endd
   ?repl('-',45)
   ?''
   Set prin off
   Set devi to scre
   Do pesan with pesanx
   Set curs off
   Retu 1
Else
   Retu 1
Endi
Retu 0

Proc dafperk
Save scre to sdaf
Set colo to /bg
@1,42 clea to 22,79
@1,42 to 22,79
@1,42 say chr(218)+repl(gl,36)+chr(191)
@3,42 say chr(195)
@3,79 say chr(180)
Declare field[2],kop[2],bts[2]
Field[1]='KODE_POS'
Field[2]='NAMA_POS'
Kop[1]='Kode'
Kop[2]='Pos Perkiraan'
Bts[1]=tl
Bts[2]=tl
Dbedit(2,43,21,78,field,'fposx','',kop,gl,bts)
*
Rest scre from sdaf
Set colo to bg+/b
Set curs on
Retu

Function fposx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=13
      Stor kode_pos to kpos
      Retu 0
   Othe
      Retu 1
Endc
Retu 0

*Proc bukaflkas
*Sele 3
*Uselan('&nf0','&nf1','&nf2','','','')
*Retu

Proc cariflkas
xBln=mont(tgl)+1
xThn=year(tgl)
If xBln=13
   xBln=1
   xThn=year(tgl)+1
Endi
Tgl=ctod('01-'+ltri(str(xBln))+'-'+ltri(str(xThn)))
Do bacaflkas
Retu

Proc pesankas
Para psn
Set colo to /r
@24,0 say spac(80)
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to bg+/b,w+/r
Retu

Proc pesankas2
Para psn
Tone(499,1)
Tone(300,1)
Set colo to /r
@24,0 say spac(80)
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to bg+/b,w+/r
I=0
Do whil i#32
   I=inke()
   If i=27
      Exit
   Endi
Endd
@24,0
Retu

Proc pesankas3
Para psn
Set colo to /r
@24,0 say spac(80)
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to bg+/b,w+/r
I=0
Do whil i#-9
   I=inke()
   If i=27
      Exit
   Endi
Endd
If i=27
   Nlp=0
Else
   Nlp=1
Endi
@24,0
Retu

* EOF KAS
