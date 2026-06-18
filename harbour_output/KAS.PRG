// ============================================================
// File    : KAS.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* TRANSAKSI KAS
*--------------
PROCEDURE inkas
Do bacaflkas
*
SELECT 10
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
If nsls=aidi1+SPACE(10)
   Set filter to id_lr='Y'
   LOCATE FOR id_lr='Y'
ENDIF
SELECT 11
Uselan('&nf0','&nf1','&nf2','','','')
*
Tgl2=tgl
STORE 0 to jml,tot
Adadis=.f.
DO WHILE .t.
   SET COLOR TO w+/r
   @0,0 say SPACE(14)+'TRANSAKSI KAS HARIAN, TGL.: '+dtoc(tgl)+', KASIR: '+nsls
   SET COLOR TO bg+/b
   @1,0 CLEAR to 23,79
   @1,0 say chr(218)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,24)+chr(194)+REPLACE(gl,25)+chr(194)+REPLACE(gl,10)+chr(191)
   @2,0 say tl+'Tanggal   '+tl+'Kode '+tl+'Nama Perkiraan'+SPACE(10)+tl+'Keterangan'+SPACE(15)+tl+'Jumlah Rp.'+tl
   @3,0 say chr(195)+REPLACE(gl,10)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,24)+chr(197)+REPLACE(gl,25)+chr(197)+REPLACE(gl,10)+chr(180)
   Do kolomtr
   B =4
   Disi=.f.
   xBln=mont(tgl)
   STORE ' ' to yt,irl
   DO WHILE .t.
      STORE SPACE(5) to kpos
      Ket  =SPACE(29)
      SELECT 10
      Set key 5 to xadadis
      @b,1 get tgl
      @b,12 get kpos pict '@!'
      Do pesankas with chr(24)+'=Edit/Hapus   ESC=Selesai'
      Read
      Set key 5 to
      If lastkey()=27
         Exit
      ENDIF
      If xBln#mont(tgl)
         xBln=mont(tgl)
         Do bacaflkas
         SELECT 11
         Uselan('&nf0','&nf1','&nf2','','','')
      ENDIF
      If adadis &&.and. xOwner
         Adadis=.f.
         Do hpstr
         Loop
      ENDIF
      @24,0
      SELECT 10
      Seek kpos
      If .not. FOUND() .or. kpos=SPACE(5)
         Do dafperk
         If lastkey()=27
            Loop
         ENDIF
      ENDIF
      Nspos=nama_pos
      Ipos =id_pos
      Irl  =id_lr
      *
      @b,12 say kpos
      @b,18 say SUBSTR(nspos,1,23)
      @b,41 say ipos
      @b,43 get ket pict '@S25'
      @b,69 get jml pict '@e   99999999'
      Read
      If lastkey()=27
         Loop
      ENDIF
      Yt='Y'
      Do pesankas with 'Data Benar? Y/T : '
      @24,49 get yt pict '!'
      Read
      If yt#'Y' .or. lastkey()=27
         Loop
      ENDIF
      SET COLOR TO bg+/b
      @b,1 say dtoc(tgl)+tl+kpos+tl+SUBSTR(nspos,1,22)+' '+ipos+tl+left(ket,25)+tl+tran(jml,'@e 99,999,999')
      *
      SELECT 11
      Flok()
      APPEND BLANK
      REPLACE tanggal with tgl,nama_ksr with nsls,kode_pos with kpos,;
           nama_pos with nspos,keterangan with ket,rp_total with jml,id_pos with ipos,id_lr with irl
      Ul()
      COMMIT
      *
      B=b+1
      Jml =0
      Kpos=SPACE(5)
      Ket =SPACE(29)
      If b>22
         Scroll(4,0,22,79,1)
         B=22
      ENDIF
   ENDDO
   Exit
ENDDO
SET CURSOR OFF
Tgl=tgl2
RELEASE kpos,npos,ket,ipos,tambah
*
SELECT 10
Close
*
SELECT 11
Close
RETURN

***KOLOM TRANSAKSI
PROCEDURE kolomtr
B=4
DO WHILE b<=22
   @b,0 say tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(24)+tl+SPACE(25)+tl+SPACE(10)+tl
   B=b+1
ENDDO
@b,0 say chr(192)+REPLACE(gl,10)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,24)+chr(193)+REPLACE(gl,25)+chr(193)+REPLACE(gl,10)+chr(217)
B=4
RETURN

*** PROSEDUR HAPUS TRANSAKSI
PROCEDURE hpstr
SET COLOR TO bg+/b
SELECT 11
Go bott &&Seek dtos(tgl)
Nr=recno()
Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
DECLARE field[8],bts[8],frm[8]
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
DbEdit(4,1,20,78,field,'fhptr',frm,'','',bts)
*
Do kolomtr
Tot=0
Go nr
DO WHILE .not. eof()
   @b,1 say dtoc(tanggal)+tl+kode_pos+tl+SUBSTR(nama_pos,1,22)+' '+id_pos+tl+left(keterangan,25)+tl+tran(rp_total,'@e 99,999,999')
   B  =b+1
   If b>20
      B=4
      Do kolomtr
   ENDIF
   Skip
ENDDO
RETURN

Function fhptr
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=13
   B   =row()
   Ket =keterangan
   Jml =rp_total
   SET CURSOR ON
   @b,43 get ket pict '@S25'
   @b,69 get jml pict '@e 99,999,999'
   Read
   If lastkey()#27
      Rlok()
      REPLACE keterangan with ket,rp_total with jml
      Ul()
      COMMIT
   ENDIF
   Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
   SET CURSOR OFF
   SET CURSOR OFF
   RETURN 1
Elseif lastkey()=-7
   SET COLOR TO gr+/b
   @row(),col() say &fds
   SET COLOR TO /bg
   Do pesankas with ''
   SET CURSOR ON
   Do pesankas with 'Dihapus? Y/T'
   If ulangi(.t.)
      Rlok()
      Dele
      Ul()
      COMMIT
      Skip
   ENDIF
   Do pesankas with 'ENTER=Edit Keterangan/Jumlah Rp.     F8=Hapus   ESC=Selesai'
   SET CURSOR OFF
   RETURN 2
Else
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

* LAPORAN TRANSAKSI KAS1
*------------------------
PROCEDURE lkas
DO WHILE .t.
   @1,0 CLEAR to 24,79
   Do pesan with 'ESC=Menu Selanjutnya'
   SET COLOR TO /bg
   If hut
      Nsls=SPACE(20)
      Adadis=.f.
      *
      SELECT 6
      Uselan('KASIR.DTA','','','','','')
      Set key 28 to dafksr
      SET COLOR TO /bg
      @2,24 say 'Nama Kasir:'get nsls pict '@!'
      @3,24 say 'Tanggal...:'get tgl
      Do pesan with 'ESC=Selesai   F1=Daftar Kasir'
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      ENDIF
      If nsls#SPACE(20)
         LOCATE FOR nama=nsls
         If .not. FOUND()
            Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
            Wait''
            Loop
         ENDIF
         Nsls=nama
         Ksr =nomer
      ENDIF
      Do pesan with 'ESC=Menu Selanjutnya'
   ENDIF
   Do bacafile
   STORE 0 to hj,hj2,dis1,rtr,jml1,jml2,totd,totk,totb
   SELECT 12
   If .not. file('&nf0')
      Do pesan2 with &takada
      SELECT 10
      Close
      SELECT 11
      Close
      Exit
   ENDIF
   Uselan('&nf0','&nf3','','','','')
   Seek dtos(tgl)+ksr
   DO WHILE .not. eof() .and. ( tanggal=tgl .and. nama_ksr=nsls )
      If left(nama_brg,6)='Diskon'
         Dis1=dis1+abs(harga_2)
      Else
         Hj  =hj  +&brs1
      ENDIF
      Skip
   ENDDO
   Close
   *
   Do bacaflkas
   *
   SELECT 10
   Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
   SELECT 11
   Uselan('&nf0','&nf2','','','','')
   Set filter to nama_ksr=nsls .and. id_pos='D'
   *
   Set alte on
   Set alte to ARUS-KAS.RTF
   ?'    LAPORAN TRANSAKSI KAS HARIAN, TGL.: '+dtoc(tgl)+',  KSR: '+nsls
   ?'   '+REPLACE(gl,74)
   ?'    PEMASUKAN :'
   ?'    - Total Penjualan Barang      '+SPACE(13)+tran(hj,'@e 999,999,999')
   Totd=hj
   Y=1
   *
   SELECT 10
   Set filter to id_pos='D'
   LOCATE FOR id_pos='D'
   DO WHILE .not. eof()
      Kpos =kode_pos
      Nspos=nama_pos
      Ipos =id_pos
      *
      SELECT 11
      Seek kpos+dtos(tgl)
      If .not. FOUND()
         Seek kpos
      ENDIF
      DO WHILE .not. eof() .and. kode_pos=kpos
         If tanggal=tgl
            ?'    - '+left(nama_pos,30)+SPACE(11)+tran(rp_total,'@e 999,999,999')
            Totd=totd+rp_total
         ENDIF
         Skip
      ENDDO
      Y=y+1
      SELECT 10
      Skip
   ENDDO
   ?SPACE(44)+REPLACE(gl,14)
   ?'      Jumlah Pemasukan'   +SPACE(37)+'Rp '+tran(totd,'@e 999,999,999')
   ?' '
   ?'    PENGELUARAN :'
   SELECT 11
   Set filter to nama_ksr=nsls .and. id_pos='K'
   *
   SELECT 10
   Set filter to id_pos='K'
   LOCATE FOR id_pos='K'
   DO WHILE .not. eof()
      Kpos =kode_pos
      Nspos=nama_pos
      Ipos =id_pos
      *
      SELECT 11
      Seek kpos+dtos(tgl)
      If .not. FOUND()
         Seek kpos
      ENDIF
      DO WHILE .not. eof() .and. kode_pos=kpos
         If tanggal=tgl
            ?'    - '+left(nama_pos,20)+' '+left(keterangan,20)+tran(rp_total,'@e 999,999,999')
            Totk=totk+rp_total
         ENDIF
         Skip
      ENDDO
      Y=y+1
      SELECT 10
      Skip
   ENDDO
   ?SPACE(44)+REPLACE(gl,14)
   ?'      Jumlah Pengeluaran'+SPACE(35)+'Rp '+tran(totk,'@e 999,999,999')
   ?SPACE(59)+REPLACE(gl,14)
   ?'      Sisa Uang Kas     '+SPACE(35)+'Rp '++tran(totd-totk,'@e 999,999,999')
   ?SPACE(59)+REPLACE(gl,14)
   ?' '
   Set alte off
   Set alte to
   Memoedit(Memoread('ARUS-KAS.RTF'),1,0,22,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If lastkey()=27
      If hut
         Loop
      ENDIF
      Exit
   ENDIF
   Do cp
   If lastkey()=27
      Loop
   ENDIF
   SET PRINTER ON
   SET DEVICE TO PRINTER
   Set marg to 5
   ?chr(27)+chr(33)+chr(1)
   Type ARUS-KAS.RTF to prin
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   If hut
      Loop
   ENDIF
   Exit
ENDDO
SET CENTURY ON
RELEASE totd,totk,dis1,hj,hj2,jml1,jml2,ipos,kpos,npos
SELECT 10
Close
SELECT 11
Close
RETURN

* DAFTAR PERKIRAAN
*------------------
PROCEDURE prk
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
Kpos=SPACE(5)
DO WHILE .t.
   Ada=.f.
   SET COLOR TO 
   @1,0 CLEAR to 23,79
   SET COLOR TO /bg
   @1,17 CLEAR to 23,62
   @1,17 to 23,62
   @1,17 say chr(218)+REPLACE(gl,5)+chr(194)+REPLACE(gl,30)+chr(194)+gl+gl+gl+chr(194)+gl+gl+gl+chr(191)
   @2,17 say tl+'Kode '+tl+'Pos Perkiraan'+SPACE(17)+tl+'D/K'+tl+'L/R'+tl
   @3,17 say chr(195)+REPLACE(gl,5)+chr(197)+REPLACE(gl,30)+chr(197)+gl+gl+gl+chr(197)+gl+gl+gl+chr(180)
   Pesanx='F4=Tambah     F5=Edit     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   DECLARE field[5],bts[5],frm[5]
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
   DbEdit(4,18,22,61,field,'fpos',frm,'','',bts)
   *
   If ada
      Ada=.f.
      Loop
   ENDIF
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CURSOR OFF
Close
RELEASE kpos,npos,ipos,irl
RETURN

Function fpos
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=-3
   B=row()+1
   Go bott
   If eof()
      B=b-1
   ENDIF
   SET CURSOR ON
   DO WHILE .t.
      Kpos=SPACE(5)
      Npos=SPACE(30)
      STORE ' ' to ipos,irl
      Tambah=.t.
      @b,17 say tl+SPACE(5)+tl+SPACE(30)+tl+'   '+tl+'   '+tl
      Do pesan with 'ESC=Selesai'
      @b,18 get kpos pict '@!'
      Read
      @24,0
      If kpos=SPACE(5) .or. lastkey()=27
         Exit
      ENDIF
      LOCATE FOR kode_pos=kpos
      If FOUND()
         Kpos=kode_pos
         Npos=nama_pos
         Ipos=id_pos
         Irl =id_lr
         Tambah=.f.
      ENDIF
      @b,24 get npos pict '@!'
      @b,56 get ipos pict '@!'
      @b,60 get irl pict '@!'
      Read
      If ipos#'D' .and. ipos#'K'
         Do pesan2 with 'D/K = Harus diisi dengan D=Debet   atau   K=Kredit'
         Loop
      ENDIF
      If irl#'Y' .and. irl#'T'
         Do pesan2 with 'L/R = diisi dengan Y=Masuk ke Lap.L/R   atau   T=untuk Transaksi Kas saja'
         Loop
      ENDIF
      Do pesan with 'Koreksi? Y/T'
      If ulangi()
         Loop
      ENDIF
      If tambah
         Flok()
         APPEND BLANK
         REPLACE kode_pos with kpos,nama_pos with npos,id_pos with ipos,id_lr with irl
      Else
         Rlok()
         REPLACE kode_pos with kpos,nama_pos with npos,id_pos with ipos,id_lr with irl
      ENDIF
      Ul()
      COMMIT
      B=b+1
      If b>22
         B=4
      ENDIF
   ENDDO
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-4
   SET CURSOR ON
   If field[pointer]='KODE_POS'
      Do pesan2 with 'Kode pos perkiraan tidak bisa diganti'
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
   Elseif fds='ID_POS'
      Kpos=kode_pos
      Ipos=id_pos
      Rlok()
      @row(),col() get &fds pict '@!'
      Read
      Ul()
      COMMIT
   Else
      Rlok()
      @row(),col() get &fds pict '@!'
      Read
      Ul()
      COMMIT
   ENDIF
   SET CURSOR OFF
   Do pesan with pesanx
   RETURN 1
Elseif lastkey()=-7
   Yt=' '
   SET COLOR TO gr+*
   @row(),col() say &fds
   SET COLOR TO g
   Do pesan with ''
   SET CURSOR ON
   Do pesan with 'Dihapus? Y/T : '
   @24,49 get yt pict '!'
   Read
   If yt='Y'
      Rlok()
      Dele
      Ul()
      COMMIT
   ENDIF
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-9
   Go top
   No=1
   SET CURSOR ON
   Do cp
   If cc=0
      RETURN 0
   ENDIF
   SET PRINTER ON
   SET DEVICE TO PRINTER
   Set marg to 10
   ?chr(27)+chr(33)+chr(1)
   ?' DAFTAR POS PERKIRAAN'
   ?REPLACE('-',45)
   DO WHILE .not. eof()
      ?tran(no,'999  ')+kode_pos+' '+nama_pos+' '+id_po
      No=no+1
      Skip
   ENDDO
   ?REPLACE('-',45)
   ?''
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 1
Else
   RETURN 1
ENDIF
RETURN 0

PROCEDURE dafperk
Save scre to sdaf
SET COLOR TO /bg
@1,42 CLEAR to 22,79
@1,42 to 22,79
@1,42 say chr(218)+REPLACE(gl,36)+chr(191)
@3,42 say chr(195)
@3,79 say chr(180)
DECLARE field[2],kop[2],bts[2]
Field[1]='KODE_POS'
Field[2]='NAMA_POS'
Kop[1]='Kode'
Kop[2]='Pos Perkiraan'
Bts[1]=tl
Bts[2]=tl
DbEdit(2,43,21,78,field,'fposx','',kop,gl,bts)
*
Rest scre from sdaf
SET COLOR TO bg+/b
SET CURSOR ON
RETURN

Function fposx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      STORE kode_pos to kpos
      RETURN 0
   Othe
      RETURN 1
ENDCASE
RETURN 0

*Proc bukaflkas
*Sele 3
*Uselan('&nf0','&nf1','&nf2','','','')
*Retu

PROCEDURE cariflkas
xBln=mont(tgl)+1
xThn=year(tgl)
If xBln=13
   xBln=1
   xThn=year(tgl)+1
ENDIF
Tgl=ctod('01-'+LTRIM(str(xBln))+'-'+LTRIM(str(xThn)))
Do bacaflkas
RETURN

PROCEDURE pesankas
PARAMETERS psn
SET COLOR TO /r
@24,0 say SPACE(80)
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO bg+/b,w+/r
RETURN

PROCEDURE pesankas2
PARAMETERS psn
Tone(499,1)
Tone(300,1)
SET COLOR TO /r
@24,0 say SPACE(80)
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO bg+/b,w+/r
I=0
DO WHILE i#32
   I=inke()
   If i=27
      Exit
   ENDIF
ENDDO
@24,0
RETURN

PROCEDURE pesankas3
PARAMETERS psn
SET COLOR TO /r
@24,0 say SPACE(80)
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO bg+/b,w+/r
I=0
DO WHILE i#-9
   I=inke()
   If i=27
      Exit
   ENDIF
ENDDO
If i=27
   Nlp=0
Else
   Nlp=1
ENDIF
@24,0
RETURN

* EOF KAS
