// ============================================================
// File    : BANT.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* PROSEDUR-2 BANTUAN

PROCEDURE cekdata
*
Tgl=tglx
xCariso="(mont(tanggal)=mont(tgl) .and. ( tanggal>=tglso .and. tanggal<=tgl2 ))"
Uselan('DATA.SET','','','','','')
xPos=RTRIM(keterangan)
*
Do bacafile
Uselan('JUAL.DTA','','','','','')
If .not. file('&nf0')
   Copy stru to &nf0
   CLOSE all
   B=23
   Indx('&nf0','dtos(TANGGAL)+FAKTUR','&nf1',2)
   Indx('&nf0','KODE_BAR+dtos(TANGGAL)','&nf2',2)
   Indx('&nf0','dtos(TANGGAL)+KODE_KSR+FAKTUR','&nf3',2)
ENDIF
CLOSE all
*
Nf0='KAS-'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
If .not. file('&nf0')                 
   Useidx('KAS.DTA')
   Copy stru to &nf0
   Close all
   B=23
   Indx('&nf0','dtos(TANGGAL)+KODE_POS+ID_POS','&nf1',2)
   Indx('&nf0','KODE_POS+dtos(TANGGAL)','&nf2',1)
ENDIF
Close all
*
Uselan('KASSA.SET','','','','','')
Idis=id_disp
Close all
*
Brs1 ="((jumlah-jumlah_rtr)* (harga_2- ((harga_2*(diskon/100))+diskon_rp)))"
Brs12="((jumlah*hbnet)-diskon_rp)"
Brs13="(jml* (hj- ((hj*(dis1/100))+dis2)))"
Brs14="((jumlah-jumlah_rtr)*harga)"
RETURN

PROCEDURE hari
Hh=dow(tgl)
Do case
   Case hh=1
      Hr='Minggu'
   Case hh=2
      Hr='Senin '
   Case hh=3
      Hr='Selasa'
   Case hh=4
      Hr='Rabu  '
   Case hh=5
      Hr='Kamis '
   Case hh=6
      Hr="Jum'at"
   Case hh=7
      Hr='Sabtu '
ENDCASE
RETURN

PROCEDURE x   &&&---KOP
PARAMETERS judul
SET CURSOR ON
SET COLOR TO w+/r
@0,0
@0,1 say RTRIM(aidi1)+iif(xpos#'',' \'+xpos,'')
@0,40-(len(judul)/2) say judul
@0,79-len(RTRIM(hr)+', '+dtoc(tgl)) say RTRIM(hr)+', '+dtoc(tgl)
@24,0
SET COLOR TO /bg,bg+/b+
RETURN

PROCEDURE pesan
PARAMETERS psn
SET COLOR TO /r
@24,0
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO /bg,w+/b
RETURN

PROCEDURE pesan1
PARAMETERS psn
SET COLOR TO /b
@23,0
SET COLOR TO w+/b
@23,40-(len(psn)/2) say psn
SET COLOR TO /bg,w+/b
RETURN

PROCEDURE pesan2
PARAMETERS psn
Tone(499,1)
Tone(300,1)
SET COLOR TO /r
@24,0
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO /bg,w+/b
I=0
DO WHILE i#32
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

PROCEDURE pesan3
PARAMETERS psn
SET COLOR TO /r
@24,0
SET COLOR TO w+/r
@24,40-(len(psn)/2) say psn
SET COLOR TO /bg,w+/b
I=0
DO WHILE i#-9
   I=inke()
   If i=27 .or. i=-3 .or. i=-4 .or. i=-7
      Exit
   ENDIF
ENDDO
If i=27 .or. i=-3 .or. i=-4 .or. i=-7
   Nlp=0
Else
   Nlp=1
ENDIF
@24,0
RETURN

***PROSES ULANGI??
Function ulangi
I=inke()
DO WHILE i#13 .and. i#84 .and. i#116
   I=inke()
   If i=27 .or. i=89 .or. i=121
      RETURN(.t.)
   ENDIF
ENDDO
RETURN(.f.)

*** GESER RECORD
PROCEDURE geser
I=0
DO WHILE i#27
   I=inke()
   If i=18 .or. i=3 .or. i=5 .or. i=22 .or. i=24 .or. i=-3 .or. i=-7 .or. i=-9
      Exit
   ENDIF
ENDDO
Do case
   Case i=18  &&&-PgUp
      Skip -no
   Case i=3   &&&-PgDn
      Skip no
   Case i=5   &&&-Up
      Skip -1
   Case i=24   &&&-Dn
      Skip
   Case i=-9
      Nlp=1
   Othe
      Nlp=0
ENDCASE
RETURN

PROCEDURE xnota
Fkt=right('00000'+LTRIM(str(nn)),5)
RETURN

PROCEDURE cp
DO WHILE .t.
   If .not. isprinter()
      Tone(700,1)
      Tone(499,1)
      Do pesan with 'Printer belum siap,  ESC=Batal  F10=Coba Lagi'
      I=0
      DO WHILE i#-9
         I=inke()
         If i=27
            Exit
         ENDIF
      ENDDO
      Do case
         Case i=27
            Cc=0
            Exit
         Case i=-9
            If isprinter()
               Cc=1
               Exit
            ENDIF
      ENDCASE
   Else
      Cc=1
      Exit
   ENDIF
ENDDO
Do pesan with 'Tunggu, sedang dicetak'
RETURN

PROCEDURE bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
RETURN

PROCEDURE bacaflkas
Nf0='KAS-'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
RETURN

PROCEDURE bukaflkas
SELECT 3
Uselan('&nf0','&nf1','&nf2','','','')
RETURN

PROCEDURE lain2
Uselan('DATA.SET','','','','','')
Go top
xPos=keterangan
Rpb =rp_debet
Rpb2=rp_kredit
pExim=path_exim
Alm1 =almt_1
Alm2 =almt_2
Hj =no_order
Hp =no_returb
Nt =no_pinjam
Hb =no_adjust
Pr1=promo1
Pr2=promo2
Pr3=promo3
Pr4=promo4
Cpo1=ket_po1
Cpo2=ket_po2
Cpo3=ket_po3
Tgl2=tanggal_2
Tgl3=tanggal_3
Npjk=npwp
Idt =id_cust
Pdt =pj_cust
SET COLOR TO n++
@1,0 CLEAR to 24,79
B=1
DO WHILE b<=23
   @b,0 say REPLACE(chr(219),80)
   B=b+1
ENDDO
SET COLOR TO /bg,bg+/b
@1,0  CLEAR to 24,79
@1,0  to 24,79
@2,1  say 'No.Urut Retur Pembelian:'get hp  pict '99999'
@3,1  say 'No.Urut Transfer Barang:'get nt  pict '99999'
@2,44 say 'No.Urut PO/Order Brg...:'get hj  pict '99999'
@3,44 say 'No.Urut Bukti Stok Opnm:'get hb  pict '99999'
@5,1  say 'Kelipatan Belanja unt 1 Poin #1 Rp:'
@6,1  say '                             #2 Rp:'
@5,36 get rpb  pict '@e 9,999,999'
@6,36 get rpb2 pict '@e 9,999,999'
@6,46 say 'Periode:           s/d           '
@6,54 get tgl2
@6,69 get tgl3
@8,1  say 'Huruf Awal di Kode Member / Plgn..:'
@8,36 get idt pict '@!'
@8,40 say '& Total Panjang Kode Member:'get pdt pict '9'
@10,1  say 'Identitas Toko:'get xpos pict '@!S20'
@10,44 say 'Folder BackUp :'get pExim pict '@!'
@12,1  say 'Alamat di Nota:'get alm1
@13,1  say '              :'get alm2
@15,1  say 'Promo di Nota :'get pr1
@16,17 get pr2
@17,17 get pr3
@18,17 get pr4
@20,1  say 'Catatan di PO :'get cpo1 pict '@S62'
@21,17 get cpo2 pict '@S62'
@22,17 get cpo3 pict '@S62'
Read
If lastkey()#27
   Rlok()
   REPLACE no_order with hj,no_returb with hp,no_pinjam with nt,no_adjust with hb,keterangan with xpos,rp_debet with rpb,rp_kredit with rpb2,;
        npwp with npjk,path_exim with pExim,tanggal_2 with tgl2,tanggal_3 with tgl3,almt_1 with alm1,almt_2 with alm2,;
        promo1 with pr1,promo2 with pr2,promo3 with pr3,promo4 with pr4,ket_po1 with cpo1,ket_po2 with cpo2,ket_po3 with cpo3,id_cust with idt,pj_cust with iif(pdt=0,1,pdt)
   Ul()
   COMMIT
ENDIF
xPos=RTRIM(xpos)
CLOSE all
RELEASE hj,hb,hp,nt,pr1,pr2,pr3,pr4,pExim,alm1,alm2,rpb,rpb2,tgl2,tgl3,cpo1,cpo2,cpo3,npjk,idt,pdt
RETURN

* SETUP PEMBULATAN
*------------------
PROCEDURE rpbulat
Uselan('RPBULAT.SET','','','','','')
SET CENTURY OFF
DO WHILE .t.
   @1,0 CLEAR to 24,79
   @1,15 to 21,63
   @1,15 say chr(218)+REPLACE(REPLACE(gl,11)+chr(194),2)+REPLACE(REPLACE(gl,7)+chr(194),2)+REPLACE(gl,7)+chr(191)
   @2,15 say tl+' Dari (Rp) '+tl+'Sampai (Rp)'+tl+' Jumlah'+tl+'keBawah'+tl+' keAtas'+tl
   @3,15 say chr(195)+REPLACE(REPLACE(gl,11)+chr(197),2)+REPLACE(REPLACE(gl,7)+chr(197),2)+REPLACE(gl,7)+chr(180)
   Pesanx='F4=Tambah   F5=Edit   F8=Hapus   F10=Cetak'
   Do pesan with pesanx
   DECLARE field[5],bts[5],frm[5]
   Field[1]='BATAS_1'
   Field[2]='BATAS_2'
   Field[3]='NILAI_RP'
   Field[4]='NILAI_B'
   Field[5]='NILAI_A'
   Frm[1]='@e 999,999,999'
   Frm[2]='@e 999,999,999'
   Frm[3]='@e   9,999'
   Frm[4]='@e   9,999'
   Frm[5]='@e   9,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   DbEdit(4,16,20,62,field,'fbulat',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CENTURY ON
SET CURSOR OFF
CLOSE all
RELEASE jml1,jml2,jml,tot,tot2
RETURN

Function fbulat
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-3
      B=row()+1
      Go bott
      If eof()
         B=b-1
      ENDIF
      SET CURSOR ON
      STORE 0 to jml1,jml2,jml,tot,tot2
      Ket=' '
      DO WHILE .t.
         Tambah=.t.
         @b,15 say tl+SPACE(11)+tl+SPACE(11)+tl+SPACE(7)+tl+SPACE(7)+tl
         Do pesan with 'ESC=Selesai'
         @b,16 get jml1 pict '@e 999,999,999'
         @b,28 get jml2 pict '@e 999,999,999'
         Read
         @24,0
         If lastkey()=27
            Exit
         ENDIF
         LOCATE FOR batas_1=jml1 .and. batas_2=jml2
         If FOUND()
            Jml =nilai_rp
            Tot =nilai_b
            Tot2=nilai_a
            Tambah=.f.
         ENDIF
         @b,42 get jml  pict '@e   9,999'
         @b,50 get tot  pict '@e   9,999'
         @b,58 get tot2 pict '@e   9,999'
         Read
         If tot2<=tot .and. tot2>0
            Tone(1500,3)
            Tone(2000,6)
            Do pesan with 'Nilai Pembulatan keAtas  HARUS  Lebih Besar dari Nilai Pembulatan keBawah'
            Wait''
            Loop
         ENDIF
         Do pesan with 'Koreksi ? Y/T'
         If ulangi()
            Loop
         ENDIF
         If tambah
            Flok()
            APPEND BLANK
            REPLACE batas_1 with jml1,batas_2 with jml2,nilai_rp with jml,nilai_b with tot,nilai_a with tot2
         Else
            Rlok()
            REPLACE nilai_rp with jml,nilai_b with tot,nilai_a with tot2
         ENDIF
         Ul()
         COMMIT

         STORE 0 to jml1,jml2,jml,tot,tot2
         B=b+1
         If b>20
            B=4
         ENDIF
      ENDDO
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-7
      SET COLOR TO gr+*/b
      @row(),col() say &fds
      Yt=' '
      SET CURSOR ON
      SET COLOR TO /bg
      Do pesan with 'DiHapus? Y/T'
      DO WHILE .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         If .not. deleted()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-9
      Go top
      No=1
      SET CURSOR ON
      Do cp
      If cc=0
         RETURN 0
      ENDIF
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to 0
      ?chr(18)
      ?' '+aidi1
      ?' '+aidi2
      ?' '+SUBSTR(aidi3,1,24)+' '+dtoc(date())
      ?' '+REPLACE('-',35)
      ?' DAFTAR PEMBULATAN TOTAL PENJUALAN'
      ?REPLACE('-',80)
      DO WHILE .not. eof()
         ?tran(no,'999.  ')+tran(batas_1,'@e 999,999,999  ')+tran(batas_1,'@e 999,999,999  ')+tran(nilai_rp,'@e 9,999  ')+tran(nilai_b,'@e 9,999  ')+tran(nilai_a,'@e 9,999  ')
         No=no+1
         Skip
      ENDDO
      ?REPLACE('-',80)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
   Case lastkey()=-4
      SET CURSOR ON
      Rlok()
      If left(fds,5)='BATAS'
         @row(),col() get &fds pict '@e 999,999,999'
         Read
      Elseif left(fds,5)='NILAI'
         @row(),col() get &fds pict '@e   9,999'
         Read
      ENDIF
      Ul()
      COMMIT
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
   Othe
      RETURN 1
ENDCASE
RETURN 0

* DAFTAR KASIR
*--------------
PROCEDURE ksr
Uselan('KASIR.DTA','','','','','')
Sls=SPACE(10)
SET CENTURY OFF
DO WHILE .t.
   SET COLOR TO 
   @1,0 CLEAR to 24,79
   SET COLOR TO /bg
   @1,8 CLEAR to 23,70
   @1,8 to 23,70
   @1,8 say chr(218)+gl+gl+REPLACE(chr(194)+REPLACE(gl,20),2)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,8)+chr(194)+gl+chr(191)
   @2,8 say tl+'No'+tl+'Nama Kasir          '+tl+'Kode/Sandi          '+tl+'#Nota'+tl+'#Tanggal'+tl+'.'+tl
   @3,8 say chr(195)+gl+gl+REPLACE(chr(197)+REPLACE(gl,20),2)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,8)+chr(197)+gl+chr(180)
   Pesanx='F5=Edit     ESC=Selesai'
   Do pesan with pesanx
   DECLARE field[6],bts[6],frm[6]
   Field[1]='NOMER'
   Field[2]='NAMA'
   Field[3]='KODE'
   Field[4]='NOTA'
   Field[5]='TANGGAL'
   Field[6]='JENS_JUAL'
   Frm[1]='99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='99999'
   Frm[5]='99-99-99'
   Frm[6]='@!'
   STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6]
   DbEdit(4,9,22,69,field,'fksrx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CENTURY ON
SET CURSOR OFF
CLOSE all
RELEASE sls,nsls,hj,tambah,field[5],bts[5],frm[5]
RETURN

Function fksrx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-4 .and. xOwner
      SET CURSOR ON
      If fds='KODE' .or. fds='NAMA'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         COMMIT
      Elseif fds='NOTA'
         Rlok()
         @row(),col() get &fds pict '99999'
         Read
         Ul()
         COMMIT
      Elseif fds='JENS_JUAL'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         COMMIT
      ENDIF
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
   Othe
      RETURN 1
ENDCASE
RETURN 0

***POSTING STOK BULANAN
PROCEDURE postok
SET CURSOR ON
SET CENTURY ON
DO WHILE .t.
   Ket=SPACE(8)
   Tgl2=tglx
   Tgl =ctod('01-'+LTRIM(str(mont(tglx)))+'-'+LTRIM(str(year(tglx))))
   Ytp =' '
   Save scre to smkdb
   SET COLOR TO w+/br
   @2,17 CLEAR to 13,60
   @2,17 to 13,60
   @2,25  say 'PROSES/HITUNG ULANG TRANSAKSI'
   @4,19  say 'Angka  di Kolom  Pembelian, Penjualan,'
   @5,19  say 'dan Retur_Pemb  akan  di-Proses Ulang,'
   @6,19  say 'sehingga Sisa Akhir Stok otomatis akan'
   @7 ,19 say 'disesuaikan dg Transaksi yang tercatat.'
   @8 ,19 say 'Jalankan Menu ini SEBELUM Posting Stok.!'
   @10,19 say 'Ketik REPROSES untuk konfirmasi:'
   @10,52 get ket pict '@!'
   Read
   If lastkey()#27 .and. ket='STOK0000'
      @11,19 say 'Perhatian...!'
      @12,19 say 'Sisa Stok Brg akan di-kosongkan semua..!'
      DO WHILE .not. ytp$'YT'
         @13,32 say 'Proses? Y/T'get ytp pict '!'
         Read
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
      If ytp='Y' .and. lastkey()#27
         Do pesan with 'Tunggu, sedang proses...'
         Bln=left(time(),5)
         Nfx='STO_'+left(bln,2)+right(bln,2)+'.DTA'
         SELECT 3
         Uselan('STOK.DTA','','','','','')
         Copy to &nfx
         Useidx('STOK.DTA')
         *
         REPLACE all stok_a with 0,stok_m with 0,stok_k with 0,stok_rb with 0
         REPLACE all tanggal_1 with ctod('  -  -    '),tanggal_2 with ctod('  -  -    '),tanggal_rb with ctod('  -  -    '),tanggal_0 with tglx
         *
         Ytp=' '
      ENDIF
      Exit
   ENDIF
   If lastkey()#27 .and. ket='REPROSES'
      @11,19 say 'ReProses dari:'get tgl
      @11,45 say 's/d:'get tgl2
      DO WHILE .not. ytp$'YT'
         @13,32 say 'Proses? Y/T'get ytp pict '!'
         Read
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
      If ytp='Y' .and. lastkey()#27
         If mont(tgl)#mont(tgl2)
            Yt=' '
            Save scre to smkdb
            SET COLOR TO w+/r,w+/b
            @17,15 CLEAR to 20,58
            @17,15 to 20,58
            @18,17 say 'Pengisian Tanggal, Bulan-nya tidak sama.'
            @19,17 say '      Tetap akan diLanjut? Y/T :'
            @19,50 get yt pict '!'
            Read
            Rest scre from smkdb
            If yt#'Y' .or. lastkey()=27
               Loop
            ENDIF
            Beda=.t.
         ENDIF
         Do pesan with 'Tunggu, sedang proses...'
         Thn =right(dtoc(tgl),2)
         Bln =subst(dtoc(tgl),4,2)
         Nfx ='STO_'+thn+bln+'.DTA'
         *
         Do konvtgl
         Do bacafile
         *
         SELECT 2
         If .not. file('&nfx')
            Ytp=' '
            Do pesan with 'Tidak Ada File STOK-'+thn+bln+' .  ReProses Data Bulan ini? Y/T : '
            @23,0
            @24,70 get ytp pict '!'
            Read
            If lastkey()=27 .or. ytp#'Y'
               Exit
            ENDIF
            Uselan('STOK.DTA','STOK5.NDX','','','','')           
         Elseif file('&nfx')
            Useidx('&nfx')
            INDEX ON kode_bar to L-STOK.TMP
            Uselan('&nfx','L-STOK.TMP','','','','')           
         ENDIF
         Jml2=reccount()
         Tgl3=ctod('  -  -    ')
         Go top
         DO WHILE .not. eof()
            Do pesan with LTRIM(str(recno()))+' / '+LTRIM(str(jml2))
            Rlok()
            REPLACE stok_m with 0,stok_k with 0,stok_rb with 0,;
                 tanggal_1 with tgl3,tanggal_2 with tgl3,tanggal_rb with tgl3
            Ul()
            Skip
         ENDDO
         *
         Do pesan with 'Proses PEMBELIAN...'
         SELECT 3
         Uselan('BELIIN.DTA','BELIIN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         ENDIF
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_bar
            Jml =jumlah
            Isi =1  &&isine_b
            *
            SELECT 2
            Seek kdb
            Rlok()
            REPLACE stok_m with stok_m+(jml*isi),tanggal_1 with tgl3
            Ul()
            *
            SELECT 3
            Skip
         ENDDO
         *
         *
         Do pesan with 'Proses RETUR PEMBELIAN...'
         SELECT 3
         Uselan('RETURBN.DTA','RETURBN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         ENDIF
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_bar
            Jml =jumlah
            *
            SELECT 2
            Seek kdb
            Rlok()
            REPLACE stok_rb with stok_rb+jml,tanggal_rb with tgl3
            Ul()
            *
            SELECT 3
            Skip
         ENDDO
         *
         Do pesan with 'Proses PECAH/MUTASI BARANG...'
         SELECT 3
         Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         ENDIF
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_brg    &&brg.tujuan
            Kdb2=kode_brg2   &&brg.asal
            Jml =jumlah
            Isi2=isine_b
            *
            SELECT 2
            Seek kdb
            Rlok()
            REPLACE stok_m with (stok_m+(jml*isi2)),tanggal_1 with tgl3
            Ul()
            COMMIT
            *
            Seek kdb2
            Rlok()
            REPLACE stok_k with (stok_k+(jml*isine_b)),tanggal_2 with tgl3
            Ul()
            COMMIT
            *
            SELECT 3
            Skip
         ENDDO
         *
         Thn =right(dtoc(tgl),2)
         Bln =subst(dtoc(tgl),4,2)
         Do bacafile
         Do pesan with 'Proses PENJUALAN...'
         DO WHILE .t.
            If file('&nf0')
               SELECT 3
               Uselan('&nf0','&nf1','','','','')
               Seek dtos(tgl)
               If .not. FOUND()
                  LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
               ENDIF
               DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
                  Tgl3=tanggal
                  Kdb =kode_bar
                  Jml =jumlah
                  Isi =1
                  *
                  SELECT 2
                  Seek kdb
                  Rlok()
                  REPLACE stok_k with stok_k+(jml*isi),tanggal_2 with tgl3
                  Ul()
                  *
                  SELECT 3
                  Skip
               ENDDO
            ENDIF
            If beda
               SET CENTURY ON
               Do carifljual
               If tgl>tgl2
                  Exit
               ENDIF
            Elseif beda=.f.
               Exit
            ENDIF
         ENDDO
         *
         Do pesan with 'Hitung Ulang Transaksi sudah selesai, selanjutnya Posting Stok?'
         *
      ENDIF
   ENDIF
   ***
   * SETELAH ITU, POSTING?
   ***
   Ytp=' '
   SET COLOR TO w+/r,w+/b
   @16,15 CLEAR to 19,58
   @16,15 to 19,58
   @17,18 say 'Proses Hitung Ulang Stok Selesai...'
   @18,18 say 'Lanjut ke Posting Stok Bulanan? Y/T:'
   @18,55 get ytp pict '!'
   Read
   If ytp#'Y' .or. lastkey()=27
      Exit
   ENDIF   
   Ket=SPACE(7)
   Tgl=tglx
   Ytp=' '
   Rest scre from smkdb
   SET COLOR TO w+/r
   @5,15 CLEAR to 19,60
   @5,15 to 19,60
   @5,22  say 'POSTING STOK AWAL / TUTUP BULAN'
   @7,18  say 'Saldo/Stok Awal  akan  terisi   Otomatis'
   @8,18  say 'dari  Akumulasi Transaksi Pembelian  dan'
   @9,18  say 'Penjualan selama 1 Bulan  atau terhitung'
   @10,18 say 'mulai  dari  Tgl.Posting Stok  Terakhir.'
   @12,18 say 'Kolom / Jumlah   " Stok Masuk & Keluar " '
   @13,18 say 'menjadi "0" (kosong) semua.'
   @17,18 say 'Ketik POSTING untuk konfirmasi:'
   @17,50 get ket pict '@!'
   SET COLOR TO gr+/r*
   @15,20 say '***PROSES INI TIDAK BISA DIULANG***'
   SET COLOR TO gr+/r
   Read
   If lastkey()=27
      Exit
   ENDIF
   If ket#'POSTING'
      Loop
   ENDIF
   @18,18 say 'Posting  s/d  Akhir Tanggal:'get tgl
   DO WHILE .not. ytp$'YT'
      @19,30 say 'Proses? Y/T'get ytp pict '!'
      Read
      If lastkey()=27
         Exit
      ENDIF
   ENDDO
   If ytp='Y' .and. lastkey()#27
      Do pesan with 'Tunggu, sedang proses...'
      Tgl3=ctod('  -  -    ')
      Thn =right(dtoc(tgl),2)
      Bln =subst(dtoc(tgl),4,2)
      Nfx ='STO_'+thn+bln+'.DTA'
      If file('&nfx')
         Do pesan2 with 'Sudah ada file STOK-'+thn+bln
         CLOSE all
         RETURN
      ENDIF
      SELECT 2
      Useidx('STOK.DTA')
      INDEX ON KODE_KLB+NAMA_BRG TO TEMP.TMP
      *
      Uselan('STOK.DTA','TEMP.TMP','','','','')
      Copy to &nfx
      Close
      *
      Useidx('STOK.DTA')
      *
      REPLACE all stok_a with ((stok_a+stok_m)-(stok_k+stok_rb)),;
               stok_m with 0,stok_k with 0,stok_rb with 0
      REPLACE all tanggal_1 with tgl3 ,tanggal_2 with tgl3 ,tanggal_rb with tgl3,tanggal_0 with tgl+1
      *
      SELECT 1
      Uselan('DATA.SET','','','','','')
      Rlok()
      REPLACE tanggal with tgl
      Ul()
   ENDIF
   Exit
ENDDO
SET COLOR TO /bg
CLOSE all
Ytp='X'   &&&UNT_LANGSUNG_CLOSE
RETURN

***BERLAKU STOK>MPB & KEU>LHUT
FUNCTION hitdis
Hbx  =(JUMLAH*HARGA)-DISKON_RP
Disr1=( HBX * (DISKON_1/100))
Disr2=( (HBX-DISR1) * (DISKON_2/100) )
Disr3=( (HBX-DISR1-DISR2) * (DISKON_3/100) )
Hbnet=( HBX-(DISR1+DISR2+DISR3)-DISKON_RP2 )
RETURN(.T.)

*DAFTAR KLP
*---------
PROCEDURE klp
Uselan('JENIS.DTA','JENIS.NDX','JENIS2.NDX','','','')
*
Klb =SPACE(5)
Nklb=SPACE(30)
DO WHILE .t.
   SET COLOR TO
   @1,0 CLEAR to 23,79
   SET COLOR TO /bg
   @1,21 CLEAR to 23,58
   @1,21 to 23,58
   @1,21 say chr(218)+REPLACE(gl,5)+chr(194)+REPLACE(gl,30)+chr(191)
   @2,21 say tl+'Kode '+tl+'Nama Kelompok Brg'+SPACE(13)+tl
   @3,21 say chr(195)+REPLACE(gl,5)+chr(197)+REPLACE(gl,30)+chr(180)
   Pesanx='F4=Tambah    F5=Edit    F8=Hapus    F10=Cetak'
   Do pesan with pesanx
   DECLARE field[5],bts[5],frm[5]
   Field[1]='KODE_KLB'
   Field[2]='NAMA_KLB'
   STORE '@!' to frm[1],frm[2],frm[3],frm[4]
   STORE tl to bts[1],bts[2],bts[3],bts[4]
   DbEdit(4,22,22,57,field,'fklb',frm,'','',bts)
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE klb,nklb
RELEASE field[4],frm[4],bts[4]
SET CENTURY ON
RETURN

Function fklb
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=-3
      Nr=recno()
      B=row()+1
      Go bott
      If eof()
         B=b-1
      ENDIF
      SET CURSOR ON
      DO WHILE .t.
         Klb =SPACE(5)
         Nklb=SPACE(30)
         Jml2=0
         Tambah=.t.
         @b,21 say tl+SPACE(5)+tl+SPACE(30)+tl
         Do pesan with 'ESC=Selesai'
         @b,22 get klb pict '@!'
         Read
         @24,0
         If klb=SPACE(5) .or. lastkey()=27
            Go nr
            Exit
         ENDIF
         LOCATE FOR kode_klb=klb
         If FOUND()
            Klb =kode_klb
            Nklb=nama_klb
            Nr  =recno()
            Tambah=.f.
         ENDIF
         Yt='Y'
         @b,28 get nklb pict '@!'
         @24,49 get yt pict '!'
         Do pesan with 'Data Benar? Y/T : '
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         ENDIF
         If tambah
            Flok()
            APPEND BLANK
         Else
            Go nr
            Rlok()
         ENDIF
         REPLACE kode_klb with klb,nama_klb with nklb
         Ul()
         COMMIT
         *
         B=b+1
         If b>20
            B=4
         ENDIF
      ENDDO
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
Elseif lastkey()=-4
      SET CURSOR ON
      Do case
         Case fds='KODE_KLB'
            STORE kode_klb to klb,klb2
            @row(),col() get klb pict '@!'
            Read
            Do pesan with pesanx
            SET CURSOR OFF
            RETURN 1
         Othe
            Rlok()
            @row(),col() get &fds pict '@!'
            Read
            Ul()
            COMMIT
      ENDCASE
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
Elseif lastkey()=-7
   Klb=kode_klb
   SET COLOR TO gr+*
   @row(),col() say &fds
   Yt=' '
   SET CURSOR ON
   SET COLOR TO /bg
   Do pesan with 'DiHapus? Y/T'
   DO WHILE .not. yt$'YT'
      @24,47 get yt pict '!'
      Read
   ENDDO
   If yt='Y' .and. (.not. deleted())
      Rlok()
      Dele
      Ul()
      COMMIT
      Skip
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
   Set marg to 0
   ?chr(18)
   ?' DAFTAR KELOMPOK BRG'
   ?REPLACE('-',62)
   DO WHILE .not. eof()
      ?tran(no,'999  ')+kode_klb+' '+nama_klb+' '+kode_dep+' '+nama_dep
      No=no+1
      Skip
   ENDDO
   ?REPLACE('-',62)
   ?''
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 1
Else
   Do pesan with 'Kode Klp.:      '
   SET CURSOR ON
   @24,43 get klb pict '@!'
   Read
   If lastkey()=27
      SET CURSOR OFF
      Do pesan with pesanx
      @24,0
      RETURN 1
   ENDIF
   Seek klb
   If .not. FOUND()
      Do pesan2 with 'Tidak ada kode '+RTRIM(klb)
   ENDIF
   Nklb=nama_klb
   SET CURSOR OFF
   Do pesan with pesanx
   @24,0
ENDIF
RETURN 0

* LOKASI/RAK
*-----------
PROCEDURE rak
Uselan('RAK.DTA','','','','','')
Kdp ='   '
Ndep=SPACE(30)
DO WHILE .t.
   SET COLOR TO
   @1,0 CLEAR to 23,79
   SET COLOR TO /bg
   @1,22 CLEAR to 23,57
   @1,22 to 23,57
   @1,22 say chr(218)+REPLACE(gl,3)+chr(194)+REPLACE(gl,30)+chr(191)
   @2,22 say tl+'Kd.'+tl+'Nama Lokasi/Rak'+SPACE(15)+tl
   @3,22 say chr(195)+REPLACE(gl,3)+chr(197)+REPLACE(gl,30)+chr(180)
   Pesanx='F4=Tambah     F5=Edit     F8=Hapus'
   Do pesan with pesanx
   DECLARE field[5],bts[5],frm[5]
   Field[1]='KODE'
   Field[2]='NAMA'
   Frm[1]='@!'
   Frm[2]='@!'
   Bts[1]=tl
   Bts[2]=tl
   DbEdit(4,23,22,56,field,'frak',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CURSOR OFF
CLOSE all
SET CENTURY ON
RETURN

Function frak
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
      RETURN 1
Elseif lastkey()=27
      RETURN 0
Elseif lastkey()=-3
      Nr=recno()
      B=row()+1
      Go bott
      If eof()
         B=b-1
      ENDIF
      SET CURSOR ON
      DO WHILE .t.
         Klb =SPACE(3)
         Nklb=SPACE(30)
         Jml2=0
         Tambah=.t.
         @b,22 say tl+SPACE(3)+tl+SPACE(30)+tl
         Do pesan with 'ESC=Selesai'
         @b,23 get klb pict '@!'
         Read
         @24,0
         If klb=SPACE(5) .or. lastkey()=27
            Go nr
            Exit
         ENDIF
         LOCATE FOR kode=klb
         If FOUND()
            Klb =kode
            Nklb=nama
            Nr  =recno()
            Tambah=.f.
         ENDIF
         @b,27 get nklb pict '@!'
         Do pesan with 'Masukan Nama Lokari/Rak'
         Read
         Yt='Y'
         Do pesan with 'Data Benar? Y/T : '
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         ENDIF
         If tambah
            Flok()
            APPEND BLANK
            REPLACE kode with klb,nama with nklb
         Else
            Go nr
            Rlok()
            REPLACE kode with klb,nama with nklb
         ENDIF
         Ul()
         COMMIT
         *
         B=b+1
         If b>20
            B=4
         ENDIF
      ENDDO
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
Elseif lastkey()=-4
      SET CURSOR ON
      Do case
         Case fds='KODE'
            STORE kode to klb,klb2
            @row(),col() get klb pict '@!'
            Read
            Do pesan with pesanx
            SET CURSOR OFF
            RETURN 1
         Othe
            Rlok()
            @row(),col() get &fds pict '@!'
            Read
            Ul()
            COMMIT
      ENDCASE
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
Elseif lastkey()=-7
      SET COLOR TO gr+*
      @row(),col() say &fds
      Yt=' '
      SET CURSOR ON
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      DO WHILE .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         If .not. deleted()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
         ENDIF
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
      Set marg to 0
      ?chr(18)
      ?' DAFTAR LOKASI/RAK'
      ?REPLACE('-',62)
      DO WHILE .not. eof()
         ?tran(no,'999  ')+kode+' '+nama
         No=no+1
         Skip
      ENDDO
      ?REPLACE('-',62)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
ENDIF
RETURN 0

PROCEDURE warna
If idis='1'
   Warna1='g,gr+/b'    &&kop/grs.kolom
   Warna2='w,w+/b'     &&idents
   Warna3='w+,w+/r'    &&isi_item
   Warna4='w+/b,/w'    &&dafkode
   Warna5='w/r'
   Warna6='gr+'
Elseif idis='2'
   Warna1='/bg,gr+/b'
   Warna2='w/b,w+/b'
   Warna3='/bg,gr+/b'
   Warna4='w+/b,/w'
   Warna5='w+/r'
   Warna6='gr+/b'
Else
   Warna1='w,/w'
   Warna2='w,/w'
   Warna3='w,/w'
   Warna4='w,/w'
   Warna5='/w,/w'
   Warna6='w'
ENDIF
If pilwarna
   SELECT 99
   Useidx('KASSA.SET')
   LOCATE FOR no_kassa=ksa
   REPLACE id_disp with idis
   COMMIT
   Close
ENDIF
RETURN

* EOF BANT
