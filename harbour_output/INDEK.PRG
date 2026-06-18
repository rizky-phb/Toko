// ============================================================
// File    : INDEK.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

* MENU UTAMA INDEX FILE
Save scre to sstok
SET TALK OFF
// SET STAT OFF  // Note: Not supported in Harbour
// SET SCOR OFF  // Note: Not supported in Harbour
SET SAFETY OFF
SET CONSOLE OFF
SET EXCLUSIVE OFF
SET DELETED ON
Set wrap on
SET CENTURY ON
SET DATE ITALIAN
Set mess to 22 center
STORE '' to aidi1,aidi2,tgl
*
Do idents
*
STORE '' to hr,thn,bln,hri,nf0,nf1,nf2,nf3
Blnthn  ='SUBSTR(dtoc(tgl),7,4)+SUBSTR(dtoc(tgl),4,2)'
Bx=chr(176)
Parameter e
Tone(1000,1)
Tone(1500,1)
Tglx=date()
Do pisitgl
DO WHILE .t.
   Set inte on
   SET CURSOR OFF
   SET COLOR TO bg
   @2,0,21,79 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO n++
   @5,29,16,54 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO w+/b
   @0,0 CLEAR to 1,79
   @0,40-(len(RTRIM(aidi1))/2) say aidi1
   @1,0 to 1,79
   @23,0 CLEAR to 24,79
   @23,0 to 23,79
   @24,2 say hr+', '+dtoc(tglx)
   @24,70 say time()
   SET COLOR TO /bg,w+/r
   @2,0
   @2,32  say  ' RE-INDEX DATA '
   @22,0
   SET COLOR TO /bg,gr+
   @4,26 to 15,52
   @13,26 say chr(195)+REPLACE(chr(196),25)+chr(180)
   @5,27 prom  [ 1. Stok Barang. . . . . ] mess 'Re-Index File Data Stok Barang'
   @6,27 prom  [ 2. Penjualan. . . . . . ] mess 'Re-Index File Data Penjualan  ( J-'+&blnthn+'.DTA )'
   @7,27 prom  [ 3. Pembelian. . . . . . ] mess 'Re-Index File Data Pembelian ( Kasir tetap Bisa Kerja )'
   @8,27 prom  [ 4. Piutang/Pelanggan. . ] mess 'Re-Index File Data Piutang dan Pelanggan ( Kasir tetap Bisa Kerja )'
   @9,27 prom  [ 5. Hutang/Supplier. . . ] mess 'Re-Index File Data Hutang dan Supplier ( Kasir tetap Bisa Kerja )'
   @10,27 prom [ 6. Biaya/Pendapatan ll. ] mess 'Re-Index File Data Biaya & Pendapatan Lain2'
   @11,27 prom [ 7. Retur/Transfr/Mutasi ] mess 'Re-Index File Data Retur Barang, Transfer, DO dan Mutasi/Pecah Stok'
   @12,27 prom [ 8. Semua Data . . . . . ] mess 'Re-Index Semua File Data ( Kasir Berhenti dulu )'
   @14,27 prom [ Hapus File Data . . . . ] mess 'Hapus Data yang Sudah Tidak diPakai Lagi   ( Minimal HARUS 3 TAHUN YG LALU )'
   Menu to p1
   STORE .f. to satu,dua,tiga,empat,lima,enam,tujuh,batal
   Tgl=tglx
   Tot=0
   Save scre to smenu2
   If (p1 %27)=0
      SET CURSOR ON
      Do pesan with 'Anda Yakin? Y/T : '
      Yt='Y'
      @22,49 get yt pict '!'
      Read
      If lastkey()=27
         Loop
      ENDIF
      If yt='Y'
         Exit
      ENDIF
   Elseif p1=1
      Satu=.t.
   Elseif p1=2
      Dua=.t.
   Elseif p1=3
      Tiga=.t.
   Elseif p1=4
      Empat=.t.
   Elseif p1=5
      Lima=.t.
   Elseif p1=6
      Enam=.t.
   Elseif p1=7
      Tujuh=.t.
   Elseif p1=8
      STORE .t. to satu,dua,tiga,empat,lima,enam,tujuh
   Elseif p1=9
      Do hapusdt
   ENDIF
   If p1<=8
      Do ifd
   ENDIF
   Rest scre from smenu2
   If p1=8 .and. lastkey()=27
      Exit
   ENDIF
ENDDO
Tone(3500,.5)
Tone(1000,1)
SET COLOR TO
SET CURSOR ON
CLEAR all
RELEASE all
CLEAR
RETURN

*** ISI TANGGAL & KONVERSI TGL KE CHR
PROCEDURE pisitgl
SET CURSOR ON
SET COLOR TO /r
DO WHILE .t.
   Do pesan with 'Tanggal Hari Ini:           '
   @22,44 get tglx
   Read
   Exit
ENDDO
Tgl=tglx
Do hari
SET CURSOR OFF
RETURN

PROCEDURE bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
RETURN

* REINDEX FILE DATA
PROCEDURE ifd
@22,0
@22,29 say 'Tunggu, sedang di-Index'
*
If satu
   Indx('JENIS','KODE_KLB','JENIS',1)
   Indx('JENIS','NAMA_KLB','JENIS2',1)
   *
   Indx('STOK','KODE_BRG','STOK',1)
   Indx('STOK','KODE_SPL+NAMA_BRG','STOK2',1)
   Indx('STOK','KODE_KLB+NAMA_BRG','STOK3',1)
   Indx('STOK','NAMA_BRG','STOK4',1)
   Indx('STOK','KODE_BAR','STOK5',1)
ENDIF
*
If dua
   For jml=1 to 10
      Nf1='JUAL-'+LTRIM(str(jml))+'.DTA'
      If (.not. batal) .or. (.not. file('&nf1'))
         Useidx('JUAL.DTA')
         Copy to &nf1
      ENDIF
   Next
   Do bacafile
   If .not. file('&nf0')
      Useidx('JUAL.DTA')
      Copy stru to &nf0
   ENDIF
   CLOSE all
   *
   Do bacafile
   Indx('&nf0','dtos(TANGGAL)+KODE_LGN+FAKTUR','&nf1',1)
   Indx('&nf0','KODE_BAR+dtos(TANGGAL)','&nf2',1)
   Indx('&nf0','dtos(TANGGAL)+KODE_KSR+FAKTUR','&nf3',1)
   *
ENDIF
*
If tiga
   Useidx('BELI.DTA')
   For jml=1 to 10
      Nf1='BELI-'+LTRIM(str(jml))+'.DTA'
      If (.not. batal) .or. (.not. file('&nf1'))
         Copy to &nf1
      ENDIF
   Next
   *
   If .not. batal
      If .not. file('BELIIN.DTA')
         Copy stru to BELIIN.DTA
      ENDIF
   ENDIF
   CLOSE all
   Indx('BELIIN','dtos(TANGGAL)+KODE_SPL+FAKTUR','BELIIN1',2)
   Indx('BELIIN','KODE_BRG+dtos(TANGGAL)','BELIIN2',2)
   Indx('BELIIN','FAKTUR','BELIIN3',2)
   *
ENDIF
*
If empat
   Indx('CUST','KODE_LGN','CUST',1)
   Indx('CUST','NAMA_LGN','CUST2',1)
   *
   Useidx('FAKTURP.DTA')
   If flock()
      REPLACE all id_bayar with ' ' for id_bayar#' '
      Unlock
      COMMIT
   ENDIF
   Indx('FAKTURP','DTOS(TANGGAL)+KODE_LGN+FAKTUR','FAKTURP1',2)
   Indx('FAKTURP','KODE_LGN+DTOS(TANGGAL)','FAKTURP2',2)
   *
   Indx('BAYARP','DTOS(TANGGAL)+KODE_LGN+FAKTUR','BAYARP1',1)
   Indx('BAYARP','KODE_LGN+DTOS(TANGGAL)+FAKTUR','BAYARP2',1)
   *
   Indx('POINTAM','DTOS(TANGGAL)+KODE_LGN','POINTAM1',2)
   Indx('POINTAM','KODE_LGN+DTOS(TANGGAL)','POINTAM2',2)
   *
ENDIF
*
If lima
   Indx('SUPP','KODE_SPL','SUPP',1)
   Indx('SUPP','NAMA_SPL','SUPP2',1)
   *
   Useidx('FAKTURH.DTA')
   If flock()
      REPLACE all id_bayar with ' ' for id_bayar#' '
      Unlock
      COMMIT
   ENDIF
   Indx('FAKTURH','dtos(TANGGAL)+KODE_SPL+FAKTUR','FAKTURH1',1)
   Indx('FAKTURH','KODE_SPL+dtos(TANGGAL)+FAKTUR','FAKTURH2',1)
   Indx('FAKTURH','KODE_SPL+FAKTUR','FAKTURH3',1)
   *
   Indx('BAYARH','dtos(TANGGAL)+KODE_SPL+FAKTUR','BAYARH1',1)
   Indx('BAYARH','KODE_SPL+dtos(TANGGAL)','BAYARH2',1)
   *
ENDIF
*
If enam
   Indx('POSAKUN','KODE_POS','POSAKUN',1)
   *
   Nf0='KAS-'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
   Nf1='KAS1'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
   Nf2='KAS2'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
   If .not. file('&nf0')
      Useidx('KAS.DTA')
      Copy stru to &nf0
      Close
   ENDIF
   Indx('&nf0','dtos(TANGGAL)+KODE_POS+ID_POS','&nf1',2)
   Indx('&nf0','KODE_POS+dtos(TANGGAL)','&nf2',1)
   *
ENDIF
*
If tujuh
   Uselan('RETURB.DTA','','','','','')
   If .not. file('RETURBN.DTA')
      Copy stru to RETURBN.DTA
   ENDIF
   CLOSE all
   Indx('RETURBN','dtos(TANGGAL)+KODE_SPL+FAKTUR','RETURBN1',1)
   Indx('RETURBN','KODE_KLB+KODE_BAR+dtos(TANGGAL)','RETURBN2',1)
   Indx('RETURBN','KODE_SPL+dtos(TANGGAL)','RETURBN3',1)
   *
   Uselan('ADJUST.DTA','','','','','')
   If .not. file('ADJUSTN.DTA')
      Copy stru to ADJUSTN.DTA
   ENDIF
   CLOSE all
   Indx('ADJUSTN','DTOS(TANGGAL)+FAKTUR+NAMA_BRG','ADJUSTN1',2)
   *
   Useidx('FAKTURO.DTA')
   Indx('FAKTURO','dtos(TANGGAL)+KODE_SPL+FAKTUR','FAKTURO1',1)
   Indx('FAKTURO','KODE_SPL+dtos(TANGGAL)+FAKTUR','FAKTURO2',1)
   *
   Indx('PECAHIN','DTOS(TANGGAL)','PECAHIN1',1)
   *
ENDIF
@17,28 CLEAR to 21,52
@17,28 to 21,52
@19,30 say '  Re-Index Selesai.   '
Do pesan with 'ESC=Keluar'
Wait''
*
RETURN

***Indeks File
FUNCTION Indx
PARAMETERS Arg4,arg5,arg6,arg3
SET COLOR TO w+*/r
@17,28 CLEAR to 21,52
@17,28 to 21,52
SET COLOR TO w+/r
@19,30 say 'File: '+SPACE(16)
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   Tot=tot+arg3
   @17,40 say LTRIM(str(tot))+' %'
   @19,36 say arg6
   Use &arg4 exclusive
   If !NetErr()
      Dele file &arg6
      INDEX ON &arg5 to &arg6
      CLOSE all
   ENDIF
ENDIF
RETURN .T.

***Buka File u/ Reindex-Exclusive
FUNCTION UseIdx
PARAMETERS Arg1
Batal=.f.
DO WHILE .t.
   If .not. file('&arg1')
      Tone(700,6)
      Do pesan with 'Tidak Ada File '+arg1
      Wait''
      Exit
   ENDIF
   Use &arg1 exclusive
   If (!NetErr())
      Exit
   ENDIF
   Tone(700,6)
   Do pesan with 'Ada User (sama) yg Sedang On-Line. ('+arg1+')'
   Inkey(0.5)
   Batal=.t.
   Exit
ENDDO
RETURN(.T.)

***Buka File di LAN
FUNCTION UseLan
PARAMETERS Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   If .not. file('&arg1')
      Tone(700,6)
      Do pesan with 'Tidak Ada File '+arg1
      Wait''
      Exit
   ENDIF
   Use &arg1
   If (!NetErr())
      Exit
   ENDIF
   Do pesan with 'Tunggu, File '+&arg1 +'Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
SET INDEX TO &arg2,&arg3,&arg4,&arg5,&arg6
RETURN(.T.)

PROCEDURE pesan
PARAMETERS psn
SET COLOR TO /r
@22,0 say SPACE(80)
SET COLOR TO w+/r
@22,40-(len(psn)/2) say psn
SET COLOR TO g
RETURN

PROCEDURE hari
Hh=dow(tglx)
Do case
   Case hh=1
      Hr='Minggu'
   Case hh=2
      Hr='Senin'
   Case hh=3
      Hr='Selasa'
   Case hh=4
      Hr='Rabu'
   Case hh=5
      Hr='Kamis'
   Case hh=6
      Hr="Jum'at"
   Case hh=7
      Hr='Sabtu'
ENDCASE
RETURN

PROCEDURE hapusdt
SET CURSOR ON
Ket=SPACE(7)
STORE tglx-365 to tgl,tgl2
Yt=' '
DO WHILE .t.
   @6,15 CLEAR to 20,58
   @6,15 to 20,58
   SET COLOR TO /bg*
   @6,30  say ' PERHATIAN...!'
   SET COLOR TO /bg
   @7,18  say 'Proses Ini akan  Menghapus  SEMUA  Data'
   @8,18  say 'Penjualan, Pembelian, Retur & Transfer.'
   @10,18 say 'Pastikan Semua Transaksi Sudah diCetak!'
   @11,18 say 'Karena Data yang Sudah diHapus Tdk Bisa'
   @12,18 say 'diKembalikan Lagi...'
   @14,18 say 'DiAnjurkan yg akan diHapus adalah  Data'
   @15,18 say 'yang Sudah Tidak terPakai Lagi.'
   @17,18 say 'Ketik DIHAPUS untuk konfirmasi:'
   @17,50 get ket pict '@!'
   Read
   If lastkey()=27
      Exit
   ENDIF
   If ket#'DIHAPUS' .and. ket#'RESET00'
      Loop
   ENDIF
   If ket='RESET00'
      STORE tglx-1825 to tgl,tgl2
      @18,18 say ' Hati-Hati, Hanya u/ Situasi "GAWAT" !'
      @19,18 say '   Semua Data Transaksi akan diHapus.'
   Elseif ket='DIHAPUS'
      @18,18 say 'Hapus Tanggal:           s/d'
      @18,32 get tgl
      @18,47 get tgl2
   ENDIF
   DO WHILE .not. yt$'YT'
      @20,30 say 'Proses? Y/T'get yt pict '!'
      Read
      If lastkey()=27
         Exit
      ENDIF
   ENDDO
   If yt='Y' .and. ket='DIHAPUS'
      Do konvtgl
      Useidx('FAKTURP.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('BAYARP.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('BELIIN.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('FAKTURH.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('BAYARH.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('KIRIMIN.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('RETURBN.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      Useidx('RETURJN.DTA')
      Dele for (tanggal>=tgl .and. tanggal<=tgl2)
      Pack
      Close
      *
      DO WHILE tgl<=tgl2
         STORE ' ' to bln,thn
         Do konvtgl
         Nf1='STO_'+bln+SUBSTR(thn,3,2)+'.DTA'
         If file('&nf1')
            Do pesan with 'Tunggu, sedang Hapus '+nf1
            Dele file &nf1
         ENDIF
         *
         Bacafile('J')
         If file('&nf0')
            Do pesan with 'Tunggu, sedang Hapus '+nf0
            Dele file &nf0
            Dele file &nf1
            Dele file &nf2
            Dele file &nf3
         ENDIF
         *
         Bacafile('B')
         If file('&nf0')
            Do pesan with 'Tunggu, sedang Hapus '+nf0
            Dele file &nf0
            Dele file &nf1
            Dele file &nf2
         ENDIF
         *
         Tgl=tgl+1
      ENDDO
   Elseif yt='Y' .and. ket='RESET00'
      @6,15 CLEAR to 20,58
      @6,15 to 20,58
      @13,24 say 'Tunggu, sedang proses...'
      Do konvtgl
      Useidx('FAKTURP.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('BAYARP.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('BELIIN.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('FAKTURH.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('BAYARH.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('KIRIMIN.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('RETURBN.DTA')
      Dele all
      Pack
      Close
      *
      Useidx('RETURJN.DTA')
      Dele all
      Pack
      Close
      *
      Uselan('STOK.DTA','','','','','')
      Flock()
      REPLACE all stok_a with (stok_a+stok_m)-stok_k,stok_m with 0,stok_k with 0,stok_ag with 0,stok_mg with 0,stok_kg with 0
      REPLACE all tanggal_1 with ctod('  -  -    '),tanggal_2 with ctod('  -  -    ')
      Unlock
      COMMIT
      Close
      *
      Dele file TOP-RANK.QTY
      Dele file TOP-RANK.RPH
      Dele file TOP-RANK.TMP
      *
      DO WHILE tgl<=tglx
         STORE ' ' to bln,thn
         Do konvtgl
         Nf1='STO_'+bln+SUBSTR(thn,3,2)+'.DTA'
         If file('&nf1')
            Do pesan with 'Tunggu, sedang Hapus '+nf1
            Dele file &nf1
         ENDIF
         *
         Bacafile('J')
         If file('&nf0')
            Do pesan with 'Tunggu, sedang Hapus '+nf0
            Dele file &nf0
            Dele file &nf1
            Dele file &nf2
            Dele file &nf3
         ENDIF
         *
         Bacafile('B')
         If file('&nf0')
            Do pesan with 'Tunggu, sedang Hapus '+nf0
            Dele file &nf0
            Dele file &nf1
            Dele file &nf2
         ENDIF
         *
         Nf0='KAS-'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
         Nf1='KAS1'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
         Nf2='KAS2'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
         If file('&nf0')
            Do pesan with 'Tunggu, sedang Hapus '+nf0
            Dele file &nf0
            Dele file &nf1
            Dele file &nf2
         ENDIF
         *
         Tgl=tgl+1
      ENDDO
   ENDIF
   If yt='Y'
      STORE .t. to satu,dua,tiga,empat,lima,enam,tujuh
      Do ifd
      *
      Tone(3500,.5)
      Tone(1000,1)
      SET COLOR TO
      SET CURSOR ON
      CLEAR all
      RELEASE all
      CLEAR
      Quit
   ENDIF
ENDDO
SET COLOR TO g
CLOSE all
RETURN

PROCEDURE konvtgl
Hri=left(dtoc(tgl),2)
Bln=SUBSTR(dtoc(tgl),4,2)
Thn=LTRIM(str(year(tgl)))
RETURN

* EOF INDEK
