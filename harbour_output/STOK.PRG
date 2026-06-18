// ============================================================
// File    : STOK.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

* MENU UTAMA
Save scre to sstok
SET TALK OFF
// SET STAT OFF  // Note: Not supported in Harbour
// SET SCOR OFF  // Note: Not supported in Harbour
SET SAFETY OFF
SET CONSOLE OFF
SET EXCLUSIVE OFF
SET CONFIRM ON
SET DELETED ON
Set wrap on
SET FIXED ON
SET CENTURY ON
SET DECIMALS TO 0
SET DATE ITALIAN
SET PROCEDURE TO dafkode
SET PROCEDURE TO paswod
SET PROCEDURE TO bant
SET PROCEDURE TO lhut
SET PROCEDURE TO lpiu
SET PROCEDURE TO kas
Set mess to 22 center
CLEAR
STORE '' to aidi1,aidi2,tgl,tgl2
*
Do idents
*
STORE '' to nlp,cc,hr,thn,bln,hri,brs1,brs12,brs13,brs14,fkt,lks,xcariso,nf0,nf1,nf2,nf3,xpos,warna1,warna2,warna3,warna4,warna5,warna6,idis
STORE .f. to keluar,Super,Owner,xOwner,Orglain,ada,klp,spl,pilwarna
Gtlayar ="'&cfx->'+LTRIM(str(cf))+'   ENTER=Hal.Berikut    BACKSPC=Hal.Seblmnya    ESC=Menu Selanjutnya'"
Gtkertas="Ganti kertas baru,     F10=Lanjutkan     ESC=Batal"
Blnthn  ='SUBSTR(dtoc(tgl),7,4)+SUBSTR(dtoc(tgl),4,2)'
Ksa=GetEnv("KASSA")
Lks=' '
Gd=chr(205)
Gl=chr(196)
Tl=chr(179)
Bx=chr(176)
Tglx=date()
Baris=row()
Parameter e
Hasil=.t.
Do pm
If (.not. hasil) .or. Super
   SET CURSOR ON
   CLEAR all
   CLEAR
   RETURN
ENDIF
Tone(1000,1)
Tone(1500,1)
Do pisitgl
DO WHILE .t.
   Set inte on
   SET CURSOR OFF
   SET COLOR TO bg
   @2,0,21,79 box bx+bx+bx+bx+bx+bx+bx+bx+bx
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
   @22,0
   @2,4  prom [ File ]      mess 'Tabel Barang, Kelompok, Pelanggan, Supplier dan Perkiraan Kas/Biaya'
   @2,16 prom [ Transaksi ] mess 'Input Transaksi PO, Pembelian dan Retur'
   @2,33 prom [ Laporan ]   mess 'Laporan Barang, Penjualan, Pembelian, Retur, Transfer'
   @2,48 prom [ Utility ]   mess 'Posting, Password-Login dan Tanggal'
   @2,64 prom [ Exit ]      mess 'Exit Program'
   Menu to p1
   If pcou()=0
      SET COLOR TO w+/b
      @24,70 say time()
   ENDIF
   If (p1 %27)=0
      Yt='Y'
      SET CURSOR ON
      SET COLOR TO /bg*
      @11,29 say ' Selesai/Keluar? Y/T:'get yt pict '!'
      Read
      If yt='Y' .and. lastkey()#27
         Exit
      ENDIF
   Elseif p1=1
      Save scre to smenu
      Do ftabel
      Rest scre from smenu
   Elseif p1=2
      Save scre to smenu
      Do intran
      Rest scre from smenu
   Elseif p1=3
      Save scre to smenu
      Do laporan
      Rest scre from smenu
   Elseif p1=4
      Save scre to smenu
      Do yutiliti
      Rest scre from smenu
   Elseif p1=5
      Save scre to smenu
      Do keluar
      Rest scre from smenu
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

PROCEDURE ftabel
kKanan='chr(84)'
kKiri='chr(69)'
DO WHILE .t.
   Set key  4 to kanan
   Set key 19 to kiri
   SET COLOR TO n++
   @4,4,12,17 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO /bg,w+/r
   @3,3 to 11,16
   @9,3 say chr(195)+REPLACE(chr(196),12)+chr(180)
   @22,0
   @4,4 prom  [ Barang.... ] mess 'Tabel Barang'
   @5,4 prom  [ Kelompok.. ] mess 'Tabel Kelompok/Kategori Barang'
   @6,4 prom  [ Lokasi/Rak ] mess 'Tabel Lokasi/Rak Barang'
   @7,4 prom  [ Pelanggan. ] mess 'Tabel Pelanggan'
   @8,4 prom  [ Supplier.. ] mess 'Tabel Supplier'
   @10,4 prom [ Perkiraan. ] mess 'Tabel Perkiraan Kas/Biaya'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to smenu2
   If (p1 %27)=0
      Exit
   Elseif p1=1
      Do x with 'Tabel Barang'
      Do lst1
   Elseif p1=2
      Do x with 'Tabel Kelompok/Kategori Barang'
      Do klp
   Elseif p1=3
      Do x with 'Tabel Lokasi/Rak'
      Do rak
   Elseif p1=4
      Spl=.f.
      Do x with 'Tabel Pelanggan'
      Do lgn
   Elseif p1=5
      Spl=.t.
      Do x with 'Tabel Supplier'
      Do spl
   Elseif p1=6
      Do x with 'Tabel Perkiraan Kas/Biaya'
      Do prk
   ENDIF
   Rest scre from smenu2
ENDDO
RETURN

PROCEDURE intran
kKanan='chr(76)'
kKiri ='chr(70)'
DO WHILE .t.
   Tgl=tglx
   Set key  4 to kanan
   Set key 19 to kiri
   SET COLOR TO n++
   @4,16,10,34 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO /bg,w+/r
   @3,15 to 9,33
   @7,15 say chr(195)+REPLACE(chr(196),17)+chr(180)
   @22,0
   @4,16 prom  [ P O ........... ] mess 'Input Transaksi PO (Purchase Order)'
   @5,16 prom  [ Pembelian...... ] mess 'Input Transaksi Pembelian'
   @6,16 prom  [ Retur Pembelian ] mess 'Input Transaksi Retur Pembelian'
   @8,16 prom  [ Stok Opname.... ] mess 'Input Stok Opname'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to smenu2
   If (p1 %27)=0
      Exit
   Elseif p1=1
      Do x with 'P O'
      Do mpo
   Elseif p1=2
      Do x with 'Pembelian'
      Do mpb
   Elseif p1=3
      Do x with 'Retur Pembelian'
      Do rtb
   Elseif p1=4
      Do x with 'Stok Opname'
      Do adj
   ENDIF
   Rest scre from smenu2
ENDDO
RETURN

PROCEDURE laporan            
kKanan='chr(85)'
kKiri ='chr(84)'
DO WHILE .t.
   Tgl=tglx
   Set key  4 to kanan
   Set key 19 to kiri
   SET COLOR TO n++
   @4,33,14,49 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO /bg,w+/r
   @3,32 to 13,48
   @9,32 say chr(195)+REPLACE(chr(196),15)+chr(180)
   @22,0 
   @4,33 prom  [ Barang.......¯] mess 'Laporan Stok Barang'
   @5,33 prom  [ Pembelian....¯] mess 'Laporan Penjualan'
   @6,33 prom  [ Penjualan....¯] mess 'Laporan Penjualan'
   @7,33 prom  [ R e t u r....¯] mess 'Laporan Retur Pembelian Barang'
   @8,33 prom  [ Opname/Mutasi¯] mess 'Laporan Stok Opname dan Mutasi/Pecah Stok Barang'
   @10,33 prom [ Point Belanja¯] mess 'Laporan Point Belanja'
   @11,33 prom [ Pelanggan....¯] mess 'Laporan Piutang Pelanggan'
   @12,33 prom [ Supplier.....¯] mess 'Laporan Hutang Supplier'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to smenu2
   If (p1 %27)=0
      Exit
   Elseif p1=1
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @5,34 CLEAR to 11,49
         @5,34 to 11,49
         @6,35 prom  [ per Kelompok ]
         @7,35 prom  [ per Supplier ]
         @8,35 prom  [ Total/Semua. ]
         @9,35 prom  [ Stok Minimal ]
         @10,35 prom [ Bulan Lalu.. ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Klp=.t.
            Spl=.f.
            Do x with 'Stok Barang / Kelompok'
            Do lst2
         Elseif p12=2
            Klp=.f.
            Spl=.t.
            Do x with 'Stok Barang / Supplier'
            Do lst2
         Elseif p12=3
            Do x with 'Stok Semua Barang'
            Do lst13
         Elseif p12=4
            Do x with 'Stok Stok Minimal'
            Do lst14
         Elseif p12=5
            Do x with 'Persediaan Stok Bulan Lalu'
            Do lstlalu
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=2
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @6,34 CLEAR to 9,56
         @6,34 to 9,56
         @7,35 prom  [ T/K per Supplier... ]
         @8,35 prom  [ Rinci per Item Brg. ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Hut=.t.
            Do x with 'Laporan Pembelian'
            Do lpb1
         Elseif p12=2
            Do x with 'Lap.Pembelian per Item Brg'
            Do lpb2
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=3
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @7,34 CLEAR to 19,57
         @7,34 to 19,57
         *@18,34 say chr(195)+repl(chr(196),22)+chr(180)
         @8,35 prom  [ Tunai  per Tanggal.. ]
         @9,35 prom  [ Tn/Krd per Pelanggan ]
         @10,35 prom [ Rinci per Plgn/Item. ]
         @11,35 prom [ Rinci per Klp. Brg.. ]
         @12,35 prom [ Rinci per No.Nota... ]
         @13,35 prom [ Total per No.Nota... ]
         @14,35 prom [ Total per Klp. Brg.. ]
         @15,35 prom [ Total per Hari...... ]
         @16,35 prom [ Total per Bulan..... ]
         @17,35 prom [ Total per Kasir..... ]
         @18,35 prom [ Total Top Rangking.. ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Hut=.f.
            Do x with 'Laporan Penjualan Tunai'
            Do lpj1
         Elseif p12=2
            Hut=.t.
            Do x with 'Lap.Penjualan per Pelanggan'
            Do lpj12      
         Elseif p12=3
            Do x with 'Lap.Penjualan per Item/Plgn'
            Do lpj2
         Elseif p12=4
            Do x with 'Lap.Penjualan per Klp.Barang'
            Do lpj21
         Elseif p12=5
            Do x with 'Rincian Barang per No.Nota'
            Do lpj4
         Elseif p12=6
            Do x with 'Lap.Penjualan per Nota'
            Do lpj41
         Elseif p12=7
            Do x with 'Lap.Penjualan Total / Klp'
            Do lpj23
         Elseif p12=8
            Do x with 'Rekap Penjualan per Hari'
            Do lpj3
         Elseif p12=9
            Do x with 'Rekap Penjualan per Bulan'
            Do lpj31
         Elseif p12=10
            Do x with 'Laporan Penjualan Total per Kasir'
            Do lpj32
         Elseif p12=11
            Do x with 'Laporan Penjualan Barang TerLaris'
            Do lpj99
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=4
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @8,34 CLEAR to 11,50
         @8,34 to 11,50
         *@11,34 say chr(195)+repl(chr(196),15)+chr(180)
         @9,35 prom  [ Pemb.per Tgl. ]
         @10,35 prom [ Pemb.per Item ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Hut=.f.
            Do x with 'Laporan Retur Pembelian'
            Do lrtb
         Elseif p12=2
            Do x with 'Laporan Retur Pembelian per Item Barang'
            Do lrtb2
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=5
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @9,34 CLEAR to 13,55
         @9,34 to 13,55
         @11,34 say chr(195)+REPLACE(chr(196),20)+chr(180)
         @10,35 prom [ Opname per Tanggal ]
         @12,35 prom [ Mutasi/Pecah Brg.. ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Do x with 'Laporan Stok Opname per Tanggal'
            Do ladj
         Elseif p12=2
            Do x with 'Laporan Mutasi/Pecah Stok Barang'
            Do lpch
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=6
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @11,34 to 15,51
         @12,35 prom [ Rincian Poin.. ]
         @13,35 prom [ Rekap Poin.... ]
         @14,35 prom [ Lap.Ambil Poin ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Do x with 'Perincian Point Belanja'
            Do poin
         Elseif p12=2
            Do x with 'Laporan Global Point Belanja'
            Do poin2
         Elseif p12=3
            Do x with 'Laporan Pengambilan Point'
            Do lambpoin
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=7
      DO WHILE .t.        
         SET COLOR TO w/b,w+/gr
         @12,34 CLEAR to 16,54
         @12,34 to 16,54
         @13,35 prom [ Rincian Faktur... ]
         @14,35 prom [ Buku Piutang..... ]
         @15,35 prom [ Daftar Pembayaran ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Hut=.f.
            Do x with 'Daftar Faktur Penjualan'
            Do fktp
         Elseif p12=2
            Hut=.f.
            Do x with 'Daftar Piutang'
            Do lpiu1
         Elseif p12=3
            Do x with 'Daftar Pembayaran Piutang'
            Do lbyrp
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=8
      DO WHILE .t.
         SET COLOR TO w/b,w+/gr
         @13,34 CLEAR to 17,54
         @13,34 to 17,54
         @14,35 prom [ Rincian Faktur... ]
         @15,35 prom [ Buku Hutang...... ]
         @16,35 prom [ Daftar Pembayaran ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Hut=.t.
            Do x with 'Daftar Faktur Pembelian'
            Do fkth
         Elseif p12=2
            Hut=.t.
            Do x with 'Daftar Hutang'
            Do lhut1
         Elseif p12=3
            Do x with 'Daftar Pembayaran Hutang'
            Do lbyrh
         ENDIF
         Rest scre from smenu2
      ENDDO
   ENDIF
   Rest scre from smenu2
ENDDO
RETURN

PROCEDURE yutiliti
kKanan='chr(69)'
kKiri ='chr(76)'
DO WHILE .t.
   Set key  4 to kanan
   Set key 19 to kiri
   SET COLOR TO n++
   @4,48,11,63 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO /bg,w+/r
   @3,47 to 10,62
   @22,0
   @4,48 prom  [ Setup User.. ] mess 'Setup User/Kasir'
   @5,48 prom  [ Lain-lain... ] mess 'Lain-lain'
   @6,48 prom  [ Pembulatan.. ] mess 'Set-Up Pembulatan Total Rupiah di Penjualan'
   @7,48 prom  [ Ganti Pswd.. ] mess 'Ganti Password'
   @8,48 prom  [ Ganti Pswd 2 ] mess 'Ganti Password Penjualan'
   @9,48 prom  [ Tanggal..... ] mess 'Tanggal Hari ini'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to smenu2
   If (p1 %27)=0
      Exit
   Elseif p1=1 .and. xOwner
      Do x with 'Setup User/Kasir'
      Do ksr
   Elseif p1=2
      Do x with 'Lain-lain'
      Do lain2
   Elseif p1=3
      Do x with 'Set-Up Pembulatan Total Rp. di Penjualan'
      Do rpbulat
   Elseif p1=4
      Do x with 'Ganti Password'
      Do pu
   Elseif p1=5
      Do x with 'Ganti Password Penjualan'
      Do pu2
   Elseif p1=6
      Do pisitgl
   ENDIF
   Rest scre from smenu2
ENDDO
RETURN

PROCEDURE keluar
kKanan='chr(70)'
kKiri ='chr(85)'
DO WHILE .t.
   Set key  4 to kanan
   Set key 19 to kiri
   SET COLOR TO n++
   @4,64,7,72 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO /bg,w+/r
   @3,63 to 6,71
   @22,0
   @4,64 prom  [ Ya... ] mess 'Keluar ke DOS - C:\>'
   @5,64 prom  [ Tidak ] mess 'Lanjut?'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to sfil
   If (p1 %27)=0
      Exit
   Elseif p1=1
      Keyboard chr(27)+chr(13)
   Elseif p1=2
      *
   ENDIF
   Rest scre from sfil
   Exit
ENDDO
RETURN

PROCEDURE kanan
Keyboard chr(27)+&kKanan
RETURN          

PROCEDURE kiri
Keyboard chr(27)+&kKiri
RETURN

*** ISI TANGGAL & KONVERSI TGL KE CHR
PROCEDURE pisitgl
SET CURSOR ON
SET COLOR TO /r
DO WHILE .t.
   Do pesan with 'Tanggal Hari Ini:           '
   @24,44 get tglx
   Read
   Exit
ENDDO
Tgl=tglx
Do cekdata
Do hari
Do warna
SET CURSOR OFF
RETURN

* EOF STOK
