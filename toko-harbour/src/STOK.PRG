PROCEDURE MAIN
* MENU UTAMA
Save scre to sstok
Set talk off
Set stat off
Set scor off
Set safe off
Set cons off
Set exclusive off
Set conf on
Set dele on
Set wrap on
Set fixe on
Set cent on
Set deci to 0
Set date ital
Set proc to dafkode
Set proc to paswod
Set proc to bant
Set proc to lhut
Set proc to lpiu
Set proc to kas
Set mess to 22 center
Clea
Stor '' to aidi1,aidi2,tgl,tgl2
*
Do idents
*
Stor '' to nlp,cc,hr,thn,bln,hri,brs1,brs12,brs13,brs14,fkt,lks,xcariso,nf0,nf1,nf2,nf3,xpos,warna1,warna2,warna3,warna4,warna5,warna6,idis
Stor .f. to keluar,Super,Owner,xOwner,Orglain,ada,klp,spl,pilwarna
Gtlayar ="'&cfx->'+ltri(str(cf))+'   ENTER=Hal.Berikut    BACKSPC=Hal.Seblmnya    ESC=Menu Selanjutnya'"
Gtkertas="Ganti kertas baru,     F10=Lanjutkan     ESC=Batal"
Blnthn  ='subs(dtoc(tgl),7,4)+subs(dtoc(tgl),4,2)'
Ksa=gete("KASSA")
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
   Set curs on
   Clea all
   Clea
   Retu
Endi
Tone(1000,1)
Tone(1500,1)
Do pisitgl
Do whil .t.
   Set inte on
   Set curs off
   Set colo to bg
   @2,0,21,79 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to w+/b
   @0,0 clea to 1,79
   @0,40-(len(rtri(aidi1))/2) say aidi1
   @1,0 to 1,79
   @23,0 clea to 24,79
   @23,0 to 23,79
   @24,2 say hr+', '+dtoc(tglx)
   @24,70 say time()
   Set colo to /bg,w+/r
   @2,0
   @22,0
   @2,4  prom [ File ]      mess 'Tabel Barang, Kelompok, Pelanggan, Supplier dan Perkiraan Kas/Biaya'
   @2,16 prom [ Transaksi ] mess 'Input Transaksi PO, Pembelian dan Retur'
   @2,33 prom [ Laporan ]   mess 'Laporan Barang, Penjualan, Pembelian, Retur, Transfer'
   @2,48 prom [ Utility ]   mess 'Posting, Password-Login dan Tanggal'
   @2,64 prom [ Exit ]      mess 'Exit Program'
   Menu to p1
   If pcou()=0
      Set colo to w+/b
      @24,70 say time()
   Endi
   If (p1 %27)=0
      Yt='Y'
      Set curs on
      Set colo to /bg*
      @11,29 say ' Selesai/Keluar? Y/T:'get yt pict '!'
      Read
      If yt='Y' .and. lastkey()#27
         Exit
      Endi
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
   Endi
Endd
Tone(3500,.5)
Tone(1000,1)
Set colo to
Set curs on
Clea all
Rele all
Clea
Retu

Proc ftabel
kKanan='chr(84)'
kKiri='chr(69)'
Do whil .t.
   Set key  4 to kanan
   Set key 19 to kiri
   Set colo to n++
   @4,4,12,17 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to /bg,w+/r
   @3,3 to 11,16
   @9,3 say chr(195)+repl(chr(196),12)+chr(180)
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
   Endi
   Rest scre from smenu2
Endd
Retu

Proc intran
kKanan='chr(76)'
kKiri ='chr(70)'
Do whil .t.
   Tgl=tglx
   Set key  4 to kanan
   Set key 19 to kiri
   Set colo to n++
   @4,16,10,34 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to /bg,w+/r
   @3,15 to 9,33
   @7,15 say chr(195)+repl(chr(196),17)+chr(180)
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
   Endi
   Rest scre from smenu2
Endd
Retu

Proc laporan            
kKanan='chr(85)'
kKiri ='chr(84)'
Do whil .t.
   Tgl=tglx
   Set key  4 to kanan
   Set key 19 to kiri
   Set colo to n++
   @4,33,14,49 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to /bg,w+/r
   @3,32 to 13,48
   @9,32 say chr(195)+repl(chr(196),15)+chr(180)
   @22,0 
   @4,33 prom  [ Barang.......?] mess 'Laporan Stok Barang'
   @5,33 prom  [ Pembelian....?] mess 'Laporan Penjualan'
   @6,33 prom  [ Penjualan....?] mess 'Laporan Penjualan'
   @7,33 prom  [ R e t u r....?] mess 'Laporan Retur Pembelian Barang'
   @8,33 prom  [ Opname/Mutasi?] mess 'Laporan Stok Opname dan Mutasi/Pecah Stok Barang'
   @10,33 prom [ Point Belanja?] mess 'Laporan Point Belanja'
   @11,33 prom [ Pelanggan....?] mess 'Laporan Piutang Pelanggan'
   @12,33 prom [ Supplier.....?] mess 'Laporan Hutang Supplier'
   Menu to p1
   Set key  4 to
   Set key 19 to
   Save scre to smenu2
   If (p1 %27)=0
      Exit
   Elseif p1=1
      Do whil .t.
         Set colo to w/b,w+/gr
         @5,34 clea to 11,49
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=2
      Do whil .t.
         Set colo to w/b,w+/gr
         @6,34 clea to 9,56
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=3
      Do whil .t.
         Set colo to w/b,w+/gr
         @7,34 clea to 19,57
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=4
      Do whil .t.
         Set colo to w/b,w+/gr
         @8,34 clea to 11,50
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=5
      Do whil .t.
         Set colo to w/b,w+/gr
         @9,34 clea to 13,55
         @9,34 to 13,55
         @11,34 say chr(195)+repl(chr(196),20)+chr(180)
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=6
      Do whil .t.
         Set colo to w/b,w+/gr
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=7
      Do whil .t.        
         Set colo to w/b,w+/gr
         @12,34 clea to 16,54
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
         Endi
         Rest scre from smenu2
      Endd
   Elseif p1=8
      Do whil .t.
         Set colo to w/b,w+/gr
         @13,34 clea to 17,54
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
         Endi
         Rest scre from smenu2
      Endd
   Endi
   Rest scre from smenu2
Endd
Retu

Proc yutiliti
kKanan='chr(69)'
kKiri ='chr(76)'
Do whil .t.
   Set key  4 to kanan
   Set key 19 to kiri
   Set colo to n++
   @4,48,11,63 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to /bg,w+/r
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
   Endi
   Rest scre from smenu2
Endd
Retu

Proc keluar
kKanan='chr(70)'
kKiri ='chr(85)'
Do whil .t.
   Set key  4 to kanan
   Set key 19 to kiri
   Set colo to n++
   @4,64,7,72 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to /bg,w+/r
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
   Endi
   Rest scre from sfil
   Exit
Endd
Retu

Proc kanan
Keyboard chr(27)+&kKanan
Retu          

Proc kiri
Keyboard chr(27)+&kKiri
Retu

*** ISI TANGGAL & KONVERSI TGL KE CHR
Proc pisitgl
Set curs on
Set colo to /r
Do whil .t.
   Do pesan with 'Tanggal Hari Ini:           '
   @24,44 get tglx
   Read
   Exit
Endd
Tgl=tglx
Do cekdata
Do hari
Do warna
Set curs off
Retu

* EOF STOK
