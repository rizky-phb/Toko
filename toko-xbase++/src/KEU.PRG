Procedure KEU
* MENU UTAMA KEUANGAN
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
Set cent on
Set date ital
Set mess to 22 center
Set proc to paswod2
Set proc to bant
Set proc to dafkode
Set proc to lhut
Set proc to lpj1
Set proc to lpj2
Set proc to kas
Set proc to lkeu
Clea
Stor '' to aidi1,aidi2,tgl,tgl2
*
Do idents
*
Stor '' to nlp,cc,hr,thn,bln,hri,nf0,nf1,nf2,nf3,nlp,xpos,brs1,brs12,brs13,brs14,pSuper,pOwner,pxOwner
Stor .f. to Super,Owner,xOwner,Orglain,klp,spl
Gtkertas="Ganti kertas baru,     F10=Lanjutkan     ESC=Batal"
Gtlayar ="'&cfx->'+ltri(str(cf))+'   ENTER=Hal.Berikut   BACKSPC=Hal.Seblmnya   ESC=Menu Selanjutnya'"
Blnthn  ='subs(dtoc(tgl),7,4)+subs(dtoc(tgl),4,2)'
Ksa=gete("KASSA")
Gd =chr(205)
Gl =chr(196)
Tl =chr(179)
Bx =chr(176)
Parameter e
Hasil=.t.
Do pm
If (.not. hasil) .or. ( .not. xOwner )
   Set curs on
   Clea all
   Clea
Return
EndIf
Tone(1000,1)
Tone(1500,1)
Tglx=date()
Do pisitgl
Uselan('DATA.SET','','','','','')
xPos=rtri(keterangan)
Close
Do While .t.
   Set inte on
   Set curs off
   Set colo to bg
   @2,0,21,79 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   Set colo to n++
   @5,31,14,50 box bx+bx+bx+bx+bx+bx+bx+bx+bx
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
   @2,34  say  'ADM.KEUANGAN'
   @22,0
   Set colo to /bg,gr+
   @4,29 clea to 13,49
   @4,29 to 13,49
   @6,29 say chr(195)+repl(gl,19)+chr(180)
   @10,29 say chr(195)+repl(gl,19)+chr(180)
   @5,30  prom [ A. Input Biaya... ] mess 'Input Kas/Biaya'
   @7,30  prom [ B. Laporan Biaya. ] mess 'Laporan Biaya'
   @8,30  prom [ C. Laporan Penjln ] mess 'Laporan Penjualan'
   @9,30  prom [ D. Supplier...... ] mess 'Laporan Supplier'
   @11,30 prom [ E. Daft.Perkiraan ] mess 'Daftar Perkiraan Kas/Biaya'
   @12,30 prom [ F. Ganti Password ] mess 'Ganti Password'
   Menu to p1
   Tgl=tglx
   Save scre to smenu2
   If (p1 %27)=0
      Set curs on
      Yt='Y'
      @22,0
      @22,30 say 'Anda Yakin? Y/T :'get yt pict '!'
      Read
      If lastkey()=27
         Loop
EndIf
      If yt='Y'
         Exit
EndIf
   Elseif p1=1
      Nsls=aidi1+spac(10)
      Lks =' '
      Hut =.t.
      Do x with 'Input Biaya/Pendapatan Lain-2'
      Do inkas
   Elseif p1=2
      Do x with 'Laporan Biaya/Pendapatan Lain-2'
      Do lkas1
   Elseif p1=3
Do While .t.
         Hut=.t.
         Set colo to w/b,w+/gr
         @9,34 clea to 14,54
         @9,34 to 14,54
         @10,35  prom [ Per Nama Kasir... ]
         @11,35  prom [ Total per Nota... ]
         @12,35  prom [ Total per Kasir.. ]
         @13,35  prom [ Total per Tanggal ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Do x with 'Laporan Penjualan per Kasir'
            Do lkas
         Elseif p12=2
            Do x with 'Laporan Penjualan per Nota'
            Do lpj41
         Elseif p12=3
            Do x with 'Laporan Penjualan Total per Kasir'
            Do lpj32
         Elseif p12=4
            Do x with 'Laporan Penjualan per Tanggal/Hari'
            Do lpj3
EndIf
         Rest scre from smenu2
EndDo
   Elseif p1=4
Do While .t.
         Hut=.t.
         Set colo to w/b,w+/gr
         @10,34 clea to 14,54
         @10,34 to 14,54
         @11,35 prom [ Rincian Faktur... ]
         @12,35 prom [ Buku Hutang...... ]
         @13,35 prom [ Daftar Pembayaran ]
         Menu to p12
         If (p12 %27)=0
            Exit
         Elseif p12=1
            Do x with 'Daftar Faktur Pembelian'
            Do fkth
         Elseif p12=2
            Do x with 'Daftar Hutang'
            Do lhut1
         Elseif p12=3
            Do x with 'Daftar Pembayaran Hutang'
            Do lbyrh
EndIf
         Rest scre from smenu2
EndDo
   Elseif p1=5
      Do x with 'Daftar Perkiraan Komponen Laba/Rugi'
      Do prk
   Elseif p1=6
      Do x with 'Ganti Password'
      Do pu
EndIf
   Rest scre from smenu2
EndDo
Tone(3500,.5)
Tone(1000,1)
Set colo to
Set curs on
Clea all
Clea
Return

*** ISI TANGGAL & KONVERSI TGL KE CHR
Return

Procedure pisitgl
Set curs on
Set colo to /r
Do While .t.
   Do pesan with 'Tanggal Hari Ini:           '
   @24,44 get tglx
   Read
   Exit
EndDo
Tgl=tglx
Do cekdata
Do hari
Set curs off
Return
Procedure xadadis
Adadis=.t.
Keyboard chr(13)
Return

* EOF KEU
