// ============================================================
// File    : KEU.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

* MENU UTAMA KEUANGAN
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
SET CENTURY ON
SET DATE ITALIAN
Set mess to 22 center
SET PROCEDURE TO paswod2
SET PROCEDURE TO bant
SET PROCEDURE TO dafkode
SET PROCEDURE TO lhut
SET PROCEDURE TO lpj1
SET PROCEDURE TO lpj2
SET PROCEDURE TO kas
SET PROCEDURE TO lkeu
CLEAR
STORE '' to aidi1,aidi2,tgl,tgl2
*
Do idents
*
STORE '' to nlp,cc,hr,thn,bln,hri,nf0,nf1,nf2,nf3,nlp,xpos,brs1,brs12,brs13,brs14,pSuper,pOwner,pxOwner
STORE .f. to Super,Owner,xOwner,Orglain,klp,spl
Gtkertas="Ganti kertas baru,     F10=Lanjutkan     ESC=Batal"
Gtlayar ="'&cfx->'+LTRIM(str(cf))+'   ENTER=Hal.Berikut   BACKSPC=Hal.Seblmnya   ESC=Menu Selanjutnya'"
Blnthn  ='SUBSTR(dtoc(tgl),7,4)+SUBSTR(dtoc(tgl),4,2)'
Ksa=GetEnv("KASSA")
Gd =chr(205)
Gl =chr(196)
Tl =chr(179)
Bx =chr(176)
Parameter e
Hasil=.t.
Do pm
If (.not. hasil) .or. ( .not. xOwner )
   SET CURSOR ON
   CLEAR all
   CLEAR
   RETURN
ENDIF
Tone(1000,1)
Tone(1500,1)
Tglx=date()
Do pisitgl
Uselan('DATA.SET','','','','','')
xPos=RTRIM(keterangan)
Close
DO WHILE .t.
   Set inte on
   SET CURSOR OFF
   SET COLOR TO bg
   @2,0,21,79 box bx+bx+bx+bx+bx+bx+bx+bx+bx
   SET COLOR TO n++
   @5,31,14,50 box bx+bx+bx+bx+bx+bx+bx+bx+bx
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
   @2,34  say  'ADM.KEUANGAN'
   @22,0
   SET COLOR TO /bg,gr+
   @4,29 CLEAR to 13,49
   @4,29 to 13,49
   @6,29 say chr(195)+REPLACE(gl,19)+chr(180)
   @10,29 say chr(195)+REPLACE(gl,19)+chr(180)
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
      SET CURSOR ON
      Yt='Y'
      @22,0
      @22,30 say 'Anda Yakin? Y/T :'get yt pict '!'
      Read
      If lastkey()=27
         Loop
      ENDIF
      If yt='Y'
         Exit
      ENDIF
   Elseif p1=1
      Nsls=aidi1+SPACE(10)
      Lks =' '
      Hut =.t.
      Do x with 'Input Biaya/Pendapatan Lain-2'
      Do inkas
   Elseif p1=2
      Do x with 'Laporan Biaya/Pendapatan Lain-2'
      Do lkas1
   Elseif p1=3
      DO WHILE .t.
         Hut=.t.
         SET COLOR TO w/b,w+/gr
         @9,34 CLEAR to 14,54
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
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=4
      DO WHILE .t.
         Hut=.t.
         SET COLOR TO w/b,w+/gr
         @10,34 CLEAR to 14,54
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
         ENDIF
         Rest scre from smenu2
      ENDDO
   Elseif p1=5
      Do x with 'Daftar Perkiraan Komponen Laba/Rugi'
      Do prk
   Elseif p1=6
      Do x with 'Ganti Password'
      Do pu
   ENDIF
   Rest scre from smenu2
ENDDO
Tone(3500,.5)
Tone(1000,1)
SET COLOR TO
SET CURSOR ON
CLEAR all
CLEAR
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
SET CURSOR OFF
RETURN

PROCEDURE xadadis
Adadis=.t.
Keyboard chr(13)
RETURN

* EOF KEU
