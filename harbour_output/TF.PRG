// ============================================================
// File    : TF.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* TRANSFER DARI OFFLINE KE SERVER
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
Empunya='YONEV Computer(0283)3300984TEGALSARI MULYA - BREBES'
Aidi1=SUBSTR(empunya,33,10)
Aidi2=SUBSTR(empunya,46,6)
STORE 0 to tgl,tgl2
DO WHILE (tgl< Len(empunya))
   Tgl = tgl + 1
   Tgl2= tgl2+ asc(SUBSTR(empunya, tgl, 1))
ENDDO
If (tgl2 # len(empunya) * 69 + 46)
   SET COLOR TO br+
   @09,32 CLEAR to 15,46
   @09,32 to 15,46 doub
   SET COLOR TO r
   @10,35 say '  ³\,/³'
   @11,35 say ' \ ë ë /'
   @12,35 say ' (  ­  )'
   @13,35 say '   \ /  '
   @14,35 say '//  ß  \\'
   SET COLOR TO r+
   @11,38 say 'ë'
   @11,40 say 'ë'
   @13,39 say 'Ü'
   SET COLOR TO r+*
   @14,39 say 'ß'
   @15,0  say ''
   @16,0  say ''
   RETURN
ENDIF
STORE '' to thn,bln,hri,nf0,nf1,nf2,nf3,nf4
Blnthn='SUBSTR(dtoc(tgl),7,4)+SUBSTR(dtoc(tgl),4,2)'
Tglx  =date()
Gd=chr(205)
Gl=chr(196)
Tl=chr(179)

CLEAR
If .not. file('F:DATA.SET')
   Tone(1000,3)
   Tone(1500,6)
   Do pesan with 'Komputer Ini Belum Tersambung ke Server.  Ulangi...'
   Wait''
   Wait''
   CLEAR
   RETURN
ENDIF

SELECT 1
Uselan('F:STOK.DTA','F:STOK.NDX','','','','')
SELECT 2
*
SELECT 3
*
Sls=SPACE(20)
STORE ' ' to hri,bln,thn
STORE tglx to tgl,tgl2
DO WHILE .t.
   SET CENTURY ON
   CLEAR
   @1,0 CLEAR to 23,79
   @2,16 say 'Data Penjualan Off-Line yg Akan di-Transfer ke Server:'
   @3,15 to 3,67
   @4,16 say 'Tanggal'get tgl
   @4,35 say 'sampai dengan'get tgl2
   Read
   @23,0
   If lastkey()=27
      Exit
   ENDIF

   Jdt=0
   Awl=1
   Do bacafile
   If .not. file('&nf0')
      Tone(1000,1)
      Tone(1500,2)
      Do pesan with 'Tidak ada File Data Penjualan '+&nf0
      Wait''
      Loop
   ENDIF

   Do pesan with 'Tunggu, sedang dicari...'
   SELECT 2
   Uselan('C:&nf0','C:&nf3','','','','')
   Seek (thn+bln+hri)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
      If .not. FOUND()
         Tone(1000,1)
         Tone(1500,2)
         Do pesan with 'Tidak Ada Data Transaksi untuk Tanggal diAtas.  Tanggal diKoreksi...'
         Wait''
         Loop
      ENDIF
   ENDIF
   Nr=recno()
   Copy to JUAL.TMP for (tanggal>=tgl .and. tanggal<=tgl2)
   Close

   Uselan('F:&nf0','F:&nf3','F:&nf1','F:&nf2','','')

   SELECT 3
   Useidx('JUAL.TMP')

   @1,0 CLEAR to 23,79
   @1,0 to 1,79 doub
   @22,0 to 22,79
   @0,1 say 'Data Penjualan di Komputer Ini, Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   Katax='ESC-Selesai   ENTER-Edit   F5-CariNota   F8-Hapus   F10-Transfer'
   Do pesan with katax
   DECLARE field[11],kop[11],frm[11],bts[11]
   Field[1]='TANGGAL'
   Field[2]='FAKTUR'
   Field[3]='KASSA'
   Field[4]='left(NAMA_KSR,15)'
   Field[5]='left(NAMA_BRG,25)'
   Field[6]='JUMLAH'
   Field[7]='HARGA_2'
   Field[8]='HARGA'
   Field[9]='DISKON'
   Field[10]='KODE_BRG'
   Field[11]='KODE_KLB'
   Kop[1]='Tanggal'
   Kop[2]='Nota'
   Kop[3]='Ks.'
   Kop[4]='Nama Kasir'
   Kop[5]='Nama Barang'
   Kop[6]='  Qty.'
   Kop[7]='   @Harga'
   Kop[8]='   @Pokok'
   Kop[9]='Disc.'
   Kop[10]='Kode Brg.'
   Kop[11]='Klp.'
   Frm[1]='99-99-99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='@!' 
   Frm[5]='@!'
   Frm[6]='@e 9999.99'
   Frm[7]='@e 9,999,999'
   Frm[8]='@e 9,999,999'
   Frm[9]='@e 99999'
   Frm[10]='@!'
   Frm[11]='@!'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   Bts[6]=''+tl
   Bts[7]=''+tl
   Bts[8]=''+tl
   Bts[9]=''+tl
   Bts[10]=''+tl
   Bts[11]=''+tl
   Go nr
   DbEdit(2,0,21,79,field,'ftf',frm,kop,gl,bts)
   *
   SET CURSOR ON
ENDDO
Close all
RETURN

Function ftf
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-4
      SET CURSOR ON
      Fkt=SPACE(5)
      Do pesan with 'No.Nota:     '
      @23,41 get fkt pict '@!'
      Read
      If lastkey()=27 .or. fkt=SPACE(5)
         Do pesan with katax
         SET CURSOR OFF
         RETURN 1
      ENDIF
      Seek (thn+bln+hri)+sls+fkt
      Do pesan with katax
      SET CURSOR OFF
      RETURN 1
   Case lastkey()=-7
      SET COLOR TO gr+*
      @row(),col() say &fds
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      SET CURSOR ON
      Ht=' '
      DO WHILE .not. ht$'YT'
         @23,47 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         If .not. dele()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-9
      Do pesan with 'Mulai Transfer? Y/T  '
      SET CURSOR ON
      Ht=' '
      DO WHILE .not. ht$'YT'
         @23,49 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         SELECT 3
         Go bott
         Jdt=recno()
         Go top
         DO WHILE .not. eof()
            Do pesan with 'Tunggu, sedang di-Transfer... '+LTRIM(str(awl))+' - '+LTRIM(str(jdt))
            Tgla=tanggal
            Tglb=tanggal_2
            Ksa =kassa
            Nsls=nama_ksr
            Fkt=faktur
            Klb=kode_klb
            Kdb=kode_brg
            Nmb=nama_brg
            St =satuan
            Jml=jumlah
            Hp =harga
            Hj =harga_2
            Dis2=diskon

            SELECT 2
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgla,tanggal_2 with tglb,faktur with fkt,kassa with ksa,;
                 nama_ksr with nsls,kode_klb with klb,kode_brg with kdb,nama_brg with nmb,;
                 satuan with st,jumlah with jml,harga with hp,harga_2 with hj,diskon with dis2
            Ul()
    
            SELECT 1
            Seek kdb
            If harga_2>0
               Rlok()
               REPLACE stok_k with (stok_k+jml),tanggal_2 with tgl
               Do rstok
               Ul()
            ENDIF

            Awl=awl+1
            SELECT 3
            Dele
            Skip
         ENDDO

         SELECT 1
         COMMIT

         SELECT 2
         COMMIT
         Close
         If awl>1
            Do pesan with 'Tunggu, sedang Hapus Data yang Lama'
            Uselan('C:&nf0','C:&nf3','C:&nf1','C:&nf2','','')
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2)
            Ul()
            COMMIT
            Close
         ENDIF
      ENDIF
      RETURN 0   
   Case lastkey()=13
      SET CURSOR ON
      If field[pointer]='JUMLAH'
         Rlok()
         @row(),col() get &fds pict '@e 9999.99'
         Read
         Ul()
         COMMIT
      Elseif left(field[pointer],5)='HARGA'
         Rlok()
         @row(),col() get &fds pict '@e 9,999,999'
         Read
         Ul()
         COMMIT
      Elseif field[pointer]='KODE_KLB' .or. field[pointer]='KODE_BRG'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         COMMIT
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE pesan
PARAMETERS psn
SET COLOR TO /r
@23,0 say SPACE(80)
SET COLOR TO w+/r
@23,40-(len(psn)/2) say psn
SET COLOR TO g
RETURN

PROCEDURE bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
RETURN

***Buka File di LAN
FUNCTION UseLan
PARAMETERS Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   Use &arg1
   If (!NetErr())
      Exit
   ENDIF
   Do pesan with 'Tunggu, File '+&arg1 +' Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
SET INDEX TO &arg2,&arg3,&arg4,&arg5,&arg6
RETURN .T.

***Buka File u/ Reindex-Exclusive
FUNCTION UseIdx
PARAMETERS Arg1
DO WHILE .t.
   Use &arg1 exclusive
   If (!NetErr())
      Exit
   ENDIF
   Tone(700,6)
   Do pesan with 'Proses diTunda, ada yg memBuka File '+arg1+'         '
   Yt=' '
   SET COLOR TO g+*
   @23,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   SET COLOR TO g
   If yt#'Y'
     Batal=.t.
     Exit
   ENDIF
   @23,0
   Inkey(0.5)
ENDDO
RETURN(.T.)
FUNCTION RLOK
Do while .not. Rlock()
   If rlock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
Return(.T.) 

FUNCTION FLOK
Do while .not. FLock()
   If flock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
Return(.T.)

FUNCTION UL
Unlock
Return(.T.)

PROCEDURE rstok
REPLACE stok with (stok_a+stok_m)-stok_k
RETURN

* EOF TF
