// ============================================================
// File    : LKSR.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN PENDPTAN KASIR
SELECT 1
*
SELECT 2
*
SELECT 3
Uselan('KASIR.DTA','','','','','')

STORE tglx to tgl,tgl2
DO WHILE .t.
   Ksa=' '
   Nsls=SPACE(20)
   SELECT 3
   Set filter to
   @1,0 CLEAR to 24,79
   @2,24 say 'Kassa.:'get ksa pict '9'
   @3,24 say 'Kasir.:'get nsls pict '@!'
   @4,24 say 'Tanggal'get tgl
   @4,43 say 'sampai dg.'get tgl2
   Do pesan with 'ESC-Selesai    Kassa=0 (u/Perincian per Kassa)'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   If nsls#SPACE(20)
      LOCATE FOR nama_ksr=RTRIM(nsls)
      If .not. FOUND()
         Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
         Wait''
         Loop
      ENDIF
      Nsls=nama_ksr
   ENDIF
   Do bacafile
   Hal =1
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF

   SELECT 1
   Uselan('&nf0','&nf3','','','','')
   SELECT 2
   Uselan('RETURJN.DTA','RETURJN1.NDX','','','','')
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   @1,1 say 'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(37)+'Hal: '+tran(hal,'99')
   @2,0 say chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,21)+REPLACE(chr(209)+REPLACE(gd,12),4)+chr(184)
   @3,0 say tl+' No.'+tl+' Ks. Nama Kasir      '+tl+'     Rp.Jual'+tl+'    Rp.Retur'+tl+'    Rp.Pokok'+tl+'   Rp.Margin'+tl
   @4,0 say chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,21)+REPLACE(chr(197)+REPLACE(gl,12),4)+chr(180)
   B=5
   STORE 0 to hp,hj,rtr,jml,jml2,jml3,tot,tot2,tot3,n
   Tgl3=tgl
   If ksa='0' .and. nsls=SPACE(20)
      SELECT 3
      Go top
      DO WHILE .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         For ch=1 to 5
             Ksa=LTRIM(str(ch))
             Do tampilksr
         Next
         @b,0   say tl+SPACE(4)+tl+SPACE(21)+REPLACE(tl+SPACE(12),4)+tl
         @b+1,0 say chr(195)+REPLACE(gl,4)+chr(194)+REPLACE(gl,21)+REPLACE(chr(197)+REPLACE(gl,12),4)+chr(180)
         SET COLOR TO w+
         @b,6 say '     J u m l a h --->'
         @b,28 say tran(jml2,'@e 9999,999,999')
         @b,41 say tran(jml3,'@e 9999,999,999')
         @b,54 say tran(jml ,'@e 9999,999,999')
         @b,67 say tran((jml2-jml3)-jml,'@e 9999,999,999')
         SET COLOR TO g
         B=b+2
         STORE 0 to jml,jml2,jml3

         SELECT 3
         Skip
      ENDDO
   Elseif ksa=' ' .and. nsls=SPACE(20)
      SELECT 3
      Go top
      DO WHILE .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         SELECT 1
         Set filter to kassa#' ' .and. nama_ksr=nsls
         DO WHILE tgl<=tgl2
            Seek dtos(tgl)
            If .not. FOUND()
               Tgl=tgl+1
            Elseif FOUND()
               N   =n+1
               @b,0 say tl+tran(n,'999.')+tl+' '+ksa+'   '+left(nsls,16)+REPLACE(tl+SPACE(12),4)+tl
               Ada=.t.
               Exit
            ENDIF
         ENDDO
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
            Hp  =hp  + &brs14
            Hj  =hj  + &brs1
            If xOwner
               @b,28 say tran(hj,'@e 9999,999,999')
               @b,54 say tran(hp,'@e 9999,999,999')
               @b,67 say tran(hj-hp,'@e 9999,999,999')
            Else
               @b,28 say tran(hj,'@e 9999,999,999')
            ENDIF
            Skip
         ENDDO
         Tgl=tgl3

         SELECT 2
         Set filter to nama_ksr2=nsls
         DO WHILE tgl<=tgl2
            Seek dtos(tgl)
            If FOUND()
               Exit
            Else
               Tgl=tgl+1
            ENDIF
         ENDDO
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
            Rtr=rtr + (jumlah*harga_2)
            If xOwner
               @b,41 say tran(rtr,'@e 9999,999,999')
               @b,67 say tran((hj-rtr)-hp,'@e 9999,999,999')
            Else
               @b,41 say tran(hj,'@e 9999,999,999')
            ENDIF
            Skip
         ENDDO

         Jml =jml +hp
         Jml2=jml2+hj
         Jml3=jml3+rtr
         Tot =tot +hp
         Tot2=tot2+hj
         Tot3=tot3+rtr
         Tgl =tgl3
         STORE 0 to hp,hj,rtr
         If ada
            Ada=.f.
            B=b+1
         ENDIF

         SELECT 3
         Skip
      ENDDO
   Elseif ksa=' ' .and. nsls#SPACE(20)
      For ch=1 to 5
          Ksa=LTRIM(str(ch))
          Do tampilksr
      Next
   Elseif ksa#' ' .and. nsls=SPACE(20)
      SELECT 3
      Go top
      DO WHILE .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         Do tampilksr
         SELECT 3
         Skip
      ENDDO
   Elseif ksa#' ' .and. nsls#SPACE(20)
      Do tampilksr
   ENDIF
   @b,0 say chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,21)+REPLACE(chr(193)+REPLACE(gl,12),4)+chr(217)
   @b+1,0 CLEAR to 24,79
   SET COLOR TO w+
   If xOwner
      @b+1,19 say 'Total=Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e 9999,999,999 ')+tran(tot,'@e 9999,999,999 ')+tran((tot2-tot3)-tot,'@e 9999,999,999')
   Else
      @b+1,19 say 'Total=Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e 9999,999,999 ')
   ENDIF
   SET COLOR TO g
   @24,0
   Do pesan3 with 'ESC-Selesai   F10-Cetak'
   If nlp=0
      Loop
   ENDIF
   Do cp
   If cc=0
      Loop
   ENDIF
   SET PRINTER ON
   SET DEVICE TO PRINTER
   Set marg to 10
   ?chr(27)+chr(33)+chr(4)
   @prow()+1,1 say 'PENDAPATAN KASIR   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @prow()+1,0 say '*'+REPLACE('=',86)+'*'
   @prow()+1,0 say '| No.| Ks. Nama Kasir          |     Rp.Jual |    Rp.Retur |    Rp.Pokok |   Rp.Margin |'
   @prow()+1,0 say '|'+REPLACE('-',86)+'|'
   B=5
   STORE 0 to hp,hj,tot,tot2,n
   Tgl3=tgl
   If ksa=' ' .and. nsls=SPACE(20)
      For ch=1 to 5
          Ksa=LTRIM(str(ch))
          SELECT 3
          Go top
          DO WHILE .not. eof()
             Nsls=nama_ksr
             Ada =.f.
             Do tampilksr2
             SELECT 3
             Skip
          ENDDO
      Next
   Elseif ksa=' ' .and. nsls#SPACE(20)
      For ch=1 to 5
          Ksa=LTRIM(str(ch))
          Do tampilksr2
      Next
   Elseif ksa#' ' .and. nsls=SPACE(20)
      SELECT 3
      Go top
      DO WHILE .not. eof()
         Nsls=nama_ksr
         Ada =.f.
         Do tampilksr2
         SELECT 3
         Skip
      ENDDO
   Elseif ksa#' ' .and. nsls#SPACE(20)
      Do tampilksr2
   ENDIF
   @b,0 say '*'+REPLACE('=',86)+'*'
   If xOwner
      @b+1,1 say '      Total ='+SPACE(17)+'Rp '+tran(tot2,'@e 9999,999,999 ')+tran(tot3,'@e  9999,999,999 ')+tran(tot,'@e  9999,999,999 ')+tran((tot2-tot3)-tot,'@e  9999,999,999')
   ENDIF
   @b+4,0 say ' '
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RETURN

PROCEDURE tampilksr
SELECT 1
If nsls#SPACE(20)
   Set filter to kassa=ksa .and. nama_ksr=nsls
Elseif nsls=SPACE(20)
   Set filter to kassa=ksa
ENDIF
DO WHILE tgl<=tgl2
   Seek dtos(tgl)
   If .not. FOUND()
      Tgl=tgl+1
   Elseif FOUND()
      N   =n+1
      @b,0 say tl+tran(n,'999.')+tl+' '+ksa+' - '+left(nsls,16)+REPLACE(tl+SPACE(12),4)+tl
      Ada=.t.
      Exit
   ENDIF
ENDDO
Tgl=tgl3
DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
   Hp  =hp  + &brs14
   Hj  =hj  + &brs1
   If xOwner
      @b,28 say tran(hj,'@e 9999,999,999')
      @b,54 say tran(hp,'@e 9999,999,999')
      @b,67 say tran(hj-hp,'@e 9999,999,999')
   Else
      @b,28 say tran(hj,'@e 9999,999,999')
   ENDIF
   Skip
ENDDO

Tgl=tgl3
SELECT 2
If nsls#SPACE(20)
   Set filter to kassa=ksa .and. nama_ksr2=nsls
Elseif nsls=SPACE(20)
   Set filter to kassa=ksa
ENDIF
DO WHILE tgl<=tgl2
   Seek dtos(tgl)
   If FOUND()
      Exit
   Else
      Tgl=tgl+1
   ENDIF
ENDDO
DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
   Rtr=rtr + (jumlah*harga_2)
   If xOwner
      @b,41 say tran(rtr,'@e 9999,999,999')
      @b,67 say tran((hj-rtr)-hp,'@e 9999,999,999')
   Else
      @b,41 say tran(hj,'@e 9999,999,999')
   ENDIF
   Skip
ENDDO

Jml =jml +hp
Jml2=jml2+hj
Jml3=jml3+rtr
Tot =tot +hp
Tot2=tot2+hj
Tot3=tot3+rtr
Tgl =tgl3
STORE 0 to hp,hj,rtr
If ada
   Ada=.f.
   B=b+1
   If b>=22
      B=5
      Hal=hal+1
      Do pesan with 'Tekan ENTER Untuk Halaman Berikutnya'
      Wait''
      @b+1,0 
      @1,1 say 'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(25)+'Hal: '+tran(hal,'99')
      Do pesan with 'Tunggu, sedang dihitung'
   ENDIF
ENDIF
RETURN

PROCEDURE tampilksr2
SELECT 1
If nsls#SPACE(20)
   Set filter to kassa=ksa .and. nama_ksr=nsls
Elseif nsls=SPACE(20)
   Set filter to kassa=ksa
ENDIF
DO WHILE tgl<=tgl2
   Seek dtos(tgl)
   If .not. FOUND()
      Tgl=tgl+1
   Elseif FOUND()
      Ada=.t.
      Exit
   ENDIF
ENDDO
Tgl=tgl3
DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr=nsls
   Hp  =hp  + &brs14
   Hj  =hj  + &brs1
   Skip
ENDDO
Tgl=tgl3

SELECT 2
If nsls#SPACE(20)
   Set filter to kassa=ksa .and. nama_ksr2=nsls
Elseif nsls=SPACE(20)
   Set filter to kassa=ksa
ENDIF
DO WHILE tgl<=tgl2
   Seek dtos(tgl)
   If .not. FOUND()
      Tgl=tgl+1
   Elseif FOUND()
      Ada=.t.
      Exit
   ENDIF
ENDDO
Tgl=tgl3
DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. nama_ksr2=nsls
   Rtr=rtr + (jumlah*harga_2)
   Skip
ENDDO
If hj#0 .or. hp#0
   N=n+1
   @b,0 say tran(n,'|999.| ')+ksa+' - '+nsls+'|'+tran(hj,'@e 9999,999,999 |')+tran(rtr,'@e 9999,999,999 |')+tran(hj,'@e 9999,999,999 |')+tran((hj-rtr)-hp,'@e 9999,999,999 |')
   B=b+1
ENDIF
Tot =tot +hp
Tot2=tot2+hj
Tot3=tot3+rtr
Tgl =tgl3
STORE 0 to hp,hj,rtr
RETURN

* EOF LKSR
