// ============================================================
// File    : LKEU.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN TRANSAKSI KAS/HARIAN
*-----------------------------
PROCEDURE lkas0
SELECT 1
Uselan('KAS.DTA','','','','','')
Go top
Tglso=tanggal
Jmlso=rp_total
Close
*
Uselan('BAYARP.DTA','BAYARP1.NDX','','','','')
SELECT 2
Uselan('BAYARH.DTA','BAYARH1.NDX','','','','')
*
STORE tglx to tgl,tgl2
DO WHILE .t.
   Tgl=tgl2
   SET CENTURY ON
   @1,0 CLEAR to 24,79
   @2,30 say 'Per Tanggal:'get tgl
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   ENDIF
   Tgl1=tglso
   Tgl2=tgl
   Jmlx=jmlso
   Tgl =ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))

   SET CENTURY ON
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung...'
   STORE 0 to jml,jml2,jml3

   Set alte on
   Set alte to BUKU-KAS.RTF
   ?' LAPORAN KAS HARIAN'
   ?' PER TANGGAL: '+dtoc(tgl)
   ?chr(213)+REPLACE(gd,5)+chr(209)+REPLACE(gd,35)+REPLACE(chr(209)+REPLACE(gd,11),2)+chr(209)+REPLACE(gd,12)+chr(184)
   ?tl+'Tgl. '+tl+'         Keterangan/Transaksi      '+tl+'      Debet'+tl+'     Kredit'+tl+'       Saldo'+tl
   ?chr(192)+REPLACE(gl,5)+chr(193)+REPLACE(gl,35)+REPLACE(chr(193)+REPLACE(gl,11),2)+chr(193)+REPLACE(gl,12)+chr(217)
   Awl =1
   Bln1=mont(tgl1)
   Bln2=mont(tgl2)
   Do pesan with 'Tunggu, sedang dihitung'
   DO WHILE bln1<=bln2
      If bln1#bln2
         SELECT 1
         Sum jumlah_byr to jml  for mont(tanggal)=bln1

         SELECT 2
         Sum jumlah_byr to jml2 for mont(tanggal)=bln1

         SELECT 3
         Do bacaflkas
         If file('&nf0')
            Do bukaflkas
            Sum jumlah_byr to rp_total for mont(tanggal)=bln1 .and. id_pos='D'
         ENDIF
         Jmlx =(jmlso+jml)-(jml2+jml3)
         STORE 0 to jml,jml2,jml3
         *
      Elseif bln1=bln2
         ?' '+left(dtoc(tgl),2)+'/'+SUBSTR(dtoc(tgl),4,2)+'  Saldo Awal per '+dtoc(tgl)+SPACE(35)+tran(jmlx,'@e 999,999,999')
         *
         DO WHILE tgl<=tgl2
            SELECT 1
            Seek dtos(tgl)
            DO WHILE .not. eof() .and. tanggal=tgl
               Jml =jumlah_byr
               Jmlx=jmlx+jml
               Kataph=RTRIM(jenis_byr)+' '+RTRIM(nama_lgn)
               If len(kataph)<35
                  Kataph=kataph+SPACE(35-len(kataph))
               Else
                  Kataph=left(kataph,35)
               ENDIF
               ?' '+left(dtoc(tgl),2)+'/'+SUBSTR(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e 999,999,999        -     ')+tran(jmlx,'@e 999,999,999')
               Skip
            ENDDO
            *
            SELECT 2
            Seek dtos(tgl)
            DO WHILE .not. eof() .and. tanggal=tgl
               Jml =jumlah_byr
               Jmlx=jmlx-jml
               Kataph=RTRIM(jenis_byr)+' '+RTRIM(nama_spl)
               If len(kataph)<35
                  Kataph=kataph+SPACE(35-len(kataph))
               Else
                  Kataph=left(kataph,35)
               ENDIF
               ?' '+left(dtoc(tgl),2)+'/'+SUBSTR(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e        -    999,999,999  ')+tran(jmlx,'@e 999,999,999')
               Skip
            ENDDO
            *
            SELECT 3
            Do bacaflkas
            If file('&nf0')
               Do bukaflkas
               Seek dtos(tgl)
               DO WHILE .not. eof() .and. tanggal=tgl
                  If uppe(left(nama_perk,5))#'MODAL'
                     Jml=rp_total
                     Idp=id_pos
                     Kataph=kode_perk+RTRIM(keterangan)
                     If len(kataph)<35
                        Kataph=kataph+SPACE(35-len(kataph))
                     Else
                        Kataph=left(kataph,35)
                     ENDIF
                     If idp='D'
                        Jmlx=jmlx+jml
                        ?' '+left(dtoc(tgl),2)+'/'+SUBSTR(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e 999,999,999        -     ')+tran(jmlx,'@e 999,999,999')
                     Else
                        Jmlx=jmlx-jml
                        ?' '+left(dtoc(tgl),2)+'/'+SUBSTR(dtoc(tgl),4,2)+'  '+kataph+tran(jml,'@e        -    999,999,999  ')+tran(jmlx,'@e 999,999,999')
                     ENDIF
                  ENDIF
                  Skip
               ENDDO
            ENDIF
            Tgl=tgl+1
         ENDDO
         *
      ENDIF
      Bln1=bln1+1
   ENDDO
   ?REPLACE(gl,80)
   ?' '
   Set alte off
   Set alte to
   SET CENTURY OFF
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('BUKU-KAS.RTF'),1,0,22,210,.t.,'',210)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If lastkey()=-9
      Do cp
      If cc#0
         SET PRINTER ON
         SET DEVICE TO PRINTER
         Set marg to 5
         ?chr(27)+chr(33)+chr(1)
         Type BUKU-KAS.RTF to prin
         Eject
         SET PRINTER OFF
         SET DEVICE TO SCREEN
      ENDIF
   ENDIF
   Tgl=tgl2
   *
ENDDO
CLOSE all
RETURN

* LAPORAN TRANSAKSI KAS1
*------------------------
PROCEDURE lkas1
SELECT 10
Uselan('POS.DTA','POS.NDX','','','','')
SELECT 11
Uselan('SUBPOS.DTA','SUBPOS2.NDX','','','','')
SELECT 3
*
STORE SPACE(5) to kpos,kspos
STORE tglx to tgl,tgl2
DO WHILE .t.
   @1,0 CLEAR to 24,79
   SELECT 10
   Set filter to id_lr='Y'
   LOCATE FOR id_lr='Y'
   SET CENTURY ON
   Set key 28 to dafperk
   @2,22 say 'Pos...:'get kpos pict '@!'
   @3,22 say 'SubPos:'get kspos pict '@!'
   @4,22 say 'Tanggal'get tgl
   @4,41 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai   F1=Daftar Perkiraan'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Do pesan with 'Tunggu, sedang dihitung'
   If kpos#SPACE(5)
      SELECT 10
      Seek kpos
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Pos Perkiraan '+RTRIM(kpos)
         Loop
      ENDIF
      Npos=nama_pos
      If kspos#SPACE(5)
         SELECT 11
         Seek kpos+kspos
         If .not. FOUND()
            Do pesan2 with 'Tidak ada Perkiraan '+RTRIM(kpos)+' - '+RTRIM(kspos)
            Loop
         ENDIF
         Nspos=nama_perk
      ENDIF
   ENDIF
   Tgl4=tgl
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   Do bacaflkas
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   *
   Do bukaflkas
   *
   STORE 1 to cf,hal,no,n
   Cfx =LTRIM(str(cf))
   Arg1='L-BYA&cfx'+'.RTF'
   SET CENTURY OFF
   Set alte on
   Set alte to &arg1
   If kpos=SPACE(5)
      ?' LAPORAN TRANSAKSI KAS'
      ?' Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(10)+'Hal:'+tran(hal,'999')
      ?chr(213)+REPLACE(gd,8)+chr(209)+REPLACE(gd,32)+chr(209)+REPLACE(gd,25)+chr(209)+REPLACE(gd,10)+chr(184)
      ?tl+'Tanggal '+tl+'Nama Perkiraan'+SPACE(18)+tl+'Keterangan'+SPACE(15)+tl+'Jumlah Rp.'+tl
      ?chr(195)+REPLACE(gl,8)+chr(197)+REPLACE(gl,32)+chr(197)+REPLACE(gl,25)+chr(197)+REPLACE(gl,10)+chr(180)
      STORE 0 to totd,totk
      DO WHILE .t.
         SELECT 3
         Set filter to id_lr='Y'
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         ENDIF
         Nr=recno()
         SET CENTURY OFF
         DO WHILE .not. eof()
            If (tanggal>=tgl .and. tanggal<=tgl2)
               If id_pos='D'
                  Totd=totd+rp_total
               Elseif id_pos='K'
                  Totk=totk+rp_total
               ENDIF
               ?tl+dtoc(tanggal)+tl+left(nama_perk,25)+kode_perk+' '+id_pos+tl+left(keterangan,25)+tl+tran(rp_total,'@e 99,999,999')+tl
               N=n+1
               If n>200
                  ?chr(192)+REPLACE(gl,8)+chr(193)+REPLACE(gl,32)+chr(193)+REPLACE(gl,25)+chr(193)+REPLACE(gl,10)+chr(217)
                  ?' '
                  N  =1
                  Hal=hal+1
                  Cf =cf+1
                  Cfx=LTRIM(str(cf))
                  Arg1='L-BYA&cfx'+'.RTF'
                  Set alte to &arg1
                  ?' LAPORAN TRANSAKSI KAS'
                  ?' Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(10)+'Hal:'+tran(hal,'999')
                  ?chr(213)+REPLACE(gd,8)+chr(209)+REPLACE(gd,32)+chr(209)+REPLACE(gd,25)+chr(209)+REPLACE(gd,10)+chr(184)
                  ?tl+'Tanggal '+tl+'Nama Perkiraan'+SPACE(18)+tl+'Keterangan'+SPACE(15)+tl+'Jumlah Rp.'+tl
                  ?chr(195)+REPLACE(gl,8)+chr(197)+REPLACE(gl,32)+chr(197)+REPLACE(gl,25)+chr(197)+REPLACE(gl,10)+chr(180)
               ENDIF   
            ENDIF
            Skip
         ENDDO
         If beda
            SET CENTURY ON
            Do cariflkas
            If tgl>tgl2
               Exit
            ENDIF
            If file('&nf0')
               Do bukaflkas
            ENDIF
         Elseif beda=.f.
            Exit
         ENDIF

      ENDDO
      ?chr(192)+REPLACE(gl,8)+chr(193)+REPLACE(gl,32)+chr(193)+REPLACE(gl,25)+chr(193)+REPLACE(gl,10)+chr(217)
      ?' Total Debet  Rp'+tran(totd,'@e 9,999,999,999')
      ?'       Kredit Rp'+tran(totk,'@e 9,999,999,999')
   Else
      If kpos#SPACE(5) .and. kspos=SPACE(5)
         ?SPACE(11)+RTRIM(npos)
         ?SPACE(11)+'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?SPACE(10)+chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,5)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,11)+chr(184)
         ?SPACE(10)+tl+' No.'+tl+'Kode '+tl+'Nama Perkiraan'+SPACE(16)+tl+' Jumlah Rp.'+tl
         ?SPACE(10)+chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,11)+chr(180)
         Y=1
         STORE 0 to jml,tot
         SELECT 11
         Seek kpos
         DO WHILE .not. eof() .and. kode_pos=kpos
            Kspos=kode_perk
            Nspos=nama_perk
            Jml=0
            Z=LTRIM(str(y))
            DO WHILE .t.
               SELECT 3
               Set order to 2
               Seek kpos+kspos
               Nr=recno()
               DO WHILE .not. eof() .and. ( kode_pos=kpos .and. kode_perk=kspos)
                  If (tanggal>=tgl .and. tanggal<=tgl2)
                     Jml=jml +rp_total
                  ENDIF
                  Skip
               ENDDO
               If beda
                  SET CENTURY ON
                  Do cariflkas
                  If tgl>tgl2
                     Exit
                  ENDIF
                  If file('&nf0')
                     Do bukaflkas
                  ENDIF
               Elseif beda=.f.
                  Exit
               ENDIF
            ENDDO
            ?SPACE(10)+tl+tran(y,'999 ')+tl+kspos+tl+nspos+tl+tran(jml,'@e 999,999,999')+tl
            Tot=tot+jml
            Y  =y + 1
            Jml=0
            Tgl=tgl4
            Do bacaflkas
            Do bukaflkas
            *
            SELECT 11
            Skip
         ENDDO
         ?SPACE(10)+chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,11)+chr(217)
         ?SPACE(44)+'Total=Rp '+tran(tot,'@e 999,999,999')
      Elseif kpos#SPACE(5) .and. kspos#SPACE(5)
         ?SPACE(11)+'POS ...: '+RTRIM(npos)
         ?SPACE(11)+'SUB POS: '+RTRIM(nspos)
         ?SPACE(11)+'Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?SPACE(10)+chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,10)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,11)+chr(184)
         ?SPACE(10)+tl+' No.'+tl+'Tanggal   '+tl+'Keterangan'+SPACE(20)+tl+' Jumlah Rp.'+tl
         ?SPACE(10)+chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,11)+chr(180)
         Y=1
         STORE 0 to jml,tot
         DO WHILE .t.
            SELECT 3
            Set order to 2
            Seek kpos+kspos
            Nr=recno()
            Y=1
            DO WHILE .not. eof() .and. ( kode_pos=kpos .and. kode_perk=kspos)
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  ?SPACE(10)+tl+tran(y,'999 ')+tl+dtoc(tanggal)+tl+keterangan+tl+tran(rp_total,'@e 999,999,999')+tl
                  Tot=tot+rp_total
                  Y  =y+1
               ENDIF
               Skip
            ENDDO
            If beda
               SET CENTURY ON
               Do cariflkas
               If tgl>tgl2
                  Exit
               ENDIF
               If file('&nf0')
                  Do bukaflkas
               ENDIF
            Elseif beda=.f.
               Exit
            ENDIF
         ENDDO
         ?SPACE(10)+chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,11)+chr(217)
         ?SPACE(49)+'Total=Rp '+tran(tot,'@e 999,999,999')
      ENDIF
   ENDIF
   Set alte off
   Set alte to
   STORE SPACE(5) to kpos,kspos
   Tgl=tgl4
   *
   Layar('L-BYA')
   *
   Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
   If lastkey()=-7
      Do hpstr2
      Loop
   ENDIF
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
   ?chr(27)+chr(33)+chr(1)
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-BYA&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CENTURY ON
CLOSE all
RETURN

*** PROSEDUR HAPUS TRANSAKSI
PROCEDURE hpstr2
SELECT 3
@1,0 CLEAR to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Pesanx='ENTER=Edit   F8=Hapus   F10=Cetak'
Do pesan with pesanx
DECLARE field[8],kop[8],bts[8],frm[8]
Field[1]='TANGGAL'
Field[2]='KODE_PERK'
Field[3]='left(NAMA_PERK,25)'
Field[4]='ID_POS'
Field[5]='left(KETERANGAN,26)'
Field[6]='RP_TOTAL'
Field[7]='left(NAMA_KSR,14)'
Kop[1]='Tanggal'
Kop[2]='Kode'
Kop[3]='Nama Perkiraan'
Kop[4]=' '
Kop[5]='Keterangan'
Kop[6]='Jumlah Rp.'
Kop[7]='Nama Kasir'
Frm[1]='99-99-9999'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]=''
Frm[6]='@e 99,999,999'
Frm[7]='@!'
Bts[1]=''+tl
Bts[2]=''+tl
Bts[3]=''+tl
Bts[4]=' '
Bts[5]=''+tl
Bts[6]=''+tl
Bts[7]=''+tl
DbEdit(2,0,21,79,field,'fhtr2',frm,kop,gl,bts)
*
SET CURSOR ON
RETURN

Function fhtr2
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
   @b,43 get ket pict '@S26'
   @b,70 get jml pict '@e 99,999,999'
   @24,0
   Read
   If lastkey()#27
      Rlok()
      REPLACE keterangan with ket,rp_total with jml
      Ul()
      COMMIT
   ENDIF
   SET CURSOR OFF
   Do pesan with pesanx
   RETURN 2
Elseif lastkey()=-7
   SET COLOR TO gr+*
   @row(),col() say &fds
   SET COLOR TO g
   Do pesan with ''
   SET CURSOR ON
   Do pesan with 'Dihapus? Y/T'
   If ulangi(.t.)
      Rlok()
      Dele
      Ul()
      COMMIT
      Skip
   ENDIF
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Else
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

* EOF LKEU
