// ============================================================
// File    : LPIU.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* DAFTAR PIUTANG
*---------------
PROCEDURE lpiu1
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
Go top
Tglso=tanggal
SELECT 2
Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')
SELECT 3
*
SELECT 4
*
SELECT 5
*
SELECT 6
Uselan('BAYARP.DTA','BAYARP1.NDX','BAYARP2.NDX','','','')
Tgl =tglso
Tgl2=tglx
Ada =.f.
Ksp =SPACE(10)
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   @1,0 CLEAR to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai     00000=Global'
   If ada
      Ada=.f.
      Keyboard chr(13)+chr(13)
   ENDIF
   Read
   Set key 28 to
   Read
   If lastkey()=27
      Exit
   ENDIF
   If ksp=SPACE(10)
      Do daflgn
   ENDIF
   If lastkey()=27
      Loop
   ENDIF
   If ksp='00000'
      Set alte on
      Set alte to PIUTANG.RTF
      ?' DAFTAR PIUTANG'
      ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,5)+REPLACE(chr(194)+REPLACE(gl,25),2)+chr(194)+REPLACE(gl,15)+chr(191)
      ?tl+' No.'+tl+'Kode '+tl+'Nama Pelanggan'+SPACE(11)+tl+'Alamat'+SPACE(19)+tl+'     Jumlah Rp.'+tl
      ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,5)+REPLACE(chr(197)+REPLACE(gl,25),2)+chr(197)+REPLACE(gl,15)+chr(180)
      Tot2=0
      No  =1
      *
      SELECT 1
      Go top
      DO WHILE .not. eof()
         Ksp  =kode_lgn
         Nsp  =nama_lgn
         Algn =almt_lgn
         STORE 0 to jml,jml2
         *
         SELECT 2
         Set order to 2
         Seek ksp
         DO WHILE .not. eof() .and. kode_lgn=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml =jml +(rp_total-rp_diskon)
               Jml2=jml2+rp_bayar
            ENDIF
            Skip
         ENDDO
         Tot=jml-jml2
         *
         If tot#0
            ?tl+tran(no,'999.')+tl+left(ksp,5)+tl+left(nsp,25)+tl+left(algn,25)+tl+tran(tot,'@e 999,999,999,999')+tl
            Tot2=tot2+tot
            No  =no+1
         ENDIF
         SELECT 1
         Skip
      ENDDO
      ?chr(192)+REPLACE(gl,78)+chr(217)
      ?SPACE(56)+'Total Rp'+tran(tot2,'@e 999,999,999,999')
      ?' '
      Set alte off
      Set alte to
      SELECT 3
      Close
      *
      Do pesan with 'ESC=Menu Selanjutnya'
      Memoedit(Memoread('PIUTANG.RTF'),1,0,22,240,.t.,'',240)
      Do pesan3 with 'ESC=Selesai   F10=Cetak'
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
      ?chr(27)+chr(33)+chr(9)
      Type PIUTANG.RTF to prin
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   Else
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      *
      Do bukuh
   ENDIF
ENDDO
SET CENTURY ON
SET CURSOR OFF
CLOSE all
RELEASE tgl2,ksp,ada,n,no,hal,b,tot,tot2,tots
RETURN

PROCEDURE koplpiu1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 15
?chr(27)+chr(33)+chr(9)
?' DAFTAR PIUTANG'
?' PER TANGGAL: '+dtoc(tgl)+SPACE(50)+'Hal:'+tran(hal,'999')
?REPLACE('=',80)
?'  No.  Nama Pelanggan                   Alamat                        Jumlah Rp.'
?REPLACE('-',80)
N=1
RETURN

* DAFTAR PEMBAYARAN PIUT.
*-----------------------
PROCEDURE lbyrp
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
SELECT 2
Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')
SELECT 3
*
SELECT 6
Uselan('BAYARP.DTA','BAYARP1.NDX','BAYARP2.NDX','','','')
*
STORE tglx to tgl,tgl2
DO WHILE .t.
   Ksp=SPACE(10)
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,27 say 'Plgn.:'get ksp pict '@!'
   @3,27 say 'Tgl..:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Pelanggan'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   @4,24 to 4,59
   STORE ' ' to jlap
   @5,24 say '1=Tunai  2=BG/Cek  3=Semua  =>'get jlap pict '@!'
   Read
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   Do case
      Case jlap='1'
         Ket="left(uppe(jenis_byr),5)='TUNAI'"
      Case jlap='2'
         Ket="left(uppe(jenis_byr),2)='BG'"
      Other
         Ket="jenis_byr#SPACE(20)"
   ENDCASE
   SET CENTURY OFF
   STORE 0 to tot,tot2,hj
   If ksp=SPACE(10)
      @1,0 CLEAR to 23,79
      @1,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,21)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,15)
      @2,0 say 'Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Nama Pelanggan'+SPACE(6)+tl+' Jumlah Rp.'+tl+' Keterangan'
      @3,0 say REPLACE(gl,8)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,21)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,15)
      B=4
      Rtot2=0
      SELECT 6
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            @b,0 say dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_lgn+tran(jumlah_byr,'@e 9999,999,999  ')+jenis_byr
            Rtot2=rtot2+jumlah_byr
            B=b+1
            If b>20
               @b,0 say REPLACE(gl,80)
               Do pesan2 with 'ESC=Selesai   SPASI=Lanjut'
               If nlp=0
                  Exit
               ENDIF
               B=4
            ENDIF
         ENDIF
         Skip
      ENDDO
      @b,0 say REPLACE(gl,80)
      @b+1,0 CLEAR to 23,79
      @b+1,41 say 'Total Rp'+tran(rtot2,'@e  99,999,999,999')
      Do pesan3 with 'ESC=Selesai   F8=Hapus   F10=Cetak'
      If lastkey()=-7
         Klp=.t.
         Spl=.f.
         Do ebyr
      ENDIF
      If nlp=0
         Loop
      ENDIF
      Do cp
      If cc=0
         Loop
      ENDIF
      STORE 1 to no,hal
      STORE 0 to tot,tot2
      Do koplbyrp1
      Go nr
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            ?'  '+dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_lgn+tran(jumlah_byr,'@e  999,999,999  ')+jenis_byr
            Tot =tot +jumlah_byr
            Tot2=tot2+jumlah_byr
            No=no+1
            If no>60
               ?REPLACE('-',89)
               ?' Sub Total   Rp'+SPACE(37)+tran(tot ,'@e 99,999,999,999')
               ?REPLACE('-',89)
               ?''
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               Do pesan3 with gtkertas
               If nlp=0
                  Cc=0
                  Exit
               ENDIF
               Do cp
               If cc=0
                  Exit
               ENDIF
               Hal=hal+1
               Tot=0
               Do koplbyrp1
            ENDIF
         ENDIF
         Skip
      ENDDO
      If cc=0
         Loop
      ENDIF
      ?REPLACE('-',89)
      ?' Sub Total   Rp'+SPACE(37)+tran(tot ,'@e 99,999,999,999')
      ?REPLACE('-',89)
      ?' T o t a l   Rp'+SPACE(37)+tran(tot2,'@e 99,999,999,999')
      ?REPLACE('=',89)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   Else
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp =nama_lgn
      Algn=almt_lgn

      @1,0 CLEAR to 23,79
      @1,8 say 'Plgn.: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
      @2,7 say chr(218)+REPLACE(gl,9)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,21)+chr(191)
      @3,7 say tl+' Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Jumlah Rp.'+tl+' Keterangan'+SPACE(10)+tl
      @4,7 say chr(195)+REPLACE(gl,9)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,21)+chr(180)
      B=5
      Rtot2=0
      SELECT 6
      Set filter to kode_lgn=ksp
      LOCATE FOR kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      DO WHILE .not. eof()
         If kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            @b,7 say tl+' '+dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+tran(jumlah_byr,'@e  999,999,999  ')+jenis_byr+tl
            Rtot2=rtot2+jumlah_byr
            B=b+1
            If b>20
               @b,7 say chr(195)+REPLACE(gl,65)+chr(180)
               Do pesan2 with 'ESC=Selesai   SPASI=Lanjut'
               If nlp=0
                  Exit
               ENDIF
               B=4
            ENDIF
         ENDIF
         Skip
      ENDDO
      @b,7 say chr(192)+REPLACE(gl,65)+chr(217)
      @b+1,0 CLEAR to 23,79
      @b+1,28 say 'Total Rp'+tran(rtot2,'@e  99,999,999,999')
      Do pesan3 with 'ESC=Selesai   F8=Hapus   F10=Cetak'
      If lastkey()=-7
         Klp=.t.
         Spl=.f.
         Do ebyr
         Nlp=0
      ENDIF
      If nlp=0
         Loop
      ENDIF
      Do cp
      If cc=0
         Loop
      ENDIF
      STORE 1 to no,hal
      STORE 0 to tot,tot2
      Do koplbyrp2
      Go nr
      DO WHILE .not. eof()
         If kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            ?'| '+dtoc(tanggal)+'| '+dtoc(tanggal_3)+'| '+faktur+'|'+tran(jumlah_byr,'@e 999,999,999| ')+jenis_byr+' |'
            Tot =tot +jumlah_byr
            Tot2=tot2+jumlah_byr
            No=no+1
            If no>55
               ?'|'+REPLACE('-',66)+'|'
               ?''
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               Do pesan3 with gtkertas
               If nlp=0
                  Cc=0
                  Exit
               ENDIF
               Do cp
               If cc=0
                  Exit
               ENDIF
               Hal=hal+1
               Do koplbyrp2
            ENDIF
         ENDIF
         Skip
      ENDDO
      If cc=0
         Loop
      ENDIF
      ?'*'+REPLACE('=',66)+'*'
      ?' Total Rp '+tran(tot2,'@e 99,999,999,999')
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDDO
SET CURSOR OFF
SET CENTURY ON
CLOSE all
RELEASE jlap,tgl2,ksp,ada,n,no,hal,b,tot,tot2,tots,rtot2,algn,fkt,jml,bnk
RETURN

PROCEDURE koplbyrp1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 2
?chr(27)+chr(33)+chr(9)
?' DAFTAR PEMBAYARAN PIUTANG'
?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(50)+'Hal:'+tran(hal,'999')
?REPLACE('=',89)
?'  Tanggal   Jt.Tempo  No.BG/Fakt  Nama Pelanggan'+SPACE(6)+'  Jumlah Rp.  Keterangan'
?REPLACE('-',89)
No=1
RETURN

PROCEDURE koplbyrp2
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(9)
?'  DAFTAR PEMBAYARAN PIUTANG'
?'  PELANGGAN: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
?'             '+algn
?'  TANGGAL..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',66)+'*'
?'| Tanggal | Jt.Tempo| No.BG/Fakt| Jumlah Rp.|  Keterangan          |'
?'|'+REPLACE('-',66)+'|'
No=1
RETURN

* LAPORAN FAKTUR PENJUALAN
*--------------------------
PROCEDURE fktp
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
SELECT 2
Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')
SELECT 3
*
SELECT 4
*
SELECT 5
*
SELECT 6
Uselan('BAYARP.DTA','BAYARP1.NDX','BAYARP2.NDX','','','')
SELECT 7
Uselan('DATA.SET','','','','','')
Alm1=almt_1
Alm2=almt_2
Rpoin =rp_debet
Rpoin2=rp_kredit
Close
*
Ksp=SPACE(10)
STORE ' ' to hri,bln,thn,jab
STORE tglx to tgl,tgl2,tgl4
Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
Ada=.f.
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   @1,0 CLEAR to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Pelanggan  ( Kode Plgn. HARUS diisi Jika Mau Melakukan Pembayaran )'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Nsp='-'
   If ksp#SPACE(10)
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp=nama_lgn
      Jab=almt_lgn
   ENDIF
   STORE 0 to no,tot,tot2,totb
   @22,0 to 22,79
   @1,0 say REPLACE(REPLACE(gl,8)+chr(194),2)+REPLACE(gl,5)+chr(194)+REPLACE(gl,21)+REPLACE(chr(194)+REPLACE(gl,11),2)+chr(194)+REPLACE(gl,8)+chr(194)+gl
   @2,0 say 'Tanggal '+tl+'Jt.Tempo'+tl+'Fakt.'+tl+'Nama Pelanggan       '+tl+'   Rp.Total'+tl+'   Rp.Bayar'+tl+'Tgl.Byr.'+tl+chr(251)
   @3,0 say REPLACE(REPLACE(gl,8)+chr(197),2)+REPLACE(gl,5)+chr(197)+REPLACE(gl,21)+REPLACE(chr(197)+REPLACE(gl,11),2)+chr(197)+REPLACE(gl,8)+chr(197)+gl
   SELECT 2
   If ksp=SPACE(10)
      Set order to 1
      Set filter to ( (tanggal>=tgl .and. tanggal<=tgl2) )
      Seek dtos(tgl)+ksp
      If .not. FOUND()
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp
            If .not. FOUND()
               LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
               If .not. FOUND()
                  Do pesan2 with 'Tidak ada Data Faktur Penjualan'
                  Loop
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   Elseif ksp#SPACE(10)
      Ada=.t.
      Set order to 2
      Set filter to ( (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp )
      Seek ksp+dtos(tgl)
      If .not. FOUND()
         LOCATE FOR kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
            If .not. FOUND()
               Do pesan2 with 'Tidak ada Data Faktur Penjualan'
               Loop
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   Nr=recno()
   Pesanx='SPASI=Pilih   B/L=Bayar   F1=Bk.Piutang  F5=Edit.Byr  F8=Hps  F9=JT  F10=Cetak'
   Do pesan with pesanx
   SET CENTURY OFF
   DECLARE field[9],bts[9],frm[9]
   Field[1]='TANGGAL'
   Field[2]='TANGGAL_3'
   Field[3]='FAKTUR'
   Field[4]='left(NAMA_LGN,21)'
   Field[5]='(RP_TOTAL-RP_DISKON)'
   Field[6]='RP_BAYAR'
   Field[7]='TANGGAL_4'
   Field[8]='ID_BAYAR'
   Frm[1]='99-99-99'
   Frm[2]='99-99-99'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@e 999,999,999'
   Frm[6]='@e 999,999,999'
   Frm[7]='99-99-999'
   Frm[8]='@!'
   Frm[9]='99-99-999'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl
   Bts[4]=tl
   Bts[5]=tl
   Bts[6]=tl
   Bts[7]=tl
   Bts[8]=tl
   Bts[9]=tl
   DbEdit(4,0,21,79,field,'ffktp',frm,'','',bts)
   *
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE fkt1,fkt2,fkt3,fkt4,fkt5,tgl2,tgl3,tgl4,;
     ket,tot,tot2,totx,toty,tots,tot2s,totxs,totys,ksp,jml,hj,yt,no,hal,alm1,alm2
     
RETURN

Function ffktp
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-4
      B  =row()
      Jml=rp_bayar
      Hj =tanggal_4
      Tone(1000,3)
      Tone(2000,6)
      SET CENTURY ON
      SET CURSOR ON
      Do pesan with 'Pastikan Data di Laporan Pembayaran Sudah diHapus lebih dulu...!'
      @b,58 get jml pict '@e 999,999,999'
      @b,70 get hj
      Read
      If lastkey()#27 .and. (rp_bayar#jml .or. tanggal_4#hj)
         Rlok()
         REPLACE rp_bayar with jml,tanggal_4 with hj
         Ul()
         COMMIT
      ENDIF
      Do pesan with pesanx
      SET CENTURY OFF
      SET CURSOR OFF
      RETURN 1
   Case lastkey()=-7
      SET CENTURY ON
      Tgl4=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      Nsls=nama_ksr
      Do bacafile
      SET CENTURY OFF
      *
      SET COLOR TO gr+*
      @row(),col() say &fds
      STORE ' ' to yt,yt2
      SET CURSOR ON
      SET COLOR TO g
      Do pesan with 'Item Faktur DiHapus? Y/T:          Item Penj.Brg ikut diHapus? Y/T:'
      DO WHILE .not. yt$'YT'
         @24,31 get yt pict '!'
         @24,74 get yt2 pict '!'
         Read
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
      If yt='Y' .or. yt2='Y'
         If yt='Y'
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
         ENDIF
         If yt2='Y'
            SELECT 3
            Uselan('STOK.DTA','STOK5.NDX','','','','')
            *
            SELECT 4
            Uselan('&nf0','&nf1','&nf2','&nf3','','')
            Seek dtos(tgl)+ksp+fkt
            DO WHILE .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_lgn=ksp .and. faktur=fkt )
               Lks=kode_lks
               Kdb=kode_bar
               Jml=jumlah
               Isi=1
               *
               Rlok()
               Dele
               Ul()
               COMMIT
               *
               SELECT 3
               Seek kdb
               Rlok()
               REPLACE stok_k&lks with stok_k&lks-(jml*isi)
               Ul()
               COMMIT
               *
               SELECT 4
               Skip
            ENDDO
            Close
            *
            SELECT 6
            LOCATE FOR kode_lgn=ksp .and. faktur=fkt
            If FOUND()
               Rlok()
               Dele
               Ul()
               COMMIT
            ENDIF
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      Tgl=tgl4
      SELECT 2
      RETURN 2
   Case lastkey()=-8
      Nr=recno()
      SET CENTURY ON
      SET CURSOR ON
      STORE tglx to tgl3,tgl4
      Do pesan with 'Jt.Tempo Tanggal:           s/d           '
      @24,36 get tgl3
      @24,51 get tgl4
      Read
      If lastkey()#27
         Save scre to smkdb
         Do pesan with 'ESC=Menu Selanjutnya'
         Copy to JT-TEMPO.TMP for ( (tanggal_3>=tgl3 .and. tanggal_3<=tgl4) .and. tanggal_4=ctod('  -  -    ') .and. rp_bayar < (rp_total-rp_diskon) )
         SELECT 99
         Useidx('JT-TEMPO.TMP')
         Sort on tanggal_3 to JT-TEMPO.DTA

         Useidx('JT-TEMPO.DTA')
         Go top

         STORE 1 to n,no,hal
         STORE 0 to tot,tot2,totx,toty,tots,tot2s,totxs,totys
         SET CURSOR ON
         Set alte on
         Set alte to JT-TEMPO.RTF
         ?' DAFTAR FAKTUR PEMBELIAN'
         ?' JT.TEMPO TANGGAL: '+dtoc(tgl3)+' s/d '+dtoc(tgl4)
         ?REPLACE(gl,77)
         ?' No. Jt.Tempo  Tgl.Jual  Faktur  Nama Pelanggan                   Jumlah Rp.'
         ?REPLACE(gl,77)

         SET CENTURY OFF
         DO WHILE .not. eof()
            Jml =((rp_total-rp_diskon)-rp_bayar)
            Totx=totx+jml
            ?tran(n,'999. ')+dtoc(tanggal_3)+'  '+dtoc(tanggal)+'  '+faktur+'   '+nama_lgn+' '+kode_lgn+tran(jml,'@e  999,999,999')

            N =n+1
            No=no+1
            Skip
         ENDDO
         ?REPLACE(gl,77)
         ?SPACE(52)+'Total = Rp'+tran(totx,'@e 99,999,999,999')
         ?REPLACE(gl,77)
         ?' '
         Set alte off
         Set alte to
         Memoedit(Memoread('JT-TEMPO.RTF'),1,0,22,240,.t.,'',240)
         Do pesan3 with 'ESC=Selesai   F10=Cetak'
         If lastkey()#27
            Do cp
            If lastkey()#27
               SET PRINTER ON
               SET DEVICE TO PRINTER
               ?chr(27)+chr(33)+chr(9)
               Set marg to 10
               Type JT-TEMPO.RTF to prin
               SET PRINTER OFF
               SET DEVICE TO SCREEN
            ENDIF
         ENDIF
         Rest scre from smkdb
      ENDIF
      Do pesan with pesanx
      SET CENTURY OFF
      SET CURSOR OFF
      SELECT 2
      Go nr
      RETURN 2
   Case lastkey()=-9
      Go nr
      STORE 1 to no,hal
      STORE 0 to tot,tot2,totx,toty,tots,tot2s,totxs,totys
      SET CURSOR ON
      Do cp
      If cc=0
         Do pesan with pesanx
         SET CURSOR OFF
         RETURN 1
      ENDIF
      Do kopfktp
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jml=((rp_total-rp_diskon)-rp_bayar)
            Tot =tot +(rp_total-rp_diskon)
            Tot2=tot2+rp_bayar
            Totx=totx+jml
            Tots =tots +(rp_total-rp_diskon)
            Tot2s=tot2s+rp_bayar
            Totxs=totxs+jml

            ?'| '+dtoc(tanggal)+' | '+dtoc(tanggal_3)+' | '+faktur+' | '+left(nama_lgn,25)+'|'+tran((rp_total-rp_diskon),'@e 999,999,999 |')+;
             tran(rp_bayar,'@e  999,999,999 |')+tran(jml,'@e 999,999,999 |')
            No=no+1
            If no>55
               ?'|'+REPLACE('-',96)+'|'
               ?'| Jumlah Hal ini Rp'+SPACE(38)+tran(tots,'@e 9999,999,999 ')+tran(tot2s,'@e  9999,999,999 ')+tran(totxs,'@e  999,999,999 |')
               ?'|'+REPLACE('-',96)+'|'
               ?''
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               Do pesan3 with gtkertas
               If nlp=0
                  Cc=0
                  Exit
               ENDIF
               Do cp
               If cc=0
                  Exit
               ENDIF
               Hal=hal+1
               STORE 0 to tots,tot2s,totxs,totys
               Do kopfktp
            ENDIF
         ENDIF
         Skip
      ENDDO
      If cc=0
         Do pesan with pesanx
         SET CURSOR OFF
         RETURN 1
      ENDIF
      If hal>1
         ?'|'+REPLACE('-',96)+'|'
         ?'| Jumlah Hal ini Rp'+SPACE(38)+tran(tots,'@e 9999,999,999 ')+tran(tot2s,'@e  9999,999,999 ')+tran(totxs,'@e  999,999,999 |')
      ENDIF
      ?'|'+REPLACE('-',96)+'|'
      ?'| Grand Total    Rp'+SPACE(38)+tran(tot ,'@e 9999,999,999 ')+tran(tot2,'@e  9999,999,999 ')+tran(totx,'@e  999,999,999 |')
      ?'*'+REPLACE('=',96)+'*'
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      Go nr
      RETURN 1
   Case lastkey()=13
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      B   =6
      No  =1
      SET CENTURY ON
      Do bacafile
      SET CENTURY OFF
      *
      Save scre to sdaf
      SET COLOR TO bg+/b
      @4,0 say dtoc(tanggal)+tl+dtoc(tanggal_3)+tl+faktur+tl+left(nama_lgn,21)+tl+tran((rp_total-rp_diskon),'@e 999,999,999')+tl+tran(rp_bayar,'@e 999,999,999')+tl+dtoc(tanggal_4)+tl+id_bayar
      SET COLOR TO /bg
      @2,38 say ':'+ksp
      @5,0 to 5,79 doub
      @6,0 CLEAR to 22,79
      @20,0 to 20,79
      @21,1 say 'Diskon:'
      @21,31 say 'Bayar Rp..:'
      @22,31 say 'User/Kasir:'
      @21,59 say 'Keterangan:'
      SET COLOR TO /bg
      @21,8 say tran(diskon,'@e 99.99% Rp')+tran(rp_diskon,'@e 999,999.99')
      @21,42 say 'Rp '+tran(rp_bayar,'@e 99,999,999')
      @22,42 say left(nama_ksr,15)
      @22,59 say keterangan
      STORE 0 to tot,totpn,totpn2,pnskr,pnskr2
      *
      SELECT 4
      Uselan('&nf0','&nf1','','','','')
      Seek dtos(tgl)+ksp+fkt
      DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_lgn=ksp .and. faktur=fkt )
         Kdby=kode_bar
         Kdbx=iif(len(RTRIM(kdby))>=7,left(kdby,2)+'x'+right(RTRIM(kdby),4),left(kdby,7))
         Ipn =id_point
         Ipn2=id_point2
         Jml =(jumlah-jumlah_rtr)
         Hj  =harga_2
         Dis1=diskon
         Jhg =jml*hj
         Tot=tot+jhg
         If (ipn=' ' .and. ipn2=' ') .and. rpoin>0
            Totpn =totpn  + (jml * (hj - (hj*(dis1/100))))
            Pnskr =(totpn-mod(totpn,rpoin))/rpoin
         ENDIF
         If (ipn=' ' .and. ipn2#' ') .and. rpoin2>0
            Totpn2=totpn2 + (jml * (hj - (hj*(dis1/100))))
            Pnskr2=(totpn2-mod(totpn2,rpoin2))/rpoin2
         ENDIF
         *
         If jml#0
            @b,0 say tran(no,'999. ')+kdbx+' '+left(nama_brg,30)+tran(jml,'@e 9999.99 ')+;
                     satuan+kode_lks+tran(hj,'@e  99,999,999 ')+tran(jml*hj,'@e 999,999,999')
            B =b+1
            No=no+1
         ENDIF
         If b>19
            Do pesan with 'Pgln=Lanjut'
            Wait''
            SET COLOR TO /bg
            B=6
         ENDIF
         Skip
      ENDDO
      Close
      @b,0 CLEAR to 19,79
      Do pesan with 'ESC=Selesai'
      Wait''
      Rest scre from sdaf
      Do pesan with pesanx
      SELECT 2
      If tot>0
         Rlok()
         REPLACE rp_total with tot,jml_poin with pnskr+pnskr2
         Ul()
         COMMIT
      ENDIF
      RETURN 1
   Case lastkey()=72 .or. lastkey()=104
      Do pesan with 'Tunggu, sedang diHitung'
      Totb=0
      Nr=recno()
      Set order to 1
      Sum (rp_total-rp_diskon),rp_bayar to tot,tot2 for (tanggal>=tgl .and. tanggal<=tgl2)
      Go nr
      SET COLOR TO gr+
      @22,34 say 'Total=Rp'+tran(tot,'@e 999,999,999,999')
      SET COLOR TO g
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=32
      Tot=((rp_total-rp_diskon)-rp_bayar)
      Rlok()
      If id_bayar=' '
         REPLACE id_bayar with chr(251)
         Totb=totb+tot
      Elseif id_bayar#' '
         REPLACE id_bayar with ' '
         Totb=totb-tot
      ENDIF
      Ul()
      COMMIT
      @22,0 to 22,79
      @22,51 say 'Yang Mau diBayar:'+tran(totb,'@e 99,999,999')
      Do pesan with pesanx
      RETURN 1
   Case lastkey()=28
      Save scre to smkdb
      SET CURSOR ON
      Nr =recno()
      Ksp=kode_lgn
      *
      Do bukuh
      SET CURSOR OFF
      Rest scre from smkdb
      Do pesan with pesanx
      SELECT 2
      Go nr
      RETURN 1
   Case lastkey()=66 .or. lastkey()=98
      If .not. ada
         Do pesan2 with 'Untuk Pembayaran Kolektif, Kode Pelanggan Harus diIsi'
         Do pesan with pesanx
         SELECT 2
         RETURN 1
      ENDIF
      No  =recno()
      Totb=0
      *
      Go nr
      DO WHILE .not. eof() .and. kode_lgn=ksp
         If id_bayar#' '
            Totb=totb+((rp_total-rp_diskon)-rp_bayar)
         ENDIF
         Skip
      ENDDO
      *
      Go no
      Tgl4=tgl
      Tgl =tanggal
      Ksp =kode_lgn
      Fkt =faktur

      Save scre to smkdb
      SET CURSOR ON
      *
      SELECT 1
      Seek ksp
      Jmlso=rp_saldo
      Tglso=tanggal
      STORE 0 to jml,jml2
      *
      SELECT 2
      xFilter=dbfilter()
      Set filter to
      Set order to 2
      Seek ksp
      DO WHILE .not. eof() .and. kode_lgn=ksp
         If (tanggal>=tglso .and. tanggal<tgl)
            Jml=jml+(rp_total-rp_diskon)
         ENDIF
         Skip
      ENDDO
      *
      SELECT 6
      Set order to 2
      Seek ksp
      DO WHILE .not. eof() .and. kode_lgn=ksp
         If (tanggal>=tglso .and. tanggal<tgl)
            Jml2=jml2+jumlah_byr
         ENDIF
         Skip
      ENDDO
      Tot2=(jmlso+jml+totb)-jml2
      *
      @17,0 CLEAR to 23,79
      @17,0 to 22,79
      SET CENTURY ON
      Ket ='Tunai'+SPACE(15)
      Tgl3=tglx
      DO WHILE .t.
         *@13,48 say 'Jumlah Piutang..=Rp'+tran(tot2,'@e 9999,999,999')
         @19,2  say 'Tanggal Pembayaran....: 'get tgl3
         @20,2  say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 999,999,999'
         @19,48 say 'Keterangan/Mutasi:'
         @20,48 get ket
         *
         Tot3=tot2-totb
         *@22,52 say 'Sisa Piutang Rp'+tran(tot3,'@e  999,999,999')
         Yt=' '
         Do pesan with 'Data Benar? Y/T'
         DO WHILE .not. yt$'YT'
            @24,48 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            ENDIF
         ENDDO
         If lastkey()=27 .or. yt#'Y'
            Exit
         ENDIF
         *
         SELECT 2
         Go nr
         DO WHILE .not. eof() .and. kode_lgn=ksp
            If id_bayar#' '
               Tot=((rp_total-rp_diskon)-rp_bayar)
               Rlok()
               REPLACE id_bayar with ' '
               If totb<tot
                  REPLACE rp_bayar with (rp_bayar+totb),keterangan with 'TITIP, MSH ADA SISA.'
               Elseif totb>=tot
                  REPLACE rp_bayar with (rp_total-rp_diskon),keterangan with ket,tanggal_4 with tgl3
               ENDIF
               Ul()
            ENDIF
            Skip
         ENDDO
         COMMIT
         Go no
         *
         SELECT 6
         If totb>0
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgl3,faktur with fkt,kode_lgn with ksp,nama_lgn with nsp,jumlah_byr with totb,jenis_byr with ket
            Ul()
            COMMIT
         ENDIF
         Exit
      ENDDO
      SET CURSOR OFF
      SET CENTURY OFF
      Rest scre from smkdb
      Do pesan with pesanx
      SELECT 2
      Set filter to &xFilter
      Go no
      Tgl=tgl4
      RETURN 2
   Case lastkey()=76 .or. lastkey()=108
      Tgl4=tgl
      Tgl =tanggal
      Ksp =kode_lgn
      Nsp =nama_lgn
      Fkt =faktur
      STORE ((rp_total-rp_diskon)-rp_bayar) to tot,totb
      xFilter=dbfilter()
      *
      Save scre to smkdb
      SET CURSOR ON
      *
      @17,0 CLEAR to 23,79
      @17,0 to 22,79
      SET CENTURY ON
      Ket ='Nota No:'+fkt+'       '
      Tgl3=tglx
      @19,2  say 'Tanggal Pembayaran....: 'get tgl3
      @20,2  say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 999,999,999'
      @19,48 say 'Keterangan/Mutasi:'
      @20,48 get ket
      Yt=' '
      Do pesan with 'Data Benar? Y/T'
      DO WHILE .not. yt$'YT'
         @24,48 get yt pict '!'
         Read
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
      If yt='Y'
         Rlok()
         REPLACE rp_bayar with rp_bayar+totb,tanggal_4 with iif(totb>=tot,tgl3,ctod('  -  -    ')),id_bayar with ' '
         Ul()
         COMMIT
         *
         SELECT 6
         If totb>0
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgl3,faktur with fkt,kode_lgn with ksp,nama_lgn with nsp,jumlah_byr with totb,jenis_byr with ket
            Ul()
            COMMIT
         ENDIF
      ENDIF
      SET CURSOR OFF
      SET CENTURY OFF
      Rest scre from smkdb
      Do pesan with pesanx
      SELECT 2
      Set filter to &xFilter
      Tgl=tgl4
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE kopfktp
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 15
?chr(27)+chr(33)+chr(4)
?' DAFTAR FAKTUR PENJUALAN'
If ksp#SPACE(10)
   ?' PELANGGAN: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
ENDIF
?' TANGGAL..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(57)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',96)+'*'
?'| Tanggal  | Jt.Tempo | No.Fkt| Nama Pelanggan           |     Jumlah |       Bayar | Kekurangan |'
?'|'+REPLACE('-',96)+'|'
No=1
RETURN

* EOF LPIU
