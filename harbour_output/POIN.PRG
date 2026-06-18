// ============================================================
// File    : POIN.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN POIN
*-------------
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
SELECT 2
Uselan('FAKTURP.DTA','FAKTURP2.NDX','FAKTURP1.NDX','','','')
SELECT 3
Uselan('STOK.DTA','STOK.NDX','','','','')
SELECT 4
*
SELECT 5
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
*Tgl   =tanggal_2
*Tgl2  =tanggal_3
Close
*
Ksp=SPACE(10)
STORE ' ' to hri,bln,thn,xbayar
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'F1=Daftar Pelanggan     ESC=Selesai'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   Nsp='-'
   If ksp#SPACE(5)
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp=nama_lgn
   ENDIF
   STORE 0 to no,tot,tot2
   Do konvtgl
   SET CENTURY OFF
   @1,0 to 1,79
   @22,0 to 22,79
   @1,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,8)+chr(194)+REPLACE(gl,28)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,5)
   @2,0 say 'Tanggal '+tl+'Nota '+tl+'Kasir    '+tl+'Kode    '+tl+'Nama Pelanggan/Member       '+tl+' Jumlah Rp.'+tl+'Point'
   @3,0 say REPLACE(gl,8)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,8)+chr(197)+REPLACE(gl,28)+chr(197)+REPLACE(gl,11)+chr(197)+REPLACE(gl,5)
   SELECT 2
   If ksp#SPACE(10)
      Set order to 1
      Set filter to kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Seek ksp+(thn+bln+hri)
      If .not. FOUND()
         Seek ksp
         If .not. FOUND()
             Do pesan2 with 'Tidak ada Data Nota Penjualan'
             Loop
         ENDIF
      ENDIF
   Else
      Set order to 2
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek (thn+bln+hri)
      If .not. FOUND()
         LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. FOUND()
             Do pesan2 with 'Tidak ada Data Nota Penjualan'
             Loop
         ENDIF
      ENDIF
   ENDIF
   Nr=recno()
   Pesanx='H=Hitung_Poin     F1=Kode_Plgn     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   SET CENTURY OFF
   DECLARE field[9],bts[9],frm[9]
   Field[1]='TANGGAL'
   Field[2]='FAKTUR'
   Field[3]='left(NAMA_KSR,9)'
   Field[4]='left(KODE_LGN,8)'
   Field[5]='left(NAMA_LGN,28)'
   Field[6]='RP_TOTAL'
   Field[7]='JML_POIN'
   Frm[1]='99-99-99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@!'
   Frm[6]='@e 999,999,999'
   Frm[7]='9999 '
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   Bts[6]=''+tl
   Bts[7]=''+tl
   DbEdit(4,0,21,79,field,'fpoin',frm,'','',bts)
   *
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

Function fpoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=28
      Nr=recno()
      Klb=SPACE(5)
      SET CURSOR ON
      Do pesan with ''
      SET COLOR TO w+/r
      @23,32 say 'Kode Plgn.:'get klb pict '@!'
      Read
      If lastkey()#27 .and. klb#SPACE(5)
         Klb=RTRIM(klb)
         SET COLOR TO g
         Set order to 2
         Seek klb
         If .not. FOUND()
           Go nr
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-7
      Tgl3=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      SET COLOR TO gr+*
      @row(),col() say &fds
      Yt=' '
      SET CURSOR ON
      SET COLOR TO g
      Do pesan with 'Data DiHapus? Y/T'
      DO WHILE .not. yt$'YT'
         @24,49 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Yt=' '
         SET CURSOR ON
         SET COLOR TO g
         Do pesan with 'Data Penjualan ikut DiHapus? Y/T'
         DO WHILE .not. yt$'YT'
            @24,58 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            ENDIF
         ENDDO
         If lastkey()=27
            Do pesan with pesanx
            SET CURSOR OFF
            RETURN 1
         ENDIF   
         If .not. deleted()
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
            SET CENTURY ON
            Do konvtgl
            *
            If yt='Y'
               Do bacafile
               If .not. file('&nf0')
                  Do pesan2 with 'Tidak ada File '+&nf0
                  Do pesan with pesanx
                  SET CURSOR OFF
                  RETURN 1
               ENDIF
               *
               SELECT 4
               Uselan('&nf0','&nf1','&nf2','&nf3','','')
               Seek dtos(tgl)+fkt+ksp
               DO WHILE .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_lgn=ksp .and. faktur=fkt )
                  Kdb=kode_brg
                  Jml=jumlah
                  Rlok()
                  Dele
                  Ul()
                  COMMIT
                  *
                  SELECT 3
                  Seek kdb
                  If FOUND()
                     Rlok()
                     REPLACE stok_k with (stok_k-jml)
                     Ul()
                     COMMIT
                  ENDIF
                  *
                  SELECT 4
                  Skip
               ENDDO
               Close
            ENDIF
         ENDIF
         Tgl=tgl3
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      SET CENTURY OFF
      SELECT 2
      RETURN 2
   Case lastkey()=-9
      STORE 1 to no,hal
      STORE 0 to tot,tot2,pnskr,totpn
      SET CURSOR ON
      Do cp
      If cc=0
         RETURN 0
      ENDIF
      Do kopoin
      Go top
      STORE kode_lgn to ksp,ksp2
      STORE nama_lgn to nsp,nsp2
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Tot  =tot  +rp_total
            Tot2 =tot2 +rp_total
            Pnskr=pnskr+jml_poin
            Totpn=totpn+jml_poin
            ?ksp+' '+left(nsp,25)+' '+faktur+' '+dtoc(tanggal)+' '+jam+' '+tran(rp_total,'@e 999,999,999 ')+tran(jml_poin,'9999')
            * 
            No=no+1
            If no>50
               ?REPLACE(gl,80)
               ?' '
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
               Do kopoin
            ENDIF
         ENDIF
         Skip
         If kode_lgn#ksp2
            STORE kode_lgn to ksp,ksp2
            STORE nama_lgn to nsp,nsp2
            ?SPACE(75)+tran(pnskr,'9999')
            Pnskr=0
         Elseif kode_lgn=ksp2
            Ksp=SPACE(10)
            Nsp=SPACE(30)
         ENDIF
      ENDDO
      ?REPLACE(gl,80)
      ?'          Grand Total'+SPACE(34)+tran(tot2,'@e 99,999,999,999 ')+tran(totpn,'@e 9,999,999')
      ?REPLACE(gl,80)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
   Case lastkey()=13
      Tglz=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      B   =6
      No  =1
      Save scre to sdaf
      @4,0 say dtoc(tanggal)+tl+faktur+tl+left(nama_ksr,9)+tl+left(kode_lgn,8)+tl+left(nama_lgn,28)+tl+tran(rp_total,'@e 999,999,999')+tl+tran(jml_poin,'9999')
      SET COLOR TO /bg
      @5,0 to 5,79
      @6,0 CLEAR to 22,79
      @22,0 to 22,79
      SET CENTURY ON
      Do konvtgl
      Do bacafile
      If .not. file('&nf0')
         Do pesan2 with 'Tidak ada File '+nf0
         Do pesan with pesanx
         SET CURSOR OFF
         SET CENTURY OFF
         RETURN 1
      ENDIF
      STORE 0 to tot,tot2,totpn,totpn2
      *
      SELECT 4
      Uselan('&nf0','&nf1','','','','')
      Seek dtos(tgl)+ksp+fkt
      DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_lgn=ksp .and. faktur=fkt )
         Kdby=kode_bar
         Kdbx=iif(len(RTRIM(kdby))>=7,left(kdby,2)+'x'+right(RTRIM(kdby),4),left(kdby,7))
         Jml =(jumlah-jumlah_rtr)
         Hj  =harga_2
         Dis1=diskon
         Jhg =jml*hj
         Tot=tot+jhg
         *
         If (id_point=' ' .and. id_point2=' ') .and. rpoin>0
            Totpn =totpn  + (jml * (hj - (hj*(dis1/100))))
         ENDIF
         If (id_point=' ' .and. id_point2#' ') .and. rpoin2>0
            Totpn2=totpn2 + (jml * (hj - (hj*(dis1/100))))
         ENDIF
         *
         If jml#0
            @b,0 say tran(no,'999. ')+kdbx+' '+left(nama_brg,30)+tran(jml,'@e 9999.99 ')+;
                     satuan+kode_lks+tran(hj,'@e  99,999,999 ')+tran(jml*hj,'@e 999,999,999')
            B =b+1
            No=no+1
         ENDIF
         If b>21
            Do pesan with 'Pgln=Lanjut'
            Wait''
            SET COLOR TO /bg
            B=6
         ENDIF
         Skip
      ENDDO
      @b,1 CLEAR to 21,79
      Do pesan with 'ESC=Selesai'
      Close
      Wait''
      *
      Pnskr =iif(rpoin>0 ,(totpn -mod(totpn ,rpoin ))/rpoin , 0 )
      Pnskr2=iif(rpoin2>0,(totpn2-mod(totpn2,rpoin2))/rpoin2, 0 )
      Tgl  =tglz
      Rest scre from sdaf
      Do pesan with pesanx
      SET CENTURY OFF
      SELECT 2
      If tot#0
         Rlok()
         REPLACE rp_total with tot,jml_poin with (pnskr+pnskr2)
         Ul()
         COMMIT
      ENDIF
      Rest scre from sdaf
      Do pesan with pesanx
      SET CENTURY OFF
      RETURN 1
   Case lastkey()=72 .or. lastkey()=104
      Nr=recno()
      Yt=' '
      SET CURSOR ON
      Do pesan with '1=Hanya_hitung_saja     2=Hitung_ulang_dan_simpan_poin : '
      @24,68 get yt pict '!'
      Read
      Do pesan with 'Tunggu, sedang diHitung'
      If yt#'2' .and. lastkey()#27
         STORE 0 to tot,tot2,totpn,pnskr
         *
         SELECT 1
         Seek ksp
         Ajp=aj_point
         Amp=am_point
         *
         SELECT 2
         Set order to 1
         Seek ksp
         DO WHILE .not. eof() .and. kode_lgn=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Tot =tot +(rp_total-rp_diskon)
               Tot2=Tot2+rp_pokok
               Totpn=totpn+jml_poin
            ENDIF
            Skip
         ENDDO
         *
         Go nr
         SET COLOR TO gr+
         @22,1  say 'Total=Rp '+LTRIM(tran(tot,'@e 9,999,999,999'))
         @22,25 say 'Margin=Rp '+LTRIM(tran(tot-tot2,'@e 999,999,999'))
         @22,50 say 'Poin= '+LTRIM(tran((totpn+ajp)-amp,'999,999  ( '))+LTRIM(tran(totpn,'@e 999999 + '))+LTRIM(tran(ajp,'@e 999999 - '))+LTRIM(tran(amp,'@e 999999 )'))
      Elseif yt='2' .and. rpoin>0
         STORE 0 to tot,tot2,totpn,pnskr
         Syarat=iif(ksp#SPACE(10),'kode_lgn=ksp','kode_lgn#SPACE(10)')
         *
         Go nr
         DO WHILE .not. eof() .and. &syarat
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Pnskr =iif(rpoin>0 ,(rp_poin -mod(rp_poin ,rpoin ))/rpoin , 0 )
               Pnskr2=iif(rpoin2>0,(rp_poin2-mod(rp_poin2,rpoin2))/rpoin2, 0 )
               Rlok()
               REPLACE jml_poin with (pnskr+pnskr2)
               Ul()
            ENDIF
            Skip
         ENDDO
         COMMIT
         *
         Go nr
      ENDIF
      SET COLOR TO g
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 2
  Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE kopoin
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(9)
?' DAFTAR PEROLEHAN POINT BELANJA'
?' TANGGAL.: '+dtoc(tgl)+' S/D '+dtoc(tgl2)+SPACE(17)+'Hal:'+tran(hal,'999')
?REPLACE(gl,80)
?'Kode       Nama Pelanggan            Nota/Tgl/Jam          Jumlah Rp.  Jml.Poin'
?REPLACE(gl,80)
No=1
RETURN

*LAPORAN POINT/GLOBAL
*--------------------
PROCEDURE poin2
SELECT 1
Uselan('CUST.DTA','CUST2.NDX','','','','')
SELECT 2
Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
SELECT 3
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
Tgl   =tanggal_2
Tgl2  =tanggal_3
Close
*
STORE ' ' to hri,bln,thn
DO WHILE .t.
   SET CENTURY ON
   SET COLOR TO /bg,w+/b
   @1,0 CLEAR to 23,79
   @2,24 say 'Tanggal'get tgl
   @2,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   SELECT 4
   Useidx('TOP-POIN.DTA')
   Copy stru to TOP-POIN.TMP
   Close
   *
   Useidx('TOP-POIN.TMP')
   *
   STORE 0 to tot,tot2,totx,jml,jml2,pnskr,totp,tajp,tamp
   STORE 1 to hal,no,n
   Ada=.f.
   *
   SELECT 1
   Go top
   DO WHILE .not. eof()
      Ksp =kode_lgn
      Nsp =nama_lgn
      Algn=almt_lgn
      Ajp  =aj_point
      Amp  =am_point
      Tglhp=tgl_ambil   &&&DIHITUNG MULAI TGL.AMBIL TERAKHIR
      Jamx =jam_ambil
      *
      SELECT 2
      Seek ksp
      DO WHILE .not. eof() .and. kode_lgn=ksp
         If tanggal=tglhp .and. jam>jamx
            Jml =jml +(rp_total-rp_diskon)
            Jml2=jml2+jml_poin
            Tot =tot +(rp_total-rp_diskon)
            Tot2=tot2+rp_pokok
         Elseif tanggal>tglhp
            Jml =jml +(rp_total-rp_diskon)
            Jml2=jml2+jml_poin
            Tot =tot +(rp_total-rp_diskon)
            Tot2=tot2+rp_pokok
         ENDIF
         Skip
      ENDDO
      If jml2>0 .or. jml>0 .or. ajp>0
         Ada=.t.
         *
         SELECT 4
         APPEND BLANK
         REPLACE kode with ksp,nama with nsp,alamat with algn,rp_jual with jml,rp_pokok with tot2,rp_margin with jml-tot2,;
              jm_poin with jml2,aj_poin with ajp,am_poin with amp,so_poin with (jml2+ajp)
         *
         Totp =totp+jml2
         Totx =totx+(jml-tot2)
         Tajp =tajp+ajp
         No=no+1
      ENDIF
      STORE 0 to jml,jml2,tot2
      *
      SELECT 1
      Skip
   ENDDO
   *
   If ada
      SELECT 6
      Close
      *
      SELECT 5
      Close
      *
      SELECT 4
      Sort on nama       to TOP-PNMA.TMP
      Sort on rp_jual /D to TOP-PRPH.TMP
      Sort on so_poin /D to TOP-PPNT.TMP
      Close
      *
      Pesanx='N/R/P=Top_Ranking     F1=Cari_Nama    F10=Cetak'
      SELECT 4
      Useidx('TOP-PNMA.TMP')
      SELECT 5
      Useidx('TOP-PPNT.TMP')
      SELECT 6
      Useidx('TOP-PRPH.TMP')
      *
      SET CENTURY OFF
      SET COLOR TO w/b,w+/g
      Ket='TOP NOTA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0 CLEAR to 23,79
      @1,1 say ket
      @2,0 to 2,79 doub
      @21,0 to 21,79
      DO WHILE .t.
         DECLARE field[11],bts[11],frm[11],kop[11]
         Do pesan with pesanx
         SET COLOR TO w/b,w+/g
         @22,0 say ' Nota   Rp'+tran(tot ,'@e 999,999,999,999')+'     Jml.Poin:'+tran(totp,'@e 9999,999')
         @23,0 say ' Margin Rp'+tran(totx,'@e 999,999,999,999')+'     Adjust..:'+tran(tajp,'@e 9999,999')+'     Sisa.:'+tran((totp+tajp)-tamp,'@e 9999,999')
         Kata1="iif(JM_POIN>0,tran(JM_POIN,'@e  999,999 '),SPACE(9))+iif(AJ_POIN>0,tran(AJ_POIN,'@e 999,999 '),SPACE(8))+iif(SO_POIN>0,tran(SO_POIN,'@e 999,999'),SPACE(7))"
         Field[1]="left(KODE,8)+' '+left(NAMA,23)+' '+left(ALAMAT,22)+tran(RP_JUAL,'@e 99999,999,999')+tran(RP_MARGIN,'@e 9999,999,999')"
         Field[2]="left(KODE,8)+' '+left(NAMA,23)+' '+left(ALAMAT,23)+&kata1"
         Kop[1]='Kode     Nama_Pelanggan          Alamat                      Rp_Nota   Rp_Margin'
         Kop[2]='Kode     Nama_Pelanggan          Alamat                    Jml.P  Adjust    Sisa'
         STORE '' to frm[1],frm[2],bts[1],bts[2]
         DbEdit(3,0,20,79,field,'flpoin',frm,kop,gl,bts)
         *
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
   Elseif .not. ada
      Tone(1500,3)
      Do pesan with 'Tidak Ada Data Penjualan'
      Wait''
   ENDIF
ENDDO
SET CENTURY ON
SET CURSOR OFF
CLOSE all
RETURN

Function flpoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   @21,1 say REPLACE(gl,15)
   SET COLOR TO gr+/b
   @21,1 say LTRIM(str(recno()))+'//'+LTRIM(str(reccount()))
   SET COLOR TO w/b,w+/g
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=SPACE(30)
      SET CURSOR ON
      Do pesan with ''
      SET COLOR TO w+/r
      @24,18 say 'Urut Nama:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#SPACE(30)
         SELECT 4
         SET COLOR TO /bg
         LOCATE FOR nama=RTRIM(nmb)
         If .not. FOUND()
           Go nr
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET COLOR TO w/b,w+/g
      SET CURSOR OFF
      RETURN 2
Elseif lastkey()=78 .or. lastkey()=110
   SELECT 4
   Ket='TOP NAMA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   RETURN 0
Elseif lastkey()=80 .or. lastkey()=112
   SELECT 5
   Ket='TOP POIN,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   Keyboard chr(4)
   RETURN 0
Elseif lastkey()=82 .or. lastkey()=114
   SELECT 5
   Ket='TOP NOTA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   RETURN 0
Elseif lastkey()=-9
      SET CURSOR ON
      Pil='2'
      Do pesan with '1=Printer     2=File : '
      @24,51 get pil pict '!'
      Read
      If pil#'1'
         No =1
         Nf='D:\POIN.CSV'
         Do pesan with 'Tunggu, sedang proses '+nf
         Set alte on
         Set alte to &nf
         ?';'+aidi1
         ?';'
         ?';REKAP POIN BELANJA'
         ?';PERIODE : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?';'
         ?'KODE;NAMA_PELANGGAN;ALAMAT;RP_NOTA;RP_MARGIN;JML_POIN;ADJUST;SISA'
         DO WHILE .not. eof()
            ?"'"+RTRIM(kode)+';'+RTRIM(nama)+';'+RTRIM(alamat)+';'+LTRIM(str(rp_jual))+';'+LTRIM(str(rp_margin))+';'+LTRIM(str(jm_poin))+';'+LTRIM(str(aj_poin))+';'+LTRIM(str(so_poin))
            No=no+1
            Skip
         ENDDO
         ?''
         Set alte off
         Set alte to
         Do pesan with 'Export selesai..., nama file: '+SUBSTR(nf,4,12)+' , di drive D:\'
         Wait''
         SET CURSOR OFF
         Do pesan with pesanx
         SET COLOR TO w/b,w+/g
         RETURN 2
      ENDIF
      *
      STORE 1 to n,no,hal
      Do cp
      If cc=0
         RETURN 0
      ENDIF
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to 5
      ?chr(27)+chr(33)+chr(4)
      ?' REKAP POIN BELANJA'
      ?' PERIODE: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(59)+'Hal:'+tran(hal,'999')
      ?REPLACE('=',129)
      ?'    No. Kode      Nama Pelanggan                Alamat                              Rp_Nota     Rp_Margin   Jml_P  Adjust    Sisa'
      ?REPLACE('-',129)
      DO WHILE .not. eof()
         ?tran(no,'@e 99,999. ')+kode+left(nama,29)+' '+left(alamat,29)+tran(rp_jual,'@e 99,999,999,999')+tran(rp_margin,'@e 99,999,999,999')+&kata1
         No=no+1
         N =n+1
         If n>50
            ?REPLACE('-',122)
            ?' '
            Eject
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
            N   =1
            Hal =hal+1
            SET PRINTER ON
            SET DEVICE TO PRINTER
            Set marg to 5
            ?chr(27)+chr(33)+chr(4)
            ?' REKAP POIN BELANJA'
            ?' PERIODE: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(59)+'Hal:'+tran(hal,'999')
            ?REPLACE('=',129)
            ?'    No. Kode      Nama Pelanggan                Alamat                              Rp_Nota     Rp_Margin   Jml_P  Adjust    Sisa'
            ?REPLACE('-',129)
         ENDIF
         Skip
      ENDDO
      ?REPLACE('-',129)
      ?SPACE(18)+'Grand Total'+SPACE(47)+tran(tot ,'@e 999,999,999,999')+tran(totx,'@e 99,999,999,999')+tran(totp,'@e 9999,999')+tran(tajp,'@e 9999,999')+tran(totp+tajp,'@e 9999,999')
      ?REPLACE('=',129)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET COLOR TO w/b,w+/g
      SET CURSOR OFF
      RETURN 1
Else
   SET CURSOR OFF
   Do pesan with pesanx
   SET COLOR TO w/b,w+/g
   RETURN 1
ENDIF
RETURN 0

*LAPORAN AMBILPOIN
*-----------------
PROCEDURE lambpoin
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
SELECT 2
Uselan('POINTAM.DTA','POINTAM2.NDX','POINTAM1.NDX','','','')
*
Ksp=SPACE(10)
STORE ' ' to hri,bln,thn,jlap
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai     F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   Nsp='-'
   If ksp#SPACE(5)
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp=nama_lgn
   ENDIF
   STORE 0 to no,tot,tot2
   Do konvtgl
   SET CENTURY OFF
   @1,0 to 1,79
   @22,0 to 22,79
   SET COLOR TO g
   SELECT 2
   If ksp#SPACE(5)
      Set order to 1
      Set filter to kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Seek ksp+dtos(tgl)
      If .not. FOUND()
         Seek ksp
         If .not. FOUND()
             Do pesan2 with 'Tidak ada Data Pengambilan Point'
             Loop
         ENDIF
      ENDIF
   Else
      Set order to 2
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek dtos(tgl)
      If .not. FOUND()
         LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. FOUND()
             Do pesan2 with 'Tidak ada Data Pengambilan Point'
             Loop
         ENDIF
      ENDIF
   ENDIF
   Nr=recno()
   Pesanx='ESC=Selesai     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   SET CENTURY OFF
   DECLARE field[1],kop[1],bts[1],frm[1]
   Field[1]="dtoc(TANGGAL)+tl+JAM+tl+left(KODE_LGN,9)+tl+left(NAMA_LGN,25)+tl+tran(iif(AM_POINT>0,AM_POINT,RP_UANG),'@e 9,999,999')+tl+left(keterangan,19)"
   Kop[1]='Tanggal '+tl+'Jam  '+tl+'Kode     '+tl+'Nama Pelanggan/Member    '+tl+'Jml.Ambil'+tl+'Catatan'
   Frm[1]=''
   Bts[1]=''
   DbEdit(2,0,21,79,field,'fampoin',frm,kop,gl,bts)
   *
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

Function fampoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   SET COLOR TO w+/b
   @23,0 CLEAR to 23,79
   @23,0 say 'Point seblmnya: '+LTRIM(tran(so_point,'@e 999,999'))+'   Catatan: '+keterangan
   Do pesan with pesanx
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=-7
   Ksp=kode_lgn
   Ajp=aj_point
   SET COLOR TO gr+*
   @row(),col() say &fds
   Yt=' '
   SET CURSOR ON
   SET COLOR TO g
   Do pesan with 'Data DiHapus? Y/T : '
   @24,49 get yt pict '!'
   Read
   If lastkey()#27 .and. yt='Y'
      Do pesan with 'Tunggu, sedang dihapus'
      Rlok()
      Dele
      Ul()
      COMMIT
      Skip
      *
      SELECT 1
      Seek ksp
      Rlok()
      REPLACE aj_point with ajp,so_point with 0,am_point with 0,tgl_ambil with ctod('  -  -    '),jam_ambil with ' ',ket_ambil with 'KOREKSI/'+dtoc(tglx)+'/'+left(time(),5)
      Ul()
      COMMIT
      *
   ENDIF
   Do pesan with pesanx
   SET CENTURY OFF
   SELECT 2
   RETURN 2
Elseif lastkey()=-9
   SET CURSOR ON
   Pil='2'
   Do pesan with '1=Printer     2=File : '
   @24,51 get pil pict '!'
   Read
   If pil#'1'
      No =1
      Nf='D:\AMBILPN.CSV'
      Do pesan with 'Tunggu, sedang proses '+nf
      Set alte on
      Set alte to &nf
      ?';;;'+aidi1
      ?';;;'
      ?';;;DAFTAR PENGAMBILAN POIN'
      ?';;;TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?';;;'
      ?'TANGGAL;JAM;KODE;NAMA_PELANGGAN;JML_AMBIL;DARI;CATATAN'
      DO WHILE .not. eof()
         ?dtoc(tanggal)+';'+jam+";'"+RTRIM(kode_lgn)+';'+RTRIM(nama_lgn)+';'+LTRIM(str(am_point))+';'+LTRIM(str(so_point))+';'+RTRIM(keterangan)
         No=no+1
         Skip
      ENDDO
      ?''
      Set alte off
      Set alte to
      Do pesan with 'Export selesai..., nama file: '+SUBSTR(nf,4,12)+' , di drive D:\'
      Wait''
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 2
   ENDIF
Else
   RETURN 1
ENDIF
RETURN 0

* EOF POIN
