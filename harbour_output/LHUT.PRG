// ============================================================
// File    : LHUT.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* DAFTAR HUTANG
*---------------
PROCEDURE lhut1
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Go top
Tglso=tanggal_2
SELECT 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
SELECT 3
*
SELECT 4
*
SELECT 5
*
SELECT 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')
SELECT 7
Uselan('RETURBN.DTA','RETURBN2.NDX','','','','')
*
Tgl =tglso
Tgl2=tglx
Ada =.f.
Ksp =SPACE(5)
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafspl
   @1,0 CLEAR to 23,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai     00000=Global'
   If ada
      Ada=.f.
      Keyboard chr(13)+chr(13)
   ENDIF
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   If ksp=SPACE(5)
      Do dafspl
   ENDIF
   If lastkey()=27
      Loop
   ENDIF
   If ksp='00000'
      @1,0 CLEAR to 23,79
      Do pesan with 'Tunggu, sedang dihitung'
      Set alte on
      Set alte to HUTANG.RTF
      ?' DAFTAR HUTANG'
      ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,5)+REPLACE(chr(194)+REPLACE(gl,25),2)+chr(194)+REPLACE(gl,15)+chr(191)
      ?tl+' No.'+tl+'Kode '+tl+'Nama Supplier '+SPACE(11)+tl+'Alamat'+SPACE(19)+tl+'     Jumlah Rp.'+tl
      ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,5)+REPLACE(chr(197)+REPLACE(gl,25),2)+chr(197)+REPLACE(gl,15)+chr(180)
      Tot2=0
      No  =1
      Go top
      DO WHILE .not. eof()
         Ksp  =kode_spl
         Nsp  =nama_spl
         Algn =almt_spl
         STORE 0 to jml,jml2,jml3
         *
         SELECT 2
         Set order to 2
         Seek ksp
         DO WHILE .not. eof() .and. kode_spl=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml =jml +rp_total &&((rp_total-rp_diskon)+rp_pajak)
               Jml2=jml2+rp_bayar
            ENDIF
            Skip
         ENDDO
         Tot=jml-(jml2+jml3)
         *
         If tot#0
            ?tl+tran(no,'999.')+tl+ksp+tl+left(nsp,25)+tl+left(algn,25)+tl+tran(tot,'@e 999,999,999,999')+tl
            Tot2=tot2+tot
            No  =no+1
         ENDIF
         SELECT 1
         Skip
      ENDDO
      ?chr(192)+REPLACE(gl,78)+chr(217)
      ?SPACE(56)+'Total Rp'+tran(tot2,'@e 999,999,999,999')
      Set alte off
      Set alte to
      @24,0
      Memoedit(Memoread('HUTANG.RTF'),1,0,21,240,.t.,'',240)
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
      Set marg to 15
      ?chr(27)+chr(33)+chr(9)
      Type HUTANG.RTF to prin
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   Else
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada kode supplier '+RTRIM(ksp)
         Loop
      ENDIF
      *
      Do bukuh
   ENDIF
ENDDO
SET CENTURY ON
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplhut1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(9)
?' DAFTAR HUTANG'
?' PER TANGGAL: '+dtoc(tgl)+SPACE(50)+'Hal:'+tran(hal,'999')
?REPLACE('=',80)
?'  No.  Nama Supplier                    Alamat                        Jumlah Rp.'
?REPLACE('-',80)
N=1
RETURN

PROCEDURE ebyr
@1,0 CLEAR to 23,79
@1,0 to 22,77
Do pesan with 'ESC=Selesai   F8=Hapus'
SET CENTURY ON
DECLARE field[7],bts[7],frm[7],kop[7]
Field[1]='TANGGAL'
Field[2]='TANGGAL_3'
Field[3]='FAKTUR'
If klp .and. spl=.f.
   Field[4]='NAMA_LGN'
   Kop[4]='Nama Pelanggan'
Elseif spl .and. klp=.f.
   Field[4]='NAMA_SPL'
   Kop[4]='Nama Supplier'
Else
   Field[4]='NAMA_TKG'
   Kop[4]='Nama Tukang'
ENDIF
Field[5]='JUMLAH_BYR'
Field[6]='JENIS_BYR'
Kop[1]='Tanggal'
Kop[2]='Jt.Tempo'
Kop[3]='Nomor'
Kop[5]='Jumlah Rp.'
Kop[6]='Jenis'
Frm[1]='99-99-99'
Frm[2]='99-99-99'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@e 999,999,999'
Frm[6]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
DbEdit(2,1,21,76,field,'fbyr',frm,kop,gl,bts)
RETURN

Function fbyr
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-7
      If klp .and. spl=.f.
         Fkt=left(faktur,5)
         Ksp=kode_lgn
         Syarat='kode_lgn=ksp'
      Elseif spl .and. klp=.f.
         Fkt=faktur
         Ksp=kode_spl
         Syarat='kode_spl=ksp'
      Else
         Ksp=kode_tkg
         Syarat='kode_tkg=ksp'
      ENDIF
      Tgl3=tanggal
      Jml =jumlah_byr
      SET COLOR TO gr+*
      @row(),col() say &fds
      Yt=' '
      SET CURSOR ON
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      DO WHILE .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         If .not. deleted()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip

            SELECT 2
            Set order to 2
            Seek ksp
            DO WHILE .not. eof() .and. &syarat
               If tanggal_4=tgl3 .and. faktur=left(fkt,5)
                  Rlok()
                  If (rp_bayar-jml)=0
                     REPLACE tanggal_4 with ctod('  -  -    ')
                  ENDIF
                  REPLACE rp_bayar with (rp_bayar-jml)
                  Ul()
                  COMMIT
               ENDIF
               Skip
            ENDDO

            SELECT 6
         ENDIF
      ENDIF
      Do pesan with 'ESC=Selesai   F8=Hapus'
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* DAFTAR PEMBAYARAN HUT.
*-----------------------
PROCEDURE lbyrh
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
SELECT 3
*
SELECT 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')
*
STORE tglx to tgl,tgl2
DO WHILE .t.
   Ksp=SPACE(5)
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafspl
   Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,27 say 'Suppl:'get ksp pict '@!'
   @3,27 say 'Tgl..:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Supplier'
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
         Ket="SUBSTR(jenis_byr,1,5)='TUNAI'"
      Case jlap='2'
         Ket="SUBSTR(jenis_byr,1,3)='BG-'"
      Other
         Ket="jenis_byr#SPACE(10)"
   ENDCASE
   SET CENTURY OFF
   STORE 0 to tot,tot2,hj
   If ksp=SPACE(5)
      @1,0 CLEAR to 23,79
      @1,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,21)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,14)
      @2,0 say 'Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Nama Supplier'+SPACE(7)+tl+'  Jumlah Rp.'+tl+' Keterangan'
      @3,0 say REPLACE(gl,8)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,21)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,14)
      B=4
      Rtot2=0
      SELECT 6
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            @b,0 say dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_spl+tran(jumlah_byr,'@e 9,999,999,999 ')+jenis_byr
            Rtot2=rtot2+jumlah_byr
            B=b+1
            If b>20
               @b,0 say REPLACE(gl,80)
               Do pesan with 'ESC=Selesai   SPASI=Lanjut'
               Wait''
               If lastkey()=27
                  Exit
               ENDIF
               B=4
            ENDIF
         ENDIF
         Skip
      ENDDO
      @b,0   say REPLACE(gl,80)
      @b+1,0 CLEAR to 23,79
      @b+1,42 say 'Total Rp'+tran(rtot2,'@e  99,999,999,999')
      Do pesan3 with 'ESC=Selesai   F8=Hapus   F10=Cetak'
      If lastkey()=-7
         Klp=.f.
         Spl=.t.
         Go nr
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
      Do koplbyrh1
      Go nr
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            ?'  '+dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_spl+tran(jumlah_byr,'@e 9,999,999,999 ')+jenis_byr
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
               Do koplbyrh1
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
         Do pesan2 with 'Tidak ada kode supplier '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp =nama_spl
      Algn=almt_spl

      @1,0 CLEAR to 23,79
      @1,8 say 'Suppl: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
      @2,7 say chr(218)+REPLACE(gl,9)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,21)+chr(191)
      @3,7 say tl+' Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Jumlah Rp.'+tl+' Keterangan'+SPACE(10)+tl
      @4,7 say chr(195)+REPLACE(gl,9)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,21)+chr(180)
      B=5
      Rtot2=0
      SELECT 6
      Set filter to kode_spl=ksp
      LOCATE FOR kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      DO WHILE .not. eof()
         If kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
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
      Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
      Bnk=SPACE(5)
      Fkt=SPACE(10)
      Jml=0
      If lastkey()=-7
         Klp=.f.
         Spl=.t.
         Go nr
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
      Do koplbyrh2
      Go nr
      DO WHILE .not. eof()
         If kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
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
               Do koplbyrh2
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
CLOSE all
SET CURSOR OFF
SET CENTURY ON
RETURN

PROCEDURE koplbyrh1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 2
?chr(27)+chr(33)+chr(9)
?' DAFTAR PEMBAYARAN HUTANG'
?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(50)+'Hal:'+tran(hal,'999')
?REPLACE('=',89)
?'  Tanggal   Jt.Tempo  No.BG/Fakt  Nama Supplier '+SPACE(6)+'  Jumlah Rp.  Keterangan'
?REPLACE('-',89)
No=1
RETURN

PROCEDURE koplbyrh2
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(9)
?'  DAFTAR PEMBAYARAN HUTANG'
?'  SUPPLIER.: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
?'             '+algn
?'  TANGGAL..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',66)+'*'
?'| Tanggal | Jt.Tempo| No.BG/Fakt| Jumlah Rp.|  Keterangan          |'
?'|'+REPLACE('-',66)+'|'
No=1
RETURN

* LAPORAN FAKTUR PEMBELIAN
*--------------------------
PROCEDURE fkth
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
SELECT 3
*
SELECT 4
Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
SELECT 5
*
SELECT 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')

Nklb='-'
STORE ' ' to hri,bln,thn
STORE tglx to tgl,tgl2,tgl4
Ada=.f.
DO WHILE .t.
   Ksp=SPACE(5)
   *
   SELECT 1
   Set filter to
   SET CENTURY ON
   Set key 28 to dafspl
   Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Supl..:'get ksp pict '@!'
   Do pesan with 'F1=Daftar Supplier   ( Kode Supl. HARUS diisi Jika Mau Melakukan Pembayaran )'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Nsp='-'
   If ksp#SPACE(5)
      Set order to 1
      Seek ksp
      If .not. FOUND()
         Set order to 2
         Seek RTRIM(ksp)
         Do dafspl
      ENDIF
      If lastkey()=27
         Loop
      ENDIF
      Nsp=nama_spl
   ENDIF
   *
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'Masukan Tanggal'
   Read
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   STORE 0 to no,tot,tot2,totb
   @22,0 to 22,79
   @1,0 say REPLACE(REPLACE(gl,8)+chr(194),2)+REPLACE(gl,8)+chr(194)+REPLACE(gl,18)+REPLACE(chr(194)+REPLACE(gl,12),2)+chr(194)+REPLACE(gl,8)
   @2,0 say 'Tanggal '+tl+'Jt.Tempo'+tl+'Faktur  '+tl+'Nama Supplier     '+tl+'    Rp.Total'+tl+'    Rp.Bayar'+tl+'Tgl.Byr.'
   @3,0 say REPLACE(REPLACE(gl,8)+chr(197),2)+REPLACE(gl,8)+chr(197)+REPLACE(gl,18)+REPLACE(chr(197)+REPLACE(gl,12),2)+chr(197)+REPLACE(gl,8)
   SELECT 2
   If ksp=SPACE(5)
      Set order to 1
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek dtos(tgl)+ksp
      If .not. FOUND()
         Seek dtos(tgl)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
            If .not. FOUND()
               LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
               If .not. FOUND()
                  Do pesan2 with 'Tidak ada Data Faktur Pembelian'
                  Loop
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   Elseif ksp#SPACE(5)
      Ada=.t.
      Set order to 2
      Set filter to ( (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp )
      Seek ksp+dtos(tgl)
      If .not. FOUND()
         LOCATE FOR kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. FOUND()
            LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
            If .not. FOUND()
               Do pesan2 with 'Tidak ada Data Faktur Pembelian'
               Loop
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   Nr=recno()
   Pesanx='SPASI=Pilih   B/L=Bayar   F1=Buku Hutang   F8=Hapus   F9=Jt.Tempo   F10=Cetak'
   Do pesan with pesanx
   SET CENTURY OFF
   DECLARE field[8],bts[8],frm[8]
   Field[1]='TANGGAL'
   Field[2]='TANGGAL_3'
   Field[3]='left(FAKTUR,8)'
   Field[4]='left(NAMA_SPL,18)'
   Field[5]='(RP_TOTAL-RP_DISKON)+RP_PAJAK'
   Field[6]='RP_BAYAR'
   Field[7]='TANGGAL_4'
   Field[8]='RP_RETUR'
   Frm[1]='99-99-99'
   Frm[2]='99-99-99'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@e 9999,999,999'
   Frm[6]='@e 9999,999,999'
   Frm[7]='99-99-999'
   Frm[8]='@e 9999,999,999'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl
   Bts[4]=tl
   Bts[5]=tl
   Bts[6]=tl
   Bts[7]=tl
   Bts[8]=tl
   DbEdit(4,0,21,79,field,'ffkth',frm,'','',bts)
   *
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE ket,tgl3,ksp,jml,hj,yt,no,hal,tot,tot2,totx,toty,tots,tot2s,totxs,totys
RETURN

Function ffkth
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-7
      Tgl4=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl

      SET COLOR TO gr+*
      @row(),col() say &fds
      STORE ' ' to yt,yt2
      SET CURSOR ON
      SET COLOR TO g
      Do pesan with 'Item Faktur DiHapus? Y/T:          Item Pemb.Brg ikut diHapus? Y/T:'
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
            Uselan('STOK.DTA','STOK.NDX','','','','')

            SELECT 4
            Seek dtos(tgl)+ksp+fkt
            DO WHILE .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_spl=ksp .and. faktur=fkt )
               Kdb=kode_brg
               Jml=jumlah
               Isi=isine_b

               Rlok()
               Dele
               Ul()
               COMMIT

               SELECT 3
               Seek kdb
               Rlok()
               REPLACE stok_m with stok_m-(jml*isi)
               Ul()
               COMMIT

               SELECT 4
               Skip
            ENDDO

            SELECT 3
            Close

            SELECT 6
            LOCATE FOR kode_spl=ksp .and. faktur=fkt
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
         Copy to JT-TEMPO.TMP for ( (tanggal_3>=tgl3 .and. tanggal_3<=tgl4) .and. tanggal_4=ctod('  -  -    ') .and. rp_bayar < ((rp_total-rp_diskon)+rp_pajak) )
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
         ?REPLACE(gl,80)
         ?' No. Jt.Tempo  Tgl.Beli  No.Faktur   Nama Supplier                    Jumlah Rp.'
         ?REPLACE(gl,80)

         SET CENTURY OFF
         DO WHILE .not. eof()
            Jml =((rp_total-rp_diskon)-rp_bayar)
            Totx=totx+jml
            ?tran(n,'999. ')+dtoc(tanggal_3)+'  '+dtoc(tanggal)+'  '+faktur+'  '+nama_spl+' '+kode_spl+tran(jml,'@e  999,999,999')

            N =n+1
            No=no+1
            Skip
         ENDDO
         ?REPLACE(gl,80)
         ?SPACE(58)+'Total = Rp'+tran(totx,'@e 9999,999,999')
         ?REPLACE(gl,80)
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
         RETURN 0
      ENDIF
      Do kopfkth
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jml=(((rp_total-rp_diskon)+rp_pajak)-rp_bayar)
            Tot =tot +((rp_total-rp_diskon)+rp_pajak)
            Tot2=tot2+rp_bayar
            Totx=totx+jml
            Tots =tots +((rp_total-rp_diskon)+rp_pajak)
            Tot2s=tot2s+rp_bayar
            Totxs=totxs+jml

            ?'| '+dtoc(tanggal)+' | '+dtoc(tanggal_3)+' | '+left(faktur,10)+'| '+left(nama_spl,20)+'|'+tran(((rp_total-rp_diskon)+rp_pajak),'@e 99,999,999,999 |')+;
             tran(rp_bayar,'@e  99,999,999,999 |')+tran(jml,'@e 99,999,999,999 |')
            No=no+1
            If no>55
               ?'|'+REPLACE('-',104)+'|'
               ?'| Jumlah Hal ini Rp'+SPACE(38)+tran(tots,'@e 99,999,999,999 ')+tran(tot2s,'@e   99,999,999,999 ')+tran(totxs,'@e  99,999,999,999 |')
               ?'|'+REPLACE('-',104)+'|'
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
               Do kopfkth
            ENDIF
         ENDIF
         Skip
      ENDDO
      If cc=0
         Do pesan with pesanx
         SET CURSOR OFF
         RETURN 1
      ENDIF
      ?'|'+REPLACE('-',104)+'|'
      ?'| Jumlah Hal ini Rp'+SPACE(38)+tran(tots,'@e 99,999,999,999 ')+tran(tot2s,'@e   99,999,999,999 ')+tran(totxs,'@e  99,999,999,999 |')
      ?'|'+REPLACE('-',104)+'|'
      ?'| Grand Total    Rp'+SPACE(38)+tran(tot ,'@e 99,999,999,999 ')+tran(tot2 ,'@e   99,999,999,999 ')+tran(totx ,'@e  99,999,999,999 |')
      ?'*'+REPLACE('=',104)+'*'
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
      Ksp =kode_spl
      Tot =0
      B   =6
      No  =1
      Save scre to sdaf
      SET COLOR TO bg+/b
      @4,0 say dtoc(tanggal)+tl+dtoc(tanggal_3)+tl+left(faktur,8)+tl+left(nama_spl,18)+tl+tran((rp_total-rp_diskon)+rp_pajak,'@e 9999,999,999')+tl+tran(rp_bayar,'@e 9999,999,999')+tl+dtoc(tanggal_4)
      SET COLOR TO /bg
      @2,38 say ':'+ksp
      @5,0 to 5,79 doub
      @6,0 CLEAR to 22,79
      @20,0 to 20,79
      @21,1 say 'Diskon:'
      @22,1 say 'PPN...:'
      @21,33 say 'Bayar:'
      @22,33 say 'Retur:'
      @21,59 say 'Usr:'
      @22,59 say 'Ket:'
      SET COLOR TO /bg
      @21,8 say tran(diskon,'@e 99.99% Rp')+tran(rp_diskon,'@e 999,999.99')
      @22,8 say tran(pajak ,'@e 99.99% Rp')+tran(rp_pajak ,'@e 999,999.99')
      @21,40 say 'Rp '+tran(rp_bayar,'@e 999,999,999')
      @22,40 say 'Rp '+tran(rp_retur,'@e 999,999,999')
      @21,63 say left(nama_ksr,15)
      @22,63 say keterangan
      SELECT 4
      Seek dtos(tgl)+ksp+fkt
      DO WHILE .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_spl=ksp .and. faktur=fkt )
         Jml =jumlah
         Hp  =(jml*harga)
         Dis1=((diskon_1/100)*(jml*harga))
         Pjk1=((pajak/100)*(hp-dis1))
         Jhg =((hp-dis1)+pjk1)
         Tot=tot+jhg
         If jml#0
            @b,0 say tran(no,'999. ')+left(kode_brg,7)+' '+left(nama_brg,29)+tran(jml,'@e  99999.99 ')+satuan+tran(harga,'@e 9,999,999 ')+tran(jhg,'@e 9999,999,999')
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
      @b,0 CLEAR to 19,79
      Do pesan with 'ESC=Selesai'
      Wait''
      Rest scre from sdaf
      Do pesan with pesanx
      SELECT 2
      If rp_total#tot
         Rlok()
         REPLACE rp_total with tot
         Ul()
         COMMIT
      ENDIF
      RETURN 1
   Case lastkey()=72 .or. lastkey()=104
      Do pesan with 'Tunggu, sedang diHitung'
      Totb=0
      Nr=recno()
      Set order to 1
      Sum ((rp_total-rp_diskon)+rp_pajak),rp_bayar to tot,tot2 for (tanggal>=tgl .and. tanggal<=tgl2)
      Go nr
      SET COLOR TO gr+
      @22,34 say 'Total=Rp'+tran(tot,'@e 9,999,999,9999')+gl+gl+gl+'Bayar=Rp'+tran(tot2,'@e 99,999,999,999')
      SET COLOR TO g
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=32
      Tot=((rp_total-rp_diskon)+rp_pajak)-rp_bayar
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
      @22,48 say 'Yang Mau diBayar:'+tran(totb,'@e 9,999,999,999')
      Do pesan with pesanx
      RETURN 1
   Case lastkey()=28
      Save scre to smkdb
      SET CURSOR ON
      Tgl4=tgl
      Ksp =kode_spl
      Nr  =recno()

      Do bukuh
      SET CURSOR OFF
      Rest scre from smkdb
      Do pesan with pesanx
      SELECT 2
      Tgl=tgl4
      Go nr
      RETURN 1
   Case lastkey()=66 .or. lastkey()=98
      If .not. ada
         Do pesan2 with 'Untuk Pembayaran Kolektif, Kode Supplier Harus diIsi'
         Do pesan with pesanx
         SELECT 2
         RETURN 1
      ENDIF
      No  =recno()
      Totb=0

      Go nr
      DO WHILE .not. eof() .and. kode_spl=ksp
         If id_bayar#' '
            Totb=totb+(((rp_total-rp_diskon)+rp_pajak)-rp_bayar)
         ENDIF
         Skip
      ENDDO

      Go no
      Tgl4=tgl
      Tgl =tanggal
      Ksp =kode_spl
      Fkt =faktur

      Save scre to smkdb
      SET CURSOR ON

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
      DO WHILE .not. eof() .and. kode_spl=ksp
         If (tanggal>=tglso .and. tanggal<tgl)
            Jml=jml+((rp_total-rp_diskon-rp_retur)+rp_pajak)
         ENDIF
         Skip
      ENDDO
      *
      SELECT 6
      Set order to 2
      Seek ksp
      DO WHILE .not. eof() .and. kode_spl=ksp
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
         *@13,48 say 'Jumlah Hutang..=Rp'+tran(tot2,'@e 9999,999,999')
         @19,2  say 'Tanggal Pembayaran....: 'get tgl3
         @20,2  say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 9,999,999,999'
         @19,48 say 'Keterangan/Mutasi:'
         @20,48 get ket
         *
         Tot3=tot2-totb
         *@22,52 say 'Sisa Hutang Rp'+tran(tot3,'@e  999,999,999')
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

         SELECT 2
         Go nr
         DO WHILE .not. eof() .and. kode_spl=ksp
            If id_bayar#' '
               Rlok()
               REPLACE tanggal_4 with tgl3,id_bayar with ' '
               If totb<tot
                  REPLACE rp_bayar with (rp_bayar+totb),keterangan with 'TITIP, MSH ADA SISA.'
               Elseif totb>=tot
                  REPLACE rp_bayar with ((rp_total-rp_diskon)+rp_pajak),keterangan with 'DIBAYAR TUNAI.'
               ENDIF
               If totb>tot
                  REPLACE keterangan with ket
               ENDIF
               Ul()
            ENDIF
            Skip
         ENDDO
         COMMIT
         Go no

         SELECT 6
         If totb#0
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgl3,tanggal_2 with dtos(tgl3),faktur with fkt,;
                 kode_spl with ksp,nama_spl with nsp,jumlah_byr with totb,jenis_byr with ket
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
      Ksp =kode_spl
      Nsp =nama_spl
      Fkt =faktur
      Totb=(((rp_total-rp_diskon-rp_retur)+rp_pajak)-rp_bayar)
      xFilter=dbfilter()
      *
      Save scre to smkdb
      SET CURSOR ON
      *
      SET COLOR TO w+/br
      @19,0 CLEAR to 23,79
      @19,0 to 23,79
      SET CENTURY ON
      Ket ='Nota No:'+fkt+'  '
      Tgl3=tglx
      @20,2 say 'Tanggal Pembayaran....:'get tgl3
      @21,2 say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 999,999,999'
      @22,2 say 'Total Rp '+LTRIM(tran(rp_total-rp_diskon,'@e 999,999,999'))+' - Retur Rp '+LTRIM(tran(rp_retur,'@e 999,999,999'))
      @20,48 say 'Keterangan/Mutasi:'
      @21,48 get ket
      *
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
         REPLACE rp_bayar with rp_bayar+totb,tanggal_4 with tgl3,id_bayar with ' '
         Ul()
         COMMIT
         *
         SELECT 6
         If totb#0
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgl3,tanggal_2 with dtos(tgl3),faktur with fkt,;
                 kode_spl with ksp,nama_spl with nsp,jumlah_byr with totb,jenis_byr with ket
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

PROCEDURE kopfkth
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(4)
?' DAFTAR FAKTUR PEMBELIAN'
If ksp#SPACE(5)
   ?' SUPPLIER: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
ENDIF
?' TANGGAL..: '+dtoc(tgl)+' SAMPAI '+dtoc(tgl2)+SPACE(62)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',104)+'*'
?'| Tanggal  | Jt.Tempo | No.Faktur | Nama Supplier       |        Jumlah |          Bayar |    Kekurangan |'
?'|'+REPLACE('-',104)+'|'
No=1
RETURN

PROCEDURE bukuh
SET CENTURY OFF
SELECT 1
Seek ksp
Tgl4 =tgl
*
If hut
   Nsp =nama_spl
   Algn=almt_spl
   Kota=kota_spl
   Syarat='kode_spl=ksp'
   Kataph='Pembelian No: '
   Kata1=' DAFTAR HUTANG (Rupiah) '
   Kata2=' Nama Sup.: '+RTRIM(nsp)+'  '+ksp
   *
   SELECT 7
   Uselan('RETURBN.DTA','RETURBN1.NDX','','','','')
   *
Else
   Nsp =nama_lgn
   Algn=almt_lgn
   Kota=kota_lgn
   Syarat='kode_lgn=ksp'
   Kataph='Penjualan Nota No:'
   Kata1=' DAFTAR PIUTANG (Rupiah)'
   Kata2=' Nama Plgn: '+RTRIM(nsp)+'  '+ksp
   *
ENDIF
@1,0 CLEAR to 23,79
Do pesan with 'Tunggu, sedang dihitung...'
STORE 0 to jml,jml2,jml3,tot,tot2,tot3
STORE 1 to n,no
*
Set alte on
Set alte to BUKU-PH.RTF
?Kata1+SPACE(25)
?Kata2
?' Alamat...: '+RTRIM(algn)+' - '+kota
?' Tanggal..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
?REPLACE(gl,4)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,10)+REPLACE(chr(194)+REPLACE(gl,13),3)+gl
?' No.'+tl+'Tanggal  '+tl+'No.Nota   '+tl+'       Jumlah'+tl+'        Bayar'+tl+'         Sisa'
?REPLACE(gl,4)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,10)+REPLACE(chr(193)+REPLACE(gl,13),3)+gl
SELECT 2
Set order to 2
Seek ksp
If FOUND()
   DO WHILE .not. eof() .and. &syarat
      If (tanggal>=tgl .and. tanggal<=tgl2)
         Jml =(rp_total-rp_diskon)
         Jml2=rp_bayar
         Tot =tot +jml
         Tot2=tot2+jml2
         ?tran(no,'999. ')+dtoc(tanggal)+'  '+faktur+iif(hut,'','     ')+tran(jml,'@e  9,999,999,999 ')+tran(jml2,'@e 9,999,999,999 ')+tran(jml-jml2,'@e 9,999,999,999')
         No=no+1
         N =n +1
      ENDIF
      Skip
   ENDDO
ENDIF
*
If hut
   SELECT 7
   Set filter to kode_spl=ksp
   LOCATE FOR tanggal>=tgl
   If FOUND()
      ?' '
      ?'     RETUR BARANG'
      ?'     ------------'
      Fkt =faktur
      Tgl3=tanggal
      DO WHILE .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jml3=jml3+(jumlah*harga)
            Skip
            If faktur#fkt
               Tot3=tot3+jml3
               ?tran(no,'999. ')+dtoc(tgl3)+'  '+fkt+SPACE(20)+tran(jml3,'@e 9,999,999,999 ')+tran(1*-jml3,'@e 9,999,999,999')
               No  =no+1
               N   =n +1
               Fkt =faktur
               Tgl3=tanggal
               Jml3=0
            ENDIF
         Else
            Skip
         ENDIF
      ENDDO
   ENDIF
ENDIF
?REPLACE(gl,68)
?SPACE(15)+'Total Rp   '+tran(tot,'@e 9,999,999,999 ')+tran(tot2+tot3,'@e 9,999,999,999 ')+tran(tot-(tot2+tot3),'@e 9,999,999,999')
?REPLACE(gl,68)
?''
Set alte off
Set alte to
SET CENTURY OFF
Do pesan with 'ESC=Menu Berikutnya'
Memoedit(Memoread('BUKU-PH.RTF'),1,0,22,210,.t.,'',210)
Do pesan3 with 'ESC=Selesai   F10=Cetak'
If lastkey()=-9
   Do cp
   If cc#0
      SET PRINTER ON
      SET DEVICE TO PRINTER
      ?chr(27)+chr(33)+chr(9)
      Type BUKU-PH.RTF to print
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDIF
Tgl=tgl4
SET CENTURY ON
*
RETURN

* EOF LHUT
