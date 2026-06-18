// ============================================================
// File    : LST2.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* DAFTAR STOK
SELECT 1
If klp
   Uselan('JENIS.DTA','JENIS.NDX','','','','')
Elseif spl
   Uselan('SUPP.DTA','SUPP.NDX','','','','')
ENDIF
SELECT 2
*
SELECT 3
Uselan('STOK.DTA','STOK.NDX','STOK2.NDX','STOK3.NDX','','')
Ikdb=' '
Lks ='T'
STORE SPACE(5) to ksp,klb
SET CENTURY OFF
DO WHILE .t.
   @1,0 CLEAR to 24,79
   Ket="Semua"
   Stk='((STOK_A+STOK_M) - (STOK_K+STOK_RB))'
   Lks='T'
   *
   SELECT 1
   If klp
      Set key 28 to dafklb
      @2,29 say 'Jenis Barang:'get klb pict '@!'
      Do pesan with 'F1=Kelompok Brg'
      Read
      Set key 28 to
      If lastkey()=27 .or. klb=SPACE(5)
         Exit
      ENDIF
   Elseif spl
      Set key 28 to dafspl
      @2,29 say 'Kode Suplier:'get ksp pict '@!'
      Do pesan with 'F1=Daftar Supplier'
      Read
      Set key 28 to
      If lastkey()=27 .or. ksp=SPACE(5)
         Exit
      ENDIF
   ENDIF
   Nsp='-'
   Nsp='-'
   Katax='Kode Brg     '+tl+'Nama Barang'+SPACE(19)+tl+'Jml.Barang '+tl+'     @Harga'+tl+' Jumlah Rp.'
   STORE 0 to no,tot,tot2,jml2,jml3
   Do pesan with 'Tunggu, sedang dihitung'
   *
   SELECT 1
   If klp
      If klb#SPACE(5)
         Seek klb
         If .not. FOUND()
            Do pesan2 with 'Tidak ada kode '+RTRIM(klb)
            Loop
         ENDIF
         Nsp=nama_klb
         Ket='JENIS: '+RTRIM(nsp)+' ('+RTRIM(klb)+')'
      ENDIF
      If lastkey()=27
         Loop
      ENDIF
      Syarat="kode_klb=klb"
      SELECT 3
      Set order to 3
      Seek klb
      Nr=recno()
   Elseif spl
      If ksp#SPACE(5)
         Set order to 1
         Seek ksp
         If .not. FOUND()
            Set order to 2
            Seek RTRIM(ksp)
            Do dafspl
         ENDIF
         Nsp=nama_spl
         Ket='SUPPLIER: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
      ENDIF
      If lastkey()=27
         Loop
      ENDIF
      Syarat="kode_spl=ksp"
      SELECT 3
      Set order to 2
      Seek ksp
      Nr=recno()
   ENDIF
   SELECT 3
   Stkx=((STOK_A+STOK_M) - (STOK_K+STOK_RB))
   *
   Sum &stk*harga to tot2
   Go nr
   DO WHILE .not. eof() .and. &syarat
      Tot=tot+(&stk*harga)
      Skip
   ENDDO
   STORE '' to kdb,kdb2,nmb,isi,st,stb,hp,hp2,hpb,hj,hj21,hj2,hj3,hj4,hj5,ppn,pjk,;
              bts3,bts4,bts5,jml,jml1,jml2,jml3,hjx,hjy,rpx,rpy,tgl0,tgl1,tgl2,tgl3,tglrb
   STORE 0  to jml,jml1,jml2,jml3,jmlrb
   STORE .f. to keluar,adadis,hapus,xUdahada,hapusi,beli
   @1,0 CLEAR to 24,79
   @1,1 say ket
   @2,0 say REPLACE(gl,13)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,11)
   @3,0 say katax
   @4,0 say REPLACE(gl,13)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,11)+chr(197)+REPLACE(gl,11)+chr(197)+REPLACE(gl,11)
   @21,0 say REPLACE(gl,13)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,11)
   @22,1  say 'Total Semua Brng=Rp.'+tran(tot2,'@e 9999,999,999')
   @22,57 say 'Jumlah =Rp.'+tran(tot,'@e 9999,999,999')
   *
   Set filter to &syarat
   Go nr
   Pesanx='F10=Cetak'
   Do pesan with pesanx
   DECLARE field[7],kop[7],bts[7],frm[7]
   Field[1]='left(KODE_BAR,13)'
   Field[2]='left(NAMA_BRG,30)'
   Field[3]='&STK'
   Field[4]='left(SATUAN,3)'
   Field[5]='HARGA'
   Field[6]='&STK*HARGA'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 999,999'
   Frm[4]='@!'
   Frm[5]='@e 999,999,999'
   Frm[6]='@e 999,999,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=' '
   Bts[5]=''+tl
   Bts[6]=''+tl
   DbEdit(5,0,20,79,field,'flst12',frm,'','',bts)
   *
   If lastkey()#-9
      Loop
   ENDIF
   Kdb=kode_brg
   Sambung='Y'
   STORE 1 to no,hal,b,n
   STORE 0 to tot,tot2
   Do mulaictk
   If lastkey()=27
      RETURN
   ENDIF
   Yt=' '
   Do pesan with 'Stok Nol ikut diCetak? Y/T : '
   @24,54 get yt pict '!'
   Read
   If lastkey()=27
      Loop
   ENDIF
   Set filter to ((STOK_A+STOK_M) - (STOK_K+STOK_RB))#0 .and. &syarat
   If yt='Y'
      Set filter to ((STOK_A+STOK_M) - (STOK_K+STOK_RB))#9999999 .and. &syarat
   ENDIF
   Go top
   Do cp
   If cc=0
      Exit
   ENDIF
   Do koplst12
   DO WHILE .not. eof() .and. &syarat
      Stkx=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
      Jml=tran(stkx,'@e 99,999 ')
      If stkx#0
         If (stkx>0 .and. stkx<1)
            Jml=tran(stkx,'@e 999.99 ')
         Elseif mod(stkx,int(stkx))>0 
            Jml=tran(stkx,'@e 999.99 ')
         ENDIF
      ENDIF
      Tot =tot +(stkx*harga)
      Tot2=tot2+(stkx*harga)
      ?tran(no,'|9999 | ')+kode_bar+' | '+left(nama_brg,30)+'| '+jml+' | '+satuan+'| '+;
       tran(harga,'@e 9,999,999.99 |')+tran(stkx*harga,'@e 999,999,999 |')
      No=no+1
      N =n+1
      If n>50
         ?'|'+REPLACE('-',98)+'|'
         ?'|             Jumlah Hal Ini'+SPACE(58)+tran(tot,'@e 9999,999,999 |')
         ?'|'+REPLACE('-',98)+'|'
         ?'|             SUB TOTAL  Rp.'+SPACE(58)+tran(tot2,'@e 9999,999,999 |')
         ?'*'+REPLACE('=',98)+'*'
         ?''
         If sambung='T'
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
         Else
            Eject
         ENDIF
         Tot=0
         Hal=hal+1
         Do koplst12
      ENDIF
      Skip
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?'|'+REPLACE('-',98)+'|'
   ?'|             Jumlah Hal Ini'+SPACE(58)+tran(tot,'@e 9999,999,999 |')
   ?'|'+REPLACE('-',98)+'|'
   ?'|             T O T A L  Rp.'+SPACE(58)+tran(tot2,'@e 9999,999,999 |')
   ?'*'+REPLACE('=',98)+'*'
   ?''
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE kdb,nmb,isi,st,ksp,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,bts3,bts4,bts5,jml,;
     jml1,jml2,jml3,nr,hjx,hjy,rpx,rpy,tgl2,tgl3,keluar,adadis,hapus,xUdahada,hapusi
SET CURSOR OFF
SET CENTURY ON
RETURN

Function flst12
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13 .and. spl
      @24,0
      Nr=recno()
      Kdb=kode_brg
      Klb=kode_klb
      Kdp=kode_dep
      SELECT 2
      Uselan('JENIS.DTA','JENIS.NDX','','','','')
      *
      SELECT 3
      Set order to 1
      SET CURSOR ON
      Do lkdb
      SET CURSOR OFF
      If klp
         Set order to 3
      Elseif spl
         Set order to 2
      ENDIF
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=-9
      RETURN 0
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE koplst12
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR STOK'
?' '+ket
?' TANGGAL: '+dtoc(tglx)+SPACE(66)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',98)+'*'
?'| No. | Barcode       | Nama Barang                   |    Sisa | Sat. |      @Pokok |   Jumlah Rp.|'
?'|'+REPLACE('-',98)+'|'
N=1
RETURN

* BARANG/KLP-TOTAL
*----------------------------
PROCEDURE lst13
SELECT 1
Uselan('STOK.DTA','STOK3.NDX','','','','')
SELECT 2
Uselan('JENIS.DTA','JENIS.NDX','','','','')

Count to jml
STORE ' ' to lks,ket
STORE tglx to tgl,tgl2
Bln=SUBSTR(dtoc(tgl),4,2)
Thn=right(dtoc(tgl),2)
DO WHILE .t.
   Ada=.f.
   @1,0 CLEAR to 24,79
   SET COLOR TO bg+/b
   @2,27 say '  -  '
   SET COLOR TO /bg
   @2,18 say 'Bln-Thn:'
   @3,18 say 'Lokasi.: T=Toko    G=Gudang   S=Semua --->'
   @2,27 get bln
   @2,30 get thn
   @3,60 get lks pict '!'
   Do pesan with 'Bln-Thn = diisi Bulan dan Tahun'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   SELECT 1
   Nf1='STO_'+bln+thn+'.DTA'
   If file('&nf1')
      Uselan('&nf1','','','','','')
      Ada=.t.
   Elseif .not. file('&nf1')
      Do pesan with 'File mungkin belum diPosting.    Tekan ENTER=Hitung Stok Bulan Sekarang'
      Wait''
      If lastkey()=27
         Loop
      ENDIF
      Uselan('STOK.DTA','STOK3.NDX','','','','')
   ENDIF
   Ket='TOKO & GUDANG'
   If lks='T'
      Ket='TOKO         '
   Elseif lks='G'
      Ket='GUDANG       '
   ENDIF
   *
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-STOK.RTF
   ?' LAPORAN PERSEDIAAN BARANG'
   ?' BLN/THN.: '+bln+'-'+thn
   ?' LOKASI  : '+ket+SPACE(18)+'TGL.CETAK: '+dtoc(tglx)
   ?'/'+REPLACE('=',91)+'\'
   ?'| No.| Kelompok Barang'+SPACE(20)+'|   Item /     Stok |    Jumlah Rp.|    Margin Rp |'
   ?'|'+REPLACE('-',91)+'|'
   Y=1
   STORE 0 to tothp,tothj,jml1,jml2,jmla,jmlb
   SELECT 2
   Go top
   DO WHILE .not. eof()
      Klb =kode_klb
      Nklb=nama_klb
      Z   =LTRIM(str(y))
      STORE 0 to jml1,jml2,hj,hp
      *
      SELECT 1
      If ada
         LOCATE FOR kode_klb=klb
      Else
         Seek klb
      ENDIF
      If FOUND()
         DO WHILE .not. eof() .and. kode_klb=klb
            Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
            *
            Jmla =jmla+1
            Jml1 =jml1+1
            Jml2 =jml2+&stk
            Jmlb =jmlb+&stk
            Hp   =hp + ( &stk * harga )
            Hj   =hj + ( &stk * harga_2 )
            Tothp=tothp + ( &stk * harga)
            Tothj=tothj + ( &stk * harga_2 )
            Skip
         ENDDO
      ENDIF
      If jml#0
         ?tran(y,'|999 | ')+nklb+' '+left(klb,4)+'|'+tran(jml1,'@e 999,999 /')+tran(jml2,'@e 9,999,999 |')+tran(hp,'@e 9,999,999,999 |')+tran(hj-hp,'@e 9,999,999,999 |')
         Y = y + 1
      ENDIF
      SELECT 2
      Skip
   ENDDO
   ?'|'+REPLACE('-',91)+'|'
   ?'|'+SPACE(34)+'Total = '+tran(jmla,'@e 999,999 /')+tran(jmlb,'@e 9,999,999 |')+tran(tothp, '@e 9,999,999,999 |')+tran(tothj-tothp, '@e 9,999,999,999 |')
   ?'\'+REPLACE('=',91)+'/'
   ?' '
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('L-STOK.RTF'),1,0,22,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
   ENDIF
   Pil='1'
   Do pesan with '1=Printer   2=File : '
   @24,50 get pil pict '!'
   Read
   If pil='2'
      No =1
      Nf='C:\DAFSTOK2.TXT'
      Do pesan with 'Tunggu, sedang proses '+nf
      Set order to 4
      Go top
      Set alte on
      Set alte to &nf
      ?'   ;DAFTAR PERSEDIAAN BARANG; ; ;'
      ?'   ;BLN/THN.: '+bln+'-'+thn
      ?'   ;LOKASI  : '+ket+'; ; ;TGL.CETAK: '+dtoc(tglx)
      ?'NO.;NAMA KELOMPOK;KODE;ITEM;JUMLAH RP'
      Y=1
      STORE 0 to tot,totx,tot2,jml,jml1,jml2
      SELECT 2
      Go top
      DO WHILE .not. eof()
         Klb =kode_klb
         Nklb=nama_klb
         Z   =LTRIM(str(y))
         Jml1=0
         *
         SELECT 1
         If ada
            LOCATE FOR kode_klb=klb
         Else
            Seek klb
         ENDIF
         If FOUND()
            DO WHILE .not. eof() .and. kode_klb=klb
               Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
               *
               Jml1=jml1+1
               Jml2=jml2+1
               Jml =jml + ( &stk * harga)
               Tot =tot + ( &stk * harga)
               Skip
            ENDDO
         ENDIF
         If jml#0
            ?LTRIM(str(y))+';'+RTRIM(nklb)+';'+RTRIM(klb)+';'+LTRIM(str(jml1))+';'+LTRIM(str(jml))
            Y = y + 1
         ENDIF
         STORE 0 to tot2,jml,jml1
         SELECT 2
         Skip
      ENDDO
      ?''
      Set alte off
      Set alte to
      Do pesan with 'Export selesai..., nama file: '+SUBSTR(nf,4,12)+' , di drive C:\'
      Wait''
      Loop
   Else
      Do cp
      If cc=0
         Loop
      ENDIF
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to 5
      ?chr(27)+chr(33)+chr(1)
      Type L-STOK.RTF to prin
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDDO
CLOSE all
RELEASE klb,lks,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* STOK MINIMAL
*--------------
PROCEDURE lst14
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
*
SELECT 3
Uselan('STOK.DTA','STOK2.NDX','STOK3.NDX','','','')
Ksp=SPACE(5)
Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
DO WHILE .t.
   SELECT 1
   Set key 28 to dafspl
   @1,0 CLEAR to 24,79
   @2,30 say 'Supl:'get ksp pict '@!'
   Do pesan with 'F1=Daftar Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   If ksp#SPACE(5)
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Supplier '+RTRIM(ksp)
         Loop
      ENDIF
      Nklb=nama_spl

      SELECT 3
      Set order to 1
      Set filter to kode_spl=ksp .and. &stk <= stok_min .and. stok_min#0
      LOCATE FOR &stk <= stok_min .and. stok_min#0
      If .not. FOUND()
         Do pesan2 with 'Tidak Ada Data'
         Loop
      ENDIF
      Kata="'Supl: '+RTRIM(nklb)+'  ('+RTRIM(ksp)+')'"
   Else
      SELECT 3
      Set order to 2
      Set filter to &stk <= stok_min .and. stok_min#0
      LOCATE FOR &stk <= stok_min .and. stok_min#0
      If .not. FOUND()
         Do pesan2 with 'Tidak Ada Data'
         Loop
      ENDIF
      Kata="'Semua Barang'"
   ENDIF

   Nr=recno()
   STORE '' to kdb,nmb,isi,st,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,;
              bts3,bts4,bts5,jml,jml1,jml2,jml3,hjx,hjy,rpx,rpy,tgl2,tgl3
   STORE .f. to keluar,adadis,hapus,xUdahada,hapusi
   @1,0 CLEAR to 24,79
   @1,1 say  &kata
   @2,0 say  REPLACE(gl,8)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,14)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,6)
   @3,0 say  'Kode Brg'+tl+'Nama Barang'+SPACE(19)+tl+'Minimal'+tl+'  Sisa Stok   '+tl+'    @Harga'+tl+' Supl.'
   @4,0 say  REPLACE(gl,8)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,14)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,6)
   @22,0 say REPLACE(gl,8)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,14)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,6)
   Pesanx='F9=Order   F10=Cetak'
   Do pesan with pesanx
   DECLARE field[7],kop[7],bts[7],frm[7]
   Field[1]='KODE_BRG'
   Field[2]='left(NAMA_BRG,30)'
   Field[3]='STOK_MIN'
   Field[4]='&STK'
   Field[5]='SATUAN'
   Field[6]='HARGA'
   Field[7]='KODE_SPL'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 9999.99'
   Frm[4]='@e 99999.99'
   Frm[5]='@!'
   Frm[6]='@e 99,999,999'
   Frm[7]='@!'
   Bts[1]=''+tl
   Bts[2]=' '+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=' '
   Bts[6]=''+tl
   Bts[7]=tl+' '
   DbEdit(5,0,21,79,field,'flst14',frm,'','',bts)
   *
ENDDO
CLOSE all
RELEASE kdb,nmb,isi,st,ksp,stb,hp,hp2,hpb,hj,hj2,hj3,hj4,hj5,bts3,bts4,bts5,jml,;
     jml1,jml2,jml3,nr,hjx,hjy,rpx,rpy,tgl2,tgl3,keluar,adadis,hapus,xUdahada,hapusi,stk
SET CURSOR OFF
RETURN

Function flst14
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-8
      Nr=recno()
      Do morder

      SELECT 3
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
   Case lastkey()=-9
      Do cp
      If cc=0
         Do pesan with pesanx
         RETURN 1
      ENDIF
      STORE 1 to no,hal,b,n
      STORE 0 to tot,tot2
      Do koplst14
      Go nr
      DO WHILE .not. eof()
         Stkx=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
         If &stk <= stok_min .and. stok_min#0
            ?tran(no,'|9999 | ')+kode_brg+' | '+nama_brg+'| '+tran(stok_min,'@e 9999.99 |')+;
             tran(stkx,'@e 999999.99 ')+satuan+' | '+tran(harga,'@e 9,999,999.99 |')
            No=no+1
            N =n+1
            If n>55
               ?'|'+REPLACE('-',89)+'|'
               ?''
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
               Hal=hal+1
               Do koplst14
            ENDIF
         ENDIF
         Skip
      ENDDO
      If cc=0
         Do pesan with pesanx
         RETURN 1
      ENDIF
      ?'*'+REPLACE('=',89)+'*'
      ?''
      Eject
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      RETURN 1
   Othe
      RETURN 1
ENDCASE
RETURN 0
PROCEDURE koplst14
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR STOK MINIMAL'
?' TANGGAL: '+dtoc(tglx)+SPACE(63)+'Hal:'+tran(hal,'999')
?'*'+REPLACE('=',89)+'*'
?'| No. | Kode Brg| Nama Barang                   | Minimal |   Sisa Stok    |       @Harga |'
?'|'+REPLACE('-',89)+'|'
N=1
RETURN

PROCEDURE morder
Warna=setcolor()
Nsp=' '
Tgl=tglx
Klb=kode_klb
Kdb=kode_brg
Nmb=nama_brg
St =satuan
Hp =harga_bl/isine_b
B  =row()
If b>=21
   B=17
ENDIF
Jml=0
Ada=.f.
Do konvtgl
SET COLOR TO gr+*
@row(),col() say &fds
Save scre to smorder

SELECT 1
Do pesan with 'F1=Daftar Supplier'
Set key 28 to dafspl
SET CURSOR ON
SET COLOR TO w+/b,gr+/b
@b+1,8 CLEAR to b+3,73
@b+1,8 to b+3,73
@b+1,10 say 'Order ke:'get ksp pict '@!'
@b+2,10 say RTRIM(nmb)+' ('+RTRIM(kdb)+')'
Read
Set key 28 to
SET COLOR TO w+/b
Seek ksp
If FOUND()
   Nsp=nama_spl
   @b+1,20 say RTRIM(nsp)+' ('+RTRIM(ksp)+')'
ENDIF
SELECT 4
Uselan('ORDERIN.DTA','ORDERIN1.NDX','','','','')
Seek dtos(tgl)+ksp+kdb
If FOUND()
   Jml=jumlah
   Ada=.t.
   SET COLOR TO w+*/b
   @b+3,33 say 'Data Sudah Ada'
   SET COLOR TO w+/b
ENDIF
@b+2,54 say 'Jumlah:'get jml pict '@e 99999'
@b+2,68 say st
Read

SELECT 4
If ada
   Rlok()
   REPLACE jumlah with jml,harga with hp
Elseif (.not. ada) .and. jml#0
   Flok()
   APPEND BLANK
   REPLACE tanggal with tgl,tanggal_2 with dtos(tanggal),faktur with '0'+(hri+bln),;
        kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,;
        nama_brg with nmb,satuan with st,jumlah with jml,harga with hp
ENDIF
Ul()
COMMIT
Close
SET COLOR TO (warna)
Rest scre from smorder
RETURN

* DAFTAR STOK
*------------
PROCEDURE lstlalu
SELECT 1
Uselan('JENIS.DTA','JENIS.NDX','','','','')
SELECT 2
*
SELECT 3
*
SET CENTURY ON
Tgl=tglx
STORE SPACE(5) to ksp,klb,kdb
Bln=SUBSTR(dtoc(tgl),4,2)
Thn=right(dtoc(tgl),2)
Hri=bln
Hr =thn
Lks='T'
DO WHILE .t.
   @1,0 CLEAR to 24,79
   SELECT 1
   Set key 28 to dafklb
   SET COLOR TO bg+/b
   @3,43 say '  -  '
   SET COLOR TO /bg
   @2,33 say 'Klp Brg.:'get klb pict '@!'
   @3,33 say 'Bln-Thn.:'
   @4,33 say 'Lokasi..:    [ T/G/S ]'
   @3,43 get bln
   @3,46 get thn
   @4,43 get lks pict '!'
   Do pesan with 'ESC=Selesai   F1=Kelompok Brg'
   Read
   Set key 28 to
   If lastkey()=27 .or. klb=SPACE(5)
      Exit
   ENDIF
   SELECT 2
   Nf1='STO_'+bln+thn+'.DTA'
   If file('&nf1')
      Uselan('&nf1','','','','','')
   Elseif .not. file('&nf1')
      Do pesan2 with 'Tidak Ada File Stok Barang '+bln+'-'+thn+'   -atau-   Belum diPosting?'
      Loop
   ENDIF
   Do pesan with '+Hitung Seluruh Barang? Y/T'
   Yt=' '
   @24,54 get yt pict '!'
   Read
   Nklb='-'
   Sambung='T'
   STORE 0 to no,tot,tot2,jml2,xhal
   If klb#SPACE(5)
      SELECT 1
      Seek klb
      If .not. FOUND()
         Do pesan2 with 'Tidak ada kode '+RTRIM(klb)
         Loop
      ENDIF
      Nklb=nama_klb
      Ket =' '
      Syarat='kode_klb=klb'
   ENDIF
   Stk='(STOK_A+STOK_M) - (STOK_K+STOK_RB)'
   Ket2=RTRIM(nklb)+' ('+RTRIM(klb)+')   BLN: '+bln+'-'+thn+' / TOKO'
   Do pesan with 'Tunggu, sedang dihitung'

   SELECT 2
   If yt='Y'
      SELECT 2
      Sum harga*&stk to jml2
   ENDIF
   LOCATE FOR kode_klb=klb
   Nr=recno()
   DO WHILE .not. eof() .and. &syarat
      Tot=tot+(harga*&stk)
      Skip
   ENDDO
   Go nr
   @1,0 CLEAR to 24,79
   @2,0 to 21,79
   @1,1 say ket2
   @2,0 say chr(218)+REPLACE(gl,7)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,15)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,12)+chr(191)
   @3,0 say tl+'Kode   '+tl+'Nama Barang'+SPACE(19)+tl+'  Jml.Barang   '+tl+'    @Harga'+tl+'  Jumlah Rp.'+tl
   @4,0 say chr(195)+REPLACE(gl,7)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,15)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,12)+chr(180)
   If yt='Y'
      @22,1  say 'Total Semua Brng=Rp.'+tran(jml2,'@e 9999,999,999')
   ENDIF
   @22,56 say 'Jumlah =Rp.'+tran(tot,'@e 9999,999,999')
   Katax='F10=Cetak'
   Do pesan with katax
   DECLARE field[14],kop[14],bts[14],frm[14]
   Field[1]='left(KODE_BRG,8)'
   Field[2]='NAMA_BRG'
   Field[3]='&STK'
   Field[4]='SATUAN'
   Field[5]='HARGA'
   Field[6]='HARGA*&STK'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@e 999999.99'
   Frm[4]='@!'
   Frm[5]='@e 99,999,999'
   Frm[6]='@e 9999,999,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=' '
   Bts[5]=''+tl
   Bts[6]=''+tl
   DbEdit(5,1,20,78,field,'flstl',frm,'','',bts)
   *
ENDDO
CLOSE all
RELEASE klb,nklb,sambung,no,tot,tot2,jml2,xhal,ket2
SET CURSOR OFF
RETURN

Function flstl
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
      If fds='&STK' .and. lks#'S'
         Kdb =kode_bar
         Jml4=stok_rb
         Jml1=stok_a
         Jml2=stok_m
         Jml3=stok_k
         B=row()
         Klm=col()+13
         Save scre to smkdb
         SET CURSOR ON
         SET COLOR TO w
         @b,klm-1 CLEAR to b+5,klm+17
         @b,klm-1 to b+5,klm+17
         @b,klm-4 say '>>>'
         @b+1,klm say 'Awal..:'get jml1 pict '@e 999999.99'
         @b+2,klm say 'Masuk.:'get jml2 pict '@e 999999.99'
         @b+3,klm say 'Keluar:'get jml3 pict '@e 999999.99'
         @b+4,klm say 'ReturB:'get jml4 pict '@e 999999.99'
         Tone(1500,3)
         Tone(2000,6)
         Do pesan with 'Perhatian !  Meng-Edit Stok Bln Lalu,  Stok yg Sekarang HARUS di-Cek lagi !'
         Read
         Rest scre from smkdb
         If lastkey()#27
            Rlok()
            REPLACE stok_a with jml1,stok_m with jml2,stok_k with jml3,stok_rb with jml4
            Jml2=(STOK_A+STOK_M) - (STOK_K+STOK_RB)
            Ul()
            COMMIT
            *
            SELECT 3
            Uselan('STOK.DTA','STOK5.NDX','','','','')
            Seek kdb
            Rlok()
            If lks='T'
               REPLACE stok_a with jml2
            Else
               REPLACE stok_ag with jml2
            ENDIF
            Ul()
            COMMIT
            Close
            *
            SELECT 2
         ENDIF
      Elseif fds='HARGA'
         Rlok()
         @row(),col() get &fds pict '@e 99,999,999'
         Read
         Ul()
         COMMIT
      ENDIF
      SET CURSOR OFF
      Do pesan with katax
      RETURN 1
   Case lastkey()=-9
      Nr=recno()
      SET CURSOR ON
      Kdb=kode_brg
      Do ctkstok
      Go nr
      Do pesan with katax
      SET CURSOR OFF
      RETURN 1
   Othe
      RETURN 1
ENDCASE
RETURN 0

* EOF LST2
