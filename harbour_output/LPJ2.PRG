// ============================================================
// File    : LPJ2.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN PENJUALAN/KLP-ITEM
*----------------------------
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK4.NDX','','','','')
SELECT 3
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
*
STORE tglx to tgl,tgl2
Ksp=SPACE(10)
DO WHILE .t.
   Nmb=SPACE(30)
   SELECT 3
   SET CENTURY ON
   Set filter to
   Set key 28 to daflgn
   @1,0 CLEAR to 24,79
   @2,20 say 'Kode Plgn:'get ksp pict '@!'
   Do pesan with 'ESC=Selesai   F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Carikode=.f.
   Nsp='-'
   If ksp#SPACE(10)
      Set order to 1
      Seek ksp
      If .not. FOUND()
         Set order to 2
         Seek RTRIM(ksp)
         Do daflgn
      ENDIF
      If lastkey()=27
         Loop
      ENDIF
      Nsp=nama_lgn
   ENDIF
   *
   SELECT 2
   @2,31
   @2,31 say RTRIM(nsp)+' ('+RTRIM(ksp)+')'
   @3,20 say 'Nama Brg.:'get nmb pict '@!'
   Do pesan with 'Masukan awalan Nama Barang'
   Read
   If lastkey()=27 .or. nmb=SPACE(30)
      Loop
   ENDIF
   Seek nmb
   If .not. FOUND()
      Carikode=.f.
      Seek RTRIM(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
      ENDIF
   ENDIF
   Klb=kode_klb
   Kdb=kode_bar
   Nmb=nama_brg
   @3,31 say nmb
   @4,20 say 'Tanggal..:'get tgl
   @4,42 say 'sampai'get tgl2
   Read
   If lastkey()=27
      Loop
   ENDIF
   Tgl4=tgl
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf2','','','','')
   Set filter to
   If ksp#SPACE(10)
      Set filter to kode_lgn=ksp
   ENDIF
   *
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal,no
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   *
   Do koplpj2
   STORE 1 to y,n
   STORE 0 to stm,hp,hj,jml,tot,tot2
   Z=LTRIM(str(y))
   DO WHILE .t.
      Seek kdb
      St =satuan
      DO WHILE .not. eof() .and. kode_bar=kdb
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=&brs1
            Stm=stm+jumlah
            Hp =hp + &brs14
            Hj =hj + &brs1
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+left(nama_ksr,10)+tl+faktur+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(harga_2,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y=y+1
            N=n+1
            If n>50
               ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,16)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
               ?''
               Hal=hal+1
               Cf =cf +1
               Cfx=LTRIM(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplpj2
               N=1
            ENDIF
         ENDIF
         Skip
      ENDDO
      If beda
         Do carifljual
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            Uselan('&nf0','&nf2','','','','')
            Set filter to kode_lgn=ksp
         ENDIF
         SET CENTURY ON
      Elseif beda=.f.
         Exit
      ENDIF
   ENDDO
   *
   If hj>0
      Tot2=tot2+ hp
      Tot =tot + hj
   ENDIF
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999,999 ')+st
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,16)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
   ?' JUAL =Rp'+tran(tot, '@e 9,999,999,999')+'      Item=  '+pesanx
   If xOwner .or. Owner
      ?' POKOK=Rp'+tran(tot2,'@e 9,999,999,999')+'    Margin=Rp'+tran(tot-tot2,'@e 999,999,999')
   ENDIF
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
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
   Set marg to 5
   ?chr(27)+chr(33)+chr(1)
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nsp,kdb,kdb2,nmb,nsp,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

PROCEDURE koplpj2
?' LAPORAN PENJUALAN'
If ksp#SPACE(10)
   ?' PELANGGAN: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
ENDIF
?' NAMA BRG : '+RTRIM(nmb)+' ('+RTRIM(kdb)+')'
?' TANGGAL  : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(30)+'HAL:'+tran(hal,'999')
?chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,16)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(191)
?tl+' No.'+tl+'Tanggal    Jam  '+tl+'Kasir     '+tl+'Nota '+tl+'   Qty.'+tl+'   @Pokok'+tl+'    @Jual'+tl+'  Rp.Jumlah'+tl
?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,16)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
RETURN

* LAPORAN PENJUALAN/KLP-ITEM
*----------------------------
PROCEDURE lpj21
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK3.NDX','','','','')
SELECT 3
Uselan('JENIS.DTA','JENIS2.NDX','JENIS.NDX','','','')
*
Klb=SPACE(5)
Klp=.t.
STORE ' ' to jlap,jlap2,ket,flt
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 3
   SET CENTURY ON
   Set filter to
   Set key 28 to dafklb
   @1,0 CLEAR to 23,79
   @2,24 say 'Kelompok:'get klb pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   @4,24 say '0 =Totalnya saja    1 =Terperinci..:'get jlap2 pict '!'
   Do pesan with 'ESC=Selesai     F1=Daftar_Kelompok_Brg'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Set order to 2
   Seek klb
   If .not. FOUND()
      Do pesan2 with 'Tidak ada Klp. '+RTRIM(klb)
      Loop
   ENDIF
   Nklb=nama_klb
   Tgl4=tgl
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf2','','','','')
   Set filter to
   *
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal,no
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   SET CENTURY OFF
   Set alte on
   Set alte to &arg1
   *
   If jlap2#'1'
      ?' LAPORAN PENJUALAN, KLP: '+RTRIM(nklb)
      ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?chr(218)+REPLACE(gl,7)+chr(194)+REPLACE(gl,28)+chr(194)+REPLACE(gl,6)+REPLACE(chr(194)+REPLACE(gl,11),3)+chr(191)
      ?tl+'Kode   '+tl+'Nama Barang'+SPACE(17)+tl+'  Qty.'+tl+'    Rp.Jual'+tl+'   Rp.Pokok'+tl+'  Rp.Margin'+tl
      ?chr(195)+REPLACE(gl,7)+chr(197)+REPLACE(gl,28)+chr(197)+REPLACE(gl,6)+REPLACE(chr(197)+REPLACE(gl,11),3)+chr(180)
      Y=1
      STORE 0 to jml,xjml,hp,hj,tot,tot2
      *
      SELECT 2
      Seek klb
      DO WHILE .not. eof() .and. kode_klb=klb
         Kdb =kode_bar
         Nmb =nama_brg
         Z=LTRIM(str(y))
         *
         DO WHILE .t.
            SELECT 1
            Seek kdb
            DO WHILE .not. eof() .and. kode_bar=kdb
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  xJml=xjml+jumlah
                  Hp =hp + &brs14
                  Hj =hj + &brs1
               ENDIF
               Skip
            ENDDO
            If beda
               Do carifljual
               If tgl>tgl2
                  Exit
               ENDIF
               If file('&nf0')
                  Uselan('&nf0','&nf2','','','','')
                  Set filter to
                  Do cekjlap
               ENDIF
            Elseif beda=.f.
               Exit
            ENDIF
         ENDDO
         *
         If hj#0
            Do fmtjml2
            ?tl+left(kdb,7)+tl+left(nmb,28)+tl+jml+tl+tran(hj,'@e 999,999,999')+tl+iif(xOwner,tran(hp,'@e 999,999,999')+tl+tran(hj-hp,'@e 999,999,999'),SPACE(11)+tl+SPACE(11))+tl
            Tot2=tot2+ hp
            Tot =tot + hj
            Y = y + 1
         ENDIF
         STORE 0 to jml,hp,hj,xjml
         SELECT 2
         Skip
      ENDDO
      ?chr(192)+REPLACE(gl,7)+chr(193)+REPLACE(gl,28)+chr(193)+REPLACE(gl,6)+REPLACE(chr(193)+REPLACE(gl,11),3)+chr(217)
      ?tran(y-1,'999 Item')+SPACE(28)+'Total Rp '+tran(tot,'@e 999,999,999 ')+iif(xOwner,tran(tot2,'@e 999,999,999 ')+tran(tot-tot2,'@e 999,999,999 '),'')
      *
   Else
      Do koplpj21
      Y=1
      STORE 0 to jml,xjml,hp,hj,tot,tot2
      DO WHILE tgl<=tgl2
         *
         SELECT 2
         Seek klb
         DO WHILE .not. eof() .and. kode_klb=klb
            Kdb =kode_bar
            Nmb =nama_brg
            Z=LTRIM(str(y))
            *
            SELECT 1
            Set filter to tanggal=tgl
            Seek kdb
            Tgl3=tanggal
            Tglan=dtoc(tanggal)
            SET CENTURY OFF
            DO WHILE .not. eof() .and. kode_bar=kdb
               Fkt =faktur
               Ksp =kode_lgn
               xJml=jumlah
               Hp  =hp + &brs14
               Hj  =hj + &brs1
               Do fmtjml2
               If xjml#0
                  ?tl+tglan+tl+fkt+tl+iif(ksp#SPACE(10),right(RTRIM(ksp),5),SPACE(5))+tl+left(kdb,7)+tl+nama_brg+tl+jml+tl+tran(harga_2-diskon_rp,'@e 9,999,999')+tl+tran(&brs1,'@e 999,999,999')+tl+iif(xOwner,tran(&brs1-&brs14,'@e 999,999,999'),SPACE(11))+tl
                  Y =y +1
                  No=no+1
                  If no>175
                     ?chr(192)+REPLACE(gl,8)+REPLACE(chr(193)+REPLACE(gl,5),2)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,9)+REPLACE(chr(193)+REPLACE(gl,11),2)+chr(217)
                     ?''
                     Hal=hal+1
                     Cf =cf+1
                     Cfx=LTRIM(str(cf))
                     Arg1='L-JUAL&cfx'+'.RTF'
                     SET CENTURY ON
                     Set alte to &arg1
                     Do koplpj21
                     No=1
                     SET CENTURY OFF
                  ENDIF
               ENDIF
               Skip
               If tanggal=tgl3
                  Tglan=SPACE(8)
               Elseif tanggal#tgl3
                  Tglan=dtoc(tanggal)
               ENDIF
            ENDDO
            Tot2=tot2+ hp
            Tot =tot + hj
            STORE 0 to jml,hp,hj,xjml
            SELECT 2
            Skip
         ENDDO
         Tgl=tgl+1
      ENDDO
      ?chr(192)+REPLACE(gl,8)+REPLACE(chr(193)+REPLACE(gl,5),2)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,9)+REPLACE(chr(193)+REPLACE(gl,11),2)+chr(217)
      ?tran(y-1,'9999 Item')+SPACE(55)+'Total Jual Rp '+tran(tot,'@e 999,999,999 ')+iif(xOwner,tran(tot-tot2,'@e 999,999,999'),'')
      If xOwner
         ?SPACE(69)+'Pokok Rp '+tran(tot2,'@e 999,999,999')
      ENDIF
      *
   ENDIF
   ?' '
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
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
   If jlap2='0'
      Set marg to 10
      ?chr(27)+chr(33)+chr(1)
   Else
      Set marg to 15
      ?chr(27)+chr(33)+chr(4)
   ENDIF
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,kdb,kdb2,nmb,nsp,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

PROCEDURE koplpj21
?' LAPORAN PENJUALAN, KLP: '+RTRIM(nklb)
?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(34)+'HAL:'+tran(hal,'999')
?chr(218)+REPLACE(gl,8)+REPLACE(chr(194)+REPLACE(gl,5),2)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,6)+chr(194)+REPLACE(gl,9)+REPLACE(chr(194)+REPLACE(gl,11),2)+chr(191)
?tl+'Tanggal '+tl+'Nota '+tl+'Plgn.'+tl+'Kode   '+tl+'Nama Barang'+SPACE(19)+tl+'  Qty.'+tl+'    @Jual'+tl+' Jumlah Rp.'+tl+'  Rp.Margin'+tl
?chr(195)+REPLACE(gl,8)+REPLACE(chr(197)+REPLACE(gl,5),2)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,6)+chr(197)+REPLACE(gl,9)+REPLACE(chr(197)+REPLACE(gl,11),2)+chr(180)
RETURN

* LAPORAN PENJUALAN/KLP-TOTAL
*----------------------------
PROCEDURE lpj23
SELECT 1
*
SELECT 2
Uselan('JENIS.DTA','JENIS.NDX','','','','')
*
Count to jml
STORE ' ' to jlap,ket
STORE tglx to tgl,tgl2
DO WHILE .t.
   @1,0 CLEAR to 23,79
   @2,21 say 'Tanggal.:'get tgl
   @2,42 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   If mont(tgl)#mont(tgl2)
      Do pesan2 with 'Bulan Harus Sama'
      Loop
   ENDIF
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf2','','','','')
   *
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal,no
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?' '+aidi1
   ?' '+REPLACE('-',len(aidi1))
   ?' REKAP PENJUALAN, PER KELOMPOK BARANG'
   ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,31)+REPLACE(chr(194)+REPLACE(gl,13),3)+chr(191)
   ?tl+' No.'+tl+'Kelompok Barang'+SPACE(16)+tl+'      Rp.Jual'+tl+'     Rp.Pokok'+tl+'    Rp.Margin'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,31)+REPLACE(chr(197)+REPLACE(gl,13),3)+chr(180)
   Y=1
   STORE 0 to tot,totx,tot2,jml
   SELECT 2
   Go top
   DO WHILE .not. eof()
      Klb =kode_klb
      Nklb=nama_klb
      Z=LTRIM(str(y))
      *
      SELECT 1
      Set filter to kode_klb=klb
      Go top
      If .not. eof()
         DO WHILE .not. eof() .and. kode_klb=klb
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml =jml +&brs14
               Tot2=tot2+&brs1
               Totx=totx+&brs14
               Tot =tot +&brs1
            ENDIF
            Skip
         ENDDO
      ENDIF
      If tot2#0
         ?tl+tran(y,'999.')+tl+klb+left(nklb,26)+tl+tran(tot2,'@e 9,999,999,999')+tl+iif(xOwner,tran(jml,'@e 9,999,999,999')+tl+tran(tot2-jml,'@e 9,999,999,999'),SPACE(13)+tl+SPACE(13))+tl
         Y = y + 1
      ENDIF
      STORE 0 to tot2,jml
      SELECT 2
      Skip
   ENDDO
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,31)+REPLACE(chr(193)+REPLACE(gl,13),3)+chr(217)
   ?SPACE(11)+'Total Rp'+SPACE(19)+tran(tot, '@e 9,999,999,999 ')+iif(xOwner,tran(totx,'@e 9,999,999,999 ')+tran(tot-totx,'@e 9,999,999,999'),'')
   ?' '
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
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
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* LAPORAN PENJUALAN/TGL/TOTAL
*----------------------------
PROCEDURE lpj3
STORE tglx to tgl,tgl2
STORE ' ' to jlap,ket
Tgl=ctod('01-'+LTRIM(str(mont(tglx)))+'-'+LTRIM(str(year(tglx))))
DO WHILE .t.
   @1,0 CLEAR to 23,79
   @2,24 say 'Tanggal.:'get tgl
   @2,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   If mont(tgl)#mont(tgl2)
      Do pesan with 'Bulan Harus Sama'
      Wait''
      Loop
   ENDIF
   @1,0 CLEAR to 23,79
   STORE 0 to tot,tot2,tdns,jml,jml2,jdns
   Do konvtgl
   *
   Tgl1=tgl
   Awl =1
   Bln1=mont(tgl)
   Bln2=mont(tgl2)
   Hr  =''
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+&nf0
      Loop
   ENDIF
   SELECT 3
   Uselan('&nf0','&nf3','','','','')
   Do pesan with 'Tunggu, sedang dihitung'
   *
   STORE 1 to cf,hal,no
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?SPACE(5)+aidi1
   ?SPACE(5)+REPLACE('-',len(aidi1))
   ?SPACE(4)+' REKAP PENJUALAN PER HARI'
   ?SPACE(4)+' TANGGAL : '+dtoc(tgl1)+' s/d '+dtoc(tgl2)
   ?SPACE(4)+chr(218)+REPLACE(gl,13)+REPLACE(chr(194)+REPLACE(gl,13),4)+chr(191)
   ?SPACE(4)+tl+'Tgl - Hari   '+tl+'     Rp.Jual '+tl+'    Rp.Pokok '+tl+'   Rp.Margin '+tl+'   Rp.Donasi '+tl
   ?SPACE(4)+chr(195)+REPLACE(gl,13)+REPLACE(chr(197)+REPLACE(gl,13),4)+chr(180)
   DO WHILE tgl<=tgl2
      Do hari
      Do konvtgl
      Tgl3=tgl
      Hri1=hri
      Bln1=hri
      STORE 0 to jml,jml2,xby
      Seek dtos(tgl)
      DO WHILE .not. eof() .and. (tanggal=tgl)
         Jml2=jml2+ &brs1
         Jml =jml + &brs14
         Jdns=jdns+ rp_donasi
         Tot2=tot2+ &brs1
         Tot =tot + &brs14
         Tdns=tdns+ rp_donasi
         Skip
      ENDDO
      ?SPACE(4)+tl+' '+hri1+' - '+hr+' '+tl+tran(jml2,'@e 9999,999,999 ')+tl+iif(xOwner,tran(jml,'@e 9999,999,999 ')+tl+tran(jml2-jml,'@e 9999,999,999 '),SPACE(13)+tl+SPACE(13))+tl+tran(jdns,'@e 9999,999,999 ')+tl
      If jml2#0
         Awl =awl+1
      ENDIF
      STORE 0 to jml,jml2,jdns
      Tgl =tgl3
      Tgl =tgl+1
   ENDDO
   Awl=awl-1
   ?SPACE(4)+chr(195)+REPLACE(gl,13)+REPLACE(chr(197)+REPLACE(gl,13),4)+chr(180)
   ?SPACE(4)+tl+'  Total   Rp.'+tl+tran(tot2,'@e 9999,999,999 ')+tl+iif(xOwner,tran(tot,'@e 9999,999,999 ')+tl+tran(tot2-tot,'@e 9999,999,999 '),SPACE(13)+tl+SPACE(13))+tl+tran(tdns,'@e 9999,999,999 ')+tl
   ?SPACE(4)+chr(195)+REPLACE(gl,13)+REPLACE(chr(197)+REPLACE(gl,13),4)+chr(180)
   If awl#0
      ?SPACE(4)+tl+'  Rata2   Rp.'+tl+tran(tot2/awl,'@e 9999,999,999 ')+tl+iif(xOwner,tran(tot/awl,'@e 9999,999,999 ')+tl+tran((tot2-tot)/awl,'@e 9999,999,999 '),SPACE(13)+tl+SPACE(13))+tl+tran(tdns/awl,'@e 9999,999,999 ')+tl
   ENDIF
   ?SPACE(4)+chr(192)+REPLACE(gl,13)+REPLACE(chr(193)+REPLACE(gl,13),4)+chr(217)
   ?' '
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Tgl=tgl1
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
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CENTURY ON
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* LAPORAN PENJUALAN/BLN GLOBAL
*------------------------------
PROCEDURE lpj31
Tgl=tglx
Do konvtgl
STORE bln+'-'+thn to tgl,tgl2
DO WHILE .t.
   SELECT 2
   Set filter to
   @1,0 CLEAR to 23,79
   @2,26 say 'Dari:   -     sampai   -'
   @2,32 get tgl pict '99-9999'
   @2,47 get tgl2 pict '99-9999'
   Do pesan with 'DiIsi Bulan-Tahun saja'
   Read
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   Bln =left(tgl,2)
   Thn =right(tgl,4)
   Bln2=left(tgl2,2)
   Thn2=right(tgl2,4)
   x1  =val(bln)
   x2  =val(bln2)
   If thn#thn2 .or. x1>x2
      Do pesan2 with 'Tahun Harus Sama  -atau-  Koreksi Lagi Bulannya'
      Loop
   ENDIF
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal,no
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?SPACE(4)+aidi1
   ?SPACE(4)+REPLACE('-',len(aidi1))
   ?SPACE(4)+'REKAP PENJUALAN PER BULAN'
   ?SPACE(4)+'BULAN: '+(tgl)+' s/d '+(tgl2)
   ?SPACE(3)+chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,5)+REPLACE(chr(194)+REPLACE(gl,15),4)+chr(191)
   ?SPACE(3)+tl+' No.'+tl+' Bln '+tl+'       Rp.Jual '+tl+'      Rp.Pokok '+tl+'     Rp.Margin '+tl+'     Rp.Donasi '+tl
   ?SPACE(3)+chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,5)+REPLACE(chr(197)+REPLACE(gl,15),4)+chr(180)
   B=5
   STORE 0 to hp,hj,jdns,tot,tot2,tdns,n
   DO WHILE x1<=x2
      xBlnthn='thn+LTRIM(str(x1))'
      If len(LTRIM(str(x1)))=1
         xBlnthn="thn+'0'+LTRIM(str(x1))"
      ENDIF
      Nf1='J-'+&xblnthn+'.DTA'
      If file('&nf1')
         Uselan('&nf1','','','','','')
         Sum &brs14,&brs1,rp_donasi to hp,hj,jdns
      ENDIF
      If x1=1
         nBln='Jan'
      Elseif x1=2
         nBln='Feb'
      Elseif x1=3
         nBln='Mar'
      Elseif x1=4
         nBln='Apr'
      Elseif x1=5
         nBln='Mei'
      Elseif x1=6
         nBln='Jun'
      Elseif x1=7
         nBln='Jul'
      Elseif x1=8
         nBln='Ags'
      Elseif x1=9
         nBln='Sep'
      Elseif x1=10
         nBln='Okt'
      Elseif x1=11
         nBln='Nov'
      Elseif x1=12
         nBln='Des'
      ENDIF
      *
      N   =n+1
      ?SPACE(3)+tl+tran(n,'999 ')+tl+' '+nbln+' '+tl+tran(hj,'@e 99,999,999,999 ')+tl+iif(xOwner,tran(hp,'@e 99,999,999,999 ')+tl+tran(hj-hp,'@e 99,999,999,999 '),+SPACE(15)+tl+SPACE(15))+tl+tran(jdns,'@e 99,999,999,999 ')+tl
      Tot =tot +hj
      Tot2=tot2+hp
      Tdns=tdns+jdns
      STORE 0 to hp,hj,jdns
      B =b+1
      X1=x1+1
   ENDDO
   ?SPACE(3)+chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,5)+REPLACE(chr(193)+REPLACE(gl,15),4)+chr(217)
   ?SPACE(5)+'TOTAL =Rp '+tran(tot,'@e 99,999,999,999  ')+iif(xOwner,tran(tot2,'@e 99,999,999,999  ')+tran(tot-tot2,'@e 99,999,999,999  '),SPACE(32))+tran(tdns,'@e 99,999,999,999')
   ?''
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai     F10=Cetak'
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
   Type &arg1 to prin
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2,nbln,xblnthn,x1,x2
Tgl=tglx
RETURN

* LAPORAN PENDPTAN KASIR
*-----------------------
PROCEDURE lpj32
SELECT 1
*
SELECT 2
*
SELECT 3
Uselan('KASIR.DTA','','','','','')
*
Ksr='  '
STORE tglx to tgl,tgl2
DO WHILE .t.
   Ada=.f.
   Nsls=SPACE(20)
   SELECT 3
   Set filter to
   Set key 28 to dafksr
   @1,0 CLEAR to 23,79
   @2,24 say 'Kasir.:'get nsls pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai dg.'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   If nsls#SPACE(20)
      LOCATE FOR nama=RTRIM(nsls)
      If .not. FOUND()
         Do pesan with 'Nama Kasir Tidak Ada.  Ulangi !'
         Wait''
         Loop
      ENDIF
      Ksr =nomer
      Nsls=nama
      Ada =.t.
   ENDIF
   Do konvtgl
   Do bacafile
   Hal =1
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf3','','','','')
   SELECT 2
   *Uselan('RETURJN.DTA','RETURJN1.NDX','','','','')
   *
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-KASIR.RTF
   ?SPACE(5)+aidi1
   ?SPACE(5)+REPLACE('-',len(aidi1))
   ?SPACE(4)+' LAPORAN PENDAPATAN KASIR'
   ?SPACE(4)+' TANGGAL: '+dtoc(tgl)+' S/D '+dtoc(tgl2)
   ?SPACE(4)+REPLACE(gl,67)
   ?SPACE(4)+' NO.  NAMA KASIR                 RP.JUAL     RP.MARGIN   RP.DONASI'
   ?SPACE(4)+REPLACE(gl,67)
   STORE 0 to hp1,hp2,hj1,hj2,jdns,tot,tot2,tdns
   STORE ' ' to sip1,sip2
   Tgl3=tgl
   No  =1
   *
   SELECT 3
   If ada
      LOCATE FOR nama=nsls
   Else
      Go top
   ENDIF
   DO WHILE .not. eof()
      Ksr =nomer
      Nsls=nama
      *
      SELECT 1
      Set filter to kode_ksr=ksr
      DO WHILE tgl<=tgl2
         Seek dtos(tgl)
         If FOUND()
            Exit
         Elseif .not. FOUND()
            Tgl=tgl+1
         ENDIF
      ENDDO
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_ksr=ksr
         Hp1  =hp1 + &brs14
         Hj1  =hj1 + &brs1
         Jdns =jdns+ rp_donasi
         Sip1='1'
         Skip
      ENDDO
      If hj1#0
         ?SPACE(4)+tran(no,'999.  ')+nsls+tran(hj1,'@e 99,999,999,999')+iif(xOwner,tran(hj1-hp1,'@e 99,999,999,999'),SPACE(14))+tran(jdns,'@e 9999,999,999')
         No=no+1
         Tot =tot +hp1
         Tot2=tot2+hj1
         Tdns=tdns+jdns
      ENDIF
      STORE 0 to hp1,hp2,hj1,hj2,jdns
      Tgl=tgl3
      *
      SELECT 3
      If ada
         Exit
      ENDIF
      Skip
   ENDDO
   ?SPACE(4)+REPLACE(gl,67)
   ?SPACE(10)+'TOTAL RP'+SPACE(12)+tran(tot2,'@e 99,999,999,999')+iif(xOwner,tran(tot2-tot,'@e 99,999,999,999'),SPACE(14))+tran(tDNS,'@e 9999,999,999')
   ?SPACE(4)+REPLACE(gl,67)
   ?''
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   SET COLOR TO w/b
   Memoedit(Memoread('L-KASIR.RTF'),1,0,23,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai     F10=Cetak'
   If lastkey()=27
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
   Type L-KASIR.RTF to print
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RETURN

* LAPORAN PENJUALAN/TGL/NOTA
*---------------------------
PROCEDURE lpj4
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
SELECT 3
Uselan('KASIR.DTA','','','','','')

Nsls=SPACE(20)
Fkt=SPACE(5)
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 3
   SET CENTURY ON
   Set filter to
   Set key 28 to dafksr
   @1,0 CLEAR to 23,79
   @2,29 say 'Tanggal.:'get tgl
   @3,29 say 'Nama Ksr:'get nsls pict '@!'
   @4,29 say 'No.Nota.:'get fkt pict '@!'
   Do pesan with 'ESC=Selesai   F1=Daftar Kasir'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   LOCATE FOR nama=nsls
   Nsls=nama
   Ksr =nomer
   STORE 0 to tot,tot2
   Bacafile('J')
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf3','&nf1','&nf2','','')
   Seek dtos(tgl)+ksr+fkt
   If .not. FOUND()
      Do pesan2 with 'Tidak ada data Tgl:'+dtoc(tgl)+'  Ksr:'+RTRIM(nsls)+'  Nota:'+RTRIM(fkt)
      Loop
   ENDIF
   Nr=recno()
   SET CENTURY OFF
   @1,0 CLEAR to 23,79
   @1,1 say '*Ksr: '+RTRIM(nsls)+'  *Tgl: '+dtoc(tgl)+'  *Nota: '+fkt+' / '+kode_lks
   @2,0 say REPLACE(gl,5)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,6)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,8)+chr(194)+REPLACE(gl,9)
   @3,0 say 'Tgl. '+tl+'Plgn '+tl+'Kode  '+tl+'Nama Barang'+SPACE(19)+tl+' Jml.Brg.  '+tl+'  @Harga'+tl+'Jumlah Rp'
   @4,0 say REPLACE(gl,5)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,11)+chr(193)+REPLACE(gl,8)+chr(193)+REPLACE(gl,9)
   B=5
   DO WHILE .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls .and. faktur=fkt)
      Do case
         Case jumlah<0
            Jml=tran(jumlah,'@e  9,999 ')
         Case jumlah=0
            Jml=tran(jumlah,'@e  9,999 ')
         Case jumlah>0 .and. jumlah<1
            Jml=tran(jumlah,'@e 999.99 ')
         Case jumlah>=1
            If mod(jumlah,int(jumlah))>0
               Jml=tran(jumlah,'@e 999.99 ')
            Else
               Jml=tran(jumlah,'@e  9,999 ')
            ENDIF
      ENDCASE
      Jhg=&brs1
      Tot=tot+jhg
      Ksp=kode_lgn
      If ksp=SPACE(10)
         Ksp='CASH '
      Else
         Ksp=right(RTRIM(ksp),5)
      ENDIF
      @b,0 say left(dtoc(tanggal),5)+' '+ksp+' '+left(kode_brg,7)+left(nama_brg,30)+jml+satuan+tran(harga_2,'@e 9,999,999')+tran(jhg,'@e 99,999,999')
      @b,0 say left(dtoc(tanggal),5)+' '+ksp+' '+left(kode_brg,7)+left(nama_brg,30)+jml+satuan+tran(harga_2,'@e 9,999,999')+tran(jhg,'@e 99,999,999')
      B=b+1
      If b>21
         @b,0 to b,79
         Do pesan with '[Pgln]-Lanjut'
         Wait''
         If lastkey()=27
            Exit
         ENDIF
         B =5
      ENDIF
      Skip
   ENDDO
   @b,0 to b,79
   @b+1,0 CLEAR to 23,79
   @b+1,60 say 'TOTAL= Rp'+tran(tot, '@e 999,999,999')
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Nota   F8=Hapus/Item Brg'
   If lastkey()=-3
      Do pesan with 'Nota diatas akan DiHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @24,55 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Go nr
         DO WHILE .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls .and. faktur=fkt)
            Kdb=kode_bar
            Jml=jumlah
            Rlok()
            Dele
            REPLACE keterangan with left(time(),5)+' '+GetEnv("KASSA")+'ADM '+dtoc(tglx)
            Ul()
            *
            SELECT 2
            Seek kdb
            Rlok()
            REPLACE stok_k with (stok_k-jml)
            Ul()
            *
            SELECT 1
            Skip
         ENDDO
         Loop
      ENDIF
   Elseif lastkey()=-7 .and. xOwner
      Hut=.t.
      Do hpj
      Loop
   ENDIF
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* LAPORAN TOTAL NO.NOTA
*-----------------------
PROCEDURE lpj41
SELECT 1
Uselan('KASIR.DTA','','','','','')
SELECT 2
*
SELECT 3
*
Nsls=SPACE(20)
STORE ' ' to hri,bln,thn,jlap,ket,flt,ksr
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafksr
   @1,0 CLEAR to 23,79
   @2,24 say 'Nm.Kasir:'get nsls pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Kasir'
   Read
   @24,0
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   If nsls#SPACE(20)
      LOCATE FOR nama=nsls
      If .not. FOUND()
         Do pesan2 with 'Tidak ada kasir '+RTRIM(nsls)
         Loop
      ENDIF
      Ksr=nomer
   ENDIF
   @1,0 CLEAR to 23,79
   Tgl4=tgl
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   Do konvtgl
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Do bukafljual
   If nsls#SPACE(20)
      Set filter to nama_ksr=nsls
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   *
   Nr=recno()
   STORE 0 to b,no,tot,tot2,tots,tots2,tdns,jml1,jml2,jdns
   STORE 1 to cf,hal,no,n
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplnota
   *
   DO WHILE .t.
      SET CENTURY OFF
      xJam=jam
      Mj  =kassa
      Fkt =faktur
      Sls =nama_ksr
      Tgl3=tanggal
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jml1 =jml1+&brs1
         Jdns =jdns+rp_donasi
         Tot  =tot +&brs1
         Tots =tots+&brs1
         Jml2 =jml2+&brs14
         Tot2 =tot2+&brs14
         Tots2=tots2+&brs14
         Tdns =tdns+rp_donasi
         Skip
         If faktur#fkt
            Kata1="tran(jml1,'@e 9999,999,999')+tl+iif(xOwner,tran(jml2,'@e 9999,999,999')+tl+tran(jml1-jml2,'@e 9999,999,999'),SPACE(12)+tl+SPACE(12))+tl+tran(jdns,'@e 9999,999,999')+tl"
            ?tl+tran(no,'9999.')+tl+left(dtoc(tgl3),2)+'/'+SUBSTR(dtoc(tgl3),4,2)+' '+xjam+tl+right(fkt,5)+'-'+mj+'-'+left(sls,12)+tl+&kata1
            No=no+1
            N =n +1
            Mj  =kassa
            xJam=jam
            Fkt =faktur
            Sls =nama_ksr
            Tgl3=tanggal
            STORE 0 to jml1,jml2,jdns
            If n>50
               ?chr(195)+REPLACE(gl,38)+REPLACE(chr(197)+REPLACE(gl,12),4)+chr(180)
               ?tl+SPACE(18)+'Jumlah hal.ini =  Rp'+tl+tran(tots,'@e 9999,999,999')+tl+iif(xOwner,tran(tots2,'@e 9999,999,999')+tl+tran(tots-tots2,'@e 9999,999,999'),SPACE(12)+tl+SPACE(12))+tl+SPACE(12)+tl
               ?tl+SPACE(18)+'Sub Total      =  Rp'+tl+tran(tot ,'@e 9999,999,999')+tl+iif(xOwner,tran(tot2 ,'@e 9999,999,999')+tl+tran(tot-tot2  ,'@e 9999,999,999'),SPACE(12)+tl+SPACE(12))+tl+SPACE(12)+tl
               ?chr(192)+REPLACE(gl,38)+REPLACE(chr(193)+REPLACE(gl,12),4)+chr(217)
               ?''
               STORE 0 to n,tots,tots2
               Hal=hal+1
               Cf =cf +1
               Cfx=LTRIM(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplnota
            ENDIF
         ENDIF
      ENDDO
      If beda
         SET CENTURY ON
         Do carifljual
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            Do bukafljual
         ENDIF
      Elseif beda=.f.
         Exit
      ENDIF
   ENDDO
   ?chr(195)+REPLACE(gl,38)+REPLACE(chr(197)+REPLACE(gl,12),4)+chr(180)
   ?tl+SPACE(18)+'Jumlah hal.ini =  Rp'+tl+tran(tots,'@e 9999,999,999')+tl+iif(xOwner,tran(tots2,'@e 9999,999,999')+tl+tran(tots-tots2,'@e 9999,999,999'),SPACE(12)+tl+SPACE(12))+tl+SPACE(12)+tl
   ?tl+SPACE(18)+'Grand Total    =  Rp'+tl+tran(tot ,'@e 9999,999,999')+tl+iif(xOwner,tran(tot2 ,'@e 9999,999,999')+tl+tran(tot-tot2  ,'@e 9999,999,999'),SPACE(12)+tl+SPACE(12))+tl+tran(tdns,'@e 9999,999,999')+tl
   ?chr(192)+REPLACE(gl,38)+REPLACE(chr(193)+REPLACE(gl,12),4)+chr(217)
   ?''
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
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
   Set marg to 20
   ?chr(27)+chr(33)+chr(4)
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-JUAL&cfx'+'.RTF'
       Type &arg1 to prin
       Eject
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE nsls,tgl2,b,no,nr,tot,tot2,jhg,jml,jml1,jml2
RETURN

PROCEDURE koplnota
?' LAPORAN PENJUALAN PER NO.NOTA'
If nsls#SPACE(20)
   ?' KASIR  : '+RTRIM(nsls)
ENDIF
?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' // Hal:'+tran(hal,'999')
?chr(218)+REPLACE(gl,5)+chr(194)+REPLACE(gl,11)+chr(194)+REPLACE(gl,20)+REPLACE(chr(194)+REPLACE(gl,12),4)+chr(191)
?tl+'  No.'+tl+'Tgl.  Jam  '+tl+'Nota-Ks-Kasir       '+tl+'     Rp Jual'+tl+'    Rp Pokok'+tl+'   Rp Margin'+tl+'   Rp Donasi'+tl
?chr(195)+REPLACE(gl,5)+chr(197)+REPLACE(gl,11)+chr(197)+REPLACE(gl,20)+REPLACE(chr(197)+REPLACE(gl,12),4)+chr(180)
RETURN

* LAPORAN PENJUALAN (TERLARIS)
*-----------------------------
PROCEDURE lpj99
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK5.NDX','STOK2.NDX','','','')
SELECT 7
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
*
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 2
   Set order to 1
   Set filter to
   *
   SELECT 7
   Set filter to
   SET CURSOR ON
   SET CENTURY ON
   Set key 28 to dafspl
   SET COLOR TO /bg
   Jlap=' '
   Ksp =SPACE(5)
   @1,0 CLEAR to 23,79
   @2,24 say 'Supplier:'get ksp pict'@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai     F1=Daftar_Supplier'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Ket2  =''
   Syarat='kode_bar#SPACE(13)'
   If ksp#SPACE(5)
      @5,24 say 'Tampilkan yg Tidak Ada Trx? Y/T :'get jlap pict '!'
      Do pesan with 'ESC=Ulangi'
      Read
      If lastkey()=27
         Loop
      ENDIF
      *
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada kode '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp =nama_spl
      Ket2=RTRIM(nsp)+' ('+RTRIM(ksp)+')'
      Syarat='kode_spl=ksp'
      *
      SELECT 2
      Set order to 2
      Set filter to &syarat
   ENDIF
   *
   Tgl4=tgl
   STORE .f. to beda,bedafile
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF
   Do konvtgl
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   *
   SELECT 1
   Uselan('&nf0','&nf2','','','','')
   *
   SELECT 3
   Useidx('TOP-RANK.DTA')
   Copy stru to TOP-RANK.TMP
   Close
   *
   Useidx('TOP-RANK.TMP')
   *
   Ada=.f.
   Awl=1
   *
   @1,0 CLEAR to 23,79
   DO WHILE .t.
      SELECT 2
      Do pesan with 'Tunggu, sedang dicari... Rec: '+LTRIM(str(reccount()))+'-          File: '+nf0
      SET COLOR TO w+/r
      *
      Go top
      DO WHILE .not. eof() .and. &syarat
         Klb =kode_klb
         Kdb =kode_bar
         *Kdby=iif(len(rtri(kdb))>=9,left(kdb,3)+'x'+right(rtri(kdb),4),left(kdb,8))
         Kdby=kode_bar
         Nmb =nama_brg
         St  =satuan
         Stk =(stok_a+stok_m)-stok_k
         Ksp =kode_spl
         Hp  =harga
         STORE 0 to jml,jhg,jhp,jdi
         @24,44 say LTRIM(str(awl))
         *
         SELECT 1
         Seek kdb
         If FOUND()
            DO WHILE .not. eof() .and. kode_bar=kdb
               If (tanggal>=tgl .and. tanggal<=tgl2)
                  Jml=jml+jumlah
                  Jhg=jhg+(jumlah*harga_2)
                  Jhp=jhp+(jumlah*harga)
                  Jdi=jdi+(jumlah*diskon_rp)
                  If kode_spl#SPACE(5)
                     Ksp=kode_spl
                  ENDIF
               ENDIF
               Skip
            ENDDO
         Elseif .not. FOUND()
            SELECT 2
            Skip
            Loop
         ENDIF
         *
         If jml#0 .or. jlap='Y'
            Ada=.t.
            SELECT 3
            If bedafile
               LOCATE FOR kode_brg=kdby
               If FOUND()
                  REPLACE jumlah with (jumlah+jml),harga with (harga+jhp),harga_2 with (harga_2+jhg),diskon with (diskon+jdi),margin with (margin+(jhg-jhp))
               Elseif .not. FOUND()
                  APPEND BLANK
                  REPLACE kode_spl with ksp,kode_klb with klb,kode_brg with kdby,nama_brg with nmb,satuan with st,harga_bl with hp,;
                       sisa with stk,jumlah with jml,harga with jhp,harga_2 with jhg,diskon with jdi,margin with (jhg-jhp)
               ENDIF
            Elseif bedafile=.f.
               APPEND BLANK
               REPLACE kode_spl with ksp,kode_klb with klb,kode_brg with kdby,nama_brg with nmb,satuan with st,harga_bl with hp,;
                    sisa with stk,jumlah with jml,harga with jhp,harga_2 with jhg,diskon with jdi,margin with (jhg-jhp)
            ENDIF
         ENDIF
         *
         Awl=awl+1
         SELECT 2
         Skip
      ENDDO
      Awl=1
      If beda
         Bedafile=.t.
         Do carifljual
         Do konvtgl
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            SELECT 1
            Uselan('&nf0','&nf2','','','','')
            Set filter to &syarat
         ENDIF
      Elseif beda=.f.
         Exit
      ENDIF
   ENDDO
   If ada
      SELECT 6
      Close
      *
      SELECT 5
      Close
      *
      SELECT 4
      Close
      *
      SELECT 3
      Sort on jumlah   /D to TOP-QTY.TMP
      Sort on harga_2  /D to TOP-RPH.TMP
      Sort on margin   /D to TOP-MRG.TMP
      Sort on nama_brg,jumlah to TOP-NMB.TMP
      Close
      *
      Pesanx='T=Top_Ranking     F1=Cari_Nama    F10=Cetak'
      SELECT 3
      Useidx('TOP-QTY.TMP')
      SELECT 4
      Useidx('TOP-RPH.TMP')
      SELECT 5
      Useidx('TOP-MRG.TMP')
      SELECT 6
      Useidx('TOP-NMB.TMP')
      *
      SET CENTURY OFF
      SET COLOR TO /bg
      Tgl=tgl4
      Ket='TOP ITEM BRG,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0 CLEAR to 23,79
      @1,1 say ket+',  '+ket2
      @2,0 to 2,79 doub
      @22,0 to 22,79
      DO WHILE .t.
         DECLARE field[11],bts[11],frm[11],kop[11]
         Do pesan with pesanx
         Field[1]='KODE_BRG'
         Field[2]='NAMA_BRG'
         Field[3]='SATUAN'
         Field[4]='JUMLAH'
         Field[5]='SISA'
         Field[6]='HARGA_2'
         Field[7]='MARGIN'
         Field[8]='KODE_SPL'
         Field[9]='KODE_KLB'
         Kop[1]='Kode'
         Kop[2]='Nama Barang'
         Kop[3]='Sat.'
         Kop[4]='   Qty.'
         Kop[5]='   Sisa'
         Kop[6]=' Total Jual'
         Kop[7]='     Margin'
         Kop[8]='Supl.'
         Kop[9]='Klp.'
         Frm[1]=''
         Frm[2]='@!'
         Frm[3]='@!'
         Frm[4]='@e 999,999'
         Frm[5]='@e 999,999'
         Frm[6]='@e 999,999,999'
         Frm[7]='@e 999,999,999'
         Frm[8]='@!'
         Frm[9]='@!'
         Bts[1]=' '
         Bts[2]=' '
         STORE tl to bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9],bts[10]
         DbEdit(3,0,21,79,field,'flpj99',frm,kop,gl,bts)
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
CLOSE all
RELEASE klb,ksp,jlap,ket,nklb,kdb,st,nmb,tgl2,jml1,jml2,jhg1,jhg2,jhp1,jhp2,awl
RETURN

Function flpj99
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
      @22,1 say REPLACE(gl,15)
      SET COLOR TO gr+/b
      @22,1 say LTRIM(str(recno()))+'//'+LTRIM(str(reccount()))
      SET COLOR TO /bg
      RETURN 1
Elseif lastkey()=27
      RETURN 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=SPACE(30)
      SET CURSOR ON
      Do pesan with ''
      SET COLOR TO w+/r
      @24,18 say 'Urut Nama Brg:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#SPACE(30)
         SET COLOR TO /bg
         LOCATE FOR nama_brg=RTRIM(nmb)
         If .not. FOUND()
           Go nr
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET COLOR TO /bg
      SET CURSOR OFF
      RETURN 1
Elseif uppe(chr(lastkey()))='T' .and. fds='NAMA_BRG'
      SELECT 6
      Ket='TOP ITEM BRG,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='JUMLAH'
      SELECT 3
      Ket='TOP QTY,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='HARGA_2'
      SELECT 4
      Ket='TOP RUPIAH,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)+chr(4)
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='MARGIN'
      SELECT 5
      Ket='TOP MARGIN,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0
      @1,1 say ket+',  '+ket2
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)+chr(4)+chr(4)
      RETURN 0
Elseif lastkey()=-9
      Go top
      SET CURSOR ON
      Pil='1'
      Do pesan with '1=Printer   2=File : '
      @24,50 get pil pict '!'
      Read
      If pil='2'
         No =1
         Nf='C:\TOPRANK.CSV'
         Do pesan with 'Tunggu, sedang proses '+nf
         Set alte on
         Set alte to &nf
         ?'   ;    ;LAPORAN PENJUALAN; ; ; ; ;'
         ?'   ;    ;TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'; ; ; ; ;'
         ?'NO.;KODE;NAMA BARANG;QTY;SISA;SAT;JUMLAH RP'
         DO WHILE .not. eof()
            Jml=tran(jumlah,'@e 999,999')
            Stk=tran(sisa,'@e 999,999')
            ?LTRIM(str(no))+';'+' '+alltrim(kode_brg)+';'+RTRIM(nama_brg)+';'+LTRIM(jml)+';'+LTRIM(stk)+';'+RTRIM(satuan)+';'+LTRIM(str(harga_2))
            No=no+1
            Skip
         ENDDO
         ?''
         Set alte off
         Set alte to
         Do pesan with 'Export selesai..., nama file: '+SUBSTR(nf,4,12)+' , di drive C:\'
         Wait''
         SET COLOR TO /bg
         SET CURSOR OFF
         Do pesan with pesanx
         RETURN 2
      ENDIF
      *
      STORE 1 to n,no,hal
      STORE 0 to jml,jml2,tot,tot2,totd,totd2
      Do cp
      If cc=0
         RETURN 0
      ENDIF
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to 5
      ?chr(27)+chr(33)+chr(4)
      ?' REKAP PENJUALAN BARANG'
      ?' '+ket
      ?' '+ket2+'  / Hal:'+tran(hal,'999')
      ?'/'+REPLACE('=',122)+'\'
      ?'| No.| Supp  | Klp. | Kode          | Nama Barang                   | Sat. |  Qty.  | Total Jual |     Diskon | Netto Jual |'
      ?'|'+REPLACE('-',122)+'|'
      DO WHILE .not. eof()
         ?tran(no,'|999 | ')+kode_spl+' | '+kode_klb+'| '+kode_brg+' | '+left(nama_brg,30)+'| '+satuan+'|'+tran(jumlah,'@e 999,999 |')+tran(harga_2,'@e 999,999,999 |')+tran(diskon,'@e 999,999,999 |')+tran(harga_2-diskon,'@e 999,999,999 |')
         Jml  =jml  +jumlah
         Jml2 =jml2 +jumlah
         Tot  =tot  +harga_2
         Tot2 =tot2 +harga_2
         Totd =totd +diskon
         Totd2=totd2+diskon
         No=no+1
         N =n+1
         If n>50
            ?'|'+REPLACE('-',122)+'|'
            ?'|'+SPACE(37)+'Jumlah Hal ini'+SPACE(23)+'|'+tran(jml2,'@e 999,999 |')+tran(tot2,'@e 999,999,999 |')+tran(totd2,'@e 999,999,999 |')+tran(tot2-totd2,'@e 999,999,999 |')
            ?'|'+SPACE(37)+'Sub Total   Rp'+SPACE(23)+'|'+tran(jml ,'@e 999,999 |')+tran(tot ,'@e 999,999,999 |')+tran(totd ,'@e 999,999,999 |')+tran(tot-totd  ,'@e 999,999,999 |')
            ?'|'+REPLACE('-',122)+'|'
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
            STORE 0 to jml2,tot2,totd2
            N   =1
            Hal =hal+1
            SET PRINTER ON
            SET DEVICE TO PRINTER
            Set marg to 5
            ?chr(27)+chr(33)+chr(4)
            ?' REKAP PENJUALAN BARANG'
            ?' '+ket
            ?' '+ket2+'  / Hal:'+tran(hal,'999')
            ?'/'+REPLACE('=',122)+'\'
            ?'| No.| Supp  | Klp. | Kode          | Nama Barang                   | Sat. |  Qty.  | Total Jual |     Diskon | Netto Jual |'
            ?'|'+REPLACE('-',122)+'|'
         ENDIF
         Skip
      ENDDO
      ?'|'+REPLACE('-',122)+'|'
      ?'|'+SPACE(37)+'Jumlah Hal ini'+SPACE(23)+'|'+tran(jml2,'@e 999,999 |')+tran(tot2,'@e 999,999,999 |')+tran(totd2,'@e 999,999,999 |')+tran(tot2-totd2,'@e 999,999,999 |')
      ?'|'+SPACE(37)+'Grand Total Rp'+SPACE(23)+'|'+tran(jml ,'@e 999,999 |')+tran(tot ,'@e 999,999,999 |')+tran(totd ,'@e 999,999,999 |')+tran(tot-totd  ,'@e 999,999,999 |')
      ?'\'+REPLACE('=',122)+'/'
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
Else
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
ENDIF
RETURN 0

* EOF LPJ2
