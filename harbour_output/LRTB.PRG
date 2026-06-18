// ============================================================
// File    : LRTB.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAP. RETUR PEMBELIAN/PENJUALAN
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
SELECT 3
Uselan('RETURBN.DTA','RETURBN1.NDX','RETURBN2.NDX','RETURBN3.NDX','','')
SELECT 4
*
SELECT 5
*Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','','','')

Ksp=SPACE(5)
Nklb='-'
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafspl
   Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
   @1,0 CLEAR to 24,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC-Selesai  F1-Daftar Supplier'
   Read
   Set key 28 to
   @24,0
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
   ENDIF
   STORE 0 to no,tot,tot2
   SELECT 3
   Set filter to
   If ksp#SPACE(5)
      Set filter to kode_spl=ksp
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,no,n,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-RETUR&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplrtb
   SET CENTURY OFF
   Fkt =faktur
   Nsls=nama_spl
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Do case
         Case jumlah<0
            Jml=tran(jumlah,'@e  99,999 ')
         Case jumlah=0
            Jml=tran(jumlah,'@e  99,999 ')
         Case jumlah>0 .and. jumlah<1
            Jml=tran(jumlah,'@e 9999.99 ')
         Case jumlah>=1
            If mod(jumlah,int(jumlah))>0
               Jml=tran(jumlah,'@e 9999.99 ')
            Else
               Jml=tran(jumlah,'@e  99,999 ')
            ENDIF
         ENDCASE
         Tot =tot +(jumlah*harga)
         Tot2=tot2+(jumlah*harga)
         ?dtoc(tanggal)+' '+left(kode_brg,7)+' '+left(nama_brg,28)+' '+darilokasi+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 99,999,999')
         Skip
         If faktur#fkt
            ?SPACE(68)+REPLACE(gl,12)
            ?SPACE(18)+nsls+'  No.Bukti: '+fkt+'   Jumlah Rp.='+tran(tot2,'@e 999,999,999')
            ?REPLACE(gl,80)
            Fkt =faktur
            Nsls=nama_spl
            Tot2=0
            No  =no+1
            If no>=20
               No =0
               Cf =cf+1
               Cfx=LTRIM(str(cf))
               Arg1='L-RETUR&cfx'+'.RTF'
               Set alte to &arg1
               Do koplrtb
            ENDIF
         ENDIF
   ENDDO
   ?REPLACE(gl,80)
   ?' TOTAL=Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   @24,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-RETUR&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       ENDIF
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hrtb
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      SET CENTURY ON
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @24,65 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
         Ul()
         COMMIT
      ENDIF
      Loop
   Elseif lastkey()=27
      Loop
   ENDIF
   Do cp
   If cc=0
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot
   Do koplrtb1
   Go nr
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jhg=(jumlah*harga)
         Tot=tot+jhg
         ?' '+dtoc(tanggal)+' '+faktur+' '+left(nama_spl,15)+'  '+left(kode_brg,7)+' '+nama_brg+tran(jumlah,'@e  999,999 ')+satuan+tran(harga,'@e 9999,999 ')+tran(jhg,'@e 999,999,999')
         No=no+1
         If no>55
            ?REPLACE('-',106)
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
            Do koplrtb1
         ENDIF
      Skip
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?REPLACE('-',106)
   ?' Total Retur Pembelian ='+SPACE(68)+'Rp'+tran(tot,'@e 999,999,999')
   ?REPLACE('=',106)
   ?''
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplrtb
?' Spl: '+RTRIM(nklb)+'-'+RTRIM(ksp)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?REPLACE(gd,8)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,27)+chr(209)+REPLACE(gd,12)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)
?'Tanggal '+tl+'Kode   '+tl+'Nama Barang'+SPACE(16)+tl+'   Jml.Brg. '+tl+'   @Harga'+tl+'     Jumlah'
?REPLACE(gl,8)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,27)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)
RETURN

PROCEDURE koplrtb1
SET PRINTER ON
SET DEVICE TO PRINTER
?chr(27)+chr(33)+chr(5)
Set marg to 20
?' LAPORAN RETUR PEMBELIAN'
If ksp#SPACE(5)
   ?' Supl.: '+RTRIM(nklb)+' - '+ksp
ENDIF
If tgl=tgl2
   ?' Tgl..: '+dtoc(tgl)+SPACE(82)+'Hal:'+tran(hal,'999')
Else
   ?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+SPACE(66)+'Hal:'+tran(hal,'999')
ENDIF
?REPLACE('=',106)
?' Tgl.     No.   Nama Supplier    Kode    Nama Barang                        Jml.Brg.   @Harga   Rp.Jumlah'
?REPLACE('-',106)
No=1
RETURN

PROCEDURE hrtb
@1,0 CLEAR to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai   F8=Hapus'
Do pesan with katax
DECLARE field[11],kop[11],frm[11],bts[11]
Field[1]='TANGGAL'
Field[2]='FAKTUR'
Field[3]='KODE_SPL'
Field[4]='left(NAMA_SPL,10)'
Field[5]='left(NAMA_BRG,30)'
Field[6]='JUMLAH'
Field[7]='HARGA'
Field[8]='DARILOKASI'
Field[9]='KODE_BRG'
Field[10]='KODE_KLB'
Kop[1]='Tanggal'
Kop[2]='No.'
Kop[3]='Spl.'
Kop[4]='Nama Supp.'
Kop[5]='Nama Barang'
Kop[6]='  Qty.'
Kop[7]='   @Harga'
Kop[8]='Lks'
Kop[9]='Kode Brg.'
Kop[10]='Klp.'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 9999.99'
Frm[7]='@e 9,999,999'
Frm[8]='@!'
Frm[9]='@!'
Frm[10]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Bts[7]=tl
Bts[8]=tl
Bts[9]=tl
Bts[10]=tl
Go nr
DbEdit(2,0,21,79,field,'fhrtb',frm,kop,gl,bts)
*
SET CURSOR ON
RETURN

Function fhrtb
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      If tanggal<tgl .or. tanggal>tgl2
         Do case
            Case lastkey()=18  &&&-PgUp
               Tone(700,2)
               Skip 18
            Case lastkey()=3   &&&-PgDn
               Tone(700,2)
               Skip -18
            Case lastkey()=5   &&&-Up
               Tone(700,2)
               Skip 1
            Case lastkey()=24   &&&-Dn
               Tone(700,2)
               Skip -1
         ENDCASE
      ENDIF
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-7
      SET COLOR TO gr+*
      @row(),col() say &fds
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      SET CURSOR ON
      Ht=' '
      DO WHILE .not. ht$'YT'
         @24,47 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         Kdb=kode_bar
         Jml=jumlah
         Lks=darilokasi
         If .not. dele()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
            *
            SELECT 2
            Seek kdb
            Rlok()
            If lks='T'
               REPLACE stok_k with (stok_k-jml)
            Else
               REPLACE stok_kg with (stok_kg-jml)
            ENDIF
            Ul()
            COMMIT

            SELECT 3
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* LAP.RETUR PEMB/ITEM
*---------------------
PROCEDURE lrtb2
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK4.NDX','','','','')

STORE ' ' to jlap,ket
STORE tglx to tgl,tgl2
Nmb=SPACE(30)
DO WHILE .t.
   SELECT 2
   Set filter to
   @1,0 CLEAR to 24,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Read
   If lastkey()=27
      Exit
   ENDIF
   If nmb=SPACE(30)
      Loop
   ENDIF
   Seek nmb
   If .not. FOUND()
      Seek RTRIM(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
      ENDIF
   ENDIF
   Klb=kode_klb
   Kdb=kode_brg
   Nmb=nama_brg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @2,34 say nmb
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   SELECT 1
   Uselan('RETURBN.DTA','RETURBN2.NDX','','','','')

   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-RETUR.RTF
   ?' LAPORAN RETUR PEMBELIAN PER ITEM BARANG'
   ?' NAMA BRG: '+RTRIM(nmb)
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,10)+chr(209)+REPLACE(gd,20)+chr(209)+REPLACE(gd,10)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)+chr(184)
   ?tl+' No.'+tl+'Tanggal   '+tl+'Nama Supplier       '+tl+'No.Faktur '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,20)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
   Y=1
   STORE 0 to stm,hp,hj,jml,tot,tot2
   Z=LTRIM(str(y))
   DO WHILE .t.
      Seek klb+kdb
      St =satuan
      DO WHILE .not. eof() .and. ( kode_klb=klb .and. kode_brg=kdb)
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=(jumlah*harga)
            Stm=stm+jumlah
            Tot=tot + jhg
            Jumlah_rtr=0
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+tl+left(nama_spl,20)+tl+faktur+'   '+darilokasi+' '+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y = y + 1
         ENDIF
         Skip
      ENDDO
      Exit
   ENDDO
   Nmb =SPACE(30)
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999 ')+st
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,20)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
   ?SPACE(17)+'TOTAL ='+SPACE(24)+pesanx+'  Rp'+tran(tot, '@e 9,999,999,999')
   Set alte off
   Set alte to
   @24,0
   Memoedit(Memoread('L-RETUR.RTF'),1,0,22,240,.t.,'',240)
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
   ?chr(27)+chr(33)+chr(4)
   Type L-RETUR.RTF to prin
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* LAP.RETUR PENJUALAN/TGL
*-------------------------
PROCEDURE lrtj
SELECT 1
*Uselan('KASIR.DTA','','','','','')
SELECT 2
Uselan('STOK.DTA','STOK.NDX','','','','')
SELECT 3
Uselan('RETURJN.DTA','RETURJN1.NDX','RETURJN2.NDX','','','')
STORE SPACE(5) to sls,fkt
STORE tglx to tgl,tgl2
DO WHILE .t.
   SET CENTURY ON
   @1,0 CLEAR to 24,79
   @2,24 say 'Tanggal'get tgl
   @2,43 say 'sampai'get tgl2
   Do pesan with 'ESC-Selesai'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   STORE 0 to b,no,tot,tot2,jhg,jml
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF

   SELECT 3
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jhg =jhg +(jumlah*harga_2)
      Tot =tot +(jumlah*harga_2)
      Tot2=tot2+(jumlah*harga)
      Skip
   ENDDO
   Go nr
   SET CENTURY OFF
   Do koplrtj
   Do tampilrtj
   Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hrtj
      Loop
   Elseif lastkey()=27
      Loop
   ENDIF
   Do cp
   If cc=0
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot
   Do koplrtj1
   Go nr
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jml=tran(jumlah,'@e 99,999')
      If jumlah#0
         If (jumlah>0 .and. jumlah<1) .or. (jumlah<0 .and. jumlah>-1)
            Jml=tran(jumlah,'@e 999.99')
         Elseif mod(jumlah,int(jumlah))>0
            Jml=tran(jumlah,'@e 999.99')
         ENDIF
      ENDIF
      Jhg=(jumlah*harga_2)
      Tot=tot+jhg
      ?' '+dtoc(tanggal)+' '+left(nama_ksr,10)+'  '+dtoc(tanggal_3)+' '+left(nama_ksrjl,10)+'  '+faktur+'   '+left(kode_brg,7)+'  '+nama_brg+jml+satuan+tran(harga_2,'@e  9,999,999 ')+tran(jhg,'@e 99,999,999')
      No=no+1
      If no>55
         ?REPLACE('-',124)
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
         Do koplrtj1
      ENDIF
      Skip
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?REPLACE('-',124)
   ?' Total Retur Penjualan ='+SPACE(86)+'Rp'+tran(tot,'@e 999,999,999')
   ?REPLACE('=',124)
   ?''
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplrtj
@1,0 say 'Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
@2,0 say REPLACE(gd,20)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,23)+chr(209)+REPLACE(gd,6)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,10)
@3,0 say 'Tanggal  Nota-Nm.Ksr'+tl+'Kode   '+tl+'Nama Barang'+SPACE(12)+tl+'  Qty.'+tl+'   @Harga'+tl+'Jumlah Rp.'
@4,0 say REPLACE(gl,20)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,23)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,10)
B=5
RETURN

PROCEDURE koplrtj1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 5
?chr(27)+chr(33)+chr(5)
?' LAPORAN RETUR PENJUALAN'
If tgl=tgl2
   ?' TANGAL: '+dtoc(tgl)+SPACE(99)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl)+' S/D '+dtoc(tgl2)+SPACE(85)+'Hal:'+tran(hal,'999')
ENDIF
?REPLACE('=',124)
?' Tanggal/Ksr.Retur    Tanggal/Ksr.Jual     Nota    Kode     Nama Barang                      Jml.Brg.     @Harga  Jumlah Rp.'
?REPLACE('-',124)
No=1
RETURN

PROCEDURE tampilrtj
DO WHILE .t.
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jml=tran(jumlah,'@e 99,999')
      If jumlah#0
         If (jumlah>0 .and. jumlah<1) .or. (jumlah<0 .and. jumlah>-1)
            Jml=tran(jumlah,'@e 999.99')
         Elseif mod(jumlah,int(jumlah))>0
            Jml=tran(jumlah,'@e 999.99')
         ENDIF
      ENDIF
      @b,0 say dtoc(tanggal)+' '+right(faktur,3)+'-'+left(nama_ksr,7)+' '+left(kode_brg,7)+' '+left(nama_brg,24)+jml+tran(harga_2,'@e 9,999,999 ')+tran(jumlah*harga_2,'@e 99,999,999')
      B=b+1
      No=no+1
      Skip
      If b>19
         @b,0 say REPLACE(gl,80)
         @b+1,0 CLEAR to 24,79
         @b+1,0 say ' TOTAL JUAL=Rp'+tran(tot,'@e 9,999,999,999')
         @b+2,0 say '      POKOK=Rp'+tran(tot2,'@e 9,999,999,999   (')+LTRIM(str(tot-tot2))+')'
         Skip -no
         Do geser
         If lastkey()=27 .or. lastkey()=-9
            Exit
         ENDIF
         No=0
         B =5
      ENDIF
   ENDDO
   @b,0 say REPLACE(gl,80)
   @b+1,0 CLEAR to 24,79
   @b+1,0 say ' TOTAL JUAL=Rp'+tran(tot,'@e 9,999,999,999')
   @b+2,0 say '      POKOK=Rp'+tran(tot2,'@e 9,999,999,999   (')+LTRIM(str(tot-tot2))+')'
   If lastkey()=27
      Exit
   ENDIF         
   Skip -no
   Do geser
   Do case
      Case lastkey()=27 .or. lastkey()=-9
         Exit
      Case lastkey()=3
         Tone(100,2)
         Skip -no
   ENDCASE
   No=0
   B =5
ENDDO
RETURN

PROCEDURE hrtj
@1,0 CLEAR to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai   F8=Hapus'
Do pesan with katax
DECLARE field[11],kop[11],frm[11],bts[11]
Field[1]='TANGGAL'
Field[2]='FAKTUR'
Field[3]='left(NAMA_KSR,10)'
Field[4]='left(NAMA_KSRJL,10)'
Field[5]='left(NAMA_BRG,30)'
Field[6]='JUMLAH'
Field[7]='HARGA_2'
Field[8]='HARGA'
Field[9]='KODE_BRG'
Field[10]='KODE_KLB'
Kop[1]='Tanggal'
Kop[2]='Nota'
Kop[3]='Ksr.Retur'
Kop[4]='Ksr.Jual'
Kop[5]='Nama Barang'
Kop[6]='  Qty.'
Kop[7]='   @Harga'
Kop[8]='   @Pokok'
Kop[9]='Kode Brg.'
Kop[10]='Klp.'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 9999.99'
Frm[7]='@e 9,999,999'
Frm[8]='@e 9,999,999'
Frm[9]='@!'
Frm[10]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Bts[7]=tl
Bts[8]=tl
Bts[9]=tl
Bts[10]=tl
Go nr
DbEdit(2,0,21,79,field,'fhrtj',frm,kop,gl,bts)
*
SET CURSOR ON
RETURN

Function fhrtj
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      If tanggal<tgl .or. tanggal>tgl2
         Do case
            Case lastkey()=18  &&&-PgUp
               Tone(700,2)
               Skip 18
            Case lastkey()=3   &&&-PgDn
               Tone(700,2)
               Skip -18
            Case lastkey()=5   &&&-Up
               Tone(700,2)
               Skip 1
            Case lastkey()=24   &&&-Dn
               Tone(700,2)
               Skip -1
         ENDCASE
      ENDIF
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-7
      SET COLOR TO gr+*
      @row(),col() say &fds
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      SET CURSOR ON
      Ht=' '
      DO WHILE .not. ht$'YT'
         @24,47 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         Kdb=kode_brg
         Jml=jumlah
         If .not. dele()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip

            SELECT 2
            Seek kdb
            Rlok()
            REPLACE stok_k with (stok_k-jml)
            Ul()
            COMMIT

            SELECT 3
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* LAP.RETUR PENJ/ITEM
*---------------------
PROCEDURE lrtj2
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK4.NDX','','','','')

STORE ' ' to jlap,ket
STORE tglx to tgl,tgl2
Nmb=SPACE(30)
DO WHILE .t.
   SELECT 2
   Set filter to
   @1,0 CLEAR to 24,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Read
   If lastkey()=27
      Exit
   ENDIF
   If nmb=SPACE(30)
      Loop
   ENDIF
   Seek nmb
   If .not. FOUND()
      Seek RTRIM(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
      ENDIF
   ENDIF
   Klb=kode_klb
   Kdb=kode_brg
   Nmb=nama_brg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @2,34 say nmb
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   SELECT 1
   Uselan('RETURJN.DTA','RETURJN2.NDX','','','','')

   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-RETUR.RTF
   ?' LAPORAN RETUR PENJUALAN PER ITEM BARANG'
   ?' NAMA BRG: '+RTRIM(nmb)
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,10)+chr(209)+REPLACE(gd,26)+chr(209)+REPLACE(gd,5)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)+chr(184)
   ?tl+' No.'+tl+'Tanggal   '+tl+'Nama Pelanggan      /Kode '+tl+'Nota '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,26)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
   Y=1
   STORE 0 to stm,hp,hj,jml,tot,tot2
   Z=LTRIM(str(y))
   DO WHILE .t.
      Seek klb+kdb
      St =satuan
      DO WHILE .not. eof() .and. ( kode_klb=klb .and. kode_brg=kdb)
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=(jumlah*harga)
            Stm=stm+jumlah
            Tot=tot + jhg
            Jumlah_rtr=0
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+tl+left(nama_lgn,20)+' '+kode_lgn+tl+faktur+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y = y + 1
         ENDIF
         Skip
      ENDDO
      Exit
   ENDDO
   Nmb =SPACE(30)
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999 ')+st
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,26)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
   ?SPACE(17)+'TOTAL ='+SPACE(25)+pesanx+'  Rp'+tran(tot, '@e 9,999,999,999')
   Set alte off
   Set alte to
   @24,0
   Memoedit(Memoread('L-RETUR.RTF'),1,0,22,240,.t.,'',240)
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
   ?chr(27)+chr(33)+chr(4)
   Type L-RETUR.RTF to prin
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* EOF LRTB
