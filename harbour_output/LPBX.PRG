// ============================================================
// File    : LPBX.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN PEMBELIAN/TGL
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
SELECT 3
Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
SELECT 4
*
SELECT 5
*

Ksp=SPACE(5)
Nklb='-'
STORE ' ' to hri,bln,thn
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafspl
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai   F1=Daftar Supplier'
   Read
   Set key 28 to
   @23,0
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
   Do konvtgl
   SELECT 3
   Set filter to
   If ksp#SPACE(5)
      Set filter to kode_spl=ksp
   ENDIF
   Seek (thn+bln+hri)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-BELI&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpb
   SET CENTURY OFF
   Fkt =faktur
   Nsls=''
   Nsp =nama_spl
   Lks =lokasi
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      If jumlah<0
            Jml=tran(jumlah,'@e  99,999 ')
      Elseif jumlah=0
            Jml=tran(jumlah,'@e  99,999 ')
      Elseif jumlah>0 .and. jumlah<1
            Jml=tran(jumlah,'@e 9999.99 ')
      Elseif jumlah>=1
            If mod(jumlah,int(jumlah))>0
               Jml=tran(jumlah,'@e 9999.99 ')
            Else
               Jml=tran(jumlah,'@e  99,999 ')
            ENDIF
      ENDIF
      Tot =tot +&brs12
      Tot2=tot2+&brs12
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+' '+left(nama_brg,27)+jml+satuan+tran(harga,'@e  9,999,999 ')+tran(&brs12,'@e 999,999,999')
      Skip
      If faktur#fkt .or. nama_spl#nsp
         ?SPACE(68)+REPLACE(gl,12)
         ?SPACE(10)+'Supplier: '+left(nsp,20)+' No.Fkt:'+fkt+' Jumlah Rp='+tran(tot2,'@e 999,999,999')
         ?REPLACE(gl,80)
         Fkt =faktur
         Nsp =nama_spl
         Lks =lokasi
         Tot2=0
         No  =no+1
         If no>=10
            No =0
            Hal=hal+1
            Cf =cf +1
            Cfx=LTRIM(str(cf))
            Arg1='L-BELI&cfx'+'.RTF'
            Set alte to &arg1
            Do koplpb
         ENDIF
      ENDIF
   ENDDO
   If xOwner
      ?SPACE(61)+'TOTAL=Rp'+tran(tot,'@e 999,999,999')
   ENDIF
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-BELI&cfx'+'.RTF'
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
   If lastkey()=-7 .and. xOwner
      Do hpb
      Loop
   Elseif lastkey()=-3 .and. xOwner
      SET CENTURY ON
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @23,66 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
         Ul()
         COMMIT
         Do pesan with 'Faktur Pembelian tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
         Yt=' '
         DO WHILE .not. yt$'YT'
            @23,70 get yt pict '!'
            Read
         ENDDO
         If yt='Y'
            SELECT 5
            Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','','','')
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
            Ul()
            COMMIT
            Close
         ENDIF
      ENDIF
      Loop
   Elseif lastkey()=27
      Loop
   ENDIF
   If xOwner
      Do cp
      If cc=0
         Loop
      ENDIF
   Else
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot,tot2
   Do koplpb1
   Go nr
   SET CENTURY OFF
   Fkt =faktur
   Nsls=nama_spl
   Lks =lokasi
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jml1=jumlah
      Jml2=0
      Isi =isine_b
      If     jml1<=isi
         Jml=tran(jml1*isi,'@e      / 9999 ')
      Elseif jml1>isi
         Jml=tran(jml1    ,'@e 9999 /      ')
      Else
         Jml=tran(jml1*isi,'@e      / 9999 ')
      ENDIF
      Jhg =((jumlah*harga)-((jumlah*harga)*(diskon/100)))
      Tot =tot +jhg
      Tot2=tot2+jhg
      ?' '+dtoc(tanggal)+'  '+left(kode_bar,13)+'  '+nama_brg+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(diskon,'@e 99,99 ')+tran(jhg,'@e 999,999,999')
      B =b+1
      No=no+1
      Skip
      If faktur#fkt .or. nama_spl#nsls
         ?SPACE(88)+REPLACE('-',12)
         ?SPACE(11)+'   Supp: '+nsls+'    No.Fkt: '+fkt+'     Lks: '+lks+'     Jumlah Rp.'+tran(tot2,'@e 9999,999,999')
         ?REPLACE('-',101)
         B =b+3
         No=no+3
         Fkt =faktur
         Nsls=nama_spl
         Lks =lokasi
         Tot2=0
      ENDIF
      If no>55
         ?REPLACE('-',101)
         ?SPACE(89)+'bersambung...'
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
         No =1
         Hal=hal+1
         Do koplpb1
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?' TOTAL PEMBELIAN'+SPACE(69)+'Rp.'+tran(tot,'@e 9999,999,999')
   ?REPLACE('=',101)
   ?' '
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplpb
?' Spl: '+RTRIM(nklb)+'-'+RTRIM(ksp)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?REPLACE(gd,9)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,27)+chr(209)+REPLACE(gd,12)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)
?'Tanggal  '+tl+'Kode   '+tl+'Nama Barang'+SPACE(16)+tl+'   Jml.Brg. '+tl+'   @Harga'+tl+'     Jumlah'
?REPLACE(gl,9)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,27)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)
RETURN

PROCEDURE koplpb1
SET PRINTER ON
SET DEVICE TO PRINTER
?chr(27)+chr(33)+chr(4)
Set marg to 10
?' LAPORAN PEMBELIAN'
If ksp#SPACE(5)
   ?' Supl.: '+RTRIM(nklb)+' - '+ksp
ENDIF
If tgl=tgl2
   ?' Tgl..: '+dtoc(tgl)+SPACE(76)+'Hal:'+tran(hal,'999')
Else
   ?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+SPACE(58)+'Hal:'+tran(hal,'999')
ENDIF
?REPLACE('=',101)
?' Tanggal   Barcode        Nama Barang                        Jml.Brg.       @Harga  Dis%   Rp.Jumlah'
?REPLACE('-',101)
No=1
RETURN

PROCEDURE hpb
@1,0 CLEAR to 23,79
@1,0 say REPLACE(gd,80)
@22,0 to 22,79
Katax='ESC=Selesai   F5=Edit   F8=Hapus'
Do pesan with katax
DECLARE field[10],kop[10],frm[10],bts[10]
Field[1]='TANGGAL'
Field[2]='JAM'
Field[3]='left(FAKTUR,7)'
Field[4]='left(NAMA_SPL,18)'
Field[5]='NAMA_BRG'
Field[6]='JUMLAH'
Field[7]='SATUAN'
Field[8]='HARGA'
Field[9]='KODE_BRG'
Field[10]='LOKASI'
Kop[1]='Tanggal'
Kop[2]='Jam'
Kop[3]='No.Fkt'
Kop[4]='Nama Supplier'
Kop[5]='Nama Barang'
Kop[6]='Qty.'
Kop[7]='St.'
Kop[8]=' @Hg.Beli'
Kop[9]='Kode Brg.'
Kop[10]='Lks'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 9999.99'
Frm[7]='@!'
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
DbEdit(2,0,21,79,field,'fhpbx',frm,kop,gl,bts)
*
SET CURSOR ON
RETURN

Function fhpbx
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
         @23,47 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         Tgl=tanggal
         Ksp=kode_spl
         Fkt=faktur
         Kdb=kode_bar
         Jml=jumlah
         Hp =harga
         Lks=lokasi
         Isi=isine_b
         Do konvtgl
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
            If hut
               If lks='G'
                  REPLACE stok_mg with (stok_mg-(jml*isi))
               Else
                  REPLACE stok_m with (stok_m-(jml*isi))
               ENDIF
            Elseif hut=.f.
               REPLACE stok_m with (stok_m+(jml*isi))
            ENDIF
            Ul()
            COMMIT
            *
            If hut .and. ksp#'TUNAI'
               SELECT 5
               Uselan('FAKTURH.DTA','FAKTURH1.NDX','','','','')
               Seek (thn+bln+hri)+ksp+fkt
               Rlok()
               REPLACE rp_total with rp_total-(jml*hp)
               Ul()
               COMMIT
               Close
            ENDIF
            *
            SELECT 3
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-4 .and. fds='JUMLAH' .or. fds='HARGA'
      Kdb=kode_bar
      Hp =harga
      Lks=lokasi
      Isi=1
      STORE jumlah to jml,jml2
      Do konvtgl
      SET CURSOR ON
      SET COLOR TO gr+*
      If fds='JUMLAH'
         @row(),col() get jml pict '@e 9999.99'
      Else
         @row(),col() get hp pict '@e 9,999,999'
      ENDIF
      Read
      *
      If lastkey()#27
         Rlok()
         REPLACE jumlah with jml,harga with hp
         Ul()
         COMMIT
         *
         SELECT 2
         Seek kdb
         Rlok()
         If hut
            If lks='G'
               REPLACE stok_mg with (stok_mg-(jml2*isi))+(jml*isi)
            Else
               REPLACE stok_m with (stok_m-(jml2*isi))+(jml*isi)
            ENDIF
            REPLACE harga with hp,harga_b with hp*isine_b
         Elseif hut=.f.
            REPLACE stok_m with (stok_m+(jml*isi))
         ENDIF
         Ul()
         COMMIT


         SELECT 3
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* LAPORAN PEMBELIAN/ITEM
*-----------------------
PROCEDURE lpb2
SELECT 1
*
SELECT 2
Uselan('STOK.DTA','STOK4.NDX','','','','')

STORE ' ' to jlap,ket
STORE tglx to tgl,tgl2
Nmb=SPACE(30)
DO WHILE .t.
   SELECT 2
   SET CENTURY ON
   Set filter to
   @1,0 CLEAR to 23,79
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
   Kdb=kode_bar
   Nmb=nama_brg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @2,34 say nmb
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @23,0
   If lastkey()=27
      Loop
   ENDIF
   SELECT 1
   Uselan('BELIIN.DTA','BELIIN2.NDX','','','','')

   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   SET CENTURY OFF
   Set alte on
   Set alte to L-BELI.RTF
   ?' LAPORAN PEMBELIAN PER ITEM BARANG'
   ?' NAMA BRG: '+RTRIM(nmb)+'   KODE: '+kdb
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,14)+chr(209)+REPLACE(gd,17)+chr(209)+REPLACE(gd,10)+chr(209)+REPLACE(gd,7)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)+chr(184)
   ?tl+' No.'+tl+'Tanggal       '+tl+'Nama Supplier    '+tl+'No.Faktur '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,14)+chr(197)+REPLACE(gl,17)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
   Y=1
   STORE 0 to stm,hp,hj,jml,tot,tot2
   Z=LTRIM(str(y))
   DO WHILE .t.
      Seek klb+kdb
      St =satuan
      DO WHILE .not. eof() .and. ( kode_klb=klb .and. kode_bar=kdb)
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=(jumlah*harga)
            Stm=stm+jumlah
            Tot=tot + jhg
            Jumlah_rtr=0
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+left(nama_spl,17)+tl+left(faktur,10)+tl+left(jml,6)+iif(lokasi='G',lokasi,' ')+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y = y + 1
         ENDIF
         Skip
      ENDDO
      Exit
   ENDDO
   Nmb =SPACE(30)
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999 ')+st
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,14)+chr(193)+REPLACE(gl,17)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
   ?SPACE(18)+'TOTAL ='+SPACE(24)+pesanx+'  Rp'+tran(tot, '@e 9,999,999,999')
   Set alte off
   Set alte to
   @23,0
   Memoedit(Memoread('L-BELI.RTF'),1,0,22,240,.t.,'',240)
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
   ?chr(27)+chr(33)+chr(4)
   Type L-BELI.RTF to prin
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,jlap,ket,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* EOF LPB1
