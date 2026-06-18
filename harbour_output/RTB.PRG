// ============================================================
// File    : RTB.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* RETUR PEMBELIAN / PENJUALAN
STORE tglx to tgl,tgl2
Tgl2=ctod('  -  -    ')
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
*
SELECT 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
SELECT 4
Uselan('RETURBN.DTA','RETURBN1.NDX','RETURBN2.NDX','RETURBN3.NDX','','')
SELECT 5
Useidx('RETURB.DTA','','','','','')
REPLACE all faktur with SPACE(5),id_data with ' '
*
SELECT 6
*
SELECT 7
Uselan('FAKTURH.DTA','FAKTURH3.NDX','','','','')
SELECT 8
Uselan('DATA.SET','','','','','')
Nn=no_returb+1
If nn>99999
   Nn=1
ENDIF
Do xnota
SELECT 9
*
Brs4 ="left(nmb,30)+tl+kdbx+tl+tran(jml,'@e 9999.99')+tl+left(st,4)+tl+tran(hp,'@e 9,999,999')+tl+tran(jhg,'@e 99,999,999')"
Brs42="left(nama_brg,30)+tl+kode_bar+tl+tran(jumlah,'@e 9999.99')+tl+left(satuan,4)+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 99,999,999')"
Fkt2 =SPACE(10)
STORE SPACE(5) to ksp,klb
STORE 0 to jml,jhg,tot,tot2,dis4
STORE .f. to hapus,isilagi,batal,ada,diedit
Lks='T'
DO WHILE .t.
   SELECT 1
   Set key 28 to dafspl
   @1,0 CLEAR to 24,79
   @1,0 say chr(218)+REPLACE(gl,44)+chr(194)+REPLACE(gl,33)+chr(191)
   @2,0 say tl+'Supplier:'+SPACE(35)+tl+'Untuk Potong Tagihan'+SPACE(13)+tl
   @3,0 say tl+'Tanggal.:'+SPACE(35)+tl+'No.Nota:'+SPACE(25)+tl
   @4,0 say tl+'No.Bukti:'+SPACE(35)+tl+'Jumlah.: Rp'+SPACE(22)+tl
   @5,0 say chr(192)+REPLACE(gl,44)+chr(193)+REPLACE(gl,33)+chr(217)
   @2,11 get ksp pict '@!'
   Do pesan with 'ESC=Selesai     F1=Daftar_Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   Nsp='-'
   If ksp=SPACE(5)
      STORE 'TUNAI' to ksp,nsp,algn
   Elseif ksp#SPACE(5)
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
      Nsp =left(nama_spl,20)
      Algn=almt_spl
   ENDIF
   @2,17 say left(nsp,27)
   @3,11 get tgl
   @4,11 get fkt pict '@!'
   @3,55 get fkt2 pict '@!'
   Read
   If lastkey()=27
      Loop
   ENDIF
   *If mont(tgl)#mont(tglx)
   *   Do pesan2 with 'Bul()an Tidak Bisa diGanti!   Ganti Tanggal?  ===>Menu Utility ->Tanggal'
   *   Loop
   *Endi
   N=1
   *
   If fkt2#SPACE(10)
      SELECT 7
      Seek ksp+fkt2
      @4,58 say LTRIM(tran(((rp_total-rp_diskon)-rp_bayar)-rp_retur,'@e 999,999,999'))
   ENDIF
   *
   SELECT 4
   Set order to 1
   Seek dtos(tgl)+ksp+fkt
   If FOUND()
      STORE .t. to isilagi,ada
      DO WHILE (.not. eof()) .and. faktur=fkt
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Jml =jumlah
         Hp  =harga
         Lks =darilokasi
         Nrx =recno()
         *
         SELECT 5
         If n=1
            Go top
         ENDIF
         REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,darilokasi with lks,nomorekod with nrx,id_data with '1'
         Skip
         N=n+1
         *
         SELECT 4
         Skip
      ENDDO
   ENDIF
   SELECT 3
   *Set filter to kode_spl=ksp
   *
   @5,0 say chr(195)+REPLACE(gl,30)+chr(194)+REPLACE(gl,13)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,4)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,10)+chr(180)
   @6,0 say tl+'Nama Barang'+SPACE(19)+tl+'BarCode      '+tl+'   Qty.'+tl+'Sat.'+tl+'   @Harga'+tl+'Jumlah Rp.'+tl
   @7,0 say chr(195)+REPLACE(gl,30)+chr(197)+REPLACE(gl,13)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,4)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,10)+chr(180)
   B=8
   Kolomrtb()
   Disi=.f.
   DO WHILE .t.
      Yt=' '
      B=8
      Kdb=SPACE(13)
      Nmb=SPACE(30)
      If isilagi
         Do hpsrtr
         Isilagi=.f.
         Jml=0
      ENDIF
      DO WHILE .t.
         SELECT 3
         Set key 5 to dataht
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         @b,0 say tl+SPACE(30)+tl+SPACE(13)+tl+SPACE(7)+tl+SPACE(4)+tl+SPACE(9)+tl+SPACE(10)+tl
         @b,1 get nmb pict '@!S30'
         Do pesan with 'Masukan Nama Barang, atau Kosongkan untuk BarCode'
         Read
         Set key 5 to
         If hapus
            Do hpsrtr
            Loop
         ENDIF
         If lastkey()=27
            Do pesan with 'Transaksi Batal? Y/T'
            Yt=' '
            DO WHILE .not. yt$'YT'
               @24,51 get yt pict '!'
               Read
            ENDDO
            If yt='Y'
               Batal=.t.
               Exit
            ENDIF
            Loop
         ENDIF
         If nmb=SPACE(30)
            Set order to 1
            @b,32 get kdb pict '@!S13'
            Do pesan with 'Masukan Barcode, atau Kosongkan untuk Selesai'
            Read
            If kdb=SPACE(13)
               Exit
            ENDIF
            Carikode=.t.
            Seek kdb
            If .not. FOUND()
               Do dafbrg
               If lastkey()=27
                  Loop
               ENDIF
            ENDIF
         Else
            Carikode=.f.
            Set order to 2
            Seek nmb
            If .not. FOUND()
               Seek RTRIM(nmb)
               Do dafbrg
               If lastkey()=27
                  Loop
               ENDIF
            ENDIF
         ENDIF
         Set order to 1
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Hp  =harga
         Stk =((stok_a+stok_m)-stok_k)
         Kdbx=kode_bar
         Do pesan with 'L (lokasi) = G/T    Sisa:'+tran(stk,'@e 999,999.99')+'    Tgl.Masuk:'+dtoc(tanggal_1)
         @b,1  say &brs4
         @b,54 say left(st,4)
         @b,46 get jml pict '@e 9999.99'
         @b,59 get hp  pict '@e 9,999,999'
         Read
         If lastkey()=27
            Loop
         ENDIF
         Jhg =(jml*hp)
         If hp=0
            @b,69 get jhg pict '@e 99,999,999'
            Read
            If jhg>0 .and. jml>0
               Hp=jhg/jml
            ENDIF
         ENDIF
         @b,69 say jhg pict '@e 99,999,999'
         *
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If lastkey()=27
            Loop
         ENDIF
         Tot=tot + jhg
         Lks=iif(lks#'T','G','T')
         @b,1 say &brs4
         *
         SELECT 5
         If n=1
            Go top
         ENDIF
         REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,darilokasi with lks,id_data with ' '
         Skip
         *
         B=b+1
         N=n+1
         Jml=0
         Jhg=0
         Kdb=SPACE(13)
         Nmb=SPACE(30)
         If b>20
            Scroll(8,1,20,78,1)
            B=20
         ENDIF
      ENDDO
      *
      If .not. batal
         STORE 0 to dis4,byr,kbl,jmlg
         Batal=.f.
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         *
         Do pesan with 'Simpan Data? Y/T: '
         Yt=' '
         @24,49 get yt pict '!'
         Read
         If lastkey()=27 .or. yt#'Y'
            Isilagi=.t.
            Loop
         ENDIF
         *
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         SELECT 5
         Go top
         DO WHILE .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
            Klb =kode_klb
            Kdb =kode_brg
            Kdbx=kode_bar
            Nmb =nama_brg
            St  =satuan
            Jml =jumlah
            Hp  =harga
            Lks =darilokasi
            Idt =id_data
            Isi =1
            *
            If idt=' '
               If jml#0
                  SELECT 4
                  Flok()
                  APPEND BLANK
                  REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,faktur_bl with fkt2,;
                       kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
                       nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,darilokasi with lks
                  Ul()
                  COMMIT
                  *
                  SELECT 3
                  Set order to 1
                  Seek kdbx
                  Rlok()
                  REPLACE stok_rb with (stok_rb+(jml*isi)),tanggal_rb with tgl
                  Ul()
               ENDIF
            ENDIF
            *
            SELECT 5
            REPLACE faktur with SPACE(5),id_data with ' '
            Skip
         ENDDO
         *
         If fkt2#SPACE(10)
            SELECT 7
            Rlok()
            REPLACE rp_retur with rp_retur+tot
            Ul()
            COMMIT
         ENDIF
         *
         *
         If .not. ada
            SELECT 8
            Rlok()
            REPLACE no_returb with nn
            Ul()
            Nn=nn+1
            If nn>99999
               Nn=1
            ENDIF
            Do xnota
         Elseif ada
            Nn=nn
            Do xnota
         ENDIF
      Else
         Batal=.f.
         SELECT 5
         REPLACE all faktur with SPACE(5),id_data with ' '
      ENDIF
      STORE 0 to jml,tot
      Kolomrtb()
      Exit
   ENDDO
   Ksp =SPACE(5)
   Fkt2=SPACE(10)
   Ada =.f.
ENDDO
CLOSE all
RELEASE brs4,brs42,ksp,klb,jml,tot,tot2,dis4,lks,kdb,kdbx,nmb,st,isi,nn,fkt,fkt2
RELEASE hapus,isilagi,batal,ada,diedit
RETURN

*** PROSEDUR HAPUS TRANSAKSI 
PROCEDURE hpsrtr
Kolomrtb()
SELECT 5
Set filter to tanggal=tgl .and. faktur=fkt
Go top
If faktur=SPACE(5)
   Set filter to
   RETURN
ENDIF
DECLARE field[8],bts[8],frm[8]
Field[1]='left(NAMA_BRG,30)'
Field[2]='left(KODE_BAR,13)'
Field[3]='JUMLAH'
Field[4]='left(SATUAN,4)'
Field[5]='HARGA'
Field[6]='JUMLAH*HARGA'
Frm[1]='@!'
Frm[2]='@!'
Frm[3]='@e 9999.99'
Frm[4]='@!'
Frm[5]='@e 9,999,999'
Frm[6]='@e 99,999,999'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Pesany='ENTER=Edit_Qty     ESC=Selesai'
Do pesan with pesany
Keyboard chr(5)
DbEdit(8,1,20,78,field,'fhprt',frm,'','',bts)
*
Kolomrtb()
Tot=0
B=8
Set filter to
Go top
DO WHILE .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
   Jhg=(jumlah*harga)
   Tot=tot+jhg
   @b,1 say &brs42
   B  =b+1
   If b>20
      Scroll(8,1,20,78,1)
      B=20
   ENDIF
   Skip
ENDDO
Nmb=SPACE(30)
Kdb=SPACE(13)
Jml=1
Hapus=.f.
RETURN

Function fhprt
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=13 .and. fds='JUMLAH'
   B  =row()
   Kdb=kode_bar
   Isi=1
   Lks=darilokasi
   Nrx=nomorekod
   STORE jumlah to jml,jml2
   SET CURSOR ON
   Do pesan with ''
   @b,46 get jml pict '@e 9999.99'
   Read
   If lastkey()#27
      REPLACE jumlah with jml
      COMMIT
      *
      If diEdit
         SELECT 3
         Set order to 1
         Seek kdb
         Rlok()
         If lks='T'
            REPLACE stok_k with (stok_k-(jml2*isi))+(jml*isi)
         Else
            REPLACE stok_kg with (stok_kg-(jml2*isi))+(jml*isi)
         ENDIF
         Ul()
         COMMIT
         *
         SELECT 4
         Go nrx
         Rlok()
         REPLACE jumlah with jml
         Ul()
         COMMIT
         SELECT 5
      ENDIF
   ENDIF
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 1
Else
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

***KOLOM RETURBELI
FUNCTION kolomrtb
DO WHILE b<21
   @b,0 say tl+SPACE(30)+tl+SPACE(13)+tl+SPACE(7)+tl+SPACE(4)+tl+SPACE(9)+tl+SPACE(10)+tl
   B =b+1
ENDDO
@b,0 say chr(192)+REPLACE(gl,30)+chr(193)+REPLACE(gl,13)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,4)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,10)+chr(217)
B=8
RETURN(.t.)

*LAP.RETUR PEMB
*--------------
PROCEDURE lrtb
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
SELECT 3
Uselan('RETURBN.DTA','RETURBN1.NDX','RETURBN2.NDX','RETURBN3.NDX','','')
SELECT 4
*
Ksp=SPACE(5)
Nsp='-'
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
?' Spl: '+RTRIM(nsp)+'-'+RTRIM(ksp)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?REPLACE(gl,8)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,28)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,10)
?'Tanggal '+tl+'Kode   '+tl+'Nama Barang'+SPACE(17)+tl+'   Jml.Brg. '+tl+'    @Harga'+tl+' Jumlah_Rp'
?REPLACE(gl,8)+chr(193)+REPLACE(gl,7)+chr(193)+REPLACE(gl,28)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,10)
RETURN

PROCEDURE koplrtb1
SET PRINTER ON
SET DEVICE TO PRINTER
?chr(27)+chr(33)+chr(5)
Set marg to 20
?' LAPORAN RETUR PEMBELIAN'
If ksp#SPACE(5)
   ?' Supl.: '+RTRIM(nsp)+' - '+ksp
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
Field[9]='KODE_BAR'
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
            Case lastkey()=3   &&&-Pgln
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
      SET COLOR TO gr+*/w
      @row(),col() say &fds
      SET COLOR TO /w
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
*
STORE ' ' to jlap,ket
STORE tglx to tgl,tgl2
Nmb=SPACE(30)
DO WHILE .t.
   SELECT 2
   Set filter to
   @1,0 CLEAR to 24,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Do pesan with 'ESC=Selesai'
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
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   SELECT 1
   Uselan('RETURBN.DTA','RETURBN2.NDX','','','','')
   *
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-RETUR.RTF
   ?' LAPORAN RETUR PEMBELIAN PER ITEM BARANG'
   ?' NAMA BRG: '+RTRIM(nmb)
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(218)+REPLACE(gl,4)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,20)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,7)+chr(194)+REPLACE(gl,9)+chr(194)+REPLACE(gl,11)+chr(191)
   ?tl+' No.'+tl+'Tanggal   '+tl+'Nama Supplier       '+tl+'No.Faktur '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,20)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,7)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
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
   Do pesan with 'ESC=Menu Selanjutnya'
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
RELEASE klb,jlap,ket,nsp,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

*EOF RTB
