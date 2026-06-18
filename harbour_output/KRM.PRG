// ============================================================
// File    : KRM.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* TRANSFER/KIRIM BARANG'
STORE tglx to tgl,tgl2
Tgl2=ctod('  -  -    ')
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
SELECT 2
*
SELECT 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
SELECT 4
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
SELECT 5
Useidx('KIRIM.DTA','','','','','')
REPLACE all faktur with SPACE(5),id_data with ' '
*
SELECT 8
Uselan('DATA.SET','','','','','')
Nn=no_pinjam+1
If nn>99999
   Nn=1
ENDIF
Do xnota
SELECT 9
*
Brs4 ="left(kdbx,13)+tl+left(nmb,30)+tl+tran(jml,'@e 99999.99 ')+left(st,4)+"+;
      "tl+tran(hp,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')"
Brs42="left(kode_bar,13)+tl+left(nama_brg,30)+tl+tran(jumlah,'@e 99999.99 ')+left(satuan,4)+"+;
      "tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')"
STORE SPACE(5) to ksp,klb
STORE 0 to jml,tot,tot2,dis4
STORE .f. to hapus,isilagi,batal,ada,diedit
Nklb=SPACE(20)
Lks='T'
DO WHILE .t.
   SELECT 1
   Set key 28 to dafspl
   @1,0 CLEAR to 23,79
   @1,1 say iif(idb='M','Dari:','Ke..:')+SPACE(35)+'Tanggal.:'+SPACE(31)
   @2,1 say SPACE(40)+'No.Bukti:'+SPACE(31)
   @1,7 get ksp pict '@!'
   @1,51 say tgl
   @2,51 say fkt pict '@!'
   Do pesan with 'F1=Daftar Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   ENDIF
   Seek ksp
   If .not. FOUND()
      Do pesan2 with 'Tidak ada kode supplier '+RTRIM(ksp)
      Loop
   ENDIF
   Nsp =nama_spl
   Algn=almt_spl
   @1,7 say ksp
   @2,7 say algn
   @1,51 get tgl
   @2,51 get fkt pict '@!'
   Read
   If lastkey()=27
      Loop
   ENDIF
   N=1
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
         Idb =id_stok
         Nrx =recno()
         *
         SELECT 5
         If n=1
            Go top
         ENDIF
         REPLACE tanggal with tgl,faktur with fkt,kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,;
              kode_bar with kdbx,nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,nomorekod with nrx,id_stok with idb,id_data with '1'
         Skip
         *
         N=n+1
         *
         SELECT 4
         Skip
      ENDDO
   ENDIF
   @3,0 say REPLACE(gd,13)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,13)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)
   @4,0 say 'BarCode      '+tl+'Nama Barang'+SPACE(19)+tl+'  Jml.Barang '+tl+'   @Harga'+tl+' Jumlah Rp.'+tl
   @5,0 say REPLACE(gl,13)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,13)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)
   B=6
   Kolompjm()
   Disi=.f.
   DO WHILE .t.
      Yt=' '
      B=6
      Kdbx=SPACE(13)
      Nmb =SPACE(30)
      If isilagi
         Do hpjm
         Isilagi=.f.
         Jml=0
      ENDIF
      DO WHILE .t.
         SELECT 3
         Set key 5 to dataht
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         @b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(13)+tl+SPACE(9)+tl+SPACE(11)
         @b,0 get kdbx pict '@S13'
         Do pesan with 'BarCode?'
         Read
         Set key 5 to
         If hapus
            Do hpjm
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
         If kdbx=SPACE(13)
            @b,14 get nmb pict '@!S30'
            Do pesan with 'Masukan Nama Barang'
            Read
            If nmb=SPACE(30)
               Exit
            ENDIF
            Nmb=RTRIM(nmb)
            Set order to 2
            Seek nmb
            Do dafbrg
         Else
            Set order to 1
            Seek kdbx
            If .not. FOUND()
               Seek RTRIM(kdbx)
               Do dafbrg
            ENDIF
         ENDIF
         If lastkey()=27
            Loop
         ENDIF
         *
         Set order to 1
         Klb=kode_klb
         Kdb=kode_brg
         Nmb=nama_brg
         St =satuan
         Hp =harga
         Jhg=0
         Do pesan with 'Jumlah Barang?'
         @b,0 say &brs4
         @b,45 get jml pict '@e 99999.99'
         @b,59 get hp  pict '@e 9,999,999'
         Read
         If lastkey()=27
            Loop
         ENDIF
         Jhg =(jml*hp)
         If hp=0
            @b,69 get jhg pict '@e 999,999,999'
            Read
            If jhg>0 .and. jml>0
               Hp=jhg/jml
            ENDIF
         ENDIF
         @b,0 say &brs4
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         ENDIF
         Tot =tot + jhg
         *
         SELECT 5
         If n=1
            Go top
         ENDIF
         REPLACE tanggal with tgl,faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,id_stok with idb,id_data with ' '
         Skip
         *
         B=b+1
         N=n+1
         Jml=0
         Kdbx=SPACE(13)
         Nmb =SPACE(30)
         If b>20
            Scroll(8,0,20,79,1)
            B=20
         ENDIF
      ENDDO
      *
      If .not. batal
         STORE 0 to dis4,byr,kbl,jmlg
         Ht=' '
         Do pesan with 'Simpan Data? Y/T:'
         SET COLOR TO g
         DO WHILE .not. ht$'YT'
            @24,49 get ht pict '!'
            Read
            If lastkey()=27
               Exit
            ENDIF
         ENDDO
         If ht='T' .or. lastkey()=27
            Isilagi=.t.
            Loop
         ENDIF
         *   
         Batal=.f.
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
            Idt=id_data
            Isi =1
            Idb =id_stok
            *
            If idt=' ' .and. jml#0
               SELECT 4
               Flok()
               APPEND BLANK
               REPLACE tanggal with tgl,jam with time(),faktur with fkt,kode_spl with ksp,;
                    nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
                    nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,id_stok with idb
               Ul()
               COMMIT
               *
               SELECT 3
               Set order to 1
               Seek kdbx
               Rlok()
               If idb='M'
                  REPLACE stok_m with (stok_m+(jml*isi))
               Elseif idb='K'
                  REPLACE stok_kr with (stok_kr+(jml*isi))
               ENDIF
               Ul()
            ENDIF
            *
            SELECT 5
            REPLACE faktur with SPACE(5),id_data with ' '
            Skip
         ENDDO
         *
         If .not. ada
            SELECT 8
            Rlok()
            REPLACE no_pinjam with nn
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
      kolompjm()
      Exit
   ENDDO
   Ksp=SPACE(5)
   Ada=.f.
ENDDO
CLOSE all
RELEASE brs4,brs42,ksp,klb,jml,tot,tot2,dis4,nklb,lks,kdb,kdbx,nmb,st,isi,nn,fkt
RELEASE hapus,isilagi,batal,ada,diedit
RETURN

*** PROSEDUR HAPUS TRANSAKSI 
PROCEDURE hpjm
kolompjm()
SELECT 5
Set filter to tanggal=tgl .and. faktur=fkt
Go top
If faktur=SPACE(5)
   Set filter to
   RETURN
ENDIF
DECLARE field[8],bts[8],frm[8]
Field[1]='left(KODE_BAR,13)'
Field[2]='left(NAMA_BRG,30)'
Field[3]='JUMLAH'
Field[4]='left(SATUAN,4)'
Field[5]='HARGA'
Field[6]='JUMLAH*HARGA'
Frm[1]='@!'
Frm[2]='@!'
Frm[3]='@e 99999.99'
Frm[4]='@!'
Frm[5]='@e 9,999,999'
Frm[6]='@e 999,999,999'
STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
Pesany='ENTER=Edit Jumlah     ESC=Selesai'
Do pesan with pesany
Keyboard chr(5)
DbEdit(6,0,20,79,field,'fhpjm',frm,'','',bts)
*
Kolompjm()
Tot=0
Set filter to
Go top
DO WHILE .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
   Jhg=(jumlah*harga)
   Tot=tot+jhg
   @b,0 say &brs42
   B  =b+1
   If b>20
      Scroll(8,0,20,79,1)
      B=20
   ENDIF
   Skip
ENDDO
Nmb =SPACE(30)
Kdbx=SPACE(13)
Jml=1
Hapus=.f.
RETURN

Function fhpjm
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
   Nrx=nomorekod
   Idb=id_stok
   STORE jumlah to jml,jml2
   SET CURSOR ON
   Do pesan with ''
   @b,45 get jml pict '@e 99999.99'
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
         If idb='M'
            REPLACE stok_m with (stok_m-(jml2*isi))+(jml*isi)
         Elseif idb='K'
            REPLACE stok_kr with (stok_kr-(jml2*isi))+(jml*isi)
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
FUNCTION kolompjm
DO WHILE b<21
   @b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(13)+tl+SPACE(9)+tl+SPACE(11)
   B =b+1
ENDDO
@b,0 say REPLACE(gl,13)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,13)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)
B=6
RETURN(.t.)

*LAP.KIRIM BARANG
*-----------------
PROCEDURE lkrm
SELECT 1
Uselan('SUPP.DTA','SUPP.NDX','','','','')
SELECT 2
*Uselan('STOK.DTA','STOK5.NDX','','','','')
SELECT 3
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
SELECT 4
*
SELECT 5
*
Ksp=SPACE(5)
Nsp='-'
STORE ' ' to hri,bln,thn,jlap
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafspl
   Tgl=ctod('01-'+LTRIM(str(mont(tgl)))+'-'+LTRIM(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Dari/Ke:'get ksp pict '@!'
   @3,24 say 'Tanggal:'get tgl
   @3,44 say 'sampai'get tgl2
   @4,24 say 'Keterangan, M=Masuk  K=Keluar :'get jlap pict '!'
   Do pesan with 'F1=Daftar Cabang'
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
      Nsp=nama_spl
   ENDIF
   Syarat='id_stok#" "'
   If jlap='K'
      Syarat='id_stok="K"'
   Elseif jlap='M'
      Syarat='id_stok="M"'
   ENDIF
   STORE 0 to no,tot,tot2
   Do konvtgl
   SELECT 3
   Set filter to &syarat
   If ksp#SPACE(5)
      Set filter to kode_spl=ksp .and. &syarat
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-ORDER&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplob
   SET CENTURY OFF
   Tglz=tanggal
   Nsls=kode_spl
   Nklb=nama_spl
   Fkt=faktur
   Idb=id_stok
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
         ?left(kode_brg,7)+'  '+left(nama_brg,30)+'  '+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 999,999,999')
         Skip
         If faktur#fkt .or. kode_spl#nsls
            ?SPACE(63)+REPLACE(gl,12)
            ?left(nklb,20)+'  Tgl: '+dtoc(tglz)+' / '+fkt+' / '+idb+'     Jumlah Rp.='+tran(tot2,'@e 9999,999,999')
            ?REPLACE(gl,75)
            Tglz=tanggal
            Nsls=kode_spl
            Nklb=nama_spl
            Fkt =faktur
            Idb =id_stok
            Tot2=0
            No  =no+1
            If no>=20
               No =0
               Hal=hal+1
               Cf =cf+1
               Cfx=LTRIM(str(cf))
               Arg1='L-ORDER&cfx'+'.RTF'
               Set alte to &arg1
               Do koplob
            ENDIF
         ENDIF
   ENDDO
   ?REPLACE(gl,75)
   ?SPACE(54)+'TOTAL=Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-ORDER&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       ENDIF
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F10=Cetak'
   If lastkey()=-3 .and. ( Owner .or. xOwner )
      SET CENTURY ON
      Do pesan with 'Order tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @23,65 get yt pict '!'
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
   STORE 1 to no,n,hal,b
   STORE 0 to jhg,tot
   Do koplob1
   Go nr
   SET CENTURY OFF
   Tglz=tanggal
   Nsls=kode_spl
   Nklb=nama_spl
   Fkt =faktur
   Idb =id_stok
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
      ?tran(no,'9999.  ')+left(kode_brg,7)+'  '+left(nama_brg,30)+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 999,999,999')
      B =b+1
      N =n+1
      No=no+1
      Skip
      If faktur#fkt .or. kode_spl#nsls
         ?SPACE(68)+REPLACE('-',12)
         ?SPACE(5)+left(nklb,20)+'  Tgl: '+dtoc(tglz)+' / '+fkt+' / '+idb+'     Jumlah Rp.='+tran(tot2,'@e 9999,999,999')
         ?REPLACE('-',80)
         Tglz=tanggal
         Nsls=kode_spl
         Nklb=nama_spl
         Fkt =faktur
         Idb =id_stok
         Tot2=0
         No  =1
         N   =n+3
         B   =b+3
      ENDIF
      If n>55
         @b,0 say REPLACE('-',80)
         @b+1,0 say ''
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
         N  =1
         Hal=hal+1
         Do koplob1
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   @b,0 say SPACE(53)+'T o t a l = Rp'+tran(tot,'@e 9,999,999,999')
   @b+1,0 say REPLACE('=',80)
   @b+2,0 say ' '
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplob
?'Supp: '+RTRIM(nsp)+' Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   //Hal:'+tran(hal,'999')
?REPLACE(gd,8)+chr(209)+REPLACE(gd,30)+chr(209)+gd+gd+gd+REPLACE(chr(209)+REPLACE(gd,9),2)+chr(209)+REPLACE(gd,11)
?'Kode    '+tl+'Nama Barang'+SPACE(19)+tl+'Lks'+tl+'     Qty.'+tl+'   @Harga'+tl+'     Jumlah'
?REPLACE(gl,8)+chr(193)+REPLACE(gl,30)+chr(193)+gl+gl+gl+REPLACE(chr(193)+REPLACE(gl,9),2)+chr(193)+REPLACE(gl,11)
RETURN

PROCEDURE koplob1
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 20
?chr(27)+chr(33)+chr(4)
@prow()+1,1 say 'DAFTAR PINJAMAN BARANG'
If ksp#SPACE(5)
   @prow()+1,1 say 'REKANAN.: '+RTRIM(nsp)+'  ('+RTRIM(ksp)+')'
ENDIF
If tgl=tgl2
   @prow()+1,1 say 'TANGGAL.: '+dtoc(tgl)
Else
   @prow()+1,1 say 'TANGGAL.: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
ENDIF
@prow(),72 say 'Hal:'+tran(hal,'999')
@prow()+1,0 say REPLACE('=',80)
@prow()+1,0 say '  No.  Kode     Nama Barang                      Jml.Brg.     @Harga   Jumlah Rp'
@prow()+1,0 say REPLACE('-',80)
B=prow()+1
RETURN

* LAPORAN TRANSFER/ITEM
*----------------------
PROCEDURE lkrm2
SELECT 1
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
SELECT 2
Uselan('STOK.DTA','STOK4.NDX','','','','')
*
STORE tglx to tgl,tgl2
Nmb=SPACE(30)
DO WHILE .t.
   SELECT 2
   SET CENTURY ON
   Set filter to
   @1,0 CLEAR to 23,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Do pesan with 'Masukan Nama Barang'
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
   *
   SELECT 1
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-TRAN&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?' LAPORAN TRANSFER / ITEM BARANG'
   ?' NAMA BRG: '+RTRIM(nmb)+'   KODE: '+kdb
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'  / Hal:'+tran(hal,'999')
   ?chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,14)+chr(209)+REPLACE(gd,5)+chr(209)+REPLACE(gd,3)+chr(209)+REPLACE(gd,6)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)+chr(184)
   ?tl+' No.'+tl+'Tanggal       '+tl+'Bukti'+tl+'G/T'+tl+'  Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,14)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,3)+chr(197)+REPLACE(gl,6)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
   Y=1
   STORE 0 to no,stm,hp,hj,jml,tot,tot2,totb,jbrt
   Z=LTRIM(str(y))
   SET CENTURY OFF
   DO WHILE .t.
      Set filter to kode_bar=kdb
      Seek dtos(tgl)
      If .not. FOUND()
         LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
      ENDIF
      St =satuan
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jhg =(jumlah*harga)
         Stm =stm+jumlah
         Tot =tot+ jhg
         xJml=jumlah
         Do fmtjml2
         ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+faktur+tl+'   '+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
         Y =y +1
         No=no+1
         If no>=200
            ?chr(195)+REPLACE(gl,4)+chr(197)+REPLACE(gl,14)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,3)+chr(197)+REPLACE(gl,6)+chr(197)+REPLACE(gl,9)+chr(197)+REPLACE(gl,11)+chr(180)
            ?' '
            No =0
            Hal=hal+1
            Cf =cf +1
            Cfx=LTRIM(str(cf))
            Arg1='L-TRAN&cfx'+'.RTF'
            Set alte to &arg1
            ?SPACE(50)+' Hal:'+tran(hal,'999')
            ?chr(213)+REPLACE(gd,4)+chr(209)+REPLACE(gd,14)+chr(209)+REPLACE(gd,5)+chr(209)+REPLACE(gd,3)+chr(209)+REPLACE(gd,6)+chr(209)+REPLACE(gd,9)+chr(209)+REPLACE(gd,11)+chr(184)
         ENDIF
         Skip
      ENDDO
      Exit
   ENDDO
   Nmb =SPACE(30)
   Pesanx=tran(stm,'@e 999,999 ')+st
   ?chr(192)+REPLACE(gl,4)+chr(193)+REPLACE(gl,14)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,3)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,11)+chr(217)
   ?SPACE(23)+'Total ='+pesanx+' Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-TRAN&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       ENDIF
       Ada=.f.
   Next
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
   Type L-TRAN1.RTF to prin
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLOSE all
RELEASE klb,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
RETURN

* LAPORAN PEMECAHAN STOK
*------------------------
PROCEDURE lpch
SELECT 1
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
SELECT 2
Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')
*
STORE tglx to tgl,tgl2
DO WHILE .t.
   Kdb=SPACE(13)
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafbrg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai  F1=Daftar Kode Barang'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   If kdb#SPACE(13)
      Seek kdb
      If .not. FOUND()
         Seek RTRIM(kdb)
      ENDIF
   ENDIF
   STORE 0 to no,tot,tot2
   STORE SPACE(5) to st,st2
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   SELECT 2
   Set filter to
   If kdb#SPACE(13)
      Set filter to kode_brg=kdb
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-PCH&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpch
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(nama_brg,30)+tl+left(nama_brg2,30)+tran(jumlah,'@e 99999 ')+left(satuan2,5)
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      Skip
      No  =no+1
      If no>=200
         No =0
         Cf =cf+1
         Cfx=LTRIM(str(cf))
         Arg1='L-PCH&cfx'+'.RTF'
         Set alte to &arg1
         Do koplpch
      ENDIF
   ENDDO
   ?REPLACE(chr(196),81)
   ?SPACE(60)+'TOTAL='+tran(tot,'@e 9,9999,999 ')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-PCH&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,22,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       ENDIF
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hpch
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      SET CENTURY ON
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @23,67 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2)
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
   Do koplpch1
   Go nr
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+'  '+nama_brg+'  '+tran(jumlah*isine_b,'9999 ')+satuan+'  '+left(kode_brg2,7)+'  '+nama_brg2+tran(jumlah,'@e 9999.99 ')+satuan2
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      No=no+1
      Skip
      If no>60
         ?REPLACE('-',114)
         ?SPACE(94)+'bersambung...'
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
         Do koplpch1
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?SPACE(42)+'TOTAL='+tran(tot2,'@e 999,999 ')+SPACE(43)+tran(tot,'@e 999,999.99 ')
   ?REPLACE('=',114)
   ?' '
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplpch
?' Kode: '+RTRIM(kdb)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?REPLACE(gd,9)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,9)
?'Tanggal  '+tl+'Nama Barang'+SPACE(19)+tl+'Asal Barang'+SPACE(19)+tl+'  Jml.Brg'
?REPLACE(gl,9)+chr(193)+REPLACE(gl,30)+chr(197)+REPLACE(gl,30)+chr(193)+REPLACE(gl,9)
RETURN

PROCEDURE koplpch1
SET PRINTER ON
SET DEVICE TO PRINTER
?chr(27)+chr(33)+chr(5)
Set marg to 15
?' LAPORAN MUTASI/PECAH STOK'
If kdb#SPACE(13)
   ?' Kode.: '+kdb
ENDIF
?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+SPACE(74)+'Hal:'+tran(hal,'999')
?REPLACE('=',114)
?'Tanggal   Nama Barang                               Jml.Brg    Asal Barang                                Jml.Brg'
?REPLACE('-',114)
No=1
RETURN

PROCEDURE hpch
@1,0 CLEAR to 23,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai  F8=Hapus'
Do pesan with katax
DECLARE field[9],kop[9],frm[9],bts[9]
Field[1]='TANGGAL'
Field[2]='left(NAMA_BRG,30)'
Field[3]='left(NAMA_BRG2,30)'
Field[4]='JUMLAH'
Field[5]='left(SATUAN2,4)'
Field[6]='ISINE_B'
Field[7]='SATUAN'
Field[8]='KODE_BRG'
Field[9]='KODE_BRG2'
Kop[1]='Tanggal'
Kop[2]='Nama Barang'
Kop[3]='Asal Barang'
Kop[4]='Jml.'
Kop[5]='Brg.'
Kop[6]=' Isi'
Kop[7]=''
Kop[8]='Kode'
Kop[9]='Kode Asal'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='9999'
Frm[5]='@!'
Frm[6]='@e 9999'
Frm[7]='@!'
Frm[8]='@!'
Frm[9]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=' '
Bts[6]=tl
Bts[7]=' '
Bts[8]=tl
Bts[9]=tl
Go nr
DbEdit(2,0,21,79,field,'fpch',frm,kop,chr(196),bts)
*
SET CURSOR ON
RETURN

Function fpch
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
         Kdb =kode_brg
         Kdb2=kode_brg2
         Jml =jumlah
         Isi =isine_b
         If .not. dele()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
            *
            SELECT 1
            Seek kdb
            Rlok()
            REPLACE stok_m with (stok_m-(jml*isi))
            Ul()
            COMMIT
            *
            Seek kdb2
            Rlok()
            REPLACE stok_k with (stok_k-(jml*isine_b))
            Ul()
            COMMIT
            Skip
            *
            SELECT 2
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

*EOF KRM
