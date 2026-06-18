// ============================================================
// File    : ADJ.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* ADJUST
*-------
SELECT 1
*
SELECT 2
*
SELECT 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
SELECT 4
Uselan('ADJUSTN.DTA','ADJUSTN1.NDX','','','','')
*
SELECT 8
Uselan('DATA.SET','','','','','')
Nn=no_trans+1
If nn>99999
   Nn=1
ENDIF
Do xnota
Fkt=ksa+right(fkt,4)
*
Tgl=tglx
STORE SPACE(5) to ksp,klb
STORE 0 to jml,tot
DO WHILE .t.
   STORE .f. to hapus,isilagi,batal,tdketemu
   *
   SELECT 1
   @1,0 CLEAR to 23,79
   @1,0 say chr(218)+REPLACE(gl,38)+chr(194)+REPLACE(gl,39)+chr(191)
   @2,0 say tl+'Tanggal..:'+SPACE(28)+tl+SPACE(39)+tl
   @3,0 say tl+SPACE(38)+tl+SPACE(39)+tl
   @4,0 say tl+'No.Bukti.:'+SPACE(28)+tl+SPACE(39)+tl
   @5,0 say chr(192)+REPLACE(gl,38)+chr(193)+REPLACE(gl,39)+chr(217)
   @2,12 get tgl
   @4,12 get fkt pict '@!'
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   ENDIF
   *
   @5,1 say REPLACE(gl,12)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,4)+REPLACE(chr(194)+REPLACE(gl,9),3)
   @6,0 say 'BarCode      '+tl+'Nama Barang'+SPACE(19)+tl+'Sat.'+tl+'Stok Komp'+tl+'    Fisik'+tl+'   Selisih'
   @7,0 say REPLACE(gl,13)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,4)+REPLACE(chr(197)+REPLACE(gl,9),3)+gl
   @5,39 say chr(193)
   Ada=.f.
   B=8
   N=1
   Kolomadj()
   DO WHILE .t.
      Yt=' '
      B =iif(isilagi,b,8)
      If isilagi
         *Do hadj
         Isilagi=.f.
         Jml=0
      ENDIF
      DO WHILE .t.
         Kdb=SPACE(13)
         Nmb=SPACE(30)
         SELECT 3
         @b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(4)+REPLACE(tl+SPACE(9),3)
         @b,0 get kdb pict '@!S13'
         Do pesan with 'Kode Barang?     ESC=Selesai'
         Read
         If lastkey()=27
            Exit
         ENDIF
         If kdb=SPACE(13)
            @b,14 get nmb pict '@!S30'
            Do pesan with 'Masukan Nama Barang'
            Read
            If nmb=SPACE(30)
               Exit
            ENDIF
            Carikode=.f.
            Set order to 2
            Seek RTRIM(nmb)
            Do dafbrg
         Else
            Carikode=.t.
            Set order to 1
            Seek kdb
            If .not. FOUND()
               Seek RTRIM(kdb)
               If .not. FOUND()
                  Carikode=.f.
                  Nmb=kdb
                  Seek RTRIM(kdb)
                  Do dafbrg
               ENDIF
            ENDIF
         ENDIF
         If tdketemu
            Tdketemu=.f.
            Tone(1500,3)
            Tone(2000,6)
            Do pesan with 'Data Tidak diTemukan...'
            Wait''
            Loop
         ENDIF
         *
         If lastkey()=27 .or. kdb=SPACE(13)
            Loop
         ENDIF
         *
         SELECT 3
         Set order to 1
         Ksp =kode_spl
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         Stb =satuan_b
         St  =satuan
         Isi =isine_b
         Hp  =harga
         Hj  =harga_2
         Jml0=stok_a
         Jml1=stok_m
         Jml2=stok_k
         Jml3=stok_rb
         Jmlx=((jml0+jml1)-(jml2-jml3))
         Kata="'1 '+RTRIM(stb)+'= '+LTRIM(tran(isi,'9999 '))+RTRIM(st)"
         STORE 0 to jml,nr
         *
         Do pesan with &kata+'.   HP: '+LTRIM(tran(hp,'@e 9,999,999'))+'   HJ: '+LTRIM(tran(hj,'@e 9,999,999'))+'     Jumlah Fisik Barang?'
         SET COLOR TO /bg
         @b,0  say kdbx
         @b,14 say left(nmb,30)
         @b,45 say left(st,4)
         @b,50 say tran(jmlx,'@e 9,999,999')
         @b,60 get jml pict '@e   9999999'
         If jam='*'
            Jmlx=0
            Tone(1500,3)
            Tone(2000,6)
            Save scre to smkdb
            SET COLOR TO w+/r
            @b+1,0
            @b+1,20 say 'Data sudah pernah di-entry sebelumnya...'
         ENDIF
         Read
         If jam='*'
            Rest scre from smkdb
         ENDIF
         SET COLOR TO /bg
         If lastkey()=27
            Loop
         ENDIF
         @b,60 say tran(jml,'@e 9,999,999')
         @b,70 say tran(jml-jmlx,'@e 99,999,999')
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         ENDIF
         *
         Rlok()
         If id_so=' '
            REPLACE stok_a with jml,stok_m with 0,stok_k with 0,stok_rb with 0,id_so with '*',tanggal_0 with tgl
         Elseif id_so='*'
            REPLACE stok_a with (stok_a+jml),tanggal_0 with tgl
         ENDIF
         Ul()
         *
         SELECT 4
         Flok()
         APPEND BLANK
         REPLACE tanggal with tgl,faktur with fkt,jam with time(),kode_spl with ksp,;
              kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              stok_a with jmlx,jumlah with jml,harga with hp,harga_2 with hj
         Ul()
         *
         B=b+1
         N=n+1
         Kdb=SPACE(13)
         Nmb=SPACE(30)
         Ada=.t.
         If b>21
            Scroll(8,0,21,79,1)
            B=21
         ENDIF
      ENDDO
      *
      If .not. batal
         Yt=' '
         Do pesan with 'Simpan Data? Y/T :'
         SET COLOR TO /bg
         DO WHILE .not. yt$'YT'
            @24,50 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            ENDIF
         ENDDO
         If yt='T' .or. lastkey()=27
            Isilagi=.t.
            Loop
         ENDIF
         *   
         Batal=.f.
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         SELECT 4
         COMMIT
         *
         SELECT 3
         COMMIT
         *
         If ada
            SELECT 8
            Rlok()
            REPLACE no_trans with nn
            Ul()
            COMMIT
            Nn=nn+1
            If nn>99999
               Nn=1
            ENDIF
            Do xnota
         Elseif ada=.f.
            Nn=nn
            Do xnota
         ENDIF
         Fkt=ksa+right(fkt,4)
      Else
         Batal=.f.
         SELECT 5
         REPLACE all faktur with SPACE(5)
      ENDIF
      STORE 0 to jml,tot
      Kolomadj()
      Exit
   ENDDO
   Ksp='   '
   Ada=.f.
ENDDO
CLOSE all
SET CONFIRM ON
RELEASE brs4,brs42,ksp,klb,jml,tot,kdb,kdbx,nmb,st,isi,nn,fkt
RELEASE hapus,isilagi,batal,ada
RETURN

***KOLOM RETURBELI
FUNCTION kolomadj
DO WHILE b<22
   @b,0 say SPACE(13)+tl+SPACE(30)+tl+'    '+REPLACE(tl+SPACE(9),3)
   B =b+1
ENDDO
@b,0 say REPLACE(gl,13)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,4)+REPLACE(chr(193)+REPLACE(gl,9),3)
B=8
RETURN(.t.)

* LAPORAN ADJUST
*---------------
PROCEDURE ladj
SELECT 1
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
SELECT 2
Uselan('ADJUSTN.DTA','ADJUSTN1.NDX','','','','')
*
Klp=.f.
STORE tglx to tgl,tgl2
DO WHILE .t.
   Kdb=SPACE(13)
   Nmb=' '
   STORE .f. to carikode,tdketemu
   *
   SELECT 1
   SET CENTURY ON
   @1,0 CLEAR to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   Do pesan with 'Ketik Kode/Nama Barangnya'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   If kdb#SPACE(13)
      Carikode=.t.
      Set order to 1
      Seek kdb
      If .not. FOUND()
         Seek RTRIM(kdb)
         If .not. FOUND()
            Carikode=.f.
            Nmb=kdb
            Seek RTRIM(kdb)
            Do dafbrg
         ENDIF
      ENDIF
      Nmb=nama_brg
   ENDIF
   @2,32 say iif(kdb#SPACE(13),nmb,'')
   If tdketemu
      Tdketemu=.f.
      Tone(1500,3)
      Tone(2000,6)
      Do pesan with 'Data Tidak diTemukan...'
      Wait''
      Loop
   ENDIF
   *
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Ulangi'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Loop
   ENDIF
   STORE 0 to no,tot,tot2
   STORE SPACE(5) to st,st2
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   SELECT 2
   Set filter to
   If kdb#SPACE(7)
      Set filter to ( kode_bar=kdb .or. nama_brg=nmb )
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   *
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do kopladj
   SET CENTURY OFF
   Tglz=tanggal
   Fktz=faktur
   ?SPACE(9)+'Tgl: '+dtoc(tglz)+' / '+fktz
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Kdby=kode_bar
      Kdby=iif(len(RTRIM(kdby))>=7,left(kdby,2)+'x'+right(RTRIM(kdby),4),left(kdby,7))
      ?kdby+'  '+left(nama_brg,26)+' '+left(satuan,4)+tran(stok_a,'@e  9999,999')+tran(jumlah,'@e  9999,999')+iif(jumlah-stok_a#0,tran(jumlah-stok_a,'@e 9,999,999 '),'        . ')+;
       tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 9,999,999,999 ')+tran((jumlah-stok_a)*harga,'@e 9,999,999,999')
      Tot2=tot2+((jumlah-stok_a)*harga)
      Tot =tot +(jumlah*harga)
      No  =no+1
      Skip
      If tanggal#tglz .or. faktur#fktz
         Tglz=tanggal
         Fktz=faktur
         No  =no+1
         ?SPACE(9)+'Tgl: '+dtoc(tglz)+' / '+fktz
      ENDIF
      If no>=50
         ?REPLACE('-',105)
         ?' '
         No =0
         Hal=hal+1
         Cf =cf +1
         Cfx=LTRIM(str(cf))
         Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
         Set alte to &arg1
         Do kopladj
      ENDIF
   ENDDO
   ?REPLACE('-',105)
   ?'         T O T A L  Rp'+SPACE(56)+tran(tot,'@e 9,999,999,999 ')+tran(tot2,'@e 9,999,999,999')
   ?REPLACE('=',105)
   ?' TGL.CETAK: '+dtoc(tglx)+' / '+left(time(),5)
   ?' '
   Set alte off
   Set alte to
   *
   Layar('C:\TEMP\L-ADJ')
   *
   Do pesan3 with 'ESC=Selesai     F8=Koreksi     F10=Cetak'
   If lastkey()=-7
      Do hadj
      Loop
   ENDIF
   If lastkey()=27
      Loop
   ENDIF
   Sambung='Y'
   Do pesan with 'Pakai Kertas berSambung? Y/T : '
   @24,55 get sambung pict '!'
   Read
   Do cp
   If cc=0
      Loop
   ENDIF
   SET PRINTER ON
   SET DEVICE TO PRINTER
   Set marg to 10
   ?chr(27)+chr(33)+chr(4)
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
       Type &arg1 to print
       *
       If sambung='Y'
          Eject
       Else
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          Do pesan3 with gtkertas +' ( '+'&cfx->'+LTRIM(str(cf))+' )'
          If nlp=0
             Cc=0
             Exit
          ENDIF
          SET PRINTER ON
          SET DEVICE TO PRINTER
          Set marg to 10
          ?chr(27)+chr(33)+chr(4)
       ENDIF
   Next
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE kopladj
?'LAPORAN STOK OPNAME'
?'TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(30)+'Hal: '+tran(hal,'999')
?REPLACE('=',105)
?'Kode     Nama Barang                Sat.    Komp.    Fisik  Selisih  @Hg.Beli     Jumlah_Rp    Selisih_Rp'
?REPLACE('-',105)
N=1
RETURN

PROCEDURE hadj
@1,0 CLEAR to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai     F5=Simpan_Ulang     ENTER=Edit'
Do pesan with katax
DECLARE field[17],kop[17],frm[17],bts[17]
Field[1]='TANGGAL'
Field[2]='JAM'
Field[3]='KODE_BAR'
Field[4]='left(NAMA_BRG,30)'
Field[5]='left(SATUAN,4)'
Field[6]='STOK_A'
Field[7]='JUMLAH'
Field[8]='JUMLAH-STOK_A'
Field[9]='HARGA'
Kop[1]='Tanggal'
Kop[2]='Jam'
Kop[3]='Barcode'
Kop[4]='Nama Barang'
Kop[5]='Sat'
Kop[6]='  Komp.'
Kop[7]='  Fisik'
Kop[8]=' Selisih'
Kop[9]='   @Pokok'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 999,999'
Frm[7]='@e 999,999'
Frm[8]='@e 9999,999'
Frm[9]='@e 9,999,999'
STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9],bts[10]
Go nr
DbEdit(2,0,21,79,field,'fhadj',frm,kop,gl,bts)
*
RELEASE field[14],kop[14],frm[14],bts[14]
SET CURSOR ON
RETURN

Function fhadj
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
   Case lastkey()=13
      Kdb=kode_bar
      Hp =harga
      STORE jumlah to jml,jml2
      SET CURSOR ON
      If field[pointer]='JUMLAH'
         @row(),col() get jml pict '@e  999999'
         Do pesan with 'Masukan Jumlah Barang'
         Read
         If lastkey()#27
            Rlok()
            REPLACE jumlah with jml
            Ul()
            COMMIT
            *
            SELECT 1
            Seek kdb
            Rlok()
            REPLACE stok_a with jml
            Ul()
            COMMIT
            *
            SELECT 2
         ENDIF
      Elseif left(field[pointer],5)='HARGA'
         @row(),col() get hp pict '@e   9999999'
         Do pesan with 'Masukan Harga Pokok/Beli'
         Read
         If lastkey()#27
            Rlok()
            REPLACE harga with hp
            Ul()
            COMMIT
         ENDIF
      Elseif field[pointer]='KODE_KLB' .or. field[pointer]='KODE_BRG'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         COMMIT
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-4
      SET CURSOR ON
      Yt=' '
      Do pesan with 'Stok Opname Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' mau diSimpan Ulang? Y/T :  '
      @24,70 get yt pict '!'
      Read
      If yt='Y'
         Do pesan with 'Tunggu, sedang proses...'
         Go nr
         DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Kdb=kode_bar
            Nmb=nama_brg
            Jml=jumlah
            *
            SELECT 1
            Set order to 1
            Seek kdb
            If .not. FOUND()
               Set order to 2
               Seek nmb
            ENDIF
            Rlok()
            If id_so=' '
               REPLACE stok_a with jml,id_so with '*',tanggal_0 with tgl
            Elseif id_so='*'
               REPLACE stok_a with (stok_a+jml),tanggal_0 with tgl
            ENDIF
            Ul()
            *
            SELECT 2
            Skip
         ENDDO
         SELECT 1
         COMMIT
         *
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      SELECT 2
      RETURN 1
   Case lastkey()=-8
      @1,30 say 'Data Yang diKoreksi'
      SET DELETED OFF
      Set filter to dele()
      Go top
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* EOF ADJ






