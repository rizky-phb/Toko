// ============================================================
// File    : LGN.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* DAFTAR PELANGGAN
*------------------
SELECT 1
Uselan('DATA.SET','','','','','')
Idt =RTRIM(id_cust)
Pdt =pj_cust
Tgl =tanggal_2
Tgl2=tanggal_3
Tglz=tglx
Close
*
SELECT 2
*
SELECT 3
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
*
STORE SPACE(10) to ksp
STORE .f. to adadis,hapus
DO WHILE .t.
   Sambung='T'
   Kota=SPACE(5)
   STORE SPACE(20) to tlp
   STORE SPACE(30) to nsp,algn
   STORE SPACE(50) to ket1,ket2
   STORE 0 to tot,plf,jwk,dis
   *
   SELECT 3
   @1,0 CLEAR to 23,79
   @23,0 to 23,79
   @1,0 say REPLACE(gl,10)+REPLACE(chr(194)+REPLACE(gl,25),2)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,6)
   @2,0 say 'Kode      '+tl+'Nama Pelanggan           '+tl+'A l a m a t'+SPACE(14)+tl+'Kota      '+tl+'Adjust'
   @3,0 say REPLACE(gl,10)+REPLACE(chr(197)+REPLACE(gl,25),2)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,6)
   Pesany='F1/F2=Cari    F4=Tambah    F8=Hapus    F10=Cetak    ENTER=Edit    H=Hit.Member'
   Do pesan with pesany
   DECLARE field[8],bts[8],frm[8]
   Field[1]='KODE_LGN'
   Field[2]='left(NAMA_LGN,25)'
   Field[3]='left(ALMT_LGN,25)'
   Field[4]='left(KOTA_LGN,10)'
   Field[5]='AJ_POINT'
   Frm[5]='999999'
   STORE '@!' to frm[1],frm[2],frm[3],frm[4]
   STORE tl   to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
   DbEdit(4,0,22,79,field,'flgnx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE ksp,nsp,algn,tlp,kota,tgl2,tglz,tot,spl,fds,nsp,adadis,hapus,dis
RETURN

Function flgnx
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=28
   Nr=recno()
   Nsp=SPACE(25)
   SET CURSOR ON
   Do pesan with 'Nama Pelanggan:'+SPACE(25)
   @24,35 get nsp pict '@!'
   Read
   Set order to 2
   Nsp=RTRIM(nsp)
   SET COLOR TO g
   Seek nsp
   If .not. FOUND()
      Go nr
   ENDIF
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-1
   Nr=recno()
   Ksp=SPACE(11)
   SET CURSOR ON
   Do pesan with 'Kode Pelanggan:'+SPACE(11)
   @24,43 get ksp pict '@!'
   Read
   Set order to 1
   Ksp=idt+right('000000'+RTRIM(ksp),pdt-len(idt))+SPACE(10-pdt)
   SET COLOR TO g
   Seek ksp
   If .not. FOUND()
      Go nr
   ENDIF
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-3 .or. lastkey()=13
   Save scre to slgn
   SET CURSOR ON
   If lastkey()=13
      Ksp =kode_lgn
      Nsp =nama_lgn
      Algn=almt_lgn
      Tlp =telp_lgn
      Kota=kota_lgn
      Hjx =jens_lgn
      Tglso=tanggal
      Tot  =rp_saldo
      Dis  =diskon
      Ket1 =left(keterangan,50)
      Ket2 =right(keterangan,50)
      Sop  =so_point
      Ajp  =aj_point
      Amp  =am_point
      Rpu  =rp_uang
      Tglhp=tgl_ambil
      Jamx =jam_ambil
      Ket  =ket_ambil
      Totpn=0
      Do kolomlgn
      *
      DO WHILE .t.
         SET COLOR TO /w
         @8,24  say ksp
         @9,24  say nsp
         @10,24 say algn
         @11,24 say tlp
         @12,24 say kota
         @12,65 say dis pict '@e  99.99'
         @13,24 say left(ket1,47)
         @14,24 say left(ket2,47)
         @16,24 say tran(ajp ,'@e 999,999')
         @16,49 say tran(amp ,'@e 999,999')
         @16,63 say LTRIM(tran(sop,'@e 999,999'))
         @17,24 say tran(totpn,'@e 999,999')
         @17,49 say dtoc(tglhp)+' / '+jamx
         @18,49 say RTRIM(ket)+iif(rpu#0,' Rp '+LTRIM(tran(rpu,'@e 99,999,999')),'')
         *
         Do pesan with 'ESC=Selesai     ENTER=Edit     H=Hitung Point'
         Wait''
         If lastkey()=13
            SET COLOR TO w+/b
            Set key -4 to xadadis
            Set key -8 to dataht
            @9,24  get nsp  pict '@!'
            @10,24 get algn pict '@!'
            @11,24 get tlp  pict '@!'
            @12,24 get kota pict '@!'
            @12,65 get dis pict '@e  99.99'
            @13,24 get ket1 pict '@S47'
            @14,24 get ket2 pict '@S47'
            Do pesan with 'Masukan Nama, dll     F5=Edit_Point     F9=Edit_Piutang'
            Read
            Set key -4 to
            Set key -8 to
            If adadis
               Adadis=.f.
               @16,24 get ajp pict '@e 999,999'
               @16,49 get amp pict '@e 999,999'
               @17,49 get tglhp
               @17,62 get jamx
               @18,49 get ket pict '@!S22'
               Do pesan with 'Masukan Sisa Ambil Poin lalu (Adjust), Tanggal/Jam Pengambilan & Catatan'
               Read
               Yt=' '
               Do pesan with 'Data Benar? Y/T : '
               @24,49 get yt pict '!'
               Read
               If lastkey()#27 .and. yt='Y'
                  Rlok()
                  REPLACE aj_point with ajp,am_point with amp,tgl_ambil with tglhp,jam_ambil with jamx,ket_ambil with ket
                  Ul()
                  COMMIT
               ENDIF
            ENDIF
            If hapus
               Hapus=.f.
               @20,44 get tglz
               @21,44 get tot pict '@e 999,999,999'
               Do pesan with 'Masukan Tanggal & Saldo Piutang yg terSisa'
               Read
               Do pesan with 'ENTER=Simpan     ESC=Batal'
               Wait''
            ENDIF
            *
            If lastkey()#27
               Rlok()
               REPLACE kode_lgn with ksp,nama_lgn with nsp,almt_lgn with algn,telp_lgn with tlp,kota_lgn with kota,jens_lgn with iif(hjx#'1',' ',hjx),;
                    diskon with dis,rp_saldo with tot,tanggal with tglz,keterangan with ket1+ket2
               Ul()
               COMMIT
            ENDIF
            Exit
         Elseif lastkey()=72 .or. lastkey()=104
            Do pesan with 'Tunggu, sedang diHitung'
            Totpn=0
            *
            SELECT 99
            Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
            Seek ksp
            DO WHILE .not. eof() .and. kode_lgn=ksp
               If tanggal=tglhp .and. jam>jamx
                  Totpn=totpn+jml_poin
               Elseif tanggal>tglhp
                  Totpn=totpn+jml_poin
               ENDIF
               Skip
            ENDDO
            SET COLOR TO /w
            @17,24 say tran(totpn,'@e 999,999')
            @18,24 say tran(ajp+totpn,'@e 999,999')
            Close
            Do pesan with 'ESC=Lanjut     A=Ambil_Poin'
            Wait''
            If lastkey()=65 .or. lastkey()=97
               Kdb =' '
               Ketx='PENGAMBILAN POIN'
               Ket =SPACE(50)
               Tglz=tglx
               STORE 0 to jml,jml2
               *
               SELECT 4
               Uselan('POINTAM.DTA','POINTAM1.NDX','POINTAM2.NDX','','','')
               SET COLOR TO w+/g
               @19,10 CLEAR to 23,71
               @19,10 to 23,71
               @19,32 say ketx
               @21,12 say 'Tanggal.:'get tglz
               @21,34 say 'Catatan:'get ket pict '@!S28'
               @22,12 say 'Poin diAmbil:'get jml pict '@e 999999'
               @22,34 say '-atau- diUangkan sejmlh Rp'get jml2 pict '@e   99999999'
               Do pesan with 'Masukan Tanggal dan Jumlah Poin yg diAmbil'
               Read
               If lastkey()=27
                  SELECT 3
                  Loop
               ENDIF
               SET COLOR TO w+/b
               @22,25 say tran(jml,'@e 999,999')
               @22,61 say tran(jml2,'@e 99,999,999')
               Yt=' '
               Do pesan with 'Data Benar? Y/T :'
               @24,49 get yt pict '!'
               Read
               If lastkey()=27 .or. yt#'Y'
                  SELECT 3
                  Loop
               ENDIF
               If jml#0 .or. jml2#0
                  Flok()
                  APPEND BLANK
                  REPLACE tanggal with tglz,jam with time(),kode_lgn with ksp,nama_lgn with nsp,rp_uang with jml2,;
                       so_point with (ajp+totpn),aj_point with ajp,am_point  with jml,id_point with kdb,keterangan with ket
                  Ul()
                  COMMIT
                  Close
                  *
                  SELECT 3
                  Rlok()
                  REPLACE so_point with (ajp+totpn),aj_point with (ajp+totpn)-jml,am_point with jml,rp_uang with jml2,tgl_ambil with tglz,jam_ambil with time(),ket_ambil with ket
                  Ul()
                  COMMIT
                  *
               ENDIF
            ENDIF
            *
            SELECT 3
            Exit
         Else
            Exit
         ENDIF
      ENDDO
   Else
      Set order to 1
      Go bott
      Ksp=kode_lgn
      Do hitlgn
      Ksp=right(RTRIM(ksp),pdt)+SPACE(11-pdt)    &&&SENGAJA SPACE(11), UNT DATALOGIC
      DO WHILE .t.
         Tgl2=ctod('01-'+LTRIM(str(mont(tglx)))+'-'+LTRIM(str(year(tglx))))
         Tambah=.t.
         Do kolomlgn
         Do pesan with 'ESC=Selesai'
         @8,24 get ksp pict '@!'
         Read
         @24,0
         If ksp=SPACE(10) .or. lastkey()=27
            Exit
         ENDIF
         Ksp=right(RTRIM(ksp),pdt)+SPACE(10-pdt)
         SET COLOR TO w+/b
         @8,24 say ksp+'  '
         Seek ksp
         If FOUND()
            Ksp =kode_lgn
            Nsp =nama_lgn
            Algn=almt_lgn
            Tlp =telp_lgn
            Kota=kota_lgn
            Hjx =jens_lgn
            Tglz=tanggal
            Tot =rp_saldo
            Dis =diskon
            Ket1=left(keterangan,50)
            Ket2=right(keterangan,50)
            Tambah=.f.
         Else
            Kota=SPACE(10)
            STORE SPACE(20) to tlp
            STORE SPACE(30) to nsp,algn,an
            STORE SPACE(50) to ket1,ket2
            STORE 0 to tot,plf,jwk,dis
            Hjx=' '
         ENDIF
         *
         SET COLOR TO w+/b
         @9,24  get nsp  pict '@!'
         @10,24 get algn pict '@!'
         @11,24 get tlp  pict '@!'
         @12,24 get kota pict '@!'
         @12,65 get dis  pict '@e  99.99'
         @13,24 get ket1 pict '@S47'
         @14,24 get ket2 pict '@S47'
         @20,44 get tglz
         @21,44 get tot pict '@e   999999999'
         Do pesan with 'Masukan Nama, dll'
         Read
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         ENDIF
         If tambah
            Flok()
            APPEND BLANK
         Else
            Rlok()
         ENDIF
         REPLACE kode_lgn with ksp,nama_lgn with nsp,almt_lgn with algn,telp_lgn with tlp,kota_lgn with kota,;
              jens_lgn with iif(hjx#'1',' ',hjx),diskon with dis,tanggal with tglz,rp_saldo with tot,keterangan with ket1+ket2
         Ul()
         COMMIT
         *
         Do hitlgn
         Ksp=ksp+SPACE(5)
         Kota=SPACE(10)
         STORE SPACE(20) to tlp
         STORE SPACE(30) to nsp,algn
         STORE SPACE(50) to ket1,ket2
         STORE 0 to tot,plf,jwk,dis,ajp,amp
         Hjx=' '
         SELECT 3
      ENDDO
   ENDIF
   Rest scre from slgn
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-4 .and. ( fds='KODE_LGN' .or. fds='SO_POINT' )
   B=row()
   K=col()
   @24,0
   SET CURSOR ON
   Rlok()
   @b,k get &fds pict '@!'
   Read
   Ul()
   COMMIT
   SET CURSOR OFF
   Do pesan with pesany
   RETURN 1
Elseif lastkey()=-7
   Ksp=kode_lgn
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
      ENDIF
   ENDIF
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-9
   SET CURSOR ON
   Sambung='Y'
   Do pesan with 'Pakai Kertas Bersambung? Y/T: '
   @24,55 get sambung pict '!'
   Read
   If sambung#'Y'
      Sambung='T'
   ENDIF
   STORE 1 to n,no,hal
   Do cp
   If lastkey()=27
      Do pesan with pesany
      SET CURSOR OFF
      RETURN 1
   ENDIF
   Do koplgn
   DO WHILE .not. eof()
      ?tran(no,'999.  ')+kode_lgn+'  '+nama_lgn+'  '+left(almt_lgn,30)+' '+left(telp_lgn,20)
      No =no+1
      N  =n+1
      If n>60
         ?REPLACE('-',121)
         ?' '
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
         Hal=hal+1
         Do koplgn
      ENDIF
     Skip
   ENDDO
   ?REPLACE('-',121)
   ?''
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 1
Elseif lastkey()=72 .or. lastkey()=104
   Nr=recno()
   Do pesan with 'Tunggu, sedang diHitung'
   Coun to jml
   @1,63 say 'Member: '+LTRIM(tran(jml,'@e 99,999'))
   Do pesan with pesany
   SET CURSOR OFF
   Go nr
   RETURN 1
Elseif lastkey()=390
   SET CURSOR ON
   Yt =' '
   Do pesan with 'Jumlah Poin akan diUpdate dari Nota Transaksi.   Proses? Y/T : '
   @24,70 get yt pict '!'
   Read
   If lastkey()#27 .or. yt='Y'
      SELECT 2
      Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
      *
      SELECT 3
      Go top
      Jdt=reccount()
      Awl=1
      DO WHILE .not. eof()
         Ksp=kode_lgn
         Idp=jens_lgn
         STORE 0 to jml,tot,pnskr
         Do pesan with 'Proses Member: '+RTRIM(ksp)+'     '+LTRIM(str(awl))+'/'+LTRIM(str(jdt))
         *
         SELECT 2
         Seek ksp
         DO WHILE .not. eof() .and. kode_lgn=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml=jml+jml_poin
               Tot=tot+(rp_total-rp_diskon)
            ENDIF
            Skip
         ENDDO
         Pnskr=jml
         *
         SELECT 3
         Rlok()
         REPLACE so_point with iif(idp=' ',pnskr,0)   &&&IDP=' ' BRARTI MEMBER
         Ul()
         Awl=awl+1
         Skip
         *
      ENDDO
      COMMIT
   ENDIF
   *
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=389
   SET COLOR TO gr+*
   @row(),col() say &fds
   SET COLOR TO g
   Nr=recno()
   Save scre to sdaf
   SET COLOR TO gr+
   @1,0 CLEAR to 23,79
   @1,0 to 1,79
   @23,0 to 23,79
   @1,27 say 'Kode Pelanggan yg Doubel'
   SET COLOR TO w
   Set order to 1
   Go top
   Kdbx=kode_lgn
   B   =2
   Awal=1
   DO WHILE .not. eof()
      If kode_lgn=kdbx .and. awal>1
         Skip -1
         @b,0 say kode_lgn+' '+nama_lgn+' '+almt_lgn
         B=b+1
         Skip
         @b,0 say kode_lgn+' '+nama_lgn+' '+almt_lgn
         B=b+1
      ENDIF
      Kdbx=kode_lgn
      Awal=awal+1
      If b>21
         Do pesan with 'Tekan sembarang tombol, unt hal berikut...'
         Wait''
         SET COLOR TO w
         B=2
      ENDIF
      Skip
   ENDDO
   Wait''
   Rest scre from sdaf
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-8
   Set alte on
   Set alte to DOBEL.CSV
   Set order to 1
   Go top
   Kdbx=kode_lgn
   Awal=1
   DO WHILE .not. eof()
      If kode_lgn=kdbx .and. awal>1
         Skip -1
         ?kode_lgn+';'+nama_lgn+';'+almt_lgn+';'+kota_lgn+';'+telp_lgn
         Skip
         ?kode_lgn+';'+nama_lgn+';'+almt_lgn+';'+kota_lgn+';'+telp_lgn
      ENDIF
      Kdbx=kode_lgn
      Awal=awal+1
      Skip
   ENDDO
   Set alte off
   Set alte to
   Rest scre from sdaf
   Do pesan with pesany
   SET CURSOR OFF
   RETURN 2
Else
   Set filter to
   RETURN 2
ENDIF
RETURN 0

PROCEDURE koplgn
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 5
?chr(27)+chr(33)+chr(4)
?' DAFTAR PELANGGAN'+SPACE(95)+'Hal:'+tran(hal,'999')
?REPLACE('=',121)
?' No.  Kode        Nama Pelanggan                  Alamat                         Telp.'
?REPLACE('-',121)
N=1
RETURN

PROCEDURE kolomlgn
SET COLOR TO w+/br
@7,10 CLEAR to 19,71
@7,10 to 19,71
@8,11  say '       Kode: '+SPACE(10)
@9,11  say '       Nama: '+SPACE(30)
@10,11 say '     Alamat: '+SPACE(37)
@11,11 say '     Telpon: '         
@12,11 say '       Kota: '+SPACE(30)+'Diskon %:'
@13,11 say '    Catatan:'
@16,11 say 'Poin Adjust:           Terakhir Ambil:        dari:'
@17,11 say ' - Berjalan:                Tgl.Ambil:'
@18,11 say 'Poin Total.:                  Catatan:'
If xOwner
   @19,10 CLEAR to 22,71
   @19,10 to 22,71
   @20,15 say 'Saldo Piutang:   Tanggal...: '+dtoc(tglz)
   @21,15 say '--------------   Saldo Awal: '+tran(tot,'@e 999,999,999')
ENDIF
RETURN

PROCEDURE hitlgn
Ksp=val(right(RTRIM(ksp),len(RTRIM(ksp))-len(idt)))+1
Ksp=idt+right('000000'+LTRIM(str(ksp)),pdt-len(idt))+SPACE(10-pdt)
RETURN

* EOF LGN
