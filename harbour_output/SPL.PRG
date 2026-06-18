// ============================================================
// File    : SPL.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* DAFTAR SUPPLIER
SELECT 1
Uselan('DATA.SET','','','','','')
SELECT 2
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Go top
Tglso=tanggal_2
*
STORE SPACE(5) to ksp,kota
STORE SPACE(25) to nsls,algn
STORE SPACE(33) to ket1,ket2
STORE 0 to tot,jwk
Telp=SPACE(14)
Tgl2=tglx
DO WHILE .t.
   STORE .f. to hapus,adadis
   SELECT 2
   Set order to 1
   @1,0 CLEAR to 24,79
   @23,0 to 23,79
   @1,0 say REPLACE(gl,5)+chr(194)+REPLACE(gl,26)+chr(194)+REPLACE(gl,26)+chr(194)+REPLACE(gl,14)+chr(194)+REPLACE(gl,5)
   @2,0 say 'Kode '+tl+'Nama Supplier             '+tl+'Alamat'+SPACE(20)+tl+'Telp.'+SPACE(9)+tl+'Kota '
   @3,0 say REPLACE(gl,5)+chr(197)+REPLACE(gl,26)+chr(197)+REPLACE(gl,26)+chr(197)+REPLACE(gl,14)+chr(197)+REPLACE(gl,5)
   Pesanx='F1=Nama     F4=Tambah     F8=Hapus     F10=Cetak     F3=Tgl.Hitung'
   Do pesan with pesanx
   DECLARE field[6],bts[6],frm[6]
   Field[1]='KODE_SPL'
   Field[2]='left(NAMA_SPL,26)'
   Field[3]='left(ALMT_SPL,26)'
   Field[4]='TELP_SPL'
   Field[5]='KOTA_SPL'
   Field[6]='JK_WAKTU'
   Frm[1]='@!'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@!'
   Frm[6]='999'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl
   Bts[4]=tl
   Bts[5]=tl
   Bts[6]=tl
   DbEdit(4,0,22,79,field,'fsplx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   ENDIF
ENDDO
SET CURSOR OFF
CLOSE all
Spl=.f.
RETURN

Function fsplx
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
      Nsls=SPACE(30)
      SET CURSOR ON
      Do pesan with 'Nama Supplier:'+SPACE(25)
      @24,34 get nsls pict '@!'
      Read
      SET COLOR TO g
      If lastkey()#27
         Set order to 2
         Nsls=RTRIM(nsls)
         Seek nsls
         If .not. FOUND()
            Go nr
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=13
      @24,0
      SET CURSOR ON
      Ksp=kode_spl
      Set order to 1
      Do lang
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=-3
      @24,0
      SET CURSOR ON
      Set order to 1
      Do mspl
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=-7
      Ksp=kode_spl
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
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=-9
      STORE 1 to n,no,hal
      Tot=0
      SET CURSOR ON
      Do cp
      If cc=0
         RETURN 0
      ENDIF
      set alte on
      set alte to spl.txt
      Do kopspl
      DO WHILE .not. eof()
         ?tran(no,'999.  ')+kode_spl+'  '+nama_spl+'  '+almt_spl+'  '+telp_spl+'  '+kota_spl
         No =no+1
         N  =n+1
         If n>60
            ?REPLACE('-',91)
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
            Do kopspl
         ENDIF
         Skip
      ENDDO
      ?REPLACE('-',91)
      ?''
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
   Case lastkey()=-2
      Nr =recno()
      SET CURSOR ON
      Do pesan with 'Tanggal Mulai Hitung Saldo Hutang:'+SPACE(10)
      @24,53 get tglso
      Read
      SET COLOR TO g
      If lastkey()#27
         Go top
         Rlok()
         REPLACE tanggal_2 with tglso
         Ul()
         COMMIT
         Go nr
      ENDIF
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE kopspl
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR SUPPLIER'+SPACE(67)+'Hal:'+tran(hal,'999')
?REPLACE('=',91)
?' No.  Kode   Nama Supplier              Alamat                     Telp.           Kota'
?REPLACE('-',91)
N=1
RETURN

PROCEDURE mspl
Save scre to smkdb
STORE SPACE(30) to nsls,algn
STORE SPACE(33) to ket1,ket2
STORE 0 to tot,jwk
Telp=SPACE(14)
If .not. hapus
   Go bott
   Ksp=kode_spl
   Do hitksp
ENDIF
DO WHILE .t.
   Tgl2=ctod('01-'+LTRIM(str(mont(tglx)))+'-'+LTRIM(str(year(tglx))))
   Tambah=.t.
   Do kopspl2
   SET COLOR TO bg+/b
   @12,32 say ksp
   If hapus
      @12,32 say ksp
      Seek ksp
   Else
      @12,32 get ksp pict '@!'
      @24,0
      Read
      @24,0
      If ksp=SPACE(5) .or. lastkey()=27
         Exit
      ENDIF
      Seek ksp
   ENDIF
   If FOUND()
      Do adaspl
      Tambah=.f.
   Else
      STORE SPACE(30) to nsls,algn
      STORE SPACE(33) to ket1,ket2
      Kota=SPACE(5)
      Telp=SPACE(14)
      Tot=0
      Jwk=jk_waktu
   ENDIF
   Set key -4 to xadadis
   Do pesan with 'F5=Edit Saldo Hutang'
   @13,32  get nsls pict '@!'
   @14,32  get algn pict '@!'
   @15,32 get telp
   @16,32 get kota pict '@!'
   @17,32 get jwk pict '999'
   @18,32 get ket1 pict '@S33'
   @19,32 get ket2 pict '@S33'
   Read
   Set key -4 to
   If adadis .and. ( Owner .or. xOwner )
      Adadis=.f.
      @21,54 get tgl2
      @22,54 get tot  pict '@e 999,999,999'
      Read
   ENDIF
   If tgl2=ctod('  -  -    ')
      Tgl2=tglx
   ENDIF
   If lastkey()=27
      If hapus
         Hapus=.f.
         Exit
      ENDIF
   ENDIF
   *
   Yt='Y'
   Do pesan with 'Data Benar? Y/T :'
   @24,49 get yt pict '!'
   Read
   If lastkey()=27 .or. yt#'Y'
      Loop
   ENDIF
   If tambah
      Flok()
      APPEND BLANK
      REPLACE kode_spl with ksp,nama_spl with nsls,almt_spl with algn,telp_spl with telp,;
           kota_spl with kota,tanggal with tgl2,rp_saldo with tot,jk_waktu with jwk,keterangan with ket1+ket2
   Else
      Rlok()
      REPLACE kode_spl with ksp,nama_spl with nsls,almt_spl with algn,telp_spl with telp,;
           kota_spl with kota,tanggal with tgl2,rp_saldo with tot,jk_waktu with jwk,keterangan with ket1+ket2
   ENDIF
   Ul()
   COMMIT
   *
   If hapus
      Hapus=.f.
      Exit
   ENDIF
   Do hitksp
ENDDO
Rest scre from smkdb
RETURN

PROCEDURE lang
Save scre to smkdb
DO WHILE .t.
   Do kopspl2
   Do adaspl
   SET COLOR TO /w
   @12,32  say ksp
   @13,32  say nsls
   @14,32  say algn
   @15,32 say telp
   @16,32 say kota
   @17,32 say tran(jwk,'999')
   @18,32 say ket1
   @19,32 say ket2
   If xOwner
      @21,54 say tgl2
      @22,54 say tran(tot ,'@e 999,999,999')
   ENDIF
   Do pesan with 'ENTER=Edit'
   Wait''
   If lastkey()=13
      Rest scre from smkdb
      Hapus=.t.
      Do mspl
   ENDIF
   Exit                      
ENDDO
Rest scre from smkdb
RETURN

PROCEDURE kopspl2
SET COLOR TO w+/br
@11,15 CLEAR to 23,65
@11,15 to 23,65
@12,17  say '         Kode:'
@13,17  say 'Nama Supplier:'
@14,17  say '       Alamat:'
@15,17 say '        Telp.:'
@16,17 say '         Kota:'
@17,17 say '     Jk.Waktu:     hari'
@18,17 say '   Keterangan:'
If xOwner
   @20,16 to 20,64
   @21,16 say 'Mulai Hitung Saldo Hutang, Tanggal...:'
   @22,16 say '-------------------------- Saldo Awal:'
ENDIF
RETURN

PROCEDURE adaspl
Ksp =kode_spl
Nsls=nama_spl
Algn=almt_spl
Kota=kota_spl
Telp=telp_spl
Tgl2=tanggal
Tot =rp_saldo
Jwk =jk_waktu
Ket1=left(keterangan,33)
Ket2=right(keterangan,33)
RETURN

PROCEDURE hitksp
Ksp=val(RTRIM(ksp))+1
Ksp=right('00000'+LTRIM(str(ksp)),iif(spl,5,5))
RETURN

* EOF SPL
