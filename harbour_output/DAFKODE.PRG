// ============================================================
// File    : DAFKODE.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* PROSEDUR DAFTAR SELURUH KODE

PROCEDURE dafklb
Set key 28 to
Save scre to sdaf
SET COLOR TO w+/br
@1,43 CLEAR to 22,79
@1,43 to 22,79
@1,43 say chr(218)+REPLACE(gl,35)+chr(191)
@3,43 say chr(195)
@3,79 say chr(180)
DECLARE field[2],kop[2],bts[2]
If klp
   Field[1]='NAMA_KLB+tl+KODE_KLB'
   Kop[1]='Nama Jenis/Kelompok            Kode'
Else
   Field[1]='NAMA+tl+KODE'
   Kop[1]='Nama Lokasi/Rak                Kd.'
ENDIF
Bts[1]=''
DbEdit(2,44,21,78,field,'fkl','',kop,gl,bts)
*
Set order to 1
Rest scre from sdaf
SET COLOR TO /bg
SET CURSOR ON
RETURN

Function fkl
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=13
   If klp
      Klb =kode_klb
      Nklb=nama_klb
   Else
      Rak=kode
   ENDIF
   RETURN 0
Elseif (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
   Set order to 2
   Seek uppe(chr(lastkey()))
   RETURN 2
Else
   RETURN 1
ENDIF
RETURN 0

PROCEDURE dafbrg
Nmb =iif(carikode,alltrim(kdb),alltrim(nmb))
Set filter to AT(nmb,iif(carikode,kode_bar,nama_brg))>0
Go top
Save scre to sdaf
Do pesan with 'ENTER=Pilih     ESC=Batal'
SET COLOR TO w+/br
@1,8 CLEAR to 22,79
@1,8 to 1,79
@23,8 to 23,79
DECLARE field[6],kop[6],frm[6],bts[6]
Field[1]="KODE_BAR+tl+left(NAMA_BRG,35)+tl+tran(HARGA,'@E 9,999,999')+tl+tran((STOK_A+STOK_M)-(STOK_K+STOK_RB),'@e 999999 ')+SATUAN"
Kop[1]='BarCode       Nama Barang                            @Pokok   Stok'
Frm[1]=''
Bts[1]=''
DbEdit(2,9,22,79,field,'xkdb',frm,kop,gl,bts)
*
Rest scre from sdaf
SET COLOR TO /bg
SET CURSOR ON
RETURN

Function xkdb
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Kdb =kode_brg
      Kdbx=kode_bar
      Nmb =nama_brg
      RETURN 0
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE dafspl
Set key 28 to
*Set filter to
*If ksp#spac(5)
*   Set filter to AT(rtri(ksp),nama_spl)>0
*Endi
Go top
Save scre to sdaf
SET COLOR TO w+/br
@1,47 CLEAR to 22,79
@1,47 to 22,79
@1,47 say chr(218)+REPLACE(gl,31)+chr(191)
DECLARE field[2],kop[2],bts[2]
Field[1]='left(NAMA_SPL,25)+tl+KODE_SPL'
Kop[1]='Nama Supplier             Kode'
Bts[1]=''
DbEdit(2,48,21,78,field,'fspl','',kop,gl,bts)
*
Set filter to
Set order to 1
Rest scre from sdaf
SET COLOR TO /bg
SET CURSOR ON
RETURN

Function fspl
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      STORE kode_spl to ksp,kspx
      *Keyboard chr(13)
      RETURN 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
      If fds='KODE_SPL'
         Set order to 1
         Seek uppe(chr(lastkey()))
      Elseif fds='left(NAMA_SPL,25)'
         Set order to 2
         Seek uppe(chr(lastkey()))
      ENDIF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE daflgn
Save scre to sdaf
Do pesan with 'Cari Kode/Nama Pelanggan, tekan A --> Z  atau  0 --> 9'
SET COLOR TO w+/br
@1,8 CLEAR to 22,79
@1,8 to 22,79
@1,8 say chr(218)+REPLACE(gl,70)+chr(191)
DECLARE field[5],kop[5],bts[5]
Field[1]='NAMA_LGN'
Field[2]='JENS_LGN'
Field[3]='KODE_LGN'
Field[4]='left(ALMT_LGN,25)'
Field[5]='KOTA_LGN'
Kop[1]='Nama Pelanggan'
Kop[2]=' '
Kop[3]='Kode'
Kop[4]='Alamat'
Kop[5]='Kota'
Bts[1]=''
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
DbEdit(2,9,21,78,field,'flgn','',kop,gl,bts)
*
Set order to 1
Rest scre from sdaf
SET COLOR TO /bg
SET CURSOR ON
RETURN

Function flgn
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Ksp=kode_lgn
      RETURN 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
      If fds='KODE_LGN'
         Set order to 1
         Seek uppe(chr(lastkey()))
      Elseif fds='NAMA_LGN'
         Set order to 2
         Seek uppe(chr(lastkey()))
      Elseif fds='left(ALMT_LGN,25)'
         Set order to 2
         Seek uppe(chr(lastkey()))
      ENDIF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE dafksr
Set key 28 to
Save scre to sdaf
SET COLOR TO w+/br
@1,55 CLEAR to 22,79
@1,55 to 22,79
@1,55 say chr(218)+REPLACE(gl,23)+chr(191)
@3,55 say chr(195)
@3,79 say chr(180)
DECLARE field[2],kop[2],bts[2]
Field[1]="NOMER+tl+NAMA"
Kop[1]='No.Nama Kasir'
Bts[1]=''
DbEdit(2,56,21,78,field,'fksr','',kop,gl,bts)
*
Rest scre from sdaf
SET COLOR TO /bg
SET CURSOR ON
RETURN

Function fksr
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Nsls=nama
      RETURN 0
   Othe
      RETURN 1
ENDCASE
RETURN 0

* EOF DAFKODE
