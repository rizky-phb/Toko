* PROSEDUR DAFTAR SELURUH KODE
Procedure dafklb
Set key 28 to
Save scre to sdaf
Set colo to w+/br
@1,43 clea to 22,79
@1,43 to 22,79
@1,43 say chr(218)+repl(gl,35)+chr(191)
@3,43 say chr(195)
@3,79 say chr(180)
Declare field[2],kop[2],bts[2]
If klp
   Field[1]='NAMA_KLB+tl+KODE_KLB'
   Kop[1]='Nama Jenis/Kelompok            Kode'
Else
   Field[1]='NAMA+tl+KODE'
   Kop[1]='Nama Lokasi/Rak                Kd.'
EndIf
Bts[1]=''
Dbedit(2,44,21,78,field,'fkl','',kop,gl,bts)
*
Set order to 1
Rest scre from sdaf
Set colo to /bg
Set curs on
Return

Function fkl
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
Return 1
Elseif lastkey()=27
Return 0
Elseif lastkey()=13
   If klp
      Klb =kode_klb
      Nklb=nama_klb
   Else
      Rak=kode
EndIf
Return 0
Elseif (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
   Set order to 2
   Seek uppe(chr(lastkey()))
Return 2
Else
Return 1
EndIf
Return 0
Procedure dafbrg
Nmb =iif(carikode,alltrim(kdb),alltrim(nmb))
Set filter to AT(nmb,iif(carikode,kode_bar,nama_brg))>0
Go top
Save scre to sdaf
Do pesan with 'ENTER=Pilih     ESC=Batal'
Set colo to w+/br
@1,8 clea to 22,79
@1,8 to 1,79
@23,8 to 23,79
Declare field[6],kop[6],frm[6],bts[6]
Field[1]="KODE_BAR+tl+left(NAMA_BRG,35)+tl+tran(HARGA,'@E 9,999,999')+tl+tran((STOK_A+STOK_M)-(STOK_K+STOK_RB),'@e 999999 ')+SATUAN"
Kop[1]='BarCode       Nama Barang                            @Pokok   Stok'
Frm[1]=''
Bts[1]=''
Dbedit(2,9,22,79,field,'xkdb',frm,kop,gl,bts)
*
Rest scre from sdaf
Set colo to /bg
Set curs on
Return

Function xkdb
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=13
      Kdb =kode_brg
      Kdbx=kode_bar
      Nmb =nama_brg
Return 0
   Othe
Return 1
Endc
Return 0
Procedure dafspl
Set key 28 to
*Set filter to
*If ksp#spac(5)
*   Set filter to AT(rtri(ksp),nama_spl)>0
*Endi
Go top
Save scre to sdaf
Set colo to w+/br
@1,47 clea to 22,79
@1,47 to 22,79
@1,47 say chr(218)+repl(gl,31)+chr(191)
Declare field[2],kop[2],bts[2]
Field[1]='left(NAMA_SPL,25)+tl+KODE_SPL'
Kop[1]='Nama Supplier             Kode'
Bts[1]=''
Dbedit(2,48,21,78,field,'fspl','',kop,gl,bts)
*
Set filter to
Set order to 1
Rest scre from sdaf
Set colo to /bg
Set curs on
Return

Function fspl
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=13
      Stor kode_spl to ksp,kspx
      *Keyboard chr(13)
Return 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
      If fds='KODE_SPL'
         Set order to 1
         Seek uppe(chr(lastkey()))
      Elseif fds='left(NAMA_SPL,25)'
         Set order to 2
         Seek uppe(chr(lastkey()))
EndIf
Return 2
   Othe
Return 1
Endc
Return 0
Procedure daflgn
Save scre to sdaf
Do pesan with 'Cari Kode/Nama Pelanggan, tekan A --> Z  atau  0 --> 9'
Set colo to w+/br
@1,8 clea to 22,79
@1,8 to 22,79
@1,8 say chr(218)+repl(gl,70)+chr(191)
Declare field[5],kop[5],bts[5]
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
Dbedit(2,9,21,78,field,'flgn','',kop,gl,bts)
*
Set order to 1
Rest scre from sdaf
Set colo to /bg
Set curs on
Return

Function flgn
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=13
      Ksp=kode_lgn
Return 0
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
EndIf
Return 2
   Othe
Return 1
Endc
Return 0
Procedure dafksr
Set key 28 to
Save scre to sdaf
Set colo to w+/br
@1,55 clea to 22,79
@1,55 to 22,79
@1,55 say chr(218)+repl(gl,23)+chr(191)
@3,55 say chr(195)
@3,79 say chr(180)
Declare field[2],kop[2],bts[2]
Field[1]="NOMER+tl+NAMA"
Kop[1]='No.Nama Kasir'
Bts[1]=''
Dbedit(2,56,21,78,field,'fksr','',kop,gl,bts)
*
Rest scre from sdaf
Set colo to /bg
Set curs on
Return

Function fksr
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=13
      Nsls=nama
Return 0
   Othe
Return 1
Endc
Return 0

* EOF DAFKODE
