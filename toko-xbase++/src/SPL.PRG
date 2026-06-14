Procedure SPL
* DAFTAR SUPPLIER
Sele 1
Uselan('DATA.SET','','','','','')
Sele 2
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Go top
Tglso=tanggal_2
*
Stor spac(5) to ksp,kota
Stor spac(25) to nsls,algn
Stor spac(33) to ket1,ket2
Stor 0 to tot,jwk
Telp=spac(14)
Tgl2=tglx
Do While .t.
   Stor .f. to hapus,adadis
   Sele 2
   Set order to 1
   @1,0 clea to 24,79
   @23,0 to 23,79
   @1,0 say repl(gl,5)+chr(194)+repl(gl,26)+chr(194)+repl(gl,26)+chr(194)+repl(gl,14)+chr(194)+repl(gl,5)
   @2,0 say 'Kode '+tl+'Nama Supplier             '+tl+'Alamat'+spac(20)+tl+'Telp.'+spac(9)+tl+'Kota '
   @3,0 say repl(gl,5)+chr(197)+repl(gl,26)+chr(197)+repl(gl,26)+chr(197)+repl(gl,14)+chr(197)+repl(gl,5)
   Pesanx='F1=Nama     F4=Tambah     F8=Hapus     F10=Cetak     F3=Tgl.Hitung'
   Do pesan with pesanx
   Declare field[6],bts[6],frm[6]
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
   Dbedit(4,0,22,79,field,'fsplx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
EndIf
EndDo
Set curs off
Clos all
Spl=.f.
Return

Return

Function fsplx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=28
      Nr=recno()
      Nsls=spac(30)
      Set curs on
      Do pesan with 'Nama Supplier:'+spac(25)
      @24,34 get nsls pict '@!'
      Read
      Set colo to g
      If lastkey()#27
         Set order to 2
         Nsls=rtri(nsls)
         Seek nsls
         If .not. foun()
            Go nr
EndIf
EndIf
      Do pesan with pesanx
      Set curs off
Return 2
   Case lastkey()=13
      @24,0
      Set curs on
      Ksp=kode_spl
      Set order to 1
      Do lang
      Set curs off
      Do pesan with pesanx
Return 2
   Case lastkey()=-3
      @24,0
      Set curs on
      Set order to 1
      Do mspl
      Set curs off
      Do pesan with pesanx
Return 2
   Case lastkey()=-7
      Ksp=kode_spl
      Set colo to gr+*
      @row(),col() say &fds
      Yt=' '
      Set curs on
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
Do While .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
EndDo
      If yt='Y'
         If .not. deleted()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
EndIf
EndIf
      Do pesan with pesanx
      Set curs off
Return 2
   Case lastkey()=-9
      Stor 1 to n,no,hal
      Tot=0
      Set curs on
      Do cp
      If cc=0
Return 0
EndIf
      set alte on
      set alte to spl.txt
      Do kopspl
Do While .not. eof()
         ?tran(no,'999.  ')+kode_spl+'  '+nama_spl+'  '+almt_spl+'  '+telp_spl+'  '+kota_spl
         No =no+1
         N  =n+1
         If n>60
            ?repl('-',91)
            ?''
            Set prin off
            Set devi to scre
            Do pesan3 with gtkertas
            If nlp=0
               Cc=0
               Exit
EndIf
            Do cp
            If cc=0
               Exit
EndIf
            Hal=hal+1
            Do kopspl
EndIf
         Skip
EndDo
      ?repl('-',91)
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
Return 1
   Case lastkey()=-2
      Nr =recno()
      Set curs on
      Do pesan with 'Tanggal Mulai Hitung Saldo Hutang:'+spac(10)
      @24,53 get tglso
      Read
      Set colo to g
      If lastkey()#27
         Go top
         Rlok()
         Repl tanggal_2 with tglso
         Ul()
         Commit
         Go nr
EndIf
      Do pesan with pesanx
      Set curs off
Return 2
   Othe
Return 1
Endc
Return 0
Procedure kopspl
Set prin on
Set devi to prin
Set marg to 20
?chr(27)+chr(33)+chr(4)
?' DAFTAR SUPPLIER'+spac(67)+'Hal:'+tran(hal,'999')
?repl('=',91)
?' No.  Kode   Nama Supplier              Alamat                     Telp.           Kota'
?repl('-',91)
N=1
Return
Procedure mspl
Save scre to smkdb
Stor spac(30) to nsls,algn
Stor spac(33) to ket1,ket2
Stor 0 to tot,jwk
Telp=spac(14)
If .not. hapus
   Go bott
   Ksp=kode_spl
   Do hitksp
EndIf
Do While .t.
   Tgl2=ctod('01-'+ltri(str(mont(tglx)))+'-'+ltri(str(year(tglx))))
   Tambah=.t.
   Do kopspl2
   Set colo to bg+/b
   @12,32 say ksp
   If hapus
      @12,32 say ksp
      Seek ksp
   Else
      @12,32 get ksp pict '@!'
      @24,0
      Read
      @24,0
      If ksp=spac(5) .or. lastkey()=27
         Exit
EndIf
      Seek ksp
EndIf
   If foun()
      Do adaspl
      Tambah=.f.
   Else
      Stor spac(30) to nsls,algn
      Stor spac(33) to ket1,ket2
      Kota=spac(5)
      Telp=spac(14)
      Tot=0
      Jwk=jk_waktu
EndIf
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
EndIf
   If tgl2=ctod('  -  -    ')
      Tgl2=tglx
EndIf
   If lastkey()=27
      If hapus
         Hapus=.f.
         Exit
EndIf
EndIf
   *
   Yt='Y'
   Do pesan with 'Data Benar? Y/T :'
   @24,49 get yt pict '!'
   Read
   If lastkey()=27 .or. yt#'Y'
      Loop
EndIf
   If tambah
      Flok()
      Appe blan
      Repl kode_spl with ksp,nama_spl with nsls,almt_spl with algn,telp_spl with telp,;
           kota_spl with kota,tanggal with tgl2,rp_saldo with tot,jk_waktu with jwk,keterangan with ket1+ket2
   Else
      Rlok()
      Repl kode_spl with ksp,nama_spl with nsls,almt_spl with algn,telp_spl with telp,;
           kota_spl with kota,tanggal with tgl2,rp_saldo with tot,jk_waktu with jwk,keterangan with ket1+ket2
EndIf
   Ul()
   Commit
   *
   If hapus
      Hapus=.f.
      Exit
EndIf
   Do hitksp
EndDo
Rest scre from smkdb
Return
Procedure lang
Save scre to smkdb
Do While .t.
   Do kopspl2
   Do adaspl
   Set colo to /w
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
EndIf
   Do pesan with 'ENTER=Edit'
   Wait''
   If lastkey()=13
      Rest scre from smkdb
      Hapus=.t.
      Do mspl
EndIf
   Exit                      
EndDo
Rest scre from smkdb
Return
Procedure kopspl2
Set colo to w+/br
@11,15 clea to 23,65
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
EndIf
Return
Procedure adaspl
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
Return
Procedure hitksp
Ksp=val(rtri(ksp))+1
Ksp=right('00000'+ltri(str(ksp)),iif(spl,5,5))
Return

* EOF SPL
