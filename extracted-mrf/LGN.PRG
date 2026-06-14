* DAFTAR PELANGGAN
*------------------
Sele 1
Uselan('DATA.SET','','','','','')
Idt =rtri(id_cust)
Pdt =pj_cust
Tgl =tanggal_2
Tgl2=tanggal_3
Tglz=tglx
Close
*
Sele 2
*
Sele 3
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
*
Stor spac(10) to ksp
Stor .f. to adadis,hapus
Do whil .t.
   Sambung='T'
   Kota=spac(5)
   Stor spac(20) to tlp
   Stor spac(30) to nsp,algn
   Stor spac(50) to ket1,ket2
   Stor 0 to tot,plf,jwk,dis
   *
   Sele 3
   @1,0 clea to 23,79
   @23,0 to 23,79
   @1,0 say repl(gl,10)+repl(chr(194)+repl(gl,25),2)+chr(194)+repl(gl,10)+chr(194)+repl(gl,6)
   @2,0 say 'Kode      '+tl+'Nama Pelanggan           '+tl+'A l a m a t'+spac(14)+tl+'Kota      '+tl+'Adjust'
   @3,0 say repl(gl,10)+repl(chr(197)+repl(gl,25),2)+chr(197)+repl(gl,10)+chr(197)+repl(gl,6)
   Pesany='F1/F2=Cari    F4=Tambah    F8=Hapus    F10=Cetak    ENTER=Edit    H=Hit.Member'
   Do pesan with pesany
   Declare field[8],bts[8],frm[8]
   Field[1]='KODE_LGN'
   Field[2]='left(NAMA_LGN,25)'
   Field[3]='left(ALMT_LGN,25)'
   Field[4]='left(KOTA_LGN,10)'
   Field[5]='AJ_POINT'
   Frm[5]='999999'
   Stor '@!' to frm[1],frm[2],frm[3],frm[4]
   Stor tl   to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
   Dbedit(4,0,22,79,field,'flgnx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   Endi
Endd
Set curs off
Clos all
Rele ksp,nsp,algn,tlp,kota,tgl2,tglz,tot,spl,fds,nsp,adadis,hapus,dis
Retu

Function flgnx
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=28
   Nr=recno()
   Nsp=spac(25)
   Set curs on
   Do pesan with 'Nama Pelanggan:'+spac(25)
   @24,35 get nsp pict '@!'
   Read
   Set order to 2
   Nsp=rtri(nsp)
   Set colo to g
   Seek nsp
   If .not. foun()
      Go nr
   Endi
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=-1
   Nr=recno()
   Ksp=spac(11)
   Set curs on
   Do pesan with 'Kode Pelanggan:'+spac(11)
   @24,43 get ksp pict '@!'
   Read
   Set order to 1
   Ksp=idt+right('000000'+rtri(ksp),pdt-len(idt))+spac(10-pdt)
   Set colo to g
   Seek ksp
   If .not. foun()
      Go nr
   Endi
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=-3 .or. lastkey()=13
   Save scre to slgn
   Set curs on
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
      Do whil .t.
         Set colo to /w
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
         @16,63 say ltri(tran(sop,'@e 999,999'))
         @17,24 say tran(totpn,'@e 999,999')
         @17,49 say dtoc(tglhp)+' / '+jamx
         @18,49 say rtri(ket)+iif(rpu#0,' Rp '+ltri(tran(rpu,'@e 99,999,999')),'')
         *
         Do pesan with 'ESC=Selesai     ENTER=Edit     H=Hitung Point'
         Wait''
         If lastkey()=13
            Set colo to w+/b
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
                  Repl aj_point with ajp,am_point with amp,tgl_ambil with tglhp,jam_ambil with jamx,ket_ambil with ket
                  Ul()
                  Commit
               Endi
            Endi
            If hapus
               Hapus=.f.
               @20,44 get tglz
               @21,44 get tot pict '@e 999,999,999'
               Do pesan with 'Masukan Tanggal & Saldo Piutang yg terSisa'
               Read
               Do pesan with 'ENTER=Simpan     ESC=Batal'
               Wait''
            Endi
            *
            If lastkey()#27
               Rlok()
               Repl kode_lgn with ksp,nama_lgn with nsp,almt_lgn with algn,telp_lgn with tlp,kota_lgn with kota,jens_lgn with iif(hjx#'1',' ',hjx),;
                    diskon with dis,rp_saldo with tot,tanggal with tglz,keterangan with ket1+ket2
               Ul()
               Commit
            Endi
            Exit
         Elseif lastkey()=72 .or. lastkey()=104
            Do pesan with 'Tunggu, sedang diHitung'
            Totpn=0
            *
            Sele 99
            Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
            Seek ksp
            Do whil .not. eof() .and. kode_lgn=ksp
               If tanggal=tglhp .and. jam>jamx
                  Totpn=totpn+jml_poin
               Elseif tanggal>tglhp
                  Totpn=totpn+jml_poin
               Endi
               Skip
            Endd
            Set colo to /w
            @17,24 say tran(totpn,'@e 999,999')
            @18,24 say tran(ajp+totpn,'@e 999,999')
            Close
            Do pesan with 'ESC=Lanjut     A=Ambil_Poin'
            Wait''
            If lastkey()=65 .or. lastkey()=97
               Kdb =' '
               Ketx='PENGAMBILAN POIN'
               Ket =spac(50)
               Tglz=tglx
               Stor 0 to jml,jml2
               *
               Sele 4
               Uselan('POINTAM.DTA','POINTAM1.NDX','POINTAM2.NDX','','','')
               Set colo to w+/g
               @19,10 clea to 23,71
               @19,10 to 23,71
               @19,32 say ketx
               @21,12 say 'Tanggal.:'get tglz
               @21,34 say 'Catatan:'get ket pict '@!S28'
               @22,12 say 'Poin diAmbil:'get jml pict '@e 999999'
               @22,34 say '-atau- diUangkan sejmlh Rp'get jml2 pict '@e   99999999'
               Do pesan with 'Masukan Tanggal dan Jumlah Poin yg diAmbil'
               Read
               If lastkey()=27
                  Sele 3
                  Loop
               Endi
               Set colo to w+/b
               @22,25 say tran(jml,'@e 999,999')
               @22,61 say tran(jml2,'@e 99,999,999')
               Yt=' '
               Do pesan with 'Data Benar? Y/T :'
               @24,49 get yt pict '!'
               Read
               If lastkey()=27 .or. yt#'Y'
                  Sele 3
                  Loop
               Endi
               If jml#0 .or. jml2#0
                  Flok()
                  Appe blan
                  Repl tanggal with tglz,jam with time(),kode_lgn with ksp,nama_lgn with nsp,rp_uang with jml2,;
                       so_point with (ajp+totpn),aj_point with ajp,am_point  with jml,id_point with kdb,keterangan with ket
                  Ul()
                  Commit
                  Close
                  *
                  Sele 3
                  Rlok()
                  Repl so_point with (ajp+totpn),aj_point with (ajp+totpn)-jml,am_point with jml,rp_uang with jml2,tgl_ambil with tglz,jam_ambil with time(),ket_ambil with ket
                  Ul()
                  Commit
                  *
               Endi
            Endi
            *
            Sele 3
            Exit
         Else
            Exit
         Endi
      Endd
   Else
      Set order to 1
      Go bott
      Ksp=kode_lgn
      Do hitlgn
      Ksp=right(rtri(ksp),pdt)+spac(11-pdt)    &&&SENGAJA SPAC(11), UNT DATALOGIC
      Do whil .t.
         Tgl2=ctod('01-'+ltri(str(mont(tglx)))+'-'+ltri(str(year(tglx))))
         Tambah=.t.
         Do kolomlgn
         Do pesan with 'ESC=Selesai'
         @8,24 get ksp pict '@!'
         Read
         @24,0
         If ksp=spac(10) .or. lastkey()=27
            Exit
         Endi
         Ksp=right(rtri(ksp),pdt)+spac(10-pdt)
         Set colo to w+/b
         @8,24 say ksp+'  '
         Seek ksp
         If foun()
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
            Kota=spac(10)
            Stor spac(20) to tlp
            Stor spac(30) to nsp,algn,an
            Stor spac(50) to ket1,ket2
            Stor 0 to tot,plf,jwk,dis
            Hjx=' '
         Endi
         *
         Set colo to w+/b
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
         Endi
         If tambah
            Flok()
            Appe blan
         Else
            Rlok()
         Endi
         Repl kode_lgn with ksp,nama_lgn with nsp,almt_lgn with algn,telp_lgn with tlp,kota_lgn with kota,;
              jens_lgn with iif(hjx#'1',' ',hjx),diskon with dis,tanggal with tglz,rp_saldo with tot,keterangan with ket1+ket2
         Ul()
         Commit
         *
         Do hitlgn
         Ksp=ksp+spac(5)
         Kota=spac(10)
         Stor spac(20) to tlp
         Stor spac(30) to nsp,algn
         Stor spac(50) to ket1,ket2
         Stor 0 to tot,plf,jwk,dis,ajp,amp
         Hjx=' '
         Sele 3
      Endd
   Endi
   Rest scre from slgn
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=-4 .and. ( fds='KODE_LGN' .or. fds='SO_POINT' )
   B=row()
   K=col()
   @24,0
   Set curs on
   Rlok()
   @b,k get &fds pict '@!'
   Read
   Ul()
   Commit
   Set curs off
   Do pesan with pesany
   Retu 1
Elseif lastkey()=-7
   Ksp=kode_lgn
   Set colo to gr+*
   @row(),col() say &fds
   Yt=' '
   Set curs on
   Set colo to g
   Do pesan with 'DiHapus? Y/T'
   Do whil .not. yt$'YT'
      @24,47 get yt pict '!'
      Read
   Endd
   If yt='Y'
      If .not. deleted()
         Rlok()
         Dele
         Ul()
         Commit
         Skip
      Endi
   Endi
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=-9
   Set curs on
   Sambung='Y'
   Do pesan with 'Pakai Kertas Bersambung? Y/T: '
   @24,55 get sambung pict '!'
   Read
   If sambung#'Y'
      Sambung='T'
   Endi
   Stor 1 to n,no,hal
   Do cp
   If lastkey()=27
      Do pesan with pesany
      Set curs off
      Retu 1
   Endi
   Do koplgn
   Do whil .not. eof()
      ?tran(no,'999.  ')+kode_lgn+'  '+nama_lgn+'  '+left(almt_lgn,30)+' '+left(telp_lgn,20)
      No =no+1
      N  =n+1
      If n>60
         ?repl('-',121)
         ?' '
         If sambung='T'
            Set prin off
            Set devi to scre
            Do pesan3 with gtkertas
            If nlp=0
               Cc=0
               Exit
            Endi
            Do cp
            If cc=0
               Exit
            Endi
         Else
            Eject
         Endi
         Hal=hal+1
         Do koplgn
      Endi
     Skip
   Endd
   ?repl('-',121)
   ?''
   Set prin off
   Set devi to scre
   Do pesan with pesany
   Set curs off
   Retu 1
Elseif lastkey()=72 .or. lastkey()=104
   Nr=recno()
   Do pesan with 'Tunggu, sedang diHitung'
   Coun to jml
   @1,63 say 'Member: '+ltri(tran(jml,'@e 99,999'))
   Do pesan with pesany
   Set curs off
   Go nr
   Retu 1
Elseif lastkey()=390
   Set curs on
   Yt =' '
   Do pesan with 'Jumlah Poin akan diUpdate dari Nota Transaksi.   Proses? Y/T : '
   @24,70 get yt pict '!'
   Read
   If lastkey()#27 .or. yt='Y'
      Sele 2
      Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
      *
      Sele 3
      Go top
      Jdt=reccount()
      Awl=1
      Do whil .not. eof()
         Ksp=kode_lgn
         Idp=jens_lgn
         Stor 0 to jml,tot,pnskr
         Do pesan with 'Proses Member: '+rtri(ksp)+'     '+ltri(str(awl))+'/'+ltri(str(jdt))
         *
         Sele 2
         Seek ksp
         Do whil .not. eof() .and. kode_lgn=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml=jml+jml_poin
               Tot=tot+(rp_total-rp_diskon)
            Endi
            Skip
         Endd
         Pnskr=jml
         *
         Sele 3
         Rlok()
         Repl so_point with iif(idp=' ',pnskr,0)   &&&IDP=' ' BRARTI MEMBER
         Ul()
         Awl=awl+1
         Skip
         *
      Endd
      Commit
   Endi
   *
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=389
   Set colo to gr+*
   @row(),col() say &fds
   Set colo to g
   Nr=recno()
   Save scre to sdaf
   Set colo to gr+
   @1,0 clea to 23,79
   @1,0 to 1,79
   @23,0 to 23,79
   @1,27 say 'Kode Pelanggan yg Doubel'
   Set colo to w
   Set order to 1
   Go top
   Kdbx=kode_lgn
   B   =2
   Awal=1
   Do whil .not. eof()
      If kode_lgn=kdbx .and. awal>1
         Skip -1
         @b,0 say kode_lgn+' '+nama_lgn+' '+almt_lgn
         B=b+1
         Skip
         @b,0 say kode_lgn+' '+nama_lgn+' '+almt_lgn
         B=b+1
      Endi
      Kdbx=kode_lgn
      Awal=awal+1
      If b>21
         Do pesan with 'Tekan sembarang tombol, unt hal berikut...'
         Wait''
         Set colo to w
         B=2
      Endi
      Skip
   Endd
   Wait''
   Rest scre from sdaf
   Do pesan with pesany
   Set curs off
   Retu 2
Elseif lastkey()=-8
   Set alte on
   Set alte to DOBEL.CSV
   Set order to 1
   Go top
   Kdbx=kode_lgn
   Awal=1
   Do whil .not. eof()
      If kode_lgn=kdbx .and. awal>1
         Skip -1
         ?kode_lgn+';'+nama_lgn+';'+almt_lgn+';'+kota_lgn+';'+telp_lgn
         Skip
         ?kode_lgn+';'+nama_lgn+';'+almt_lgn+';'+kota_lgn+';'+telp_lgn
      Endi
      Kdbx=kode_lgn
      Awal=awal+1
      Skip
   Endd
   Set alte off
   Set alte to
   Rest scre from sdaf
   Do pesan with pesany
   Set curs off
   Retu 2
Else
   Set filter to
   Retu 2
Endi
Retu 0

Proc koplgn
Set prin on
Set devi to prin
Set marg to 5
?chr(27)+chr(33)+chr(4)
?' DAFTAR PELANGGAN'+spac(95)+'Hal:'+tran(hal,'999')
?repl('=',121)
?' No.  Kode        Nama Pelanggan                  Alamat                         Telp.'
?repl('-',121)
N=1
Retu

Proc kolomlgn
Set colo to w+/br
@7,10 clea to 19,71
@7,10 to 19,71
@8,11  say '       Kode: '+spac(10)
@9,11  say '       Nama: '+spac(30)
@10,11 say '     Alamat: '+spac(37)
@11,11 say '     Telpon: '         
@12,11 say '       Kota: '+spac(30)+'Diskon %:'
@13,11 say '    Catatan:'
@16,11 say 'Poin Adjust:           Terakhir Ambil:        dari:'
@17,11 say ' - Berjalan:                Tgl.Ambil:'
@18,11 say 'Poin Total.:                  Catatan:'
If xOwner
   @19,10 clea to 22,71
   @19,10 to 22,71
   @20,15 say 'Saldo Piutang:   Tanggal...: '+dtoc(tglz)
   @21,15 say '--------------   Saldo Awal: '+tran(tot,'@e 999,999,999')
Endi
Retu

Proc hitlgn
Ksp=val(right(rtri(ksp),len(rtri(ksp))-len(idt)))+1
Ksp=idt+right('000000'+ltri(str(ksp)),pdt-len(idt))+spac(10-pdt)
Retu

* EOF LGN
