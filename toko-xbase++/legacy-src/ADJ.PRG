* ADJUST
*-------
Sele 1
*
Sele 2
*
Sele 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Sele 4
Uselan('ADJUSTN.DTA','ADJUSTN1.NDX','','','','')
*
Sele 8
Uselan('DATA.SET','','','','','')
Nn=no_trans+1
If nn>99999
   Nn=1
Endi
Do xnota
Fkt=ksa+right(fkt,4)
*
Tgl=tglx
Stor spac(5) to ksp,klb
Stor 0 to jml,tot
Do whil .t.
   Stor .f. to hapus,isilagi,batal,tdketemu
   *
   Sele 1
   @1,0 clea to 23,79
   @1,0 say chr(218)+repl(gl,38)+chr(194)+repl(gl,39)+chr(191)
   @2,0 say tl+'Tanggal..:'+spac(28)+tl+spac(39)+tl
   @3,0 say tl+spac(38)+tl+spac(39)+tl
   @4,0 say tl+'No.Bukti.:'+spac(28)+tl+spac(39)+tl
   @5,0 say chr(192)+repl(gl,38)+chr(193)+repl(gl,39)+chr(217)
   @2,12 get tgl
   @4,12 get fkt pict '@!'
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   Endi
   *
   @5,1 say repl(gl,12)+chr(194)+repl(gl,30)+chr(194)+repl(gl,4)+repl(chr(194)+repl(gl,9),3)
   @6,0 say 'BarCode      '+tl+'Nama Barang'+spac(19)+tl+'Sat.'+tl+'Stok Komp'+tl+'    Fisik'+tl+'   Selisih'
   @7,0 say repl(gl,13)+chr(197)+repl(gl,30)+chr(197)+repl(gl,4)+repl(chr(197)+repl(gl,9),3)+gl
   @5,39 say chr(193)
   Ada=.f.
   B=8
   N=1
   Kolomadj()
   Do whil .t.
      Yt=' '
      B =iif(isilagi,b,8)
      If isilagi
         *Do hadj
         Isilagi=.f.
         Jml=0
      Endi
      Do whil .t.
         Kdb=spac(13)
         Nmb=spac(30)
         Sele 3
         @b,0 say spac(13)+tl+spac(30)+tl+spac(4)+repl(tl+spac(9),3)
         @b,0 get kdb pict '@!S13'
         Do pesan with 'Kode Barang?     ESC=Selesai'
         Read
         If lastkey()=27
            Exit
         Endi
         If kdb=spac(13)
            @b,14 get nmb pict '@!S30'
            Do pesan with 'Masukan Nama Barang'
            Read
            If nmb=spac(30)
               Exit
            Endi
            Carikode=.f.
            Set order to 2
            Seek rtri(nmb)
            Do dafbrg
         Else
            Carikode=.t.
            Set order to 1
            Seek kdb
            If .not. foun()
               Seek rtri(kdb)
               If .not. foun()
                  Carikode=.f.
                  Nmb=kdb
                  Seek rtri(kdb)
                  Do dafbrg
               Endi
            Endi
         Endi
         If tdketemu
            Tdketemu=.f.
            Tone(1500,3)
            Tone(2000,6)
            Do pesan with 'Data Tidak diTemukan...'
            Wait''
            Loop
         Endi
         *
         If lastkey()=27 .or. kdb=spac(13)
            Loop
         Endi
         *
         Sele 3
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
         Kata="'1 '+rtri(stb)+'= '+ltri(tran(isi,'9999 '))+rtri(st)"
         Stor 0 to jml,nr
         *
         Do pesan with &kata+'.   HP: '+ltri(tran(hp,'@e 9,999,999'))+'   HJ: '+ltri(tran(hj,'@e 9,999,999'))+'     Jumlah Fisik Barang?'
         Set colo to /bg
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
            Set colo to w+/r
            @b+1,0
            @b+1,20 say 'Data sudah pernah di-entry sebelumnya...'
         Endi
         Read
         If jam='*'
            Rest scre from smkdb
         Endi
         Set colo to /bg
         If lastkey()=27
            Loop
         Endi
         @b,60 say tran(jml,'@e 9,999,999')
         @b,70 say tran(jml-jmlx,'@e 99,999,999')
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         Endi
         *
         Rlok()
         If id_so=' '
            Repl stok_a with jml,stok_m with 0,stok_k with 0,stok_rb with 0,id_so with '*',tanggal_0 with tgl
         Elseif id_so='*'
            Repl stok_a with (stok_a+jml),tanggal_0 with tgl
         Endi
         Ul()
         *
         Sele 4
         Flok()
         Appe blan
         Repl tanggal with tgl,faktur with fkt,jam with time(),kode_spl with ksp,;
              kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              stok_a with jmlx,jumlah with jml,harga with hp,harga_2 with hj
         Ul()
         *
         B=b+1
         N=n+1
         Kdb=spac(13)
         Nmb=spac(30)
         Ada=.t.
         If b>21
            Scroll(8,0,21,79,1)
            B=21
         Endi
      Endd
      *
      If .not. batal
         Yt=' '
         Do pesan with 'Simpan Data? Y/T :'
         Set colo to /bg
         Do whil .not. yt$'YT'
            @24,50 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            Endi
         Endd
         If yt='T' .or. lastkey()=27
            Isilagi=.t.
            Loop
         Endi
         *   
         Batal=.f.
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         Sele 4
         Commit
         *
         Sele 3
         Commit
         *
         If ada
            Sele 8
            Rlok()
            Repl no_trans with nn
            Ul()
            Commit
            Nn=nn+1
            If nn>99999
               Nn=1
            Endi
            Do xnota
         Elseif ada=.f.
            Nn=nn
            Do xnota
         Endi
         Fkt=ksa+right(fkt,4)
      Else
         Batal=.f.
         Sele 5
         Repl all faktur with spac(5)
      Endi
      Stor 0 to jml,tot
      Kolomadj()
      Exit
   Endd
   Ksp='   '
   Ada=.f.
Endd
Clos all
Set conf on
Rele brs4,brs42,ksp,klb,jml,tot,kdb,kdbx,nmb,st,isi,nn,fkt
Rele hapus,isilagi,batal,ada
Retu

***KOLOM RETURBELI
Func kolomadj
Do whil b<22
   @b,0 say spac(13)+tl+spac(30)+tl+'    '+repl(tl+spac(9),3)
   B =b+1
Endd
@b,0 say repl(gl,13)+chr(193)+repl(gl,30)+chr(193)+repl(gl,4)+repl(chr(193)+repl(gl,9),3)
B=8
Retu(.t.)

* LAPORAN ADJUST
*---------------
Proc ladj
Sele 1
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Sele 2
Uselan('ADJUSTN.DTA','ADJUSTN1.NDX','','','','')
*
Klp=.f.
Stor tglx to tgl,tgl2
Do whil .t.
   Kdb=spac(13)
   Nmb=' '
   Stor .f. to carikode,tdketemu
   *
   Sele 1
   Set cent on
   @1,0 clea to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   Do pesan with 'Ketik Kode/Nama Barangnya'
   Read
   @24,0
   If lastkey()=27
      Exit
   Endi
   If kdb#spac(13)
      Carikode=.t.
      Set order to 1
      Seek kdb
      If .not. foun()
         Seek rtri(kdb)
         If .not. foun()
            Carikode=.f.
            Nmb=kdb
            Seek rtri(kdb)
            Do dafbrg
         Endi
      Endi
      Nmb=nama_brg
   Endi
   @2,32 say iif(kdb#spac(13),nmb,'')
   If tdketemu
      Tdketemu=.f.
      Tone(1500,3)
      Tone(2000,6)
      Do pesan with 'Data Tidak diTemukan...'
      Wait''
      Loop
   Endi
   *
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Ulangi'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Loop
   Endi
   Stor 0 to no,tot,tot2
   Stor spac(5) to st,st2
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Sele 2
   Set filter to
   If kdb#spac(7)
      Set filter to ( kode_bar=kdb .or. nama_brg=nmb )
   Endi
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
   Endi
   Nr=recno()
   *
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do kopladj
   Set cent off
   Tglz=tanggal
   Fktz=faktur
   ?spac(9)+'Tgl: '+dtoc(tglz)+' / '+fktz
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Kdby=kode_bar
      Kdby=iif(len(rtri(kdby))>=7,left(kdby,2)+'x'+right(rtri(kdby),4),left(kdby,7))
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
         ?spac(9)+'Tgl: '+dtoc(tglz)+' / '+fktz
      Endi
      If no>=50
         ?repl('-',105)
         ?' '
         No =0
         Hal=hal+1
         Cf =cf +1
         Cfx=ltri(str(cf))
         Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
         Set alte to &arg1
         Do kopladj
      Endi
   Endd
   ?repl('-',105)
   ?'         T O T A L  Rp'+spac(56)+tran(tot,'@e 9,999,999,999 ')+tran(tot2,'@e 9,999,999,999')
   ?repl('=',105)
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
   Endi
   If lastkey()=27
      Loop
   Endi
   Sambung='Y'
   Do pesan with 'Pakai Kertas berSambung? Y/T : '
   @24,55 get sambung pict '!'
   Read
   Do cp
   If cc=0
      Loop
   Endi
   Set prin on
   Set devi to prin
   Set marg to 10
   ?chr(27)+chr(33)+chr(4)
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='C:\TEMP\L-ADJ&cfx'+'.RTF'
       Type &arg1 to print
       *
       If sambung='Y'
          Eject
       Else
          Set prin off
          Set devi to scre
          Do pesan3 with gtkertas +' ( '+'&cfx->'+ltri(str(cf))+' )'
          If nlp=0
             Cc=0
             Exit
          Endi
          Set prin on
          Set devi to prin
          Set marg to 10
          ?chr(27)+chr(33)+chr(4)
       Endi
   Next
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Retu

Proc kopladj
?'LAPORAN STOK OPNAME'
?'TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(30)+'Hal: '+tran(hal,'999')
?repl('=',105)
?'Kode     Nama Barang                Sat.    Komp.    Fisik  Selisih  @Hg.Beli     Jumlah_Rp    Selisih_Rp'
?repl('-',105)
N=1
Retu

Proc hadj
@1,0 clea to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai     F5=Simpan_Ulang     ENTER=Edit'
Do pesan with katax
Declare field[17],kop[17],frm[17],bts[17]
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
Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8],bts[9],bts[10]
Go nr
Dbedit(2,0,21,79,field,'fhadj',frm,kop,gl,bts)
*
Rele field[14],kop[14],frm[14],bts[14]
Set curs on
Retu

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
         Endc
      Endi
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=13
      Kdb=kode_bar
      Hp =harga
      Stor jumlah to jml,jml2
      Set curs on
      If field[pointer]='JUMLAH'
         @row(),col() get jml pict '@e  999999'
         Do pesan with 'Masukan Jumlah Barang'
         Read
         If lastkey()#27
            Rlok()
            Repl jumlah with jml
            Ul()
            Commit
            *
            Sele 1
            Seek kdb
            Rlok()
            Repl stok_a with jml
            Ul()
            Commit
            *
            Sele 2
         Endi
      Elseif left(field[pointer],5)='HARGA'
         @row(),col() get hp pict '@e   9999999'
         Do pesan with 'Masukan Harga Pokok/Beli'
         Read
         If lastkey()#27
            Rlok()
            Repl harga with hp
            Ul()
            Commit
         Endi
      Elseif field[pointer]='KODE_KLB' .or. field[pointer]='KODE_BRG'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         Commit
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Case lastkey()=-4
      Set curs on
      Yt=' '
      Do pesan with 'Stok Opname Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' mau diSimpan Ulang? Y/T :  '
      @24,70 get yt pict '!'
      Read
      If yt='Y'
         Do pesan with 'Tunggu, sedang proses...'
         Go nr
         Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Kdb=kode_bar
            Nmb=nama_brg
            Jml=jumlah
            *
            Sele 1
            Set order to 1
            Seek kdb
            If .not. foun()
               Set order to 2
               Seek nmb
            Endi
            Rlok()
            If id_so=' '
               Repl stok_a with jml,id_so with '*',tanggal_0 with tgl
            Elseif id_so='*'
               Repl stok_a with (stok_a+jml),tanggal_0 with tgl
            Endi
            Ul()
            *
            Sele 2
            Skip
         Endd
         Sele 1
         Commit
         *
      Endi
      Do pesan with katax
      Set curs off
      Sele 2
      Retu 1
   Case lastkey()=-8
      @1,30 say 'Data Yang diKoreksi'
      Set dele off
      Set filter to dele()
      Go top
      Do pesan with katax
      Set curs off
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

* EOF ADJ






