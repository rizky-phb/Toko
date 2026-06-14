* TRANSFER/KIRIM BARANG'
Stor tglx to tgl,tgl2
Tgl2=ctod('  -  -    ')
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
*
Sele 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Sele 4
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
Sele 5
Useidx('KIRIM.DTA','','','','','')
Repl all faktur with spac(5),id_data with ' '
*
Sele 8
Uselan('DATA.SET','','','','','')
Nn=no_pinjam+1
If nn>99999
   Nn=1
Endi
Do xnota
Sele 9
*
Brs4 ="left(kdbx,13)+tl+left(nmb,30)+tl+tran(jml,'@e 99999.99 ')+left(st,4)+"+;
      "tl+tran(hp,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')"
Brs42="left(kode_bar,13)+tl+left(nama_brg,30)+tl+tran(jumlah,'@e 99999.99 ')+left(satuan,4)+"+;
      "tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')"
Stor spac(5) to ksp,klb
Stor 0 to jml,tot,tot2,dis4
Stor .f. to hapus,isilagi,batal,ada,diedit
Nklb=spac(20)
Lks='T'
Do whil .t.
   Sele 1
   Set key 28 to dafspl
   @1,0 clea to 23,79
   @1,1 say iif(idb='M','Dari:','Ke..:')+spac(35)+'Tanggal.:'+spac(31)
   @2,1 say spac(40)+'No.Bukti:'+spac(31)
   @1,7 get ksp pict '@!'
   @1,51 say tgl
   @2,51 say fkt pict '@!'
   Do pesan with 'F1=Daftar Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   Endi
   Seek ksp
   If .not. foun()
      Do pesan2 with 'Tidak ada kode supplier '+rtri(ksp)
      Loop
   Endi
   Nsp =nama_spl
   Algn=almt_spl
   @1,7 say ksp
   @2,7 say algn
   @1,51 get tgl
   @2,51 get fkt pict '@!'
   Read
   If lastkey()=27
      Loop
   Endi
   N=1
   *
   Sele 4
   Set order to 1
   Seek dtos(tgl)+ksp+fkt
   If foun()
      Stor .t. to isilagi,ada
      Do whil (.not. eof()) .and. faktur=fkt
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
         Sele 5
         If n=1
            Go top
         Endi
         Repl tanggal with tgl,faktur with fkt,kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,;
              kode_bar with kdbx,nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,nomorekod with nrx,id_stok with idb,id_data with '1'
         Skip
         *
         N=n+1
         *
         Sele 4
         Skip
      Endd
   Endi
   @3,0 say repl(gd,13)+chr(209)+repl(gd,30)+chr(209)+repl(gd,13)+chr(209)+repl(gd,9)+chr(209)+repl(gd,11)
   @4,0 say 'BarCode      '+tl+'Nama Barang'+spac(19)+tl+'  Jml.Barang '+tl+'   @Harga'+tl+' Jumlah Rp.'+tl
   @5,0 say repl(gl,13)+chr(197)+repl(gl,30)+chr(197)+repl(gl,13)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)
   B=6
   Kolompjm()
   Disi=.f.
   Do whil .t.
      Yt=' '
      B=6
      Kdbx=spac(13)
      Nmb =spac(30)
      If isilagi
         Do hpjm
         Isilagi=.f.
         Jml=0
      Endi
      Do whil .t.
         Sele 3
         Set key 5 to dataht
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         @b,0 say spac(13)+tl+spac(30)+tl+spac(13)+tl+spac(9)+tl+spac(11)
         @b,0 get kdbx pict '@S13'
         Do pesan with 'BarCode?'
         Read
         Set key 5 to
         If hapus
            Do hpjm
            Loop
         Endi
         If lastkey()=27
            Do pesan with 'Transaksi Batal? Y/T'
            Yt=' '
            Do whil .not. yt$'YT'
               @24,51 get yt pict '!'
               Read
            Endd
            If yt='Y'
               Batal=.t.
               Exit
            Endi
            Loop
         Endi
         If kdbx=spac(13)
            @b,14 get nmb pict '@!S30'
            Do pesan with 'Masukan Nama Barang'
            Read
            If nmb=spac(30)
               Exit
            Endi
            Nmb=rtri(nmb)
            Set order to 2
            Seek nmb
            Do dafbrg
         Else
            Set order to 1
            Seek kdbx
            If .not. foun()
               Seek rtri(kdbx)
               Do dafbrg
            Endi
         Endi
         If lastkey()=27
            Loop
         Endi
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
         Endi
         Jhg =(jml*hp)
         If hp=0
            @b,69 get jhg pict '@e 999,999,999'
            Read
            If jhg>0 .and. jml>0
               Hp=jhg/jml
            Endi
         Endi
         @b,0 say &brs4
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         Endi
         Tot =tot + jhg
         *
         Sele 5
         If n=1
            Go top
         Endi
         Repl tanggal with tgl,faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,id_stok with idb,id_data with ' '
         Skip
         *
         B=b+1
         N=n+1
         Jml=0
         Kdbx=spac(13)
         Nmb =spac(30)
         If b>20
            Scroll(8,0,20,79,1)
            B=20
         Endi
      Endd
      *
      If .not. batal
         Stor 0 to dis4,byr,kbl,jmlg
         Ht=' '
         Do pesan with 'Simpan Data? Y/T:'
         Set colo to g
         Do whil .not. ht$'YT'
            @24,49 get ht pict '!'
            Read
            If lastkey()=27
               Exit
            Endi
         Endd
         If ht='T' .or. lastkey()=27
            Isilagi=.t.
            Loop
         Endi
         *   
         Batal=.f.
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         Sele 5
         Go top
         Do whil .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
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
               Sele 4
               Flok()
               Appe blan
               Repl tanggal with tgl,jam with time(),faktur with fkt,kode_spl with ksp,;
                    nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
                    nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,id_stok with idb
               Ul()
               Commit
               *
               Sele 3
               Set order to 1
               Seek kdbx
               Rlok()
               If idb='M'
                  Repl stok_m with (stok_m+(jml*isi))
               Elseif idb='K'
                  Repl stok_kr with (stok_kr+(jml*isi))
               Endi
               Ul()
            Endi
            *
            Sele 5
            Repl faktur with spac(5),id_data with ' '
            Skip
         Endd
         *
         If .not. ada
            Sele 8
            Rlok()
            Repl no_pinjam with nn
            Ul()
            Nn=nn+1
            If nn>99999
               Nn=1
            Endi
            Do xnota
         Elseif ada
            Nn=nn
            Do xnota
         Endi
      Else
         Batal=.f.
         Sele 5
         Repl all faktur with spac(5),id_data with ' '
      Endi
      Stor 0 to jml,tot
      kolompjm()
      Exit
   Endd
   Ksp=spac(5)
   Ada=.f.
Endd
Clos all
Rele brs4,brs42,ksp,klb,jml,tot,tot2,dis4,nklb,lks,kdb,kdbx,nmb,st,isi,nn,fkt
Rele hapus,isilagi,batal,ada,diedit
Retu

*** PROSEDUR HAPUS TRANSAKSI 
Proc hpjm
kolompjm()
Sele 5
Set filter to tanggal=tgl .and. faktur=fkt
Go top
If faktur=spac(5)
   Set filter to
   Retu
Endi
Declare field[8],bts[8],frm[8]
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
Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
Pesany='ENTER=Edit Jumlah     ESC=Selesai'
Do pesan with pesany
Keyboard chr(5)
Dbedit(6,0,20,79,field,'fhpjm',frm,'','',bts)
*
Kolompjm()
Tot=0
Set filter to
Go top
Do whil .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
   Jhg=(jumlah*harga)
   Tot=tot+jhg
   @b,0 say &brs42
   B  =b+1
   If b>20
      Scroll(8,0,20,79,1)
      B=20
   Endi
   Skip
Endd
Nmb =spac(30)
Kdbx=spac(13)
Jml=1
Hapus=.f.
Retu

Function fhpjm
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=13 .and. fds='JUMLAH'
   B  =row()
   Kdb=kode_bar
   Isi=1
   Nrx=nomorekod
   Idb=id_stok
   Stor jumlah to jml,jml2
   Set curs on
   Do pesan with ''
   @b,45 get jml pict '@e 99999.99'
   Read
   If lastkey()#27
      Repl jumlah with jml
      Commit
      *
      If diEdit
         Sele 3
         Set order to 1
         Seek kdb
         Rlok()
         If idb='M'
            Repl stok_m with (stok_m-(jml2*isi))+(jml*isi)
         Elseif idb='K'
            Repl stok_kr with (stok_kr-(jml2*isi))+(jml*isi)
         Endi
         Ul()
         Commit
         *
         Sele 4
         Go nrx
         Rlok()
         Repl jumlah with jml
         Ul()
         Commit
         Sele 5
      Endi
   Endi
   Do pesan with pesany
   Set curs off
   Retu 1
Else
   Set curs off
   Retu 1
Endi
Retu 0

***KOLOM RETURBELI
Func kolompjm
Do whil b<21
   @b,0 say spac(13)+tl+spac(30)+tl+spac(13)+tl+spac(9)+tl+spac(11)
   B =b+1
Endd
@b,0 say repl(gl,13)+chr(193)+repl(gl,30)+chr(193)+repl(gl,13)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)
B=6
Retu(.t.)

*LAP.KIRIM BARANG
*-----------------
Proc lkrm
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','','','','')
Sele 2
*Uselan('STOK.DTA','STOK5.NDX','','','','')
Sele 3
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
Sele 4
*
Sele 5
*
Ksp=spac(5)
Nsp='-'
Stor ' ' to hri,bln,thn,jlap
Stor tglx to tgl,tgl2
Do whil .t.
   Sele 1
   Set cent on
   Set key 28 to dafspl
   Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
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
   Endi
   If ksp#spac(5)
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada Supplier '+rtri(ksp)
         Loop
      Endi
      Nsp=nama_spl
   Endi
   Syarat='id_stok#" "'
   If jlap='K'
      Syarat='id_stok="K"'
   Elseif jlap='M'
      Syarat='id_stok="M"'
   Endi
   Stor 0 to no,tot,tot2
   Do konvtgl
   Sele 3
   Set filter to &syarat
   If ksp#spac(5)
      Set filter to kode_spl=ksp .and. &syarat
   Endi
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
   Endi
   Nr=recno()
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='L-ORDER&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplob
   Set cent off
   Tglz=tanggal
   Nsls=kode_spl
   Nklb=nama_spl
   Fkt=faktur
   Idb=id_stok
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
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
            Endi
         Endc
         Tot =tot +(jumlah*harga)
         Tot2=tot2+(jumlah*harga)
         ?left(kode_brg,7)+'  '+left(nama_brg,30)+'  '+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 999,999,999')
         Skip
         If faktur#fkt .or. kode_spl#nsls
            ?spac(63)+repl(gl,12)
            ?left(nklb,20)+'  Tgl: '+dtoc(tglz)+' / '+fkt+' / '+idb+'     Jumlah Rp.='+tran(tot2,'@e 9999,999,999')
            ?repl(gl,75)
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
               Cfx=ltri(str(cf))
               Arg1='L-ORDER&cfx'+'.RTF'
               Set alte to &arg1
               Do koplob
            Endi
         Endi
   Endd
   ?repl(gl,75)
   ?spac(54)+'TOTAL=Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-ORDER&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       Endi
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F10=Cetak'
   If lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Order tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      Do whil .not. yt$'YT'
         @23,65 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
         Ul()
         Commit
      Endi
      Loop
   Elseif lastkey()=27
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Stor 1 to no,n,hal,b
   Stor 0 to jhg,tot
   Do koplob1
   Go nr
   Set cent off
   Tglz=tanggal
   Nsls=kode_spl
   Nklb=nama_spl
   Fkt =faktur
   Idb =id_stok
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
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
            Endi
      Endc
      Tot =tot +(jumlah*harga)
      Tot2=tot2+(jumlah*harga)
      ?tran(no,'9999.  ')+left(kode_brg,7)+'  '+left(nama_brg,30)+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 999,999,999')
      B =b+1
      N =n+1
      No=no+1
      Skip
      If faktur#fkt .or. kode_spl#nsls
         ?spac(68)+repl('-',12)
         ?spac(5)+left(nklb,20)+'  Tgl: '+dtoc(tglz)+' / '+fkt+' / '+idb+'     Jumlah Rp.='+tran(tot2,'@e 9999,999,999')
         ?repl('-',80)
         Tglz=tanggal
         Nsls=kode_spl
         Nklb=nama_spl
         Fkt =faktur
         Idb =id_stok
         Tot2=0
         No  =1
         N   =n+3
         B   =b+3
      Endi
      If n>55
         @b,0 say repl('-',80)
         @b+1,0 say ''
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
         N  =1
         Hal=hal+1
         Do koplob1
      Endi
   Endd
   If cc=0
      Loop
   Endi
   @b,0 say spac(53)+'T o t a l = Rp'+tran(tot,'@e 9,999,999,999')
   @b+1,0 say repl('=',80)
   @b+2,0 say ' '
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Retu

Proc koplob
?'Supp: '+rtri(nsp)+' Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   //Hal:'+tran(hal,'999')
?repl(gd,8)+chr(209)+repl(gd,30)+chr(209)+gd+gd+gd+repl(chr(209)+repl(gd,9),2)+chr(209)+repl(gd,11)
?'Kode    '+tl+'Nama Barang'+spac(19)+tl+'Lks'+tl+'     Qty.'+tl+'   @Harga'+tl+'     Jumlah'
?repl(gl,8)+chr(193)+repl(gl,30)+chr(193)+gl+gl+gl+repl(chr(193)+repl(gl,9),2)+chr(193)+repl(gl,11)
Retu

Proc koplob1
Set prin on
Set devi to prin
Set marg to 20
?chr(27)+chr(33)+chr(4)
@prow()+1,1 say 'DAFTAR PINJAMAN BARANG'
If ksp#spac(5)
   @prow()+1,1 say 'REKANAN.: '+rtri(nsp)+'  ('+rtri(ksp)+')'
Endi
If tgl=tgl2
   @prow()+1,1 say 'TANGGAL.: '+dtoc(tgl)
Else
   @prow()+1,1 say 'TANGGAL.: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
Endi
@prow(),72 say 'Hal:'+tran(hal,'999')
@prow()+1,0 say repl('=',80)
@prow()+1,0 say '  No.  Kode     Nama Barang                      Jml.Brg.     @Harga   Jumlah Rp'
@prow()+1,0 say repl('-',80)
B=prow()+1
Retu

* LAPORAN TRANSFER/ITEM
*----------------------
Proc lkrm2
Sele 1
Uselan('KIRIMIN.DTA','KIRIMIN1.NDX','','','','')
Sele 2
Uselan('STOK.DTA','STOK4.NDX','','','','')
*
Stor tglx to tgl,tgl2
Nmb=spac(30)
Do whil .t.
   Sele 2
   Set cent on
   Set filter to
   @1,0 clea to 23,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Do pesan with 'Masukan Nama Barang'
   Read
   If lastkey()=27
      Exit
   Endi
   If nmb=spac(30)
      Loop
   Endi
   Seek nmb
   If .not. foun()
      Seek rtri(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
      Endi
   Endi
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
   Endi
   *
   Sele 1
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='L-TRAN&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   ?' LAPORAN TRANSFER / ITEM BARANG'
   ?' NAMA BRG: '+rtri(nmb)+'   KODE: '+kdb
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'  / Hal:'+tran(hal,'999')
   ?chr(213)+repl(gd,4)+chr(209)+repl(gd,14)+chr(209)+repl(gd,5)+chr(209)+repl(gd,3)+chr(209)+repl(gd,6)+chr(209)+repl(gd,9)+chr(209)+repl(gd,11)+chr(184)
   ?tl+' No.'+tl+'Tanggal       '+tl+'Bukti'+tl+'G/T'+tl+'  Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+repl(gl,4)+chr(197)+repl(gl,14)+chr(197)+repl(gl,5)+chr(197)+repl(gl,3)+chr(197)+repl(gl,6)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)+chr(180)
   Y=1
   Stor 0 to no,stm,hp,hj,jml,tot,tot2,totb,jbrt
   Z=ltri(str(y))
   Set cent off
   Do whil .t.
      Set filter to kode_bar=kdb
      Seek dtos(tgl)
      If .not. foun()
         Loca for (tanggal>=tgl .and. tanggal<=tgl2)
      Endi
      St =satuan
      Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jhg =(jumlah*harga)
         Stm =stm+jumlah
         Tot =tot+ jhg
         xJml=jumlah
         Do fmtjml2
         ?tl+tran(y,'999.')+tl+dtoc(tanggal)+' '+jam+tl+faktur+tl+'   '+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
         Y =y +1
         No=no+1
         If no>=200
            ?chr(195)+repl(gl,4)+chr(197)+repl(gl,14)+chr(197)+repl(gl,5)+chr(197)+repl(gl,3)+chr(197)+repl(gl,6)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)+chr(180)
            ?' '
            No =0
            Hal=hal+1
            Cf =cf +1
            Cfx=ltri(str(cf))
            Arg1='L-TRAN&cfx'+'.RTF'
            Set alte to &arg1
            ?spac(50)+' Hal:'+tran(hal,'999')
            ?chr(213)+repl(gd,4)+chr(209)+repl(gd,14)+chr(209)+repl(gd,5)+chr(209)+repl(gd,3)+chr(209)+repl(gd,6)+chr(209)+repl(gd,9)+chr(209)+repl(gd,11)+chr(184)
         Endi
         Skip
      Endd
      Exit
   Endd
   Nmb =spac(30)
   Pesanx=tran(stm,'@e 999,999 ')+st
   ?chr(192)+repl(gl,4)+chr(193)+repl(gl,14)+chr(193)+repl(gl,5)+chr(193)+repl(gl,3)+chr(193)+repl(gl,6)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(217)
   ?spac(23)+'Total ='+pesanx+' Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   Ada=.f.
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-TRAN&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       Endi
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Set prin on
   Set devi to prin
   Set marg to 10
   ?chr(27)+chr(33)+chr(9)
   Type L-TRAN1.RTF to prin
   Set prin off
   Set devi to scre
Endd
Clos all
Rele klb,nklb,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Retu

* LAPORAN PEMECAHAN STOK
*------------------------
Proc lpch
Sele 1
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Sele 2
Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')
*
Stor tglx to tgl,tgl2
Do whil .t.
   Kdb=spac(13)
   Sele 1
   Set cent on
   Set key 28 to dafbrg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai  F1=Daftar Kode Barang'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   Endi
   If kdb#spac(13)
      Seek kdb
      If .not. foun()
         Seek rtri(kdb)
      Endi
   Endi
   Stor 0 to no,tot,tot2
   Stor spac(5) to st,st2
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Sele 2
   Set filter to
   If kdb#spac(13)
      Set filter to kode_brg=kdb
   Endi
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
   Endi
   Nr=recno()
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='L-PCH&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpch
   Set cent off
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
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
         Cfx=ltri(str(cf))
         Arg1='L-PCH&cfx'+'.RTF'
         Set alte to &arg1
         Do koplpch
      Endi
   Endd
   ?repl(chr(196),81)
   ?spac(60)+'TOTAL='+tran(tot,'@e 9,9999,999 ')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-PCH&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,22,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       Endi
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hpch
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      Do whil .not. yt$'YT'
         @23,67 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2)
         Ul()
         Commit
      Endi
      Loop
   Elseif lastkey()=27
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot
   Do koplpch1
   Go nr
   Set cent off
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+'  '+nama_brg+'  '+tran(jumlah*isine_b,'9999 ')+satuan+'  '+left(kode_brg2,7)+'  '+nama_brg2+tran(jumlah,'@e 9999.99 ')+satuan2
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      No=no+1
      Skip
      If no>60
         ?repl('-',114)
         ?spac(94)+'bersambung...'
         ?''
         Eject
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
         No =1
         Hal=hal+1
         Do koplpch1
      Endi
   Endd
   If cc=0
      Loop
   Endi
   ?spac(42)+'TOTAL='+tran(tot2,'@e 999,999 ')+spac(43)+tran(tot,'@e 999,999.99 ')
   ?repl('=',114)
   ?' '
   Eject
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Retu

Proc koplpch
?' Kode: '+rtri(kdb)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(26)+'Hal:'+tran(hal,'999')
?repl(gd,9)+chr(209)+repl(gd,30)+chr(209)+repl(gd,30)+chr(209)+repl(gd,9)
?'Tanggal  '+tl+'Nama Barang'+spac(19)+tl+'Asal Barang'+spac(19)+tl+'  Jml.Brg'
?repl(gl,9)+chr(193)+repl(gl,30)+chr(197)+repl(gl,30)+chr(193)+repl(gl,9)
Retu

Proc koplpch1
Set prin on
Set devi to prin
?chr(27)+chr(33)+chr(5)
Set marg to 15
?' LAPORAN MUTASI/PECAH STOK'
If kdb#spac(13)
   ?' Kode.: '+kdb
Endi
?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+spac(74)+'Hal:'+tran(hal,'999')
?repl('=',114)
?'Tanggal   Nama Barang                               Jml.Brg    Asal Barang                                Jml.Brg'
?repl('-',114)
No=1
Retu

Proc hpch
@1,0 clea to 23,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai  F8=Hapus'
Do pesan with katax
Declare field[9],kop[9],frm[9],bts[9]
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
Dbedit(2,0,21,79,field,'fpch',frm,kop,chr(196),bts)
*
Set curs on
Retu

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
         Endc
      Endi
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-7
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Ht=' '
      Do whil .not. ht$'YT'
         @23,47 get ht pict '!'
         Read
      Endd
      If ht='Y'
         Kdb =kode_brg
         Kdb2=kode_brg2
         Jml =jumlah
         Isi =isine_b
         If .not. dele()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
            *
            Sele 1
            Seek kdb
            Rlok()
            Repl stok_m with (stok_m-(jml*isi))
            Ul()
            Commit
            *
            Seek kdb2
            Rlok()
            Repl stok_k with (stok_k-(jml*isine_b))
            Ul()
            Commit
            Skip
            *
            Sele 2
         Endi
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

*EOF KRM
