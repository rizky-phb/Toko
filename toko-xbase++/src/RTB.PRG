Procedure RTB
* RETUR PEMBELIAN / PENJUALAN
Stor tglx to tgl,tgl2
Tgl2=ctod('  -  -    ')
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
*
Sele 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Sele 4
Uselan('RETURBN.DTA','RETURBN1.NDX','RETURBN2.NDX','RETURBN3.NDX','','')
Sele 5
Useidx('RETURB.DTA','','','','','')
Repl all faktur with spac(5),id_data with ' '
*
Sele 6
*
Sele 7
Uselan('FAKTURH.DTA','FAKTURH3.NDX','','','','')
Sele 8
Uselan('DATA.SET','','','','','')
Nn=no_returb+1
If nn>99999
   Nn=1
EndIf
Do xnota
Sele 9
*
Brs4 ="left(nmb,30)+tl+kdbx+tl+tran(jml,'@e 9999.99')+tl+left(st,4)+tl+tran(hp,'@e 9,999,999')+tl+tran(jhg,'@e 99,999,999')"
Brs42="left(nama_brg,30)+tl+kode_bar+tl+tran(jumlah,'@e 9999.99')+tl+left(satuan,4)+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 99,999,999')"
Fkt2 =spac(10)
Stor spac(5) to ksp,klb
Stor 0 to jml,jhg,tot,tot2,dis4
Stor .f. to hapus,isilagi,batal,ada,diedit
Lks='T'
Do While .t.
   Sele 1
   Set key 28 to dafspl
   @1,0 clea to 24,79
   @1,0 say chr(218)+repl(gl,44)+chr(194)+repl(gl,33)+chr(191)
   @2,0 say tl+'Supplier:'+spac(35)+tl+'Untuk Potong Tagihan'+spac(13)+tl
   @3,0 say tl+'Tanggal.:'+spac(35)+tl+'No.Nota:'+spac(25)+tl
   @4,0 say tl+'No.Bukti:'+spac(35)+tl+'Jumlah.: Rp'+spac(22)+tl
   @5,0 say chr(192)+repl(gl,44)+chr(193)+repl(gl,33)+chr(217)
   @2,11 get ksp pict '@!'
   Do pesan with 'ESC=Selesai     F1=Daftar_Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
EndIf
   Nsp='-'
   If ksp=spac(5)
      Stor 'TUNAI' to ksp,nsp,algn
   Elseif ksp#spac(5)
      Set order to 1
      Seek ksp
      If .not. foun()
         Set order to 2
         Seek rtri(ksp)
         Do dafspl
EndIf
      If lastkey()=27
         Loop
EndIf
      Nsp =left(nama_spl,20)
      Algn=almt_spl
EndIf
   @2,17 say left(nsp,27)
   @3,11 get tgl
   @4,11 get fkt pict '@!'
   @3,55 get fkt2 pict '@!'
   Read
   If lastkey()=27
      Loop
EndIf
   *If mont(tgl)#mont(tglx)
   *   Do pesan2 with 'Bul()an Tidak Bisa diGanti!   Ganti Tanggal?  ===>Menu Utility ->Tanggal'
   *   Loop
   *Endi
   N=1
   *
   If fkt2#spac(10)
      Sele 7
      Seek ksp+fkt2
      @4,58 say ltri(tran(((rp_total-rp_diskon)-rp_bayar)-rp_retur,'@e 999,999,999'))
EndIf
   *
   Sele 4
   Set order to 1
   Seek dtos(tgl)+ksp+fkt
   If foun()
      Stor .t. to isilagi,ada
Do While (.not. eof()) .and. faktur=fkt
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Jml =jumlah
         Hp  =harga
         Lks =darilokasi
         Nrx =recno()
         *
         Sele 5
         If n=1
            Go top
EndIf
         Repl tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,darilokasi with lks,nomorekod with nrx,id_data with '1'
         Skip
         N=n+1
         *
         Sele 4
         Skip
EndDo
EndIf
   Sele 3
   *Set filter to kode_spl=ksp
   *
   @5,0 say chr(195)+repl(gl,30)+chr(194)+repl(gl,13)+chr(194)+repl(gl,7)+chr(194)+repl(gl,4)+chr(194)+repl(gl,9)+chr(194)+repl(gl,10)+chr(180)
   @6,0 say tl+'Nama Barang'+spac(19)+tl+'BarCode      '+tl+'   Qty.'+tl+'Sat.'+tl+'   @Harga'+tl+'Jumlah Rp.'+tl
   @7,0 say chr(195)+repl(gl,30)+chr(197)+repl(gl,13)+chr(197)+repl(gl,7)+chr(197)+repl(gl,4)+chr(197)+repl(gl,9)+chr(197)+repl(gl,10)+chr(180)
   B=8
   Kolomrtb()
   Disi=.f.
Do While .t.
      Yt=' '
      B=8
      Kdb=spac(13)
      Nmb=spac(30)
      If isilagi
         Do hpsrtr
         Isilagi=.f.
         Jml=0
EndIf
Do While .t.
         Sele 3
         Set key 5 to dataht
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         @b,0 say tl+spac(30)+tl+spac(13)+tl+spac(7)+tl+spac(4)+tl+spac(9)+tl+spac(10)+tl
         @b,1 get nmb pict '@!S30'
         Do pesan with 'Masukan Nama Barang, atau Kosongkan untuk BarCode'
         Read
         Set key 5 to
         If hapus
            Do hpsrtr
            Loop
EndIf
         If lastkey()=27
            Do pesan with 'Transaksi Batal? Y/T'
            Yt=' '
Do While .not. yt$'YT'
               @24,51 get yt pict '!'
               Read
EndDo
            If yt='Y'
               Batal=.t.
               Exit
EndIf
            Loop
EndIf
         If nmb=spac(30)
            Set order to 1
            @b,32 get kdb pict '@!S13'
            Do pesan with 'Masukan Barcode, atau Kosongkan untuk Selesai'
            Read
            If kdb=spac(13)
               Exit
EndIf
            Carikode=.t.
            Seek kdb
            If .not. foun()
               Do dafbrg
               If lastkey()=27
                  Loop
EndIf
EndIf
         Else
            Carikode=.f.
            Set order to 2
            Seek nmb
            If .not. foun()
               Seek rtri(nmb)
               Do dafbrg
               If lastkey()=27
                  Loop
EndIf
EndIf
EndIf
         Set order to 1
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Hp  =harga
         Stk =((stok_a+stok_m)-stok_k)
         Kdbx=kode_bar
         Do pesan with 'L (lokasi) = G/T    Sisa:'+tran(stk,'@e 999,999.99')+'    Tgl.Masuk:'+dtoc(tanggal_1)
         @b,1  say &brs4
         @b,54 say left(st,4)
         @b,46 get jml pict '@e 9999.99'
         @b,59 get hp  pict '@e 9,999,999'
         Read
         If lastkey()=27
            Loop
EndIf
         Jhg =(jml*hp)
         If hp=0
            @b,69 get jhg pict '@e 99,999,999'
            Read
            If jhg>0 .and. jml>0
               Hp=jhg/jml
EndIf
EndIf
         @b,69 say jhg pict '@e 99,999,999'
         *
         Yt='Y'
         Do pesan with 'Data Benar? Y/T :'
         @24,49 get yt pict '!'
         Read
         If lastkey()=27
            Loop
EndIf
         Tot=tot + jhg
         Lks=iif(lks#'T','G','T')
         @b,1 say &brs4
         *
         Sele 5
         If n=1
            Go top
EndIf
         Repl tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,;
              nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,;
              satuan with st,jumlah with jml,harga with hp,darilokasi with lks,id_data with ' '
         Skip
         *
         B=b+1
         N=n+1
         Jml=0
         Jhg=0
         Kdb=spac(13)
         Nmb=spac(30)
         If b>20
            Scroll(8,1,20,78,1)
            B=20
EndIf
EndDo
      *
      If .not. batal
         Stor 0 to dis4,byr,kbl,jmlg
         Batal=.f.
         @22,57 say 'Total Rp:'+tran(tot,'@e 9,999,999,999')
         *
         Do pesan with 'Simpan Data? Y/T: '
         Yt=' '
         @24,49 get yt pict '!'
         Read
         If lastkey()=27 .or. yt#'Y'
            Isilagi=.t.
            Loop
EndIf
         *
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         Sele 5
         Go top
Do While .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
            Klb =kode_klb
            Kdb =kode_brg
            Kdbx=kode_bar
            Nmb =nama_brg
            St  =satuan
            Jml =jumlah
            Hp  =harga
            Lks =darilokasi
            Idt =id_data
            Isi =1
            *
            If idt=' '
               If jml#0
                  Sele 4
                  Flok()
                  Appe blan
                  Repl tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,faktur_bl with fkt2,;
                       kode_spl with ksp,nama_spl with nsp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
                       nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,darilokasi with lks
                  Ul()
                  Commit
                  *
                  Sele 3
                  Set order to 1
                  Seek kdbx
                  Rlok()
                  Repl stok_rb with (stok_rb+(jml*isi)),tanggal_rb with tgl
                  Ul()
EndIf
EndIf
            *
            Sele 5
            Repl faktur with spac(5),id_data with ' '
            Skip
EndDo
         *
         If fkt2#spac(10)
            Sele 7
            Rlok()
            Repl rp_retur with rp_retur+tot
            Ul()
            Commit
EndIf
         *
         *
         If .not. ada
            Sele 8
            Rlok()
            Repl no_returb with nn
            Ul()
            Nn=nn+1
            If nn>99999
               Nn=1
EndIf
            Do xnota
         Elseif ada
            Nn=nn
            Do xnota
EndIf
      Else
         Batal=.f.
         Sele 5
         Repl all faktur with spac(5),id_data with ' '
EndIf
      Stor 0 to jml,tot
      Kolomrtb()
      Exit
EndDo
   Ksp =spac(5)
   Fkt2=spac(10)
   Ada =.f.
EndDo
Clos all
Rele brs4,brs42,ksp,klb,jml,tot,tot2,dis4,lks,kdb,kdbx,nmb,st,isi,nn,fkt,fkt2
Rele hapus,isilagi,batal,ada,diedit
Return

*** PROSEDUR HAPUS TRANSAKSI 
Return

Procedure hpsrtr
Kolomrtb()
Sele 5
Set filter to tanggal=tgl .and. faktur=fkt
Go top
If faktur=spac(5)
   Set filter to
Return
EndIf
Declare field[8],bts[8],frm[8]
Field[1]='left(NAMA_BRG,30)'
Field[2]='left(KODE_BAR,13)'
Field[3]='JUMLAH'
Field[4]='left(SATUAN,4)'
Field[5]='HARGA'
Field[6]='JUMLAH*HARGA'
Frm[1]='@!'
Frm[2]='@!'
Frm[3]='@e 9999.99'
Frm[4]='@!'
Frm[5]='@e 9,999,999'
Frm[6]='@e 99,999,999'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Pesany='ENTER=Edit_Qty     ESC=Selesai'
Do pesan with pesany
Keyboard chr(5)
Dbedit(8,1,20,78,field,'fhprt',frm,'','',bts)
*
Kolomrtb()
Tot=0
B=8
Set filter to
Go top
Do While .not. eof() .and. (tanggal=tgl .and. faktur=fkt)
   Jhg=(jumlah*harga)
   Tot=tot+jhg
   @b,1 say &brs42
   B  =b+1
   If b>20
      Scroll(8,1,20,78,1)
      B=20
EndIf
   Skip
EndDo
Nmb=spac(30)
Kdb=spac(13)
Jml=1
Hapus=.f.
Return

Function fhprt
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
Return 1
Elseif lastkey()=27
Return 0
Elseif lastkey()=13 .and. fds='JUMLAH'
   B  =row()
   Kdb=kode_bar
   Isi=1
   Lks=darilokasi
   Nrx=nomorekod
   Stor jumlah to jml,jml2
   Set curs on
   Do pesan with ''
   @b,46 get jml pict '@e 9999.99'
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
         If lks='T'
            Repl stok_k with (stok_k-(jml2*isi))+(jml*isi)
         Else
            Repl stok_kg with (stok_kg-(jml2*isi))+(jml*isi)
EndIf
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
EndIf
EndIf
   Do pesan with pesany
   Set curs off
Return 1
Else
   Set curs off
Return 1
EndIf
Return 0

***KOLOM RETURBELI
Function kolomrtb
Do While b<21
   @b,0 say tl+spac(30)+tl+spac(13)+tl+spac(7)+tl+spac(4)+tl+spac(9)+tl+spac(10)+tl
   B =b+1
EndDo
@b,0 say chr(192)+repl(gl,30)+chr(193)+repl(gl,13)+chr(193)+repl(gl,7)+chr(193)+repl(gl,4)+chr(193)+repl(gl,9)+chr(193)+repl(gl,10)+chr(217)
B=8
Return(.t.)

*LAP.RETUR PEMB
*--------------
Procedure lrtb
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
Uselan('STOK.DTA','STOK5.NDX','','','','')
Sele 3
Uselan('RETURBN.DTA','RETURBN1.NDX','RETURBN2.NDX','RETURBN3.NDX','','')
Sele 4
*
Ksp=spac(5)
Nsp='-'
Stor tglx to tgl,tgl2
Do While .t.
   Sele 1
   Set cent on
   Set key 28 to dafspl
   Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 24,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC-Selesai  F1-Daftar Supplier'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
EndIf
   Nsp='-'
   If ksp#spac(5)
      Set order to 1
      Seek ksp
      If .not. foun()
         Set order to 2
         Seek rtri(ksp)
         Do dafspl
EndIf
      If lastkey()=27
         Loop
EndIf
      Nsp=nama_spl
EndIf
   Stor 0 to no,tot,tot2
   Sele 3
   Set filter to
   If ksp#spac(5)
      Set filter to kode_spl=ksp
EndIf
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
EndIf
   Nr=recno()
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Stor 1 to cf,no,n,hal
   Cfx =ltri(str(cf))
   Arg1='L-RETUR&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplrtb
   Set cent off
   Fkt =faktur
   Nsls=nama_spl
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
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
EndIf
         Endc
         Tot =tot +(jumlah*harga)
         Tot2=tot2+(jumlah*harga)
         ?dtoc(tanggal)+' '+left(kode_brg,7)+' '+left(nama_brg,28)+' '+darilokasi+jml+satuan+tran(harga,'@e 9,999,999 ')+tran(jumlah*harga,'@e 99,999,999')
         Skip
         If faktur#fkt
            ?spac(68)+repl(gl,12)
            ?spac(18)+nsls+'  No.Bukti: '+fkt+'   Jumlah Rp.='+tran(tot2,'@e 999,999,999')
            ?repl(gl,80)
            Fkt =faktur
            Nsls=nama_spl
            Tot2=0
            No  =no+1
            If no>=20
               No =0
               Cf =cf+1
               Cfx=ltri(str(cf))
               Arg1='L-RETUR&cfx'+'.RTF'
               Set alte to &arg1
               Do koplrtb
EndIf
EndIf
EndDo
   ?repl(gl,80)
   ?' TOTAL=Rp'+tran(tot,'@e 9,999,999,999')
   Set alte off
   Set alte to
   @24,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-RETUR&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,21,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
EndIf
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hrtb
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
Do While .not. yt$'YT'
         @24,65 get yt pict '!'
         Read
EndDo
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
         Ul()
         Commit
EndIf
      Loop
   Elseif lastkey()=27
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot
   Do koplrtb1
   Go nr
   Set cent off
Do While .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Jhg=(jumlah*harga)
         Tot=tot+jhg
         ?' '+dtoc(tanggal)+' '+faktur+' '+left(nama_spl,15)+'  '+left(kode_brg,7)+' '+nama_brg+tran(jumlah,'@e  999,999 ')+satuan+tran(harga,'@e 9999,999 ')+tran(jhg,'@e 999,999,999')
         No=no+1
         If no>55
            ?repl('-',106)
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
            Do koplrtb1
EndIf
      Skip
EndDo
   If cc=0
      Loop
EndIf
   ?repl('-',106)
   ?' Total Retur Pembelian ='+spac(68)+'Rp'+tran(tot,'@e 999,999,999')
   ?repl('=',106)
   ?''
   Set prin off
   Set devi to scre
EndDo
Set curs off
Clos all
Return
Procedure koplrtb
?' Spl: '+rtri(nsp)+'-'+rtri(ksp)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(26)+'Hal:'+tran(hal,'999')
?repl(gl,8)+chr(194)+repl(gl,7)+chr(194)+repl(gl,28)+chr(194)+repl(gl,12)+chr(194)+repl(gl,10)+chr(194)+repl(gl,10)
?'Tanggal '+tl+'Kode   '+tl+'Nama Barang'+spac(17)+tl+'   Jml.Brg. '+tl+'    @Harga'+tl+' Jumlah_Rp'
?repl(gl,8)+chr(193)+repl(gl,7)+chr(193)+repl(gl,28)+chr(193)+repl(gl,12)+chr(193)+repl(gl,10)+chr(193)+repl(gl,10)
Return
Procedure koplrtb1
Set prin on
Set devi to prin
?chr(27)+chr(33)+chr(5)
Set marg to 20
?' LAPORAN RETUR PEMBELIAN'
If ksp#spac(5)
   ?' Supl.: '+rtri(nsp)+' - '+ksp
EndIf
If tgl=tgl2
   ?' Tgl..: '+dtoc(tgl)+spac(82)+'Hal:'+tran(hal,'999')
Else
   ?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+spac(66)+'Hal:'+tran(hal,'999')
EndIf
?repl('=',106)
?' Tgl.     No.   Nama Supplier    Kode    Nama Barang                        Jml.Brg.   @Harga   Rp.Jumlah'
?repl('-',106)
No=1
Return
Procedure hrtb
@1,0 clea to 24,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai   F8=Hapus'
Do pesan with katax
Declare field[11],kop[11],frm[11],bts[11]
Field[1]='TANGGAL'
Field[2]='FAKTUR'
Field[3]='KODE_SPL'
Field[4]='left(NAMA_SPL,10)'
Field[5]='left(NAMA_BRG,30)'
Field[6]='JUMLAH'
Field[7]='HARGA'
Field[8]='DARILOKASI'
Field[9]='KODE_BAR'
Field[10]='KODE_KLB'
Kop[1]='Tanggal'
Kop[2]='No.'
Kop[3]='Spl.'
Kop[4]='Nama Supp.'
Kop[5]='Nama Barang'
Kop[6]='  Qty.'
Kop[7]='   @Harga'
Kop[8]='Lks'
Kop[9]='Kode Brg.'
Kop[10]='Klp.'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@!'
Frm[6]='@e 9999.99'
Frm[7]='@e 9,999,999'
Frm[8]='@!'
Frm[9]='@!'
Frm[10]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Bts[7]=tl
Bts[8]=tl
Bts[9]=tl
Bts[10]=tl
Go nr
Dbedit(2,0,21,79,field,'fhrtb',frm,kop,gl,bts)
*
Set curs on
Return

Function fhrtb
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
EndIf
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-7
      Set colo to gr+*/w
      @row(),col() say &fds
      Set colo to /w
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Ht=' '
Do While .not. ht$'YT'
         @24,47 get ht pict '!'
         Read
EndDo
      If ht='Y'
         Kdb=kode_bar
         Jml=jumlah
         Lks=darilokasi
         If .not. dele()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
            *
            Sele 2
            Seek kdb
            Rlok()
            If lks='T'
               Repl stok_k with (stok_k-jml)
            Else
               Repl stok_kg with (stok_kg-jml)
EndIf
            Ul()
            Commit

            Sele 3
EndIf
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Othe
Return 1
Endc
Return 0

* LAP.RETUR PEMB/ITEM
*---------------------
Procedure lrtb2
Sele 1
*
Sele 2
Uselan('STOK.DTA','STOK4.NDX','','','','')
*
Stor ' ' to jlap,ket
Stor tglx to tgl,tgl2
Nmb=spac(30)
Do While .t.
   Sele 2
   Set filter to
   @1,0 clea to 24,79
   @2,24 say 'Nama Brg:'get nmb pict '@!'
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
EndIf
   If nmb=spac(30)
      Loop
EndIf
   Seek nmb
   If .not. foun()
      Seek rtri(nmb)
      Do dafbrg
      If lastkey()=27
         Loop
EndIf
EndIf
   Klb=kode_klb
   Kdb=kode_bar
   Nmb=nama_brg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @2,34 say nmb
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @24,0
   If lastkey()=27
      Loop
EndIf
   Sele 1
   Uselan('RETURBN.DTA','RETURBN2.NDX','','','','')
   *
   @1,0 clea to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Set alte on
   Set alte to L-RETUR.RTF
   ?' LAPORAN RETUR PEMBELIAN PER ITEM BARANG'
   ?' NAMA BRG: '+rtri(nmb)
   ?' TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   ?chr(218)+repl(gl,4)+chr(194)+repl(gl,10)+chr(194)+repl(gl,20)+chr(194)+repl(gl,10)+chr(194)+repl(gl,7)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)+chr(191)
   ?tl+' No.'+tl+'Tanggal   '+tl+'Nama Supplier       '+tl+'No.Faktur '+tl+'   Qty.'+tl+'   @Harga'+tl+'  Rp.Jumlah'+tl
   ?chr(195)+repl(gl,4)+chr(197)+repl(gl,10)+chr(197)+repl(gl,20)+chr(197)+repl(gl,10)+chr(197)+repl(gl,7)+chr(197)+repl(gl,9)+chr(197)+repl(gl,11)+chr(180)
   Y=1
   Stor 0 to stm,hp,hj,jml,tot,tot2
   Z=ltri(str(y))
Do While .t.
      Seek klb+kdb
      St =satuan
Do While .not. eof() .and. ( kode_klb=klb .and. kode_bar=kdb)
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jhg=(jumlah*harga)
            Stm=stm+jumlah
            Tot=tot + jhg
            Jumlah_rtr=0
            Do fmtjml
            ?tl+tran(y,'999.')+tl+dtoc(tanggal)+tl+left(nama_spl,20)+tl+faktur+'   '+darilokasi+' '+tl+jml+tl+tran(harga,'@e 9,999,999')+tl+tran(jhg,'@e 999,999,999')+tl
            Y = y + 1
EndIf
         Skip
EndDo
      Exit
EndDo
   Nmb =spac(30)
   Jml2=stm
   Pesanx=tran(jml2,'@e 999,999 ')+st
   ?chr(192)+repl(gl,4)+chr(193)+repl(gl,10)+chr(193)+repl(gl,20)+chr(193)+repl(gl,10)+chr(193)+repl(gl,7)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(217)
   ?spac(17)+'TOTAL ='+spac(24)+pesanx+'  Rp'+tran(tot, '@e 9,999,999,999')
   Set alte off
   Set alte to
   Do pesan with 'ESC=Menu Selanjutnya'
   Memoedit(Memoread('L-RETUR.RTF'),1,0,22,240,.t.,'',240)
   Do pesan3 with 'ESC=Selesai   F10=Cetak'
   If nlp=0
      Loop
EndIf
   Do cp
   If cc=0
      Loop
EndIf
   Set prin on
   Set devi to prin
   Set marg to 10
   ?chr(27)+chr(33)+chr(4)
   Type L-RETUR.RTF to prin
   Set prin off
   Set devi to scre
EndDo
Clos all
Rele klb,jlap,ket,nsp,nmb,z,tgl2,jml,xjml,hp,hj,tot,tot2
Return

*EOF RTB
