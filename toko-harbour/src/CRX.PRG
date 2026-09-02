* PROSEDUR-2 BANTUAN
Proc cekdata
*
Uselan('DATA.SET','','','','','')
xPos =rtri(keterangan)
dTgl =day(tgl)
*
Do bacafile
Uselan('JUAL.DTA','','','','','')
If .not. file('&nf0')
   Copy stru to &nf0
   Clos all
   B=23
   Indx('&nf0','dtos(TANGGAL)+KODE_LGN+FAKTUR','&nf1',2)
   Indx('&nf0','KODE_BAR+dtos(TANGGAL)','&nf2',2)
   Indx('&nf0','dtos(TANGGAL)+KODE_KSR+FAKTUR','&nf3',2)
Endi
Clos all
*
Nf0='KAS-'+right(dtoc(tgl),2)+subs(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+right(dtoc(tgl),2)+subs(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+right(dtoc(tgl),2)+subs(dtoc(tgl),4,2)+'.NDX'
If .not. file('&nf0')                 
   Useidx('KAS.DTA')
   Copy stru to &nf0
   Close all
   B=23
   Indx('&nf0','dtos(TANGGAL)+KODE_POS+ID_POS','&nf1',2)
   Indx('&nf0','KODE_POS+dtos(TANGGAL)','&nf2',1)
Endi
*
Brs1 ="(jumlah * (harga_2- ((harga_2*(diskon/100))+diskon_rp)))"
Brs13="(jml * (hj- ((hj*(dis1/100))+dis2)))"
Retu

Func brs2
Kdby=kode_bar
Set colo to &warna3
@b,0 say spac(8)+tl+spac(30)+tl+spac(12)+tl+spac(10)+tl+spac(5)+tl+spac(10)
@b,0 say iif(len(rtri(kdby))>=9,left(kdby,3)+'x'+right(rtri(kdby),4),left(kdby,8))
@b,9 say left(nama_brg,30)
@b,40 say tran(jumlah,'@e 9999.99 ')+left(satuan,4)
@b,53 say tran(harga_2,'@e 99,999,999')
@b,64 say iif(diskon#0,tran(diskon,'@e 99.99'),'     ')
@b,70 say tran(jhg,'@e 99,999,999')
Retu(.t.)

Func brs3
Kdby=kdbx
Set colo to &warna3
@b,0 say spac(8)+tl+spac(30)+tl+spac(12)+tl+spac(10)+tl+spac(5)+tl+spac(10)
@b,0 say iif(len(rtri(kdby))>=9,left(kdby,3)+'x'+right(rtri(kdby),4),left(kdby,8))
@b,9 say left(nmb,30)
@b,40 say tran(jml,'@e 9999.99 ')+left(st,4)
@b,53 say tran(hj,'@e 99,999,999')
@b,64 say iif(dis1#0,tran(dis1,'@e 99.99'),'     ')
@b,70 say tran(jhg,'@e 99,999,999')
Retu(.t.)

Proc bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
Retu

Proc bacaflkas
Nf0='KAS-'+subs(dtoc(tgl),9,2)+subs(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+subs(dtoc(tgl),9,2)+subs(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+subs(dtoc(tgl),9,2)+subs(dtoc(tgl),4,2)+'.NDX'
Retu

***Indeks File
Func Indx
Para Arg4,arg5,arg6,arg3
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   Useidx("&arg4")
   Inde on &arg5 to &arg6
Endi
Clos all
Retu .T.

Proc hari
Hh=dow(tgl)
If hh=1
   Hr='Minggu'
Elseif hh=2
   Hr='Senin'
Elseif hh=3
   Hr='Selasa'
Elseif hh=4
   Hr='Rabu'
Elseif hh=5
   Hr='Kamis'
Elseif hh=6
   Hr="Jum'at"
Elseif hh=7
   Hr='Sabtu'
Endi
Retu

Proc pesan
Para psn
Set colo to &warna5
@24,0 say spac(80)
@24,40-(len(psn)/2) say psn
Set colo to &warna1
Retu

Proc pesan2
Para psn
Tone(499,1)
Tone(300,1)
Set colo to &warna5
@24,0 say spac(80)
@24,40-(len(psn)/2) say psn
Set colo to &warna1
Wait''
@24,0
Retu

Proc pesan3
Para psn
Set colo to &warna5
@24,0 say spac(80)
@24,40-(len(psn)/2) say psn
Set colo to &warna1
I=0
Do whil i#-9
   I=inke()
   If i=27 .or. i=28 .or. i=-3 .or. i=-4 .or. i=-7 .or. i=70 .or. i=74 .or. i=80 .or. i=102 .or. i=106 .or. i=112
      Exit
   Endi
Endd
If i=27 .or. i=28 .or. i=-3 .or. i=-4 .or. i=-7 .or. i=70 .or. i=74 .or. i=80 .or. i=102 .or. i=106 .or. i=112
   Nlp=0
Else
   Nlp=1
Endi
@24,0
Retu

***PROSES ULANGI??
Function ulangi
I=inke()
Do whil i#13 .and. i#84 .and. i#116
   I=inke()
   If i=27 .or. i=89 .or. i=121
      Retu(.t.)
   Endi
Endd
Retu(.f.)

Proc xnota
Fkt=right('00000'+ltri(str(nn)),5)
Retu

Proc tampil
Save scre to stampil
Kata1=rtri(aidi1)
Kata2=rtri(aidi2)
Kata3=' '+rtri(nsls)+' . '+kata1+' . '+kata2+' . '+hr+', '+dtoc(tgl)+' . '+iif(rpoin#0,'1 Point = Rp '+ltri(tran(rpoin,'@e 999,999'))+' . ','')
Kata4=' '+kata1+' . '+kata2+' . '+hr+', '+dtoc(tgl)+' . '
Pjg =len(kata3)
Pjg2=len(kata4)
Do whil .t.
   Set colo to &warna1
   @4,0 to 4,79 doub
   @6,0 say repl(gl,8)+chr(194)+repl(gl,30)+chr(194)+repl(gl,12)+chr(194)+repl(gl,10)+chr(194)+repl(gl,5)+chr(194)+repl(gl,10)
   Do nomor with tot,1,56
   Do nomor with kbl,20,53
   Do pesan with 'F3=Kas   F4=Rekap   F5=Set_Print   F10=Ctk_Ulang   F11=CekPoin   ENTER=Transaksi'
   Set curs off
   Stor 1 to awl,jln
   I=0
   Timernd1= Val(Subs(Time(), 4, 2))
   Do whil awl<=pjg
      Katax=subs(kata3,awl,(pjg-awl))+subs(kata3,1,awl)
      Katay=subs(kata4,awl,(pjg-awl))+subs(kata4,1,awl)
      Set colo to &warna5
      @0,71  say time()
      If dow(tgl)=1
         If idis#'0'
            Set colo to /bg
         Endi
      Elseif dow(tgl)=2
         Set colo to /w
      Elseif dow(tgl)=3
         If idis#'0'
            Set colo to w+/r
         Endi
      Elseif dow(tgl)=4
         If idis#'0'
            Set colo to br+/b
         Endi
      Elseif dow(tgl)=5
         If idis#'0'
            Set colo to w+/gr
         Endi
      Elseif dow(tgl)=6
         If idis#'0'
            Set colo to gr+/b
         Endi
      Elseif dow(tgl)=7
         If idis#'0'
            Set colo to gr+/br
         Endi
      Endi
      @5,0
      @5,12 say subs(katax,1,54)
      Awl=awl+1
      Jln=jln+1
      I  =inke(.3)
      If i#0
         Exit
      Endi
      If awl=pjg
         Awl=1
      Endi
      If jln>=6
         Jln=1
         Nulise=left(katay,20)+spac(20)
         *Kedisple(nulise)
      Endi
      *
      Timernd2= Val(Subs(Time(), 4, 2))
      If (timernd2 - timernd1 = 3 .OR. timernd2 - timernd1 = -55)
         Save scre to sdaf
         Tampil2()
         Timernd1= Val(Subs(Time(), 4, 2))
         Rest scre from sdaf
      Endi
   Endd
   If i=-9
      Fktx=Fkt
      Fkt =val(fktx)-1
      Tglz=tgl
      Stor nsls to nsls2,nsls4
      *
      Set curs on
      Do pesan with 'Tanggal:              No.Nota:         Nama Ksr:          '
      @24,19 get tgl
      @24,41 get fkt pict '99999'
      @24,59 say rtri(nsls2)
      Read
      Fkt=right('00000'+ltri(str(fkt)),5)
      If lastkey()=27
         Tgl=tglz
         Fkt=fktx
         Loop
      Endi
      Do bacafile
      *
      Sele 4
      Uselan('&nf0','&nf3','','','','')
      Seek dtos(tgl)+ksr+fkt
      If foun()
         Ulang=.t.
         Nr   =recno()
         Plg  =kode_lgn
         Ksls =kode_sls
         Kolomjl()
         Set colo to &warna1
         @4,0 say repl(gl,8)+chr(194)+repl(gl,30)+chr(194)+repl(gl,12)+chr(194)+repl(gl,10)+chr(194)+repl(gl,5)+chr(194)+repl(gl,10)
         @5,0 say 'Kode Brg'+tl+'Nama Barang'+spac(19)+tl+' Jml.Barang '+tl+'    @Harga'+tl+'@Dis%'+tl+'Jumlah Rp.'
         @6,0 say repl(gl,8)+chr(197)+repl(gl,30)+chr(197)+repl(gl,12)+chr(197)+repl(gl,10)+chr(197)+repl(gl,5)+chr(197)+repl(gl,10)
         Set colo to &warna2
         @1,18 clea to 3,79
         @1,63 say 'Tgl.: '
         @2,63 say 'Nota: '
         @3,63 say 'Ksr.: '
         Set colo to &warna2
         @1,69 say tgl
         @2,69 say fkt+spac(5)
         @3,69 say left(nsls2,10)
         Stor 0 to no,tot,tjml,dis31,byr,blt,dns
         Set filter to
         Do whil .not. eof() .and. ( tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt )
            If left(nama_brg,6)#'Diskon'
               Jhg=&brs1
               Brs2()
               If jumlah#0
                  No =no+1
               Endi
               B  =b+1
               Tot=tot+jhg
               Tjml=tjml+jumlah
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               Endi
            Elseif left(nama_brg,6)='Diskon'
               Dis31=dis31+abs(harga_2)
            Endi
            Byr=byr+rp_bayar
            Blt=blt+rp_bulat
            Dns=dns+rp_donasi
            *
            Skip
         Endd
         Set colo to &warna1
         @b,0 say spac(8)+tl+spac(30)+tl+spac(12)+tl+spac(10)+tl+spac(5)+tl+spac(10)
         *
         Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
         Do nomor with tot,1,56
         Set colo to &warna6
         If blt#0
            @19,57 say 'Pembulatan :'
            @19,75 say tran(blt,'@e 9,999')
         Endi
         @20,69 say tran(tot+blt,'@e 999,999,999')
         @21,69 say tran(dis31,'@e 999,999,999')
         @22,69 say tran(byr,'@e 999,999,999')
         @23,69 say iif(dns>0,tran(dns,'@e 999,999,999'),'')
         *
         Stor spac(30) to nsp,jab
         Kta=spac(10)
         If plg#spac(10)
            Sele 10
            Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
            Seek dtos(tgl)+plg+fkt
            If foun()
               Dis3 =diskon
               Dis31=rp_diskon
               Pnskr=jml_poin
               xPoin=pra_poin
               Tot3 =rp_sisa
            Elseif .not. foun()
               Uselan('CUST.DTA','CUST.NDX','','','','')
               Seek plg
            Endi
            Nsp=nama_lgn
            Jab=almt_lgn
            Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
            *
            Set colo to w+/b
            @20,7  say plg
            @21,7  say nsp
            @22,7  say left(jab,25)
            @23,7  say spac(13)
            @23,7  say ltri(tran(pnskr,'9999'))
            If tot3#0
               Set colo to gr+/b
               @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
               @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
               @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
               @23,33 say 'Sisa/Kurang: '+ltri(tran(kbl,'@e 999,999,999'))
               Set colo to /bg
            Endi
            Close
         Endi
         *
         Do pesan3 with 'F4=Nota_Batal     F5=Edit     F10=Cetak_Nota'
         If lastkey()=-3
            Do pesan with ''
            Do paswod2
            If lastkey()=27 .or. xOwner=.f.
               Rest scre from stampil
               Tgl=tglz
               Fkt=fktx
               Nsp=spac(30)
               Jab=spac(30)
               Kta=spac(10)
               Plg=spac(10)
               Ksls=spac(5)
               Loop
            Endi
            *
            Do pesan with 'Nota diatas akan DiHapus? Y/T'
            Yt=' '
            Do whil .not. yt$'YT'
               @24,55 get yt pict '!'
               Read
               If lastkey()=27
                  Exit
               Endi
            Endd
            If yt='Y'
               Do pesan with 'Tunggu, sedang dihapus'
               Sele 4
               Go nr
               Do whil .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt)
                  Kdbx=kode_bar
                  Jml =jumlah
                  Rlok()
                  Repl keterangan with dtoc(date())+' '+left(time(),5)+' '+ksa+'KSR '
                  Dele
                  Ul()
                  Commit
                  *
                  Sele 3
                  Set order to 1
                  Seek kdbx
                  Rlok()
                  Repl stok_k&lks with (stok_k&lks-jml)
                  Ul()
                  Commit
                  *
                  Sele 4
                  Skip
               Endd
               If plg#spac(10)
                  Sele 10
                  Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
                  Seek dtos(tgl)+plg+fkt
                  Rlok()
                  Dele
                  Ul()
                  Commit
                  Close
               Endi
            Endi
            *
         Elseif lastkey()=-4
            Do pesan with ''
            Do paswod2
            If lastkey()=27 .or. xOwner=.f.
               Rest scre from stampil
               Fkt=fktx
               Tgl=tglz
               Loop
            Endi
            xAda=0
            Stor 1 to n,no
            *
            Sele 4
            Go nr
            Do whil .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt)
               Lks =kode_lks
               Ksp =kode_spl
               Klb =kode_klb
               Kdb =kode_brg
               Kdbx=kode_bar
               Nmb =nama_brg
               St  =satuan
               Isi =isine_b
               Jml =jumlah
               Hp  =harga
               Hj  =harga_2
               Dis1=diskon
               Dis2=diskon_rp
               Ket =keterangan
               Ipn =id_point
               Ipn2=id_point2
               Nrx =recno()
               Nrbyr=recno()
               *
               Rlok()
               Repl rp_bayar with 0,rp_donasi with 0,rp_bulat with 0
               Ul()
               *
               If left(nmb,6)='Diskon'
                  Rlok()
                  Dele
                  Ul()
               Elseif left(nmb,6)#'Diskon'
                  Sele 5
                  If n=1
                     Go top
                  Endi
                  Repl tanggal with tgl,faktur with fkt,kassa with ksa,kode_ksr with ksr,nama_ksr with nsls,kode_lks with lks,kode_spl with ksp,kode_lgn with plg,;
                       kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,isine_b with isi,jumlah with jml,harga with hp,harga_2 with hj,;
                       diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2,keterangan with ket,rectran with nrx,id_data with '1'
                  Skip
                  N =n+1
                  No=no+1
               Endi
               *
               Sele 4
               Skip
            Endd
            Lks=lksx
            Stor .t. to diEdit,isilagi
            Exit
            *
         Elseif lastkey()=-9
            Sele 4
            Go nr
            Do ctknota
            *If iprn='D' .or. (iprn='T' .and. idCut='T')
            *   Do kopnota
            *Endi
         Else
            Ulang=.f.
         Endi
      Endi
      Sele 4
      Close
      *
      Ulang=.f.
      Fkt =fktx
      Tot =0
      Jml =1
      Tgl =tglz
      Nsls=nsls4
      Jab =spac(30)
      Kta =spac(10)
      Plg =spac(10)
      Ksls=spac(5)
      Stor spac(30) to nsp,nsls5
      Rele tgl3,tglz,sls2,nsls3,nsls4
      Rest scre from stampil
      Loop
   Elseif i=-4
      Save scre to sdaf
      @24,0
      Set curs on
      Set colo to w+/br
      @7,18 clea to 23,62
      @7,18 to 23,62
      @7,24 say  'SET PRINTER, LACI/CASHDRW & KODE'
      @8 ,21 say 'a. Printer= Dotmatrix/Thermal:    [D/T]'
      @9 ,21 say '   Lebar Roll=  58mm / 80mm..:    [5/8]'
      @10,21 say 'b. Batas Tepi Kiri Nota......:    spasi'
      @11,21 say 'c. Jarak Baris antar Nota....:    spasi'
      @12,21 say 'd. Tersambung ke CashDrawer..:    [Y/T]'
      @13,21 say 'e. Printer pakai AutoCutter..:    [Y/T]'
      @14,21 say 'f. Kunci Pencarian Data Brg..:    [0/1]'
      @15,21 say '   0 =Acak      1 =Huruf Depan'
      @16,21 say 'g. Tampil Jumlah Poin di Nota:    [012]'
      @17,21 say 'h. Mode Transaksi Kasir......:    [ABC]'
      @18,21 say '   A =Lengkap   B/C =Sederhana'
      @19,21 say 'i. Komputer ini Server/Client:    [S/C]'
      @20,21 say 'j. Sumber Pencarian Data Brg.:    [0/1]'
      @21,21 say '   0 = dari Data Lokal'
      @22,21 say '   1 = dari Data Server/LAN'
      @8 ,52 get iprn pict '@!'
      @9 ,52 get lr pict '9'
      @10,52 get jt pict '99'
      @11,52 get jn pict '99'
      @12,52 get idrw  pict '@!'
      @13,52 get idcut pict '@!'
      @14,52 get jk pict '!'
      @16,52 get ipoin pict '!'
      @17,52 get xKsr  pict '!'
      @19,52 get ikomp pict '!'
      @20,52 say idata
      Read
      If ikomp='C'
         @20,52 get idata pict '!'
         Read
      Endi
      Rest scre from sdaf
      *
      If lastkey()#27
         Sele 99
         Useidx('KASSA.SET')
         Loca for no_kassa=ksa
         Repl mode_kasir with xKsr,id_prin with iif(iprn#'D','T',iprn),id_drwr with iif(idrw#'Y','T',idrw),id_cut with iif(idcut#'Y','T',idcut),id_komp with iif(ikomp#'C','S',ikomp),;
              id_data with iif(idata#'0','1',idata),id_poin with iPoin,cari_kode with iif(jk#'1','0',jk),jark_nota with jn,jark_tepi with jt,lebr_roll with iif(lr#'5','8',lr),tanggal with tgl
         Commit
         Close
      Endi
   Elseif i=-2
      Save scre to skas
      Set curs on
      Do inkas
      Set curs off
      Warna('g,bg+/b','')
      Rest scre from skas
      Loop
   Elseif i=-3
      Tglz =tgl
      Ulang=.f.
      xOwner=.f.
      Do pesan with ''
      Do paswod2
      If lastkey()=27 .or. xOwner=.f.
         Rest scre from stampil
         Tgl=tglz
         Loop
      Endi
      *
      Pil=' '
      Save scre to skas
      Set curs on
      If xOwner .and. lastkey()#27
         Do whil .t.
            Do pesan with 'Jenis Lap,  1=Rekap Barang   2=Rekap Nota   3=Rekap  [1/2/3]:      ESC=Selesai'
            @24,63 get pil pict '!'
            Read
            If lastkey()=27 .or. pil='0'
               Exit
            Endi
            If pil='1'
               Do lpj99
            Elseif pil='2'
               Do lnota
            Else
               Do lnota2
            Endi
         Endd
      Elseif lastkey()#27
          Do lnota2
      Endi
      Set curs off
      Set colo to /bg,w+/r
      Rest scre from skas
      Tgl=tglz
      Tot=0
      Loop
   Elseif i=-7
      Sls2=spac(20)
      Do pesan with ''
      Set colo to w+/b*,b/b
      @24,69 say '_'
      Set colo to w+/r
      @24,53  say 'Kode Kasir/User:'get sls2 pict '@!S10'
      Read
      Set colo to w+/r,w+/b
      If lastkey()=27 .or. sls2#sls
         Loop
      Endi
      Do cp
      Set devi to prin
      Set prin on
      If idrw='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
      Endi
      Set devi to scre
      Set prin off
      Loop
   Elseif i=-8
      Do kalk
      Loop
   Elseif i=81 .or. i=113
      Idis='1'
      Pilwarna=.t.
      *
      Do warna
      Exit
   Elseif i=65 .or. i=97
      Idis='2'
      Pilwarna=.t.
      *
      Do warna
      Exit
   Elseif i=90 .or. i=122
      Idis='0'
      Pilwarna=.t.
      *
      Do warna
      Exit
   Elseif i=389
      Plg=spac(10)
      Save scre to skas
      Stor .t. to member,cekpoin,hanyaCekp
      Do cekplg
      Stor .f. to cekpoin,member,hanyaCekp
      Rest scre from skas
      Plg  =spac(10)
      xPoin=0
      Loop
   Endi
   If i=13 .or. i=27
      Exit
   Endi
Endd
Rest scre from stampil
Set colo to &warna5
@0,71 say time()
Set curs on
Set colo to &warna6
@1,19 clea to  3,79
If diEdit
   Set colo to &warna2
   @1,63 say 'Tgl.: '
   @2,63 say 'Nota: '
   @3,63 say 'Ksr.: '
   Set colo to &warna6
   @1,69 say tgl
   @2,69 say fkt+spac(5)
   @3,69 say left(nsls2,10)
Endi
If lihat
   Set colo to &warna3
   @12,1 say spac(30)+'Lihat Harga Barang'+spac(30)
   Set colo to &warna1
Endi
Retu

Proc warna
If idis='1'
   Warna1='g,gr+/b'    &&kop/grs.kolom
   Warna2='w,w+/b'     &&idents
   Warna3='w+,w+/r'    &&isi_item
   Warna4='w+/b,/w'    &&dafkode
   Warna5='w/r'
   Warna6='gr+'
Elseif idis='2'
   Warna1='/bg,gr+/b'
   Warna2='w/b,w+/b'
   Warna3='/bg,gr+/b'
   Warna4='w+/b,/w'
   Warna5='w+/r'
   Warna6='gr+/b'
Else
   Warna1='w,/w'
   Warna2='w,/w'
   Warna3='w,/w'
   Warna4='w,/w'
   Warna5='/w,/w'
   Warna6='w'
Endi
If pilwarna
   Sele 99
   Useidx('KASSA.SET')
   Loca for no_kassa=ksa
   Repl id_disp with idis
   Commit
   Close
Endi
Retu

Proc tampil2
Set colo to gr+
Clea
Stor 1 to br,klm,awl,tjam
Tot2= 0
Do while (tot2 = 0)
   B  = Val(right(Time(), 2)) + klm
   Kl = Val(right(Time(), 2)) + 101 + br
   Br = mod(b, 24)
   Klm= mod(kl, 80)
   @ 0,1 say hr+', '+dtoc(tgl)+'  '+left(time(),5)
   @ br, klm say kata1+' // '+nsls
   Awl=awl+1
   If awl=pjg2
      Awl=1
   Endi
   Katay=subs(kata4,awl,(pjg2-awl))+subs(kata4,1,awl)
   Tot2 =InKey(2)
   Tjam =tjam+1
   If tjam=4
      Tjam=1
      Nulise=left(katay,20)+spac(20)
   Else
      Nulise=left(katay,20)+spac(7)+left(time(),5)+spac(8)
   Endi
   *Kedisple(nulise)
   @ br, klm say spac(50)
Enddo
Set colo to /bg
Retu

Proc cp
Do whil .t.
   If .not. PrinterReady()
      Tone(700,1)
      Tone(499,1)
      Do pesan with 'Printer belum siap,  ESC=Batal  F10=Coba Lagi'
      I=0
      Do whil i#-9
         I=inke()
         If i=27
            Exit
         Endi
      Endd
      If i=27
         Exit
      Elseif i=-9
         If PrinterReady()
            Exit
         Endi
      Endi
   Else
      Exit
   Endi
Endd
Do pesan with 'Tunggu, sedang dicetak'
Retu


* Cek kesiapan printer untuk Windows/Harbour
Func PrinterReady
Return .T.

***Buka File di LAN
Func UseLan
Para Arg1,arg2,arg3,arg4,arg5,arg6
Local cAlias
Do while (.T.)
   cAlias=HBALIAS(arg1)
   Use (arg1) Alias (cAlias)
   If (!neterr())
      Exit
   Endi
   Do pesan with 'Tunggu, File '+&arg1 +' Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Endd
Set inde to &arg2,&arg3,&arg4,&arg5,&arg6
Retu .T.

***Buka File u/ Reindex-Exclusive
Func UseIdx
Para Arg1
Local cAlias
Do whil .t.
   cAlias=HBALIAS(arg1)
   Use (arg1) Alias (cAlias) exclusive
   If (!neterr())
      Exit
   Endi
   Tone(700,6)
   Do pesan with 'Proses diTunda, ada yg memBuka File '+arg1+'         '
   Yt=' '
   Set colo to gr+/b
   @24,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   Set colo to w+/b
   If yt#'Y'
     Batal=.t.
     Exit
   Endi
   @24,0
   Inkey(0.5)
Endd
Retu(.T.)
Func HBALIAS
Para cFile
Local cAlias,nPos
cAlias=upper(cFile)
nPos=rat('\',cAlias)
If nPos>0
   cAlias=substr(cAlias,nPos+1)
Endi
nPos=rat('/',cAlias)
If nPos>0
   cAlias=substr(cAlias,nPos+1)
Endi
nPos=rat('.',cAlias)
If nPos>0
   cAlias=left(cAlias,nPos-1)
Endi
cAlias=strtran(cAlias,'-','_')
cAlias=strtran(cAlias,'.','_')
cAlias=strtran(cAlias,' ','_')
Retu cAlias
Func RLOK
Do while .not. Rlock()
   If rlock()
      Exit
   Endi
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Endd
Return(.T.) 

Func FLOK
Do while .not. FLock()
   If flock()
      Exit
   Endi
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Endd
Return(.T.)

Func UL
Unlock
Return(.T.)

Proc daflgn
Local cSearch := Upper(AllTrim(plg))
Save scre to sdaf
Set order to 2
Set filter to
If !Empty(cSearch) .and. cSearch != '0'
   Set filter to (cSearch $ Upper(nama_lgn)) .or. (cSearch $ Upper(kode_lgn))
   Go top
   If Eof()
      Set filter to
      Go top
      Do pesan with 'Data tdk ditemukan, menampilkan semua.  ENTER=Pilih  ESC=Batal'
   Else
      Do pesan with 'Pilih Pelanggan: Panah u/ Pilih, Huruf u/ Cari, ENTER=Pilih, ESC=Batal'
   EndIf
Else
   Go top
   Do pesan with 'Pilih Pelanggan: Panah u/ Pilih, Huruf u/ Cari, ENTER=Pilih, ESC=Batal'
EndIf

Set colo to w+/br
@1,10 clea to 22,70
@1,10 to 1,70 doub
@22,10 to 22,70
Declare field[4],kop[4],bts[4]
field[1] := "KODE_LGN + tl + Left(NAMA_LGN, 24) + tl + Left(ALMT_LGN, 20)"
kop[1]   := "Kode      Nama Pelanggan           Alamat"
bts[1]   := ""
dbedit(2, 11, 21, 69, field, "flgn", "", kop, gl, bts)

Set filter to
Set order to 1
Rest scre from sdaf
Set colo to &warna1
Set curs on
Retu

Function flgn(mode, pointer)
Do case
   Case mode < 4
      Return 1
   Case lastkey() == 27
      Return 0
   Case lastkey() == 13
      Plg := kode_lgn
      Keyboard Chr(13)
      Return 0
   Case (lastkey() >= 65 .and. lastkey() <= 90) .or. (lastkey() >= 97 .and. lastkey() <= 122) .or. (lastkey() >= 48 .and. lastkey() <= 57)
      Set order to 2
      Seek Upper(Chr(lastkey()))
      Return 2
   Otherwise
      Return 1
EndCase
Return 0

Proc dafbrg
Save scre to sdaf
Do pesan with 'Pilih item Barangnya, kemudian Enter'
Set colo to &warna4
Set filter to
If ikomp='C' .and. idata='0' .and. jk='0'
   Sele 77
   @1,69 say 'STOK LOKAL'
Endi
*
If jk='0'
   Nmb =iif(carikode,alltrim(kdb),alltrim(nmb))
   Set filter to AT(nmb,iif(carikode,kode_bar,nama_brg))>0
   Go top
   If eof()
      Do pesan with 'DATA BARANG TIDAK DITEMUKAN...!'
   Endi
Endi
@1,0 clea to 22,79
@1,0 to 1,79
@23,0 to 23,79
@1,0 say 'Cari= '+rtri(nmb)
Declare field[11],kop[11],frm[11],bts[11]
Field[1]="left(NAMA_BRG,30)+' '+KODE_BAR+tran(HARGA_2,'@e  9,999,999')+tran(iif(idp='1',HARGA_32,HARGA_3),'@e 9,999,999 / ')+ltri(tran(BATAS_3,'9999 '))+SATUAN"
Field[2]="left(NAMA_BRG,30)+' '+KODE_BAR+tran(HARGA_2,'@e  9,999,999')+tran(HARGA_3,'@e 9,999,999 ')+tran(((STOK_A+STOK_M) - (STOK_K+STOK_RB)),'@e 9999,999 ')+SATUAN"
Kop[1]='Nama Barang                    Barcode          @Harga   @Grosir   Sat'
Kop[2]='Nama Barang                    Barcode          @Harga   @Grosir   Sisa Stok'
Frm[1]=''
Frm[2]=''
Bts[1]=' '
Bts[2]=' '
Dbedit(2,0,22,79,field,'xkdb2',frm,kop,chr(196),bts)
*
Set filte to
Rest scre from sdaf
Set colo to &warna1
Retu

Function xkdb2
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=13
      Kdbx=kode_bar
      Nmb =nama_brg
      Retu 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122)
      Seek uppe(chr(lastkey()))
      Retu 1
   Othe
      Retu 1
Endc
Retu 0

***NOMOR YG GEDE-2
Proc nomor
Para tot,br,klm
Local cBox := Chr(219)
Local cHalf := Chr(220)
If ValType(tot) == "C"
   tot := Val(tot)
EndIf
tot = Round(tot, 0)
Set colo to &warna6
@br,klm-iif(br=20,20,38) clea to br+2,klm+3
Chtot=ltri(str(tot, 14, 0))
Awl=1
Akr=len(chtot)
DO WHILE awl <= LEN(chtot)
   IF awl = 4 .OR. awl = 7
      @br+2, klm+1 SAY ' ' + cHalf
      klm = klm - 2
   ENDIF
   DO CASE
      CASE SUBSTR(chtot, akr, 1) = '1'
         @br  , klm+2 SAY cHalf
         @br+1, klm+2 SAY cBox
         @br+2, klm+2 SAY cBox
         klm = klm + 2
      CASE SUBSTR(chtot, akr, 1) = '2'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cHalf + cHalf + cBox
         @br+2, klm SAY cBox  + cHalf + cHalf
      CASE SUBSTR(chtot, akr, 1) = '3'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY ' '   + cHalf + cBox
         @br+2, klm SAY cHalf + cHalf + cBox
      CASE SUBSTR(chtot, akr, 1) = '4'
         @br  , klm SAY cHalf + ' '   + cHalf
         @br+1, klm SAY cBox  + cHalf + cBox
         @br+2, klm SAY '  '  + cBox
      CASE SUBSTR(chtot, akr, 1) = '5'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cBox  + cHalf + cHalf
         @br+2, klm SAY cHalf + cHalf + cBox
      CASE SUBSTR(chtot, akr, 1) = '6'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cBox  + cHalf + cHalf
         @br+2, klm SAY cBox  + cHalf + cBox
      CASE SUBSTR(chtot, akr, 1) = '7'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY '  '  + cBox
         @br+2, klm SAY '  '  + cBox
      CASE SUBSTR(chtot, akr, 1) = '8'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cBox  + cHalf + cBox
         @br+2, klm SAY cBox  + cHalf + cBox
      CASE SUBSTR(chtot, akr, 1) = '9'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cBox  + cHalf + cBox
         @br+2, klm SAY cHalf + cHalf + cBox
      CASE SUBSTR(chtot, akr, 1) = '0'
         @br  , klm SAY cHalf + cHalf + cHalf
         @br+1, klm SAY cBox  + ' '   + cBox
         @br+2, klm SAY cBox  + cHalf + cBox
   ENDCASE
   awl = awl + 1
   akr = akr - 1
   klm = klm - 4
ENDDO
Set colo to &warna3
Retu

Proc xubahjml2   && UBAH QTY.AWAL
Ubahjml2=.t.
Keyboard chr(13)
Retu

Proc xubahjml   && UBAH 1 ITEM DIATAS
Ubahjml=.t.
Keyboard chr(13)
Retu

Proc xadadis  && ADA DISKON?
Adadis=.t.
Keyboard chr(13)
Retu

Proc xadamem  && ADA MEMBER?
Member=.t.
Keyboard chr(13)
Retu

Proc xlihat  && LIHAT HRG
Lihat=.t.
Save scre to slihat
Set colo to gr+/b
@b+2,1 say spac(30)+'Lihat Harga Barang'+spac(30)
Set colo to /bg
Retu

Proc paswod2
xOwner=.f.
xSele=select()
Sele 10
Uselan('DATA.SET','','','','','')
Set colo to w+*/r
@24,59 say 'Password:'
Set colo to w+*/b
@24,68 say '_'
@24,69 say spac(11)
Set colo to b/b,b/b
Tone(1500,1)
Tone(1000,3)
Awal=1
Do whil awal<=3
   In=spac(15)
   @24,69 get in pict '@S11'
   Read
   If lastkey()=27 .or. in=spac(15)
      Exit
   Endi
   If faktur=left(in,10)
      xOwner=.T.
      Exit
   Endi
Endd
Set colo to /bg,w+/b
Close
Sele(xsele)
Retu

Proc konvtgl
If len(ltri(str(day(tgl))))=1
   Hri='0'+ltri(str(day(tgl)))
Else
   Hri=ltri(str(day(tgl)))
Endi
If len(ltri(str(mont(tgl))))=1
   Bln='0'+ltri(str(mont(tgl)))
Else
   Bln=ltri(str(mont(tgl)))
Endi
Thn=ltri(str(year(tgl)))
Retu

Proc nol3
Byr=byr*1000
Keyboard chr(13)
Retu

Proc cekbulat
If file('RPBULAT.SET')
   Sele 11
   Uselan('RPBULAT.SET','','','','','')
   Go top
   Do whil .not. eof()
      If (tot>=batas_1 .and. tot<=batas_2) .and. nilai_rp#0
         Blt=mod(tot,nilai_rp)
         If blt<=nilai_b
            Blt=-blt
            Tot=tot+blt
         Elseif blt>=nilai_a
            Blt=nilai_rp-blt
            Tot=tot+blt
         Endi
         Exit
      Endi
      Skip
   Endd
   Close
   *
   Sele(xsele)
Endi
Retu

Proc cekplg
If member .or. byr<(tot-dis31)
   If iPoin='2'
      Cekpoin=.t.
   Endi
   *
   Plg=spac(11)
   Sele 10
   Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
   Do whil .t.
      Set key 28 to daflgn
      @20,7 get plg pict '@!'
      Do pesan with 'Masukan Kode Pelanggan,    F1=Daftar Pelanggan     0=Cash'
      Set colo to w+/b,/w
      Read
      Set key 28 to
      If lastkey()=27
         Exit
      Endi
      Plgz=rtri(plg)
      If plg=spac(11)
         Plg=spac(10)
      Elseif plg#spac(11)
         If val(right(rtri(plg),3))#0
            *Plg=iif(val(right(rtri(plg),2))>0, idtx+right('000000'+rtri(plg),pdt-len(idtx))+spac(10-pdt) , left(plg,10))
            Plg=idtx+right('000000'+rtri(plg),pdt-len(idtx))+spac(10-pdt)
         Endi
      Endi
      @20,7 say plg
      Set order to 1
      Seek plg
      If .not. foun()
         *Plg=plgz
         Do daflgn
         If lastkey()=27
            Loop
         Endi
      Endi
      Nsp  =nama_lgn
      Jab  =almt_lgn
      Sop  =so_point
      Ajp  =aj_point
      Amp  =am_point
      Rpu  =rp_uang
      Tglso=tanggal
      Tglhp=tgl_ambil   &&&POIN DIHITUNG DARI > SEJAK TERAKHIR AMBIL
      Ketam=ket_ambil
      Jamx =jam_ambil
      If dis3>0
         Adadis=.t.
      Endi
      Stor 0 to pnsbl,totbl,totmr
      *
      If left(plg,3)='0  '
         Plg=spac(10)
         Stor spac(30) to nsp,jab
         Stor 0 to tot3,kbl
         Set colo to w+/b
         @19,0 say repl(gl,8)+chr(193)+repl(gl,30)+chr(193)+repl(gl,13)+chr(193)+repl(gl,9)+chr(193)+repl(gl,5)+chr(193)+repl(gl,10)
         @20,1  say 'Kode:                                       '+spac(12) +'Jml.Belanja:'+spac(13)
         @21,1  say 'Nama:'+spac(31)+'                    Pembulatan :'+spac(13)
         @22,1  say 'Almt:'+spac(31)+'                    Disc.:     %'+spac(13)
         @23,1  say 'Poin:'+spac(31)+'                    Jml.Bayar  :'+spac(13)
      Endi
      Plg=left(plg,10)
      Set colo to w+/b
      @20,7 say plg+'  '
      @21,7 say nsp
      @22,7 say jab
      @23,7 say ltri(tran(pnskr,'999 + '))+ltri(tran(pnskr2,'999'))
      Do pesan with 'Tunggu, sedang dihitung...'
      *
      Sele 11
      Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
      Seek plg
      Stor 0 to jml1,jml2
      Do whil .not. eof() .and. kode_lgn=plg
         If (tanggal>=tglso .and. tanggal<=tgl)
            Jml1=jml1+(rp_total-rp_diskon)
            Jml2=jml2+rp_bayar
         Endi
         Skip
      Endd
      Tot3=(jml1-jml2)
      If tot3#0
         Set colo to gr+/b
         @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
         Set colo to /bg
      Endi
      *
      If cekpoin
         Seek plg
         Do whil .not. eof() .and. kode_lgn=plg
            If (tanggal>=tglhp .and. tanggal<=tglb)
               If tanggal=tglhp .and. jam>jamx
                  Pnsbl=pnsbl+jml_poin
                  Totbl=totbl+(rp_total-rp_diskon)
                  Totmr=totmr+(rp_total-rp_pokok)
               Elseif tanggal>tglhp
                  Pnsbl=pnsbl+jml_poin
                  Totbl=totbl+(rp_total-rp_diskon)
                  Totmr=totmr+(rp_total-rp_pokok)
               Endi
            Endi
            Skip
         Endd
         xPoin=(pnsbl+ajp)
         Set colo to w+/b
         *
         Sele 10
         If hanyaCekp    &&&.and. tgl_ambil#ctod('  -  -    ')
            @23,7 say spac(30)
            @23,7 say ltri(tran(xPoin,'@e 999,999'))   &&&+'   Belanja= '+ltri(tran(totbl,'@e 9,999,999,999'))+'   Margin= '+ltri(tran(totmr,'@e 999,999,999 '))
            *
            Set colo to w+/r
            @16,13 clea to 19,63
            @16,13 to 19,63
            @16,27 say 'PENGAMBILAN POIN YG LALU'
            @17,15 say 'Tanggal: '+dtoc(tgl_ambil)+' / '+jam_ambil
            @18,15 say 'Jumlah.: '+ltri(tran(amp,'@e 999,999'))+iif(rpu#0,' / '+ltri(tran(rpu,'@e 999,999,999')),'')+' / '+rtri(ket_ambil)
            Do pesan with 'A=Ambil_Poin     ESC=Ulangi'
         Else
            Do pesan with 'ENTER=Lanjut_Transaksi     ESC=Ulangi'
         Endi
         Wait''
         Set colo to w+/b
         @23,7 say spac(7)
         If lastkey()=27
            Plg=spac(11)
            Loop
         Elseif hanyaCekp .and. (lastkey()=65 .or. lastkey()=97)
            Set curs on
            Ketx='PENGAMBILAN POIN'
            Ket =spac(50)
            Tglz=tgl
            Stor 0 to jml,jml2
            *
            Sele 11
            Uselan('POINTAM.DTA','POINTAM1.NDX','POINTAM2.NDX','','','')
            Do pesan with 'Masukan Tanggal dan Jumlah Point / Voucer (Rp) yg diAmbil'
            Set colo to w+/g
            @16,10 clea to 19,71
            @16,10 to 19,71
            @16,32 say ketx
            @17,12 say 'Tanggal.:'get tglz
            @17,34 say 'Catatan:'get ket pict '@!S28'
            @18,12 say 'Poin diAmbil:'get jml pict '@e 999999'
            @18,34 say '-atau- diUangkan sejmlh Rp'get jml2 pict '@e   99999999'
            Do pesan with 'Masukan Tanggal dan Jumlah Poin yg diAmbil'
            Read
            If lastkey()=27
               Sele 10
               Exit
            Endi
            Set colo to w+/b
            @18,25 say tran(jml,'@e 999,999')
            @18,61 say tran(jml2,'@e 99,999,999')
            Yt=' '
            Do pesan with 'Data Benar? Y/T :'
            @24,49 get yt pict '!'
            Read
            If lastkey()=27
               Sele 10
               Exit
            Endi
            If ket#spac(50) .and. (jml1#0 .or. jml2#0)
               Flok()
               Appe blan
               Repl tanggal with tglz,jam with time(),kode_lgn with plg,nama_lgn with nsp,rp_uang with jml2,;
                    so_point with xPoin,am_point with jml,id_point with kdb,keterangan with ket
               Ul()
               Commit
               Close
               *
               Sele 10
               Rlok()
               Repl so_point with xPoin,aj_point with xPoin-jml,am_point with jml,rp_uang with jml2,tgl_ambil with tglz,jam_ambil with time(),ket_ambil with ket
               Ul()
               Commit
               *
            Endi
         Endi
         *
      Else
         If tot3#0
            Set colo to gr+/b
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            Set colo to /bg
         Endi
      Endi
      Close
      *
      Exit
   Endd
   Close
   *Dis31=(tot*(dis3/100))
   *If byr>(tot-dis31)
   *   @22,63 say dis3  pict '@e 99.99'
   *   @22,69 say dis31 pict '@e 999,999,999'
   *   Do pesan with 'ENTER=Lanjut'
   *   Wait''
   *Endi
Endi
Retu

*OUTPUT KE DISPLAY/DRAWER
*-------------------------
Func kedisple
If idis='Y'
   Parameters xkata
   Xopen = Fopen("COM3",1)
   If (ferror() != 0)
      Do pesan with 'Tidak Tersambung ke OutPut--> COM1/Display'
      Inkey(1)
      Retu(.f.)
   Endi
   Fwrite(xopen,xkata,40)
   Fclose(xopen)
Endi
Retu(.t.)

Func bukadrw
Xopen = Fopen("LPT1",1)
If (ferror() != 0)
   Do pesan with 'Tidak Tersambung ke OutPut--> LPT1/CashDrawer'
   Inkey(1)
   Retu(.f.)
Endi
*Fwrite(xopen,chr(27)+'p'+chr(0)+chr(100)+chr(250),5)      &&PRINTR EPSON, SAMSUNG->EPSON
Fwrite(xopen,chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'255',7)  &&PRINTR STAR->EPSON
Fclose(xopen)
Retu(.t.)

***Kalkulator
Proc kalk
Private rupacalc, barcalc, kolcalc, opr, ha_opr, n_calc, mhasil, ;
      mcalc_tot, in_opr, j_opr, len_opr, hasil, pos_calc, pos_prs, ;
      minplus, nil_calc, prs_calc, scr_calc
Save scre to scr_calc
Barcalc= Row()
Kolcalc= Col()
If (Row() <= 11 .AND. Col() < 40)
   Row_calc= Row() + 2
   Col_calc= 4
Endi
If (Row() <= 11 .AND. Col() >= 40)
   Row_calc= Row() + 2
   Col_calc= 44
Endi
If (Row() > 11 .AND. Col() < 40)
   Row_calc= Row() - 8
   Col_calc= 4
Endi
If (Row() > 11 .AND. Col() >= 40)
   Row_calc= Row() - 8
   Col_calc= 44
Endif
Set colo to w+/b
@ row_calc, col_calc clear to row_calc +6, col_calc +30
@ row_calc, col_calc to row_calc +6, col_calc +30
@ row_calc, col_calc +11 say "Calculator"
@ row_calc +2, col_calc +2 say "Operasi ="
@ row_calc +3, col_calc +2 say "Hasil   ="
@ row_calc +4, col_calc +2 say "Total   ="
@ row_calc +6, col_calc +6 say "[R]-Reset  [H]-Hasil"
Stor 0 to hasil,calc_tot
Stor '' to opr,ha_opr
Do while (.T.)
   n_calc= 1
   @ row_calc +2, col_calc +12 say Space(17)
   mhasil= right(tran(hasil, "#,###,###,###.###"), 3)
   mcalc_tot= right(tran(calc_tot, "#,###,###,###.###"), 3)
   @ row_calc +3, col_calc +12 say iif(hasil = 0, iif(opr == ;
      "", Space(17), tran(0, "#,###,###,###,###")), ;
      iif(mhasil = "000", tran(hasil, "#,###,###,###,###"), ;
      tran(hasil, "#,###,###,###.###")))
   @ row_calc +4, col_calc +12 say iif(calc_tot = 0, iif(opr == ;
      "", Space(17), tran(0, "#,###,###,###,###")), ;
      iif(mcalc_tot = "000", tran(calc_tot, ;
      "#,###,###,###,###"), tran(calc_tot, "#,###,###,###.###")))
   opr= ""
   Do while (n_calc <= 17)
      @ row_calc +2, col_calc +12 say opr
      in_opr= InKey(0)
      if lastkey()=27 &&(re())
         exit
      endif
      if (in_opr == 28)
         loop
      endif
      if (in_opr == 13)
         if (n_calc > 1)
            if (right(opr, 1) $ "./*-+")
               loop
            endif
            exit
         else
            loop
         endif
      endif
      if (n_calc = 1 .AND. (in_opr = 104 .OR. in_opr = 72))
         opr= ha_opr
         n_calc= Len(opr) + 1
         loop
      endif
      if (in_opr = 82 .OR. in_opr = 114)
         if (n_calc == 1)
            exit
         endif
         n_calc= 1
         opr= ""
         @ row_calc +2, col_calc +12 say Space(17)
      endif
      if (in_opr == 8)
         n_calc= iif(n_calc = 1, 1, n_calc - 1)
         opr= iif(n_calc = 1, "", Left(opr, Len(opr) - 1))
         @ row_calc + 1, col_calc + 12 say Space(17)
         loop
      endif
      j_opr= 0
      len_opr= Len(opr)
      do while (len_opr > 0)
         if (SubStr(opr, len_opr, 1) $ "-+/*")
            j_opr= j_opr + 1
         endif
         len_opr= len_opr - 1
      enddo
      if (Chr(in_opr) $ "%=" .AND. (j_opr > 1 .OR. "*" $ opr .OR. ;
            "/" $ opr .OR. n_calc <= 2 .OR. !("+" $ opr .OR. "-" ;
            $ opr) .OR. "%" $ opr .OR. "=" $ opr) .OR. ;
            Chr(in_opr) $ "/*-+" .AND. n_calc = 1)
         loop
      endif
      if (n_calc > 1 .AND. right(opr, 1) $ "./*-+=%" .AND. ;
            Chr(in_opr) $ "./*-+=%")
         loop
      endif
      if (Chr(in_opr) $ "%=*+-./0123456789")
         n_calc= n_calc + 1
         opr= opr + Chr(in_opr)
      Endi
   Enddo
   If (in_opr == 27)
      exit
   Endi
   If (in_opr = 82 .OR. in_opr = 114)
      hasil= 0
      calc_tot= 0
   Else
      if ("%" $ opr .OR. "=" $ opr)
         pos_calc= iif("+" $ opr, At("+", opr), At("-", opr))
         pos_prs= iif("%" $ opr, At("%", opr), At("=", opr))
         minplus= SubStr(opr, pos_calc, 1)
         nil_calc= Val(Left(opr, pos_calc - 1))
         prs_calc= Val(SubStr(opr, pos_calc + 1, pos_prs - pos_calc - 1)) / 100
         hasil= iif(minplus = "+", nil_calc + prs_calc * nil_calc, nil_calc - prs_calc * nil_calc)
      elseif ("/0" $ opr)
         hasil= 0
      else
         hasil= &opr
      endif
      calc_tot= calc_tot + hasil
   Endi
   ha_opr= iif(hasil = 0, "", LTrim(iif(mod(hasil, 1) = 0, Str(hasil, 17), Str(hasil, 17, 3))))
Enddo
Rest screen from scr_calc
Set colo to /w,bg+/b
@ barcalc, kolcalc say ""
Scr_calc= ""
Rele rupacalc, barcalc, kolcalc, opr, ha_opr, n_calc, mhasil, ;
     mcalc_tot, in_opr, j_opr, len_opr, hasil, pos_calc, pos_prs, ;
     minplus, nil_calc, prs_calc, scr_calc
Retu

***PROSES UNTUK NOTA TUNDA
*-------------------------
Proc notunda
Ditunda=.t.
Keyboard chr(13)
Retu

Proc cektunda
If file ('&nfy')
   Sele 99
   Useidx('&nfy')
   Pack
   If eof()
      Close
      Dele file &nfy
      Adatunda=.f.
   Elseif recno()>=1
      Adatunda=.t.
   Endi
Endi
Retu

Proc caritunda
Save scre to sdaf
Pesanx='ENTER=Pilih     F4=Hapus Semua     F8=Hapus Item ini     ESC=Keluar'
Do pesan with pesanx
Set colo to /bg
@1,4  clea to 23,76
@1,4  to 1,76  doub
@23,4 to 23,76
@1,28 say'Item Barang yg Tertunda'
Declare field[7],kop[7],bts[7],frm[7]
Field[1]="JAM+tl+left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+SATUAN+tl+tran(HARGA_2,'@e 9,999,999 ')+tl+tran(JUMLAH*HARGA_2,'@e 99,999,999')"
Kop[1]='No.   Nama Barang                      Jml.Barang     @Harga   Jumlah Rp'
Frm[1]=''
Stor '@!' to frm[1],frm[2],frm[3]
Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
Set colo to /bg
Go top
Dbedit(2,4,22,76,field,'ftunda',frm,kop,gl,bts)
*
Rest scre from sdaf
Set colo to w+/b
Set curs on
Retu

Function ftunda
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=13 .and. jam#'---'
   Isilagi =.t.
   Tot=1
   Wkt=jam
   Do whil .not. eof() .and. jam=wkt
      Ksp =kode_spl
      Klb =kode_klb
      Kdb =kode_brg
      Kdbx=kode_bar
      Nmb =nama_brg
      St  =satuan
      Jml =jumlah
      Hp  =harga
      Hj  =harga_2
      Hj2 =harga_22
      Dis1=diskon
      Dis2=diskon_rp
      Ipn =id_point
      Ipn2=id_point2
      Ket =keterangan
      *
      Sele 5
      Repl tanggal with tgl,faktur with fkt,kode_ksr with ksr,nama_ksr with nsls,kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
           nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2,keterangan with ket,id_data with ' '
      Skip
      *
      Sele 99
      Dele
      Skip
      If jam='---' .and. nama_brg=repl('-',30) .and. satuan='-----'
         Dele
      Endi
   Endd
   Retu 0
Elseif lastkey()=-3
   Do pesan with ''
   Do paswod2
   If xOwner .and. lastkey()#27
      xOwner=.f.
      Yt=' '
      Set curs on
      Do pesan with 'Hapus Semua item yg diTunda? Y/T : '
      Do whil .not. yt$'YT'
         @24,57 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Dele All
         Commit
      Endi
   Endi
   Set colo to /bg
   Set curs off
   Retu 0
Elseif lastkey()=-7
   B =row()
   Kl=col()
   Do pesan with ''
   Do paswod2
   If xOwner .and. lastkey()#27
      xOwner=.f.
      Set colo to gr+*/b
      @b,kl say &fds
      Yt= ' '
      Set curs on
      Set colo to /bg
      Do pesan with 'Hapus item ini? Y/T : '
      Do whil .not. yt$'YT'
         @24,51 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Dele
         Commit
         Skip
      Endi
   Endi
   Do pesan with pesanx
   Set colo to /bg
   Set curs off
   Retu 2
Else
   Retu 1
Endi
Retu 0

* LAPORAN TOTAL NO.NOTA
*-----------------------
Proc lnota
@1,0 clea to 23,79
Set colo to /bg
@9,26 clea to 14,53
@9,26 to 14,53
@10,27 say 'Rekap Nota Penjualan'
@11,27 say 'Kasir  : '+left(nsls,17)
@13,27 say 'Tanggal:'get tgl
Do pesan with 'Masukan Tanggal yg mau diRekap'
Read
Do konvtgl
Do bacafile
Do pesan with 'Tunggu, sedang dihitung'
*
Sele 4
Uselan('&nf0','&nf3','','','','')
Set filter to id_data=' ' .and. kode_ksr=ksr
Seek dtos(tgl)
If .not. foun()
   Loca for tanggal>=tgl
Endi
*
Nr=recno()
Stor 1 to hal,noz
Stor 0 to tot,totd,tdns,jml1,jds,jdns
Arg1='L-NOTA1.RTF'
Set alte on
Set alte to &arg1
?' '+aidi1
?' REKAP NOTA PENJUALAN'
?' TANGGAL: '+dtoc(tgl)+' // '+left(time(),5)
?' KASIR  : '+nsls
?repl('=',32)
?' No. Nota  Jam   Dns   Jumlah Rp'
?repl('-',32)
Fktz=faktur
Jamz=jam
Slsz=nama_ksr
Tgl3=tanggal
Do whil .not. eof() .and. tanggal=tgl
   Jml1=jml1+&brs1
   Jds =jds +(jumlah*diskon_rp)
   Jdns=jdns+rp_donasi
   Tot =tot +&brs1
   Totd=totd+(jumlah*diskon_rp)
   Tdns=tdns+rp_donasi
   Skip
   If faktur#fktz
      ?tran(noz,'999. ')+right(fktz,4)+'/'+jamz+iif(jdns#0,tran(jdns,'@e 99999 '),'      ')+tran(jml1,'@e 999,999,999')
      Noz=noz+1
      Fktz=faktur
      Jamz=jam
      Slsz=nama_ksr
      Tgl3=tanggal
      Stor 0 to jml1,jds,jdns
   Endi
Endd
?repl('-',32)
If totd#0
   ?spac(10)+'Jumlah Rp'+tran(tot+totd,'@e 9,999,999,999')
   ?spac(10)+'Diskon Rp'+tran(totd,'@e 9,999,999,999')
Endi
?spac(10)+'Total  Rp'+tran(tot,'@e 9,999,999,999')
If tdns#0
   ?spac(10)+'Donasi Rp'+tran(tdns,'@e 9,999,999,999')
Endi
?repl('=',32)
?' '
Set alte off
Set alte to
@24,0
Do pesan with 'ESC=Menu Selanjutnya'
Set key 13 to xit
Memoedit(Memoread('&arg1'),1,0,23,210,.t.,'',210)
Set key 13 to
Do pesan3 with 'ESC=Selesai   F10=Cetak'
If lastkey()#27
   Do cp
   If lastkey()#27
      Set prin on
      Set devi to prin
      Set marg to jt
      ?chr(27)+chr(33)+chr(4)
      Type &arg1 to prin
      @prow()+(jn+5),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      Endi
      Set prin off
      Set devi to scre
   Endi
Endi
Sele 4
Close
Rele fktz,jamz,slsz,noz,nz
Retu

Proc lnota2
Do pesan with 'Rekap Penjualan'
*
@1,0 clea to 23,79
Set colo to /bg
@9,26 clea to 14,53
@9,26 to 14,53
@10,27 say 'Rekap Pendapatan'
@11,27 say 'Kasir  : '+left(nsls,17)
@13,27 say 'Tanggal:'get tgl
Do pesan with 'Masukan Tanggal yg mau diRekap'
Read
*
Do konvtgl
Do bacafile
Do pesan with 'Tunggu, sedang dihitung'
*
Sele 4
Uselan('&nf0','&nf3','','','','')
Set filter to kode_ksr=ksr
Seek dtos(tgl)
If .not. foun()
   Loca for tanggal>=tgl
Endi
*
Stor 1 to hal,noz
Stor 0 to tot,totd,totk,tdns,jml1,jml2,jmlb
Arg1='L-NOTA1.RTF'
Set alte on
Set alte to &arg1
?' '+aidi1
?' REKAP PENDAPATAN'
?' TANGGAL: '+dtoc(tgl)+' / '+left(time(),5)
?' KASIR  : '+nsls
?repl('=',iif(lr='8',40,32))
?'PEMASUKAN:'
Do whil .not. eof() .and. tanggal=tgl
   Jml1=jml1+&brs1
   Tot =tot +&brs1
   Tdns=tdns+rp_donasi
   If id_data='K'
      Jml2=jml2+&brs1
      Jmlb=jmlb+rp_bayar
   Endi
   Skip
Endd
?'- Penjualan Barang   '+spac(iif(lr='8',8,0))+tran(tot,'@e 999,999,999')
If jml2#0
?'- Penjualan Kredit   '+spac(iif(lr='8',8,0))+'('+tran(jml2-jmlb,'@e 999,999,999')+')'
Endi
If tdns#0
?'- Uang Donasi        '+spac(iif(lr='8',8,0))+tran(tdns,'@e 999,999,999')
Endi
*
Do bacaflkas
*
Sele 11
Uselan('&nf0','&nf2','','','','')
Sele 10
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
Set filter to id_pos='D'
Loca for id_pos='D'
Do whil .not. eof()
   Kpos =kode_pos
   Nspos=nama_pos
   Ipos =id_pos
   *
   Sele 11
   Set filter to nama_ksr=nsls .and. id_pos='D'
   Seek kpos+dtos(tgl)
   If .not. foun()
      Seek kpos
   Endi
   Do whil .not. eof() .and. kode_pos=kpos
      If tanggal=tgl
         ?'- '+left(keterangan,iif(lr='8',27,19))+tran(rp_total,'@e 999,999,999')
         Totd=totd+rp_total
      Endi
      Skip
   Endd
   Sele 10
   Skip
Endd
Totd=totd+(tot-(jml2-jmlb))+tdns
?spac(iif(lr='8',29,21))+repl('-',11)
?'  Jml.Pemasukan   Rp'+spac(iif(lr='8',9,1))+tran(totd,'@e 999,999,999')
?' '
?'PENGELUARAN:'
*
Sele 10
Set filter to id_pos='K'
Loca for id_pos='K'
Do whil .not. eof()
   Kpos =kode_pos
   Kspos=kode_pos
   Ipos =id_pos
   *
   Sele 11
   Set filter to nama_ksr=nsls .and. id_pos='K'
   Seek kpos+dtos(tgl)
   If .not. foun()
      Seek kpos
   Endi
   Do whil .not. eof() .and. kode_pos=kpos
      If tanggal=tgl
         ?'- '+left(keterangan,iif(lr='8',27,19))+tran(rp_total,'@e 999,999,999')
         Totk=totk+rp_total
      Endi
      Skip
   Endd
   Sele 10
   Skip
Endd
?spac(iif(lr='8',29,21))+repl('-',11)
?'  Jml.Pengeluaran Rp'+spac(iif(lr='8',9,1))+tran(totk,'@e 999,999,999')
?' '
?spac(iif(lr='8',29,21))+repl('-',11)
?'  Sisa Uang Kas   Rp'+spac(iif(lr='8',9,1))+tran(totd-totk,'@e 999,999,999')
?spac(iif(lr='8',29,21))+repl('-',11)
?' '
Set alte off
Set alte to
Ada=.f.
*
Do pesan with 'ESC=Menu Selanjutnya'
Set key 13 to xit
Memoedit(Memoread('&arg1'),1,0,23,210,.t.,'',210)
Set key 13 to
Do pesan3 with 'ESC=Selesai   F10=Cetak'
If lastkey()#27
   Do cp
   If lastkey()#27
      Set prin on
      Set devi to prin
      Set marg to jt
      ?chr(27)+chr(33)+iif(iprn='D',chr(5),chr(4))
      Type &arg1 to print
      @prow()+(jn+2),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      Endi
      Set prin off
      Set devi to scre
   Endi
Endi
Close
*
Retu

* LAPORAN PENJUALAN (TERLARIS)
*-----------------------------
Proc lpj99
*
Sele 3
Close
*
Do whil .t.
   Jlap=' '
   Set colo to
   @1,0 clea to 24,79
   Set colo to /bg,w+/b
   @9,26 clea to 14,53
   @9,26 to 14,53
   @10,28 say 'Rekap Item Barang'
   @11,28 say 'Penjualan Semua Kasir'
   @13,28 say 'Tanggal Trx:'get tgl
   Do pesan with 'Masukan Tanggal yg mau diRekap'
   Read
   @24,0
   If lastkey()=27
      Exit
   Endi
   Do bacafile
   *
   Sele 4
   Uselan('&nf0','&nf2','','','','')
   *
   Sele 3
   Uselan('STOK.DTA','STOK5.NDX','','','','')
   *
   Sele 13
   Useidx('TOP-RANK.DTA')
   Copy stru to TOP-RANK.TMP
   Close
   *
   Ada =.f.
   Awl =1
   Bln =subs(dtoc(tgl),4,2)
   *
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang Proses... Rec: '+ltri(str(reccount()))+'-          File: '+nf0
   Set colo to w+/r
   *
   Sele 4
   Set filter to
   *
   Sele 3
   Go top
   Do whil .not. eof()
      Klb =kode_klb
      Kdb =kode_brg
      Kdbx=kode_bar
      Nmb =nama_brg
      St  =satuan
      Stk =(stok_a+stok_m)-stok_k
      Ksp =kode_spl
      Stor 0 to jml,jhg,jhp,jds
      @24,44 say ltri(str(awl))
      *
      Sele 4
      Seek kdbx
      Do whil .not. eof() .and. kode_bar=kdbx
         If tanggal=tgl
            Jml=jml+jumlah
            Jhg=jhg+(jumlah*(harga_2-diskon_rp))
            Jhp=jhp+(jumlah*harga)
            Jds=jds+(jumlah*diskon_rp)
            If kode_spl#spac(5)
               Ksp=kode_spl
            Endi
         Endi
         Skip
      Endd
      *
      If jml#0
         Ada=.t.
         Sele 13
         Useidx('TOP-RANK.TMP')
         Appe blan
         Repl kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              jumlah with jml,sisa with stk,harga with jhp,harga_2 with jhg,margin with jhg-jhp,diskon with jds
      Endi
      *
      Awl=awl+1
      Sele 3
      Skip
   Endd
   *
   Awl=1
   If ada
      Sele 16
      Close
      *
      Sele 15
      Close
      *
      Sele 14
      Close
      *
      Sele 13
      Sort on jumlah   /D to TOP-RANK.QTY
      Sort on harga_2  /D to TOP-RANK.RPH
      Sort on kode_brg to TOP-RANK.KDB
      Sort on nama_brg,jumlah to TOP-RANK.NMB
      Close
      *
      Pesanx='T=Top Ranking   F1=Nama Brg.   F10=Cetak'
      Sele 13
      Useidx('TOP-RANK.QTY')
      Sele 14
      Useidx('TOP-RANK.RPH')
      Sele 15
      Useidx('TOP-RANK.NMB')
      Sele 16
      Useidx('TOP-RANK.KDB')
      Sele 15
      *
      Ket='TOP NAMA,  Tgl: '+dtoc(tgl)
      Set colo to /bg
      @1,0 clea to 23,79
      @1,1 say ket
      @2,0 to 2,79 doub
      @22,0 to 22,79
      Keyboard chr(4)
      Do whil .t.
         Declare field[10],bts[10],frm[10],kop[10]
         Do pesan with pesanx
         Field[1]='KODE_BAR'
         Field[2]='NAMA_BRG'
         Field[3]='JUMLAH'
         Field[4]='left(SATUAN,3)'
         Field[5]='DISKON'
         Field[6]='HARGA_2'
         Kop[1]='Kode'
         Kop[2]='Nama Barang'
         Kop[3]='Terjual'
         Kop[4]='Sat.'
         Kop[5]='   Diskon'
         Kop[6]=' Jumlah Rp'
         Frm[3]='@e 999999.99'
         Frm[5]='@e 9,999,999'
         Frm[6]='@e 99,999,999'
         Stor '@!' to frm[1],frm[2],frm[4]
         Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8]
         Dbedit(3,0,21,79,field,'flpj99',frm,kop,gl,bts)
         *
         If lastkey()=27
            Exit
         Endi
      Endd
   Elseif .not. ada
      Tone(1500,3)
      Do pesan with 'Tidak Ada Data Penjualan'
      Wait''
   Endi
Endd
Sele 16
Close
*
Sele 15
Close
*
Sele 14
Close
*
Sele 13
Close
*
Sele 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
Retu

Function flpj99
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   @22,0 say repl(gl,15)
   Set colo to gr+/b
   @22,0 say ' '+ltri(str(recno()))+'/'+ltri(str(reccount()))+' '
   Set colo to /bg
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=spac(30)
      Set curs on
      Do pesan with ''
      Set colo to w+/r
      @23,18 say 'Urut Nama Brg:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#spac(30)
         Set colo to w+/w
         Loca for nama_brg=rtri(nmb)
         If .not. foun()
           Go nr
         Endi
      Endi
      Do pesan with pesanx
      Set colo to w+/b
      Set curs off
      Retu 1
Elseif uppe(chr(lastkey()))='T' .and. fds='KODE_BAR'
      Sele 16
      Ket='TOP KODE,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Retu 0
Elseif uppe(chr(lastkey()))='T' .and. fds='NAMA_BRG'
      Sele 15
      Ket='TOP NAMA,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)
      Retu 0
Elseif uppe(chr(lastkey()))='T' .and. fds='JUMLAH'
      Sele 13
      Ket='TOP QTY,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)+chr(4)
      Retu 0
Elseif uppe(chr(lastkey()))='T' .and. fds='HARGA_2'
      Sele 14
      Ket='TOP RUPIAH,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)
      Retu 0
Elseif lastkey()=-9
      Go top
      Stor 1 to n,no,hal
      Stor 0 to tot,dis
      Do cp
      If lastkey()=0
         Retu 0
      Endi
      Set prin on
      Set devi to prin
      Set marg to jt
      @prow()+1,0 say chr(27)+chr(33)+chr(5)
      @prow()+1,0 say 'REKAP PENJUALAN'
      @prow()+1,0 say ket+' // '+left(time(),5)
      @prow()+1,0 say 'Kode    Nama Barang           Jumlah Rp.'
      @prow()+1,0 say repl('=',40)
      Do whil .not. eof()
         @prow()+1,0 say kode_brg+' '+left(nama_brg,30)
         @prow()+1,2 say tran(jumlah,'@e 999,999 ')+satuan+'='+tran(diskon,'@e 999,999,999 =')+tran(harga_2,'@e 999,999,999')
         Tot=tot+harga_2
         Dis=dis+diskon
         No =no+1
         N  =n+1
         Skip
      Endd
      @prow()+1,0  say repl('-',40)
      @prow()+1,0  say 'Jumlah Rp'+tran(tot+dis,'@e 9,999,999,999')
      @prow()+1,0  say 'Diskon Rp'+tran(dis    ,'@e 9,999,999,999')
      @prow()+1,0  say 'Total  Rp'+tran(tot    ,'@e 9,999,999,999')
      @prow()+1,0 say chr(27)+chr(33)+chr(4)
      @prow()+(jn+5),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      Endi
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
      Retu 1
Else
      Set curs off
      Do pesan with pesanx
      Retu 1
Endi
Retu 0

Proc ceklokal
If ikomp='C' .and. idata='0' .and. jk='0'
   Do pesan with 'Tunggu, sedang mempersiapkan File '+nfc+' ke Data Lokal'
   Sele 77
   Close
   *
   Sele 3
   Set order to 2
   Copy to &nfc field kode_bar,nama_brg,satuan,harga,harga_2,harga_21,;
                      harga_3,harga_32,batas_3,stok_a,stok_m,stok_k,stok_rb
   *
   Sele 77
   Useidx('&nfc')
Endi
Retu

* EOF CRX
