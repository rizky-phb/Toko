* PROSEDUR-2 BANTUAN

Proc cekdata
*
Tgl=tglx
xCariso="(mont(tanggal)=mont(tgl) .and. ( tanggal>=tglso .and. tanggal<=tgl2 ))"
Uselan('DATA.SET','','','','','')
xPos=rtri(keterangan)
*
Do bacafile
Uselan('JUAL.DTA','','','','','')
If .not. file('&nf0')
   Copy stru to &nf0
   Clos all
   B=23
   Indx('&nf0','dtos(TANGGAL)+FAKTUR','&nf1',2)
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
Close all
*
Uselan('KASSA.SET','','','','','')
Idis=id_disp
Close all
*
Brs1 ="((jumlah-jumlah_rtr)* (harga_2- ((harga_2*(diskon/100))+diskon_rp)))"
Brs12="((jumlah*hbnet)-diskon_rp)"
Brs13="(jml* (hj- ((hj*(dis1/100))+dis2)))"
Brs14="((jumlah-jumlah_rtr)*harga)"
Retu

Proc hari
Hh=dow(tgl)
Do case
   Case hh=1
      Hr='Minggu'
   Case hh=2
      Hr='Senin '
   Case hh=3
      Hr='Selasa'
   Case hh=4
      Hr='Rabu  '
   Case hh=5
      Hr='Kamis '
   Case hh=6
      Hr="Jum'at"
   Case hh=7
      Hr='Sabtu '
Endc
Retu

Proc x   &&&---KOP
Para judul
Set curs on
Set colo to w+/r
@0,0
@0,1 say rtri(aidi1)+iif(xpos#'',' \'+xpos,'')
@0,40-(len(judul)/2) say judul
@0,79-len(rtri(hr)+', '+dtoc(tgl)) say rtri(hr)+', '+dtoc(tgl)
@24,0
Set colo to /bg,bg+/b+
Retu

Proc pesan
Para psn
Set colo to /r
@24,0
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to /bg,w+/b
Retu

Proc pesan1
Para psn
Set colo to /b
@23,0
Set colo to w+/b
@23,40-(len(psn)/2) say psn
Set colo to /bg,w+/b
Retu

Proc pesan2
Para psn
Tone(499,1)
Tone(300,1)
Set colo to /r
@24,0
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to /bg,w+/b
I=0
Do whil i#32
   I=inke()
   If i=27
      Exit
   Endi
Endd
If i=27
   Nlp=0
Else
   Nlp=1
Endi
@24,0
Retu

Proc pesan3
Para psn
Set colo to /r
@24,0
Set colo to w+/r
@24,40-(len(psn)/2) say psn
Set colo to /bg,w+/b
I=0
Do whil i#-9
   I=inke()
   If i=27 .or. i=-3 .or. i=-4 .or. i=-7
      Exit
   Endi
Endd
If i=27 .or. i=-3 .or. i=-4 .or. i=-7
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

*** GESER RECORD
Proc geser
I=0
Do whil i#27
   I=inke()
   If i=18 .or. i=3 .or. i=5 .or. i=22 .or. i=24 .or. i=-3 .or. i=-7 .or. i=-9
      Exit
   Endi
Endd
Do case
   Case i=18  &&&-PgUp
      Skip -no
   Case i=3   &&&-PgDn
      Skip no
   Case i=5   &&&-Up
      Skip -1
   Case i=24   &&&-Dn
      Skip
   Case i=-9
      Nlp=1
   Othe
      Nlp=0
Endc
Retu

Proc xnota
Fkt=right('00000'+ltri(str(nn)),5)
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
      Do case
         Case i=27
            Cc=0
            Exit
         Case i=-9
            If PrinterReady()
               Cc=1
               Exit
            Endi
      Endc
   Else
      Cc=1
      Exit
   Endi
Endd
Do pesan with 'Tunggu, sedang dicetak'
Retu

* Cek kesiapan printer untuk Windows/Harbour
Func PrinterReady
Return .T.

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

Proc bukaflkas
Sele 3
Uselan('&nf0','&nf1','&nf2','','','')
Retu

Proc lain2
Uselan('DATA.SET','','','','','')
Go top
xPos=keterangan
Rpb =rp_debet
Rpb2=rp_kredit
pExim=path_exim
Alm1 =almt_1
Alm2 =almt_2
Hj =no_order
Hp =no_returb
Nt =no_pinjam
Hb =no_adjust
Pr1=promo1
Pr2=promo2
Pr3=promo3
Pr4=promo4
Cpo1=ket_po1
Cpo2=ket_po2
Cpo3=ket_po3
Tgl2=tanggal_2
Tgl3=tanggal_3
Npjk=npwp
Idt =id_cust
Pdt =pj_cust
Set colo to n++
@1,0 clea to 24,79
B=1
Do whil b<=23
   @b,0 say repl(chr(219),80)
   B=b+1
Endd
Set colo to /bg,bg+/b
@1,0  clea to 24,79
@1,0  to 24,79
@2,1  say 'No.Urut Retur Pembelian:'get hp  pict '99999'
@3,1  say 'No.Urut Transfer Barang:'get nt  pict '99999'
@2,44 say 'No.Urut PO/Order Brg...:'get hj  pict '99999'
@3,44 say 'No.Urut Bukti Stok Opnm:'get hb  pict '99999'
@5,1  say 'Kelipatan Belanja unt 1 Poin #1 Rp:'
@6,1  say '                             #2 Rp:'
@5,36 get rpb  pict '@e 9,999,999'
@6,36 get rpb2 pict '@e 9,999,999'
@6,46 say 'Periode:           s/d           '
@6,54 get tgl2
@6,69 get tgl3
@8,1  say 'Huruf Awal di Kode Member / Plgn..:'
@8,36 get idt pict '@!'
@8,40 say '& Total Panjang Kode Member:'get pdt pict '9'
@10,1  say 'Identitas Toko:'get xpos pict '@!S20'
@10,44 say 'Folder BackUp :'get pExim pict '@!'
@12,1  say 'Alamat di Nota:'get alm1
@13,1  say '              :'get alm2
@15,1  say 'Promo di Nota :'get pr1
@16,17 get pr2
@17,17 get pr3
@18,17 get pr4
@20,1  say 'Catatan di PO :'get cpo1 pict '@S62'
@21,17 get cpo2 pict '@S62'
@22,17 get cpo3 pict '@S62'
Read
If lastkey()#27
   Rlok()
   Repl no_order with hj,no_returb with hp,no_pinjam with nt,no_adjust with hb,keterangan with xpos,rp_debet with rpb,rp_kredit with rpb2,;
        npwp with npjk,path_exim with pExim,tanggal_2 with tgl2,tanggal_3 with tgl3,almt_1 with alm1,almt_2 with alm2,;
        promo1 with pr1,promo2 with pr2,promo3 with pr3,promo4 with pr4,ket_po1 with cpo1,ket_po2 with cpo2,ket_po3 with cpo3,id_cust with idt,pj_cust with iif(pdt=0,1,pdt)
   Ul()
   Commit
Endi
xPos=rtri(xpos)
Clos all
Rele hj,hb,hp,nt,pr1,pr2,pr3,pr4,pExim,alm1,alm2,rpb,rpb2,tgl2,tgl3,cpo1,cpo2,cpo3,npjk,idt,pdt
Retu

* SETUP PEMBULATAN
*------------------
Proc rpbulat
Uselan('RPBULAT.SET','','','','','')
Set cent off
Do whil .t.
   @1,0 clea to 24,79
   @1,15 to 21,63
   @1,15 say chr(218)+repl(repl(gl,11)+chr(194),2)+repl(repl(gl,7)+chr(194),2)+repl(gl,7)+chr(191)
   @2,15 say tl+' Dari (Rp) '+tl+'Sampai (Rp)'+tl+' Jumlah'+tl+'keBawah'+tl+' keAtas'+tl
   @3,15 say chr(195)+repl(repl(gl,11)+chr(197),2)+repl(repl(gl,7)+chr(197),2)+repl(gl,7)+chr(180)
   Pesanx='F4=Tambah   F5=Edit   F8=Hapus   F10=Cetak'
   Do pesan with pesanx
   Declare field[5],bts[5],frm[5]
   Field[1]='BATAS_1'
   Field[2]='BATAS_2'
   Field[3]='NILAI_RP'
   Field[4]='NILAI_B'
   Field[5]='NILAI_A'
   Frm[1]='@e 999,999,999'
   Frm[2]='@e 999,999,999'
   Frm[3]='@e   9,999'
   Frm[4]='@e   9,999'
   Frm[5]='@e   9,999'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   Dbedit(4,16,20,62,field,'fbulat',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   Endi
Endd
Set cent on
Set curs off
Clos all
Rele jml1,jml2,jml,tot,tot2
Retu

Function fbulat
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-3
      B=row()+1
      Go bott
      If eof()
         B=b-1
      Endi
      Set curs on
      Stor 0 to jml1,jml2,jml,tot,tot2
      Ket=' '
      Do whil .t.
         Tambah=.t.
         @b,15 say tl+spac(11)+tl+spac(11)+tl+spac(7)+tl+spac(7)+tl
         Do pesan with 'ESC=Selesai'
         @b,16 get jml1 pict '@e 999,999,999'
         @b,28 get jml2 pict '@e 999,999,999'
         Read
         @24,0
         If lastkey()=27
            Exit
         Endi
         Loca for batas_1=jml1 .and. batas_2=jml2
         If foun()
            Jml =nilai_rp
            Tot =nilai_b
            Tot2=nilai_a
            Tambah=.f.
         Endi
         @b,42 get jml  pict '@e   9,999'
         @b,50 get tot  pict '@e   9,999'
         @b,58 get tot2 pict '@e   9,999'
         Read
         If tot2<=tot .and. tot2>0
            Tone(1500,3)
            Tone(2000,6)
            Do pesan with 'Nilai Pembulatan keAtas  HARUS  Lebih Besar dari Nilai Pembulatan keBawah'
            Wait''
            Loop
         Endi
         Do pesan with 'Koreksi ? Y/T'
         If ulangi()
            Loop
         Endi
         If tambah
            Flok()
            Appe blan
            Repl batas_1 with jml1,batas_2 with jml2,nilai_rp with jml,nilai_b with tot,nilai_a with tot2
         Else
            Rlok()
            Repl nilai_rp with jml,nilai_b with tot,nilai_a with tot2
         Endi
         Ul()
         Commit

         Stor 0 to jml1,jml2,jml,tot,tot2
         B=b+1
         If b>20
            B=4
         Endi
      Endd
      Do pesan with pesanx
      Set curs off
      Retu 2
   Case lastkey()=-7
      Set colo to gr+*/b
      @row(),col() say &fds
      Yt=' '
      Set curs on
      Set colo to /bg
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
      Do pesan with pesanx
      Set curs off
      Retu 2
   Case lastkey()=-9
      Go top
      No=1
      Set curs on
      Do cp
      If cc=0
         Retu 0
      Endi
      Set prin on
      Set devi to prin
      Set marg to 0
      ?chr(18)
      ?' '+aidi1
      ?' '+aidi2
      ?' '+subs(aidi3,1,24)+' '+dtoc(date())
      ?' '+repl('-',35)
      ?' DAFTAR PEMBULATAN TOTAL PENJUALAN'
      ?repl('-',80)
      Do whil .not. eof()
         ?tran(no,'999.  ')+tran(batas_1,'@e 999,999,999  ')+tran(batas_1,'@e 999,999,999  ')+tran(nilai_rp,'@e 9,999  ')+tran(nilai_b,'@e 9,999  ')+tran(nilai_a,'@e 9,999  ')
         No=no+1
         Skip
      Endd
      ?repl('-',80)
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
      Retu 1
   Case lastkey()=-4
      Set curs on
      Rlok()
      If left(fds,5)='BATAS'
         @row(),col() get &fds pict '@e 999,999,999'
         Read
      Elseif left(fds,5)='NILAI'
         @row(),col() get &fds pict '@e   9,999'
         Read
      Endi
      Ul()
      Commit
      Set curs off
      Do pesan with pesanx
      Retu 1
   Othe
      Retu 1
Endc
Retu 0

* DAFTAR KASIR
*--------------
Proc ksr
Uselan('KASIR.DTA','','','','','')
Sls=spac(10)
Set cent off
Do whil .t.
   Set colo to 
   @1,0 clea to 24,79
   Set colo to /bg
   @1,8 clea to 23,70
   @1,8 to 23,70
   @1,8 say chr(218)+gl+gl+repl(chr(194)+repl(gl,20),2)+chr(194)+repl(gl,5)+chr(194)+repl(gl,8)+chr(194)+gl+chr(191)
   @2,8 say tl+'No'+tl+'Nama Kasir          '+tl+'Kode/Sandi          '+tl+'#Nota'+tl+'#Tanggal'+tl+'.'+tl
   @3,8 say chr(195)+gl+gl+repl(chr(197)+repl(gl,20),2)+chr(197)+repl(gl,5)+chr(197)+repl(gl,8)+chr(197)+gl+chr(180)
   Pesanx='F5=Edit     ESC=Selesai'
   Do pesan with pesanx
   Declare field[6],bts[6],frm[6]
   Field[1]='NOMER'
   Field[2]='NAMA'
   Field[3]='KODE'
   Field[4]='NOTA'
   Field[5]='TANGGAL'
   Field[6]='JENS_JUAL'
   Frm[1]='99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='99999'
   Frm[5]='99-99-99'
   Frm[6]='@!'
   Stor tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6]
   Dbedit(4,9,22,69,field,'fksrx',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   Endi
Endd
Set cent on
Set curs off
Clos all
Rele sls,nsls,hj,tambah,field[5],bts[5],frm[5]
Retu

Function fksrx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-4 .and. xOwner
      Set curs on
      If fds='KODE' .or. fds='NAMA'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         Commit
      Elseif fds='NOTA'
         Rlok()
         @row(),col() get &fds pict '99999'
         Read
         Ul()
         Commit
      Elseif fds='JENS_JUAL'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         Commit
      Endi
      Set curs off
      Do pesan with pesanx
      Retu 1
   Othe
      Retu 1
Endc
Retu 0

***POSTING STOK BULANAN
Proc postok
Set curs on
Set cent on
Do whil .t.
   Ket=spac(8)
   Tgl2=tglx
   Tgl =ctod('01-'+ltri(str(mont(tglx)))+'-'+ltri(str(year(tglx))))
   Ytp =' '
   Save scre to smkdb
   Set colo to w+/br
   @2,17 clea to 13,60
   @2,17 to 13,60
   @2,25  say 'PROSES/HITUNG ULANG TRANSAKSI'
   @4,19  say 'Angka  di Kolom  Pembelian, Penjualan,'
   @5,19  say 'dan Retur_Pemb  akan  di-Proses Ulang,'
   @6,19  say 'sehingga Sisa Akhir Stok otomatis akan'
   @7 ,19 say 'disesuaikan dg Transaksi yang tercatat.'
   @8 ,19 say 'Jalankan Menu ini SEBELUM Posting Stok.!'
   @10,19 say 'Ketik REPROSES untuk konfirmasi:'
   @10,52 get ket pict '@!'
   Read
   If lastkey()#27 .and. ket='STOK0000'
      @11,19 say 'Perhatian...!'
      @12,19 say 'Sisa Stok Brg akan di-kosongkan semua..!'
      Do whil .not. ytp$'YT'
         @13,32 say 'Proses? Y/T'get ytp pict '!'
         Read
         If lastkey()=27
            Exit
         Endi
      Endd
      If ytp='Y' .and. lastkey()#27
         Do pesan with 'Tunggu, sedang proses...'
         Bln=left(time(),5)
         Nfx='STO_'+left(bln,2)+right(bln,2)+'.DTA'
         Sele 3
         Uselan('STOK.DTA','','','','','')
         Copy to &nfx
         Useidx('STOK.DTA')
         *
         Repl all stok_a with 0,stok_m with 0,stok_k with 0,stok_rb with 0
         Repl all tanggal_1 with ctod('  -  -    '),tanggal_2 with ctod('  -  -    '),tanggal_rb with ctod('  -  -    '),tanggal_0 with tglx
         *
         Ytp=' '
      Endi
      Exit
   Endi
   If lastkey()#27 .and. ket='REPROSES'
      @11,19 say 'ReProses dari:'get tgl
      @11,45 say 's/d:'get tgl2
      Do whil .not. ytp$'YT'
         @13,32 say 'Proses? Y/T'get ytp pict '!'
         Read
         If lastkey()=27
            Exit
         Endi
      Endd
      If ytp='Y' .and. lastkey()#27
         If mont(tgl)#mont(tgl2)
            Yt=' '
            Save scre to smkdb
            Set colo to w+/r,w+/b
            @17,15 clea to 20,58
            @17,15 to 20,58
            @18,17 say 'Pengisian Tanggal, Bulan-nya tidak sama.'
            @19,17 say '      Tetap akan diLanjut? Y/T :'
            @19,50 get yt pict '!'
            Read
            Rest scre from smkdb
            If yt#'Y' .or. lastkey()=27
               Loop
            Endi
            Beda=.t.
         Endi
         Do pesan with 'Tunggu, sedang proses...'
         Thn =right(dtoc(tgl),2)
         Bln =subst(dtoc(tgl),4,2)
         Nfx ='STO_'+thn+bln+'.DTA'
         *
         Do konvtgl
         Do bacafile
         *
         Sele 2
         If .not. file('&nfx')
            Ytp=' '
            Do pesan with 'Tidak Ada File STOK-'+thn+bln+' .  ReProses Data Bulan ini? Y/T : '
            @23,0
            @24,70 get ytp pict '!'
            Read
            If lastkey()=27 .or. ytp#'Y'
               Exit
            Endi
            Uselan('STOK.DTA','STOK5.NDX','','','','')           
         Elseif file('&nfx')
            Useidx('&nfx')
            Inde on kode_bar to L-STOK.TMP
            Uselan('&nfx','L-STOK.TMP','','','','')           
         Endi
         Jml2=reccount()
         Tgl3=ctod('  -  -    ')
         Go top
         Do whil .not. eof()
            Do pesan with ltri(str(recno()))+' / '+ltri(str(jml2))
            Rlok()
            Repl stok_m with 0,stok_k with 0,stok_rb with 0,;
                 tanggal_1 with tgl3,tanggal_2 with tgl3,tanggal_rb with tgl3
            Ul()
            Skip
         Endd
         *
         Do pesan with 'Proses PEMBELIAN...'
         Sele 3
         Uselan('BELIIN.DTA','BELIIN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2)
         Endi
         Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_bar
            Jml =jumlah
            Isi =1  &&isine_b
            *
            Sele 2
            Seek kdb
            Rlok()
            Repl stok_m with stok_m+(jml*isi),tanggal_1 with tgl3
            Ul()
            *
            Sele 3
            Skip
         Endd
         *
         *
         Do pesan with 'Proses RETUR PEMBELIAN...'
         Sele 3
         Uselan('RETURBN.DTA','RETURBN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2)
         Endi
         Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_bar
            Jml =jumlah
            *
            Sele 2
            Seek kdb
            Rlok()
            Repl stok_rb with stok_rb+jml,tanggal_rb with tgl3
            Ul()
            *
            Sele 3
            Skip
         Endd
         *
         Do pesan with 'Proses PECAH/MUTASI BARANG...'
         Sele 3
         Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')
         Seek dtos(tgl)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2)
         Endi
         Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
            Tgl3=tanggal
            Kdb =kode_brg    &&brg.tujuan
            Kdb2=kode_brg2   &&brg.asal
            Jml =jumlah
            Isi2=isine_b
            *
            Sele 2
            Seek kdb
            Rlok()
            Repl stok_m with (stok_m+(jml*isi2)),tanggal_1 with tgl3
            Ul()
            Commit
            *
            Seek kdb2
            Rlok()
            Repl stok_k with (stok_k+(jml*isine_b)),tanggal_2 with tgl3
            Ul()
            Commit
            *
            Sele 3
            Skip
         Endd
         *
         Thn =right(dtoc(tgl),2)
         Bln =subst(dtoc(tgl),4,2)
         Do bacafile
         Do pesan with 'Proses PENJUALAN...'
         Do whil .t.
            If file('&nf0')
               Sele 3
               Uselan('&nf0','&nf1','','','','')
               Seek dtos(tgl)
               If .not. foun()
                  Loca for (tanggal>=tgl .and. tanggal<=tgl2)
               Endi
               Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
                  Tgl3=tanggal
                  Kdb =kode_bar
                  Jml =jumlah
                  Isi =1
                  *
                  Sele 2
                  Seek kdb
                  Rlok()
                  Repl stok_k with stok_k+(jml*isi),tanggal_2 with tgl3
                  Ul()
                  *
                  Sele 3
                  Skip
               Endd
            Endi
            If beda
               Set cent on
               Do carifljual
               If tgl>tgl2
                  Exit
               Endi
            Elseif beda=.f.
               Exit
            Endi
         Endd
         *
         Do pesan with 'Hitung Ulang Transaksi sudah selesai, selanjutnya Posting Stok?'
         *
      Endi
   Endi
   ***
   * SETELAH ITU, POSTING?
   ***
   Ytp=' '
   Set colo to w+/r,w+/b
   @16,15 clea to 19,58
   @16,15 to 19,58
   @17,18 say 'Proses Hitung Ulang Stok Selesai...'
   @18,18 say 'Lanjut ke Posting Stok Bulanan? Y/T:'
   @18,55 get ytp pict '!'
   Read
   If ytp#'Y' .or. lastkey()=27
      Exit
   Endi   
   Ket=spac(7)
   Tgl=tglx
   Ytp=' '
   Rest scre from smkdb
   Set colo to w+/r
   @5,15 clea to 19,60
   @5,15 to 19,60
   @5,22  say 'POSTING STOK AWAL / TUTUP BULAN'
   @7,18  say 'Saldo/Stok Awal  akan  terisi   Otomatis'
   @8,18  say 'dari  Akumulasi Transaksi Pembelian  dan'
   @9,18  say 'Penjualan selama 1 Bulan  atau terhitung'
   @10,18 say 'mulai  dari  Tgl.Posting Stok  Terakhir.'
   @12,18 say 'Kolom / Jumlah   " Stok Masuk & Keluar " '
   @13,18 say 'menjadi "0" (kosong) semua.'
   @17,18 say 'Ketik POSTING untuk konfirmasi:'
   @17,50 get ket pict '@!'
   Set colo to gr+/r*
   @15,20 say '***PROSES INI TIDAK BISA DIULANG***'
   Set colo to gr+/r
   Read
   If lastkey()=27
      Exit
   Endi
   If ket#'POSTING'
      Loop
   Endi
   @18,18 say 'Posting  s/d  Akhir Tanggal:'get tgl
   Do whil .not. ytp$'YT'
      @19,30 say 'Proses? Y/T'get ytp pict '!'
      Read
      If lastkey()=27
         Exit
      Endi
   Endd
   If ytp='Y' .and. lastkey()#27
      Do pesan with 'Tunggu, sedang proses...'
      Tgl3=ctod('  -  -    ')
      Thn =right(dtoc(tgl),2)
      Bln =subst(dtoc(tgl),4,2)
      Nfx ='STO_'+thn+bln+'.DTA'
      If file('&nfx')
         Do pesan2 with 'Sudah ada file STOK-'+thn+bln
         Clos all
         Retu
      Endi
      Sele 2
      Useidx('STOK.DTA')
      Inde on KODE_KLB+NAMA_BRG TO TEMP.TMP
      *
      Uselan('STOK.DTA','TEMP.TMP','','','','')
      Copy to &nfx
      Close
      *
      Useidx('STOK.DTA')
      *
      Repl all stok_a with ((stok_a+stok_m)-(stok_k+stok_rb)),;
               stok_m with 0,stok_k with 0,stok_rb with 0
      Repl all tanggal_1 with tgl3 ,tanggal_2 with tgl3 ,tanggal_rb with tgl3,tanggal_0 with tgl+1
      *
      Sele 1
      Uselan('DATA.SET','','','','','')
      Rlok()
      Repl tanggal with tgl
      Ul()
   Endi
   Exit
Endd
Set colo to /bg
Clos all
Ytp='X'   &&&UNT_LANGSUNG_CLOSE
Retu

***BERLAKU STOK>MPB & KEU>LHUT
Func hitdis
Hbx  =(JUMLAH*HARGA)-DISKON_RP
Disr1=( HBX * (DISKON_1/100))
Disr2=( (HBX-DISR1) * (DISKON_2/100) )
Disr3=( (HBX-DISR1-DISR2) * (DISKON_3/100) )
Hbnet=( HBX-(DISR1+DISR2+DISR3)-DISKON_RP2 )
Retu(.T.)

*DAFTAR KLP
*---------
Proc klp
Uselan('JENIS.DTA','JENIS.NDX','JENIS2.NDX','','','')
*
Klb =spac(5)
Nklb=spac(30)
Do whil .t.
   Set colo to
   @1,0 clea to 23,79
   Set colo to /bg
   @1,21 clea to 23,58
   @1,21 to 23,58
   @1,21 say chr(218)+repl(gl,5)+chr(194)+repl(gl,30)+chr(191)
   @2,21 say tl+'Kode '+tl+'Nama Kelompok Brg'+spac(13)+tl
   @3,21 say chr(195)+repl(gl,5)+chr(197)+repl(gl,30)+chr(180)
   Pesanx='F4=Tambah    F5=Edit    F8=Hapus    F10=Cetak'
   Do pesan with pesanx
   Declare field[5],bts[5],frm[5]
   Field[1]='KODE_KLB'
   Field[2]='NAMA_KLB'
   Stor '@!' to frm[1],frm[2],frm[3],frm[4]
   Stor tl to bts[1],bts[2],bts[3],bts[4]
   Dbedit(4,22,22,57,field,'fklb',frm,'','',bts)
   If lastkey()=27
      Exit
   Endi
Endd
Set curs off
Clos all
Rele klb,nklb
Rele field[4],frm[4],bts[4]
Set cent on
Retu

Function fklb
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=-3
      Nr=recno()
      B=row()+1
      Go bott
      If eof()
         B=b-1
      Endi
      Set curs on
      Do whil .t.
         Klb =spac(5)
         Nklb=spac(30)
         Jml2=0
         Tambah=.t.
         @b,21 say tl+spac(5)+tl+spac(30)+tl
         Do pesan with 'ESC=Selesai'
         @b,22 get klb pict '@!'
         Read
         @24,0
         If klb=spac(5) .or. lastkey()=27
            Go nr
            Exit
         Endi
         Loca for kode_klb=klb
         If foun()
            Klb =kode_klb
            Nklb=nama_klb
            Nr  =recno()
            Tambah=.f.
         Endi
         Yt='Y'
         @b,28 get nklb pict '@!'
         @24,49 get yt pict '!'
         Do pesan with 'Data Benar? Y/T : '
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         Endi
         If tambah
            Flok()
            Appe blan
         Else
            Go nr
            Rlok()
         Endi
         Repl kode_klb with klb,nama_klb with nklb
         Ul()
         Commit
         *
         B=b+1
         If b>20
            B=4
         Endi
      Endd
      Do pesan with pesanx
      Set curs off
      Retu 2
Elseif lastkey()=-4
      Set curs on
      Do case
         Case fds='KODE_KLB'
            Stor kode_klb to klb,klb2
            @row(),col() get klb pict '@!'
            Read
            Do pesan with pesanx
            Set curs off
            Retu 1
         Othe
            Rlok()
            @row(),col() get &fds pict '@!'
            Read
            Ul()
            Commit
      Endc
      Set curs off
      Do pesan with pesanx
      Retu 1
Elseif lastkey()=-7
   Klb=kode_klb
   Set colo to gr+*
   @row(),col() say &fds
   Yt=' '
   Set curs on
   Set colo to /bg
   Do pesan with 'DiHapus? Y/T'
   Do whil .not. yt$'YT'
      @24,47 get yt pict '!'
      Read
   Endd
   If yt='Y' .and. (.not. deleted())
      Rlok()
      Dele
      Ul()
      Commit
      Skip
   Endi
   Do pesan with pesanx
   Set curs off
   Retu 2
Elseif lastkey()=-9
   Go top
   No=1
   Set curs on
   Do cp
   If cc=0
      Retu 0
   Endi
   Set prin on
   Set devi to prin
   Set marg to 0
   ?chr(18)
   ?' DAFTAR KELOMPOK BRG'
   ?repl('-',62)
   Do whil .not. eof()
      ?tran(no,'999  ')+kode_klb+' '+nama_klb+' '+kode_dep+' '+nama_dep
      No=no+1
      Skip
   Endd
   ?repl('-',62)
   ?''
   Set prin off
   Set devi to scre
   Do pesan with pesanx
   Set curs off
   Retu 1
Else
   Do pesan with 'Kode Klp.:      '
   Set curs on
   @24,43 get klb pict '@!'
   Read
   If lastkey()=27
      Set curs off
      Do pesan with pesanx
      @24,0
      Retu 1
   Endi
   Seek klb
   If .not. foun()
      Do pesan2 with 'Tidak ada kode '+rtri(klb)
   Endi
   Nklb=nama_klb
   Set curs off
   Do pesan with pesanx
   @24,0
Endi
Retu 0

* LOKASI/RAK
*-----------
Proc rak
Uselan('RAK.DTA','','','','','')
Kdp ='   '
Ndep=spac(30)
Do whil .t.
   Set colo to
   @1,0 clea to 23,79
   Set colo to /bg
   @1,22 clea to 23,57
   @1,22 to 23,57
   @1,22 say chr(218)+repl(gl,3)+chr(194)+repl(gl,30)+chr(191)
   @2,22 say tl+'Kd.'+tl+'Nama Lokasi/Rak'+spac(15)+tl
   @3,22 say chr(195)+repl(gl,3)+chr(197)+repl(gl,30)+chr(180)
   Pesanx='F4=Tambah     F5=Edit     F8=Hapus'
   Do pesan with pesanx
   Declare field[5],bts[5],frm[5]
   Field[1]='KODE'
   Field[2]='NAMA'
   Frm[1]='@!'
   Frm[2]='@!'
   Bts[1]=tl
   Bts[2]=tl
   Dbedit(4,23,22,56,field,'frak',frm,'','',bts)
   *
   If lastkey()=27
      Exit
   Endi
Endd
Set curs off
Clos all
Set cent on
Retu

Function frak
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
      Retu 1
Elseif lastkey()=27
      Retu 0
Elseif lastkey()=-3
      Nr=recno()
      B=row()+1
      Go bott
      If eof()
         B=b-1
      Endi
      Set curs on
      Do whil .t.
         Klb =spac(3)
         Nklb=spac(30)
         Jml2=0
         Tambah=.t.
         @b,22 say tl+spac(3)+tl+spac(30)+tl
         Do pesan with 'ESC=Selesai'
         @b,23 get klb pict '@!'
         Read
         @24,0
         If klb=spac(5) .or. lastkey()=27
            Go nr
            Exit
         Endi
         Loca for kode=klb
         If foun()
            Klb =kode
            Nklb=nama
            Nr  =recno()
            Tambah=.f.
         Endi
         @b,27 get nklb pict '@!'
         Do pesan with 'Masukan Nama Lokari/Rak'
         Read
         Yt='Y'
         Do pesan with 'Data Benar? Y/T : '
         @24,49 get yt pict '!'
         Read
         If yt#'Y' .or. lastkey()=27
            Loop
         Endi
         If tambah
            Flok()
            Appe blan
            Repl kode with klb,nama with nklb
         Else
            Go nr
            Rlok()
            Repl kode with klb,nama with nklb
         Endi
         Ul()
         Commit
         *
         B=b+1
         If b>20
            B=4
         Endi
      Endd
      Do pesan with pesanx
      Set curs off
      Retu 2
Elseif lastkey()=-4
      Set curs on
      Do case
         Case fds='KODE'
            Stor kode to klb,klb2
            @row(),col() get klb pict '@!'
            Read
            Do pesan with pesanx
            Set curs off
            Retu 1
         Othe
            Rlok()
            @row(),col() get &fds pict '@!'
            Read
            Ul()
            Commit
      Endc
      Set curs off
      Do pesan with pesanx
      Retu 1
Elseif lastkey()=-7
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
      Do pesan with pesanx
      Set curs off
      Retu 2
Elseif lastkey()=-9
      Go top
      No=1
      Set curs on
      Do cp
      If cc=0
         Retu 0
      Endi
      Set prin on
      Set devi to prin
      Set marg to 0
      ?chr(18)
      ?' DAFTAR LOKASI/RAK'
      ?repl('-',62)
      Do whil .not. eof()
         ?tran(no,'999  ')+kode+' '+nama
         No=no+1
         Skip
      Endd
      ?repl('-',62)
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
      Retu 1
Endi
Retu 0

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

* EOF BANT
