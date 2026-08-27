PROCEDURE MAIN
***CEK KURANGAN KALO CTK.ULANG
* PENJUALAN
Set talk off
Set stat off
Set scor off
Set safe off
Set cons off
Set exclusive off
Set conf on
Set dele on
Set wrap on
Set fixe on
Set cent on
Set deci to 0
Set date ital
Set proc to crx
Set proc to kas
Set key -6 to kalk
Clea
Stor '' to aidi1,aidi2,empunya
*
Do idents
*
Blnthn='subs(dtoc(tgl),7,4)+subs(dtoc(tgl),4,2)'
Tgl   =date()
Ksa   =gete("KASSA")
Stor '' to hr,brs1,brs13,fkt,fktx,nfc,nf0,nf1,nf2,nf3,nf4,pr1,pr2,pr3,pr4,klb,xpos,slihat,tglz,tgly,;
           warna1,warna2,warna3,warna4,warna5,warna6
Stor .f. to atas,bawah,xCetak,xOwner,ada,lihat,diedit,onlen,pilwarna,memsusul,cekpoin,hanyaCekp
Stor .f. to ubahjml,ubahjml2,hapus,isilagi,ulang,batal,adadis,member,nonctk
Gd=chr(205)
Gl=chr(196)
Tl=chr(179)
Do hari
Do cekdata

Sele 3
*
Sele 4
*
Sele 5
*
Sele 6
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
Alm1  =almt_1
Alm2  =almt_2
Alm3  =almt_3
Tgla  =tanggal_2
Tglb  =tanggal_3
Idtx  =rtri(id_cust)
Pdt   =pj_cust
Close
*
Sele 99
Useidx('KASSA.SET')
Loca for no_kassa=ksa
If .not. foun()
   Appe blan
   Repl no_kassa with ksa,mode_kasir with 'A',jark_nota with 4,jark_tepi with 0,lebr_roll with '8',cari_kode with '0',;
        id_komp with 'S',id_data with '0',id_prin with 'T',id_drwr with 'T',id_cut with 'T',id_disp with '2',id_poin with '0',tanggal with tgl
   Commit
Endi   
xKsr=mode_kasir
Iprn =id_prin
Idrw =id_drwr
Idis =id_disp
Idcut=id_cut
Ikomp=id_komp
Idata=id_data
iPoin=id_poin
Jk   =cari_kode
Lr   =lebr_roll
Jn   =jark_nota
Jt   =jark_tepi
Close
*
Do warna
*
Jml   =1
Tglso=tgl
Stor 0   to jml2,hj,hj2,hp,dis1,dis2,dis3,dis31,dis4,jhg,byr,tot,tot2,tot3,kbl,blt,dns,nr,nrx,nrs,nrbyr,xAda2,kl,jwk,jmlso,xhal,totpn,totpn2,pnskr,pnskr2,pnsbl,isi,xPoin
Stor ' ' to id_ctk,lks,lksx,kdb,kdbx,nmb,st,stb,rn,ket1,ket2,ipn,ipn2
Stor .f. to ubahjml,ubahjml2,hapus,isilagi,ulang,batal,adadis,adatunda,ditunda
Do whil .t.
   Sele 6
   Uselan('KASIR.DTA','','','','','')
   Ksr ='  '
   Sls =spac(20)
   Awal=0
   Set colo to
   @0,0 clea to 24,79
   Set colo to /bg,w/b
   @9,22 clea to 14,58
   @9,22 to 14,58
   @10,24 say 'Hari, Tgl.: '+hr+', '+dtoc(tgl)
   @11,24 say 'No. Kassa.: '+ksa
   @12,24 say 'Kode Kasir: '
   @13,24 say 'Cetak Nota:    T =Tidak_Cetak_Nota'
   @12,36 get sls pict '@!' valid sls#spac(20)
   Do pesan with 'Masukan Kode Kasir     ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   Endi
   Loca for kode=sls
   If .not. foun()
      Set colo to r+/b
      @12,36 say 'x x x x x x'
      Tone(1000,3)
      Tone(1500,3)
      Tone(2000,6)
      Do pesan with 'Kode Kasir Salah.  Ulangi !'
      Wait''
      Loop
   Endi
   Stor nama to nsls,nsls2
   Stor jens_jual to jj,itk
   Ksr=nomer
   *
   Set colo to /bg,w+/b
   Set colo to /bg,gr+/b
   @12,24 say 'Nama Kasir: '+nsls
   @13,36 get id_ctk pict '@!'
   Read
   Set colo to w+
   If lastkey()=27
      Loop
   Endi
   If id_ctk='T'
      xCetak=.f.
   Else
      xCetak=.t.
   Endi
   *
   If tanggal#tgl
      Rlok()
      Repl tanggal with tgl
      If jj=' '
         Repl nota with 0
      Endi
      Ul()
   Endi
   Nn=nota+1
   If nn>99999
      Nn=1
   Endi
   Do xnota
   Do bacafile
   *
   Nfc='C:\TEMP\BR'+subs(&blnthn,3,2)+right(&blnthn,2)+'.TMP'
   Nfx='JUAL-'+ksa+'.DTA'
   Nfy='TUNDA-'+ksa+'.TMP'
   Do cektunda
   *
   If .not. file ('&nfx') .or. (.not. file('&nf0'))
      Do pesan2 with 'Belum ada File Data untuk Kassa-'+ksa+' , Lapor ke Kantor, jalankan Re-Index'
      Loop
   Else
      Sele 5
      Useidx('&nfx')
      If batal
         Batal=.f.
         Loop
      Endi
      Go top
      If nama_ksr=nsls
         Yt='Y'
         Set colo to w+/r
         @15,21 clea to 20,59
         @15,21 to 20,59
         @16,23 say 'Ada Nota yang Belum di-Simpan...!'
         @17,23 say 'Tanggal: '+dtoc(tanggal)+' , Nota: '+faktur
         @19,23 say 'Mau diLanjutkan unt diSimpan? Y/T'get yt pict '!'
         Read
         If yt='T'
            Repl all kode_ksr with '  ',nama_ksr with spac(20),id_data with ' '
         Else
            Isilagi=.t.
         Endi
      Elseif nama_ksr#nsls
         Repl all kode_ksr with '  ',nama_ksr with spac(20),id_data with ' '
      Endi
      Set colo to w+
   Endi
   Sele 3
   Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
   Do ceklokal
   *
   N  =1
   Jab=spac(30)
   Stor spac(10) to plg,kta
   Stor ctod('  -  -    ') to tgl2,cTgljt
   Stor spac(25) to nsp,nsls5
   Stor spac(5) to ksls
   Stor ' ' to idp,yt,lks,lksx
   Do whil .t.
      Clea
      Set colo to &warna5
      @0,0
      @0,1 say hr+', '+dtoc(tgl)
      Set colo to &warna2
      @1,0 clea to 3,79
      @1,1 say right(aidi1,17)
      @2,1 say aidi2
      @3,1 say 'Kasir: '+nsls
      @2,65 say 'Nota:'
      @3,65 say 'Item:'
      Set colo to &warna1
      @4,0 say repl(gl,8)+chr(194)+repl(gl,30)+chr(194)+repl(gl,12)+chr(194)+repl(gl,10)+chr(194)+repl(gl,5)+chr(194)+repl(gl,10)
      @6,0 say repl(gl,8)+chr(197)+repl(gl,30)+chr(197)+repl(gl,12)+chr(197)+repl(gl,10)+chr(197)+repl(gl,5)+chr(197)+repl(gl,10)
      Kolomjl()
      B=7
      @24,0
      Set colo to &warna2
      @2,74 say fkt
      Set colo to &warna1
      @5,0 say 'Kode Brg'+tl+'Nama Barang'+spac(19)+tl+' Jml.Barang '+tl+'    @Harga'+tl+'@Dis%'+tl+'Jumlah Rp.'
      If xCetak .and. (.not. isilagi)
         *If xCetak .and. nonctk=.f.  &&&.and. iprn='D' 
         *   Do kopnota
         *Elseif xCetak .and. nonctk=.f.  &&&.and. (iprn='T' .and. idCut='T') 
         *   Do kopnota
         *Else
         *   Nonctk=.f.
         *Endi
      Endi
      If (.not. isilagi)
         Tot3=0
         Plg =spac(10)
         Stor spac(25) to nsp,jab
         @24,0
         Do tampil
         If lastkey()=27
            Exit
         Endi
         B=7
         If diEdit
            Sele 5
            Go top
            Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
               Jhg =&brs1
               Brs2()
               B  =b+1
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               Endi
               Skip
            Endd
            B=7
         Endi
         *
      Endi
      If pilwarna
         Pilwarna=.f.
         Loop
      Endi
      *
      Stor 0 to dis1,tot,totr,totpn,totpn2,pnskr,pnskr2,kbl,tjml,no,jmlx
      Jml =1
      vKdb=spac(13)
      vNmb=spac(30)
      Idp =' '
      If isilagi
         Do hpsjual
         Isilagi=.f.
         *If diEdit
         *   Sele 4
         *   Close
         *Endi
      Endi
      Do whil .t.
         Stor spac(13) to kdb,kdbx
         Ksp=spac(5)
         Nmb=spac(30)
         Stor 0 to dis1,dis2,jhg
         *
         If lihat
            Rest scre from slihat
         Endi
         Stor .f. to lihat,memsusul
         *
          If .not. lihat
             Set key -1 to xadamem
             Set key 28 to xadamem
          Endi
          *
          Set key  5 to xubahjml
          *
          If jj=' '
             Set key -8 to notunda
          Endi
          *
          Set key 42 to xubahjml2
          Set key 43 to xubahjml2
          Set key 28 to xadamem
          Set key -1 to xadamem
         Do nomor with tot,1,56
         Set colo to &warna1
         @b,0 say spac(8)+tl+spac(30)+tl+spac(12)+tl+spac(10)+tl+spac(5)+tl+spac(10)
         If jml#1
            Set colo to &warna5
            @b,40  say jml pict '@e 9999.99'
         Endi
         Set colo to &warna2
         If diEdit=.f.
            @3,71  say tran(tjml,'999 /')+tran(no,'999')
         Endi
         @23,7 say spac(4)
         @23,7 say ltri(tran(pnskr,'999 + '))+ltri(tran(pnskr2,'999'))
         Set colo to &warna6
         @20,69 say tran(tot,'@e 999,999,999')
         If tot3#0
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            Set colo to &warna1
         Endi
         *
         Sele 3
         Set colo to &warna1
         @b,0 get kdb pict '@!S8'
         Do pesan with 'BarCode?   n*=Qty   '+chr(24)+'=Koreksi   F1=Member   F9=Tunda   ESC=Selesai'
         Read
         Set key  42 to
         Set key  43 to
         Set key  28 to
         Set key   5 to
         Set key  -8 to 
         Set colo to &warna3
         If lastkey()=27 .and. (ubahjml=.f. .and. ubahjml2=.f.)
            Exit
         Endi
         Set colo to &warna1
         *      
         If n>200
            Tone(2000,1)
            Tone(1500,1)
            Do pesan with 'Kode Brg untuk 1 Nota Tidak Boleh Lebih dari 200 item.   ENTER-Selesai'
            Wait''
            Exit
         Endi
         *
         If ubahjml .and. n>1
            Do hpsjual
            Ubahjml=.f.
            Loop
         Elseif ubahjml .and. n=1
            Ubahjml=.f.
         Endi
         *
         If ubahjml2
            Ubahjml2=.f.
            Jml=val(kdb)
            Loop
         Endi
         *
         Do cekplg
         If member
            Member=.f.
            If no>0
               Memsusul=.t.
               Do hpsjual
            Endi
            Loop
         Endi
         *
         If ditunda
            Exit
         Endi
         *
         Sele 3
         If xKsr='A'
            If left(kdb,2)='0 '
               Klb ='EX-'
               Nmb =spac(30)
               Isi =1
               St  ='  x  '
               Ipn =' '
               Ipn2=' '
               Dis1=0
               Stor 0 to hj,hj2,hjx
               @b,9  get nmb pict '@!S30'
               @b,40 get jml pict '   9999'
               @b,48 say left(st,4)
               @b,53 get hj  pict '@e   99999999'
               Read
               Hp =hj/1.05
               Hj2=hj
            Else
               If kdb=spac(13)
                  Carikode=.f.
                  Nmb=spac(30)
                  @b,9 get nmb pict '@!S30'
                  Do pesan with 'Nama Barang?'
                  Read
                  If nmb=spac(30)
                     Exit
                  Endi
                  Nmb=rtri(nmb)
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
                        Set order to 2
                        Seek rtri(kdb)
                     Endi
                     Nmb=kdb
                     Do dafbrg
                  Endi
               Endi
               Kdbx=kode_bar
               *
               Sele 3
               Set order to 1
               Seek kdbx
               Hj  =harga_2
               If lastkey()=27 .or. kdbx=spac(13)
                  Loop
               Endi
               *
               If hj>0
                  Ada=.f.
                  Sele 5
                  Go top
                  Do whil .not. eof() .and. ( tanggal=tgl .and. kode_ksr=ksr .and. faktur=fkt )
                     If lihat=.f. .and. (kode_brg=kdb .or. kode_bar=kdbx)
                        Jml=jml+jumlah
                        Nr2=recno()
                        Ada=.t.
                        Exit
                     Endi
                     Skip
                  Endd
               Endi
               *
               Sele 3
               Set order to 1
               Ksp =kode_spl
               Klb =kode_klb
               Kdb =kode_brg
               Nmb =nama_brg
               St  =satuan
               Isi =isine_b
               Hp  =harga+(harga*(pajak/100))
               Hjx =persen
               Dis1=diskon_1
               Dis2=diskon_rp
               Ipn =id_point
               Ipn2=id_point2
               Stor harga_2 to hj,hj2,vHj2
               Hj =iif(idp='1',iif(harga_21>0,harga_21,harga_2),harga_2)
               *
               Brs3()
               Do cekhj
               Do cariuntung
               If hp>hj
                  Hp=hj/1.05
               Endi
               *
               Jhg=&brs13
               Brs3()
               If lihat
                  Lihat=.f.
                  Do pesan with 'Transaksi Terakhir Hanya untuk MeLihat Harga...'
                  Wait''
                  Rest scre from slihat
                  Loop
               Endi
            Endi
         Elseif xKsr='B' .or. xKsr='C'
            Klb ='EX-'
            Nmb ='-BELANJA-'
            Isi =1
            St  ='  x  '
            Ipn =' '
            Ipn2=' '
            Dis1=0
            Stor 0 to hj,hj2,hjx
            @b,9  say nmb
            @b,40 say jml pict '  9,999'
            @b,48 say left(st,4)
            If xKsr='B'
               @b,40 get jml pict '   9999'
            Endi
            @b,53 get hj  pict '@e   99999999'
            Read
            Hp =hj/1.05
            Hj2=hj
         Endi
         *
         @24,0
         If lastkey()=27
            Loop
         Endi
         *
         Jhg =&brs13
         Brs3()
         Tot =tot + jhg
         Tot2=tot2+ (jml*hp)
         Byr =byr+(tot3+tot)
         Tjml=tjml+jml
         *
         Do hitpoin
         *
         Nulise=left(nmb,20)+tran(jml,'@e 999 ')+left(st,3)+'=Rp'+tran(jhg,'@e 99,999,999')
         *Kedisple(nulise)
         *
         Sele 5
         If n=1
            Go top
         Elseif ada
            Go nr2
         Endi
         Repl tanggal with tgl,faktur with fkt,sip with '1',kode_ksr with ksr,nama_ksr with nsls,;
              kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              isine_b with isi,jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,;
              kode_lks with lks,id_point with ipn,id_point2 with ipn2,rectran with 0,recstok with 0,id_data with ' '
         Skip
         Nr=recno()
         If ada
            Ada=.f.
            Stor 0 to tot,totpn,totpn2,tjml,no
            Kolomjl()
            Go top
            Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
               Jml =jumlah
               Hj  =harga_2
               Ipn =id_point
               Ipn2=id_point2
               Dis1=diskon
               Jhg =&brs13
               If recno()=nr2
                  Set colo to gr+
                  @b,40 say tran(jumlah,'@e 9999.99')
                  Set colo to w+
               Endi
               Brs2()
               If jml#0
                  No =no+1
               Endi
               B  =b+1
               Tot=tot+jhg
               Tjml=tjml+jumlah
               *
               Do hitpoin
               *
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               Endi
               Skip
            Endd
            B =b-1
            No=no-1
         Endi
         *
         B  =b+1
         N  =n+1
         No =no+1
         Jml=1
         Kdb=spac(13)
         Nmb=spac(30)
         If b>18
            Scroll(7,0,18,79,1)
            B=18
         Endi
      Endd
      *
      If lihat
         Stor 0 to tot,kbl
         Lihat=.f.
         Wait''
         @7,0 clea to 18,79
         Loop
      Endi
      *
      If ditunda
         Ditunda =.f.
         Adatunda=.t.
         Wkt=subs(time(),2,1)+subs(time(),4,2)+right(time(),2)
         *
         Sele 5
         Repl all jam with wkt
         Copy to TUNDA.&ksa for ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
         Repl all kode_ksr with '  ',nama_ksr with spac(20)
         Go top
         *
         Sele 99
         Do cektunda
         If .not. file('&nfy')
            Useidx('JUAL.DTA')
            Copy stru to &nfy
         Endi
         Useidx('&nfy')
         Jml=reccount()
         Appe from TUNDA.&ksa
         If reccount()#jml
            Appe blan
            Repl jam with '-----',nama_brg with repl('-',30),satuan with '-----'
         Endi
         *
         Do caritunda
         Do cektunda
         *
         Sele 5
         Loop
      Endi
      *
      Stor 0 to dis31,blt,dns
      xSele=select()
      Do cekbulat
      Byr=0
      If jj=' '
         Byr=tot3+tot
      Endi
      Nulise='                    '+'Jumlah Rp.'+tran(tot,'@e 99,999,999')
      *Kedisple(nulise)
      Do whil .t.
         Batal=.f.
         Stor .f. to batal,adadis,member,donasi
         Set key  28 to xadamem
         Set key  46 to nol3
         Set key  68 to xadadis
         Set key 100 to xadadis
         Set colo to &warna6
         If blt#0
            @19,57 say 'Pembulatan :'
            @19,75 say tran(blt,'@e 9,999')
         Endi
         @20,69 say tran(tot,'@e 999,999,999')
         @23,7 say ltri(tran(pnskr,'999 + '))+ltri(tran(pnskr2,'999'))
         If tot3#0
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
         Endi
         Dis31=(tot*(dis3/100))
         @21,63 say dis3  pict '@e 99.99'
         @21,69 say dis31 pict '@e 999,999,999'
         @22,69 get byr pict '@e   999999999' &&valid byr>=tot
         Do pesan with 'Bayar?     +_=Donasi     D=Diskon     F1=Member     Total Riil =Rp '+ltri(tran(tot3+((tot+blt)-dis31),'@e 999,999,999'))
         Set colo to &warna4
         Read
         Set key  28 to
         Set key  46 to
         Set key  68 to
         Set key 100 to
         Set key 109 to
         If adadis
            Adadis=.f.
            @21,63 get dis3 pict '@e 99.99'
            Do pesan with 'Diskon = n % dari Total Rp   **Total Rp. sudah Termasuk Diskon per Item Brg**' 
            Read
            If dis3=999
               Hj  =hj
               Dis31=0
            Elseif dis3>0
               Dis31=(tot*(dis3/100))
               @21,63 say dis3  pict '@e 99.99'
               @21,69 say dis31 pict '@e 999,999,999'
            Elseif dis3=0
               @21,69 get dis31 pict '@e 999,999,999'
               Read
               If dis31>0
                  Dis3=(dis31/tot)*100
                  @21,63 say dis3 pict '@e 99.99'
                  @21,69 say dis31 pict '@e 999,999,999'
               Endi
            Else
               Dis3=dis3
            Endi
            Byr=tot3+(tot-dis31)
            @22,69 get byr pict '@e 999,999,999'
            Do pesan with 'Isi Jumlah Pembayaran   ( Total - Diskon = Rp '+ltri(tran(tot3+((tot+blt)-dis31),'@e 999,999,999'))+' )'
            Read
         Endi
         *
         Do cekplg
         *
         If lastkey()=27
            If diEdit
               Tone(1500,0.5)
               Tone(2000,2)
               Do pesan with 'Nota ini adalah Nota Edit,  Simpan dulu... lalu Edit kembali pakai F10/F5'
               Wait''
               Loop
            Endi
            Exit
         Endi
         *
         Itk =' '
         Kbl=abs((byr-(tot3+((tot+blt)-dis31))))
         Set colo to &warna6
         @22,69 say byr pict '@e 999,999,999'
         If byr>(tot-dis31) &&.and. plg=spac(10)
            @20,22 clea to 23,55
            @23,42 say 'UANG KEMBALIAN'
            Do nomor with kbl,20,53
         Endi
         *
         If byr<(tot3+(tot-dis31)) .and. (tot-dis31)#0
            Set colo to gr+/b
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            Do pesan with '    Benar Masuk Piutang? Y/T:'
            Set colo to w+/r
            @24,1 say 'Sisa/Kurang Rp '+ltri(tran(kbl,'@e 999,999,999'))
            Yt=' '
            Do whil .not. yt$'YT'
               @24,55 get yt pict '!'
               Read
               If lastkey()=27
                  Exit
               Endi
            Endd
            If lastkey()=27 .or. yt='T'
               Loop
            Endi
            Itk='K'
         Endi
         Do pesan with iif(xcetak,'ENTER=Simpan+Cetak','C=Simpan+Cetak     ENTER=Simpan/Tdk.Ctk')+iif(kbl>0,'     -+D=Donasi','')+'     ESC=Koreksi'
         Wait''
         If kbl>0 .and. (lastkey()=43 .or. lastkey()=45 .or. lastkey()=68 .or. lastkey()=100)
            Do pesan with 'Masukan Jumlah Uang untuk Donasi'
            Set colo to ,/w
            @23,69 get dns pict '@e        9999'
            Read
            Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
            Set colo to &warna6
            @20,22 clea to 23,55
            @23,69 say dns pict '@e 999,999,999'
            @23,42 say 'UANG KEMBALIAN'
            Do nomor with kbl,20,53
            Do pesan with iif(xcetak,'ENTER=Simpan+Cetak','C=Simpan+Cetak     ENTER=Simpan/Tdk.Ctk')+'     ESC=Koreksi'
            Wait''
         Endi
         Exit
      Endd
      If lastkey()=27 .or. isilagi
         If diEdit
            Tone(1500,0.5)
            Tone(2000,2)
            Do pesan with 'Nota ini adalah Nota Edit,  Simpan dulu... lalu Edit kembali pakai F10/F5'
            Wait''
            Loop
            Exit
         Endi
         Isilagi=.t.
         Loop
      Endi
      Do pesan with 'Tunggu, sedang diCetak'
      I=uppe(chr(lastkey()))
      xAda2=0
      *
      Sele 5
      If xCetak .and. lastkey()=13 .and. tot#0 
         Go top
         Do ctknota
      Elseif xCetak
         NonCtk=.t.
      Endi
      *
      Sele 4
      Do pesan with 'Tunggu, Nota sedang diSimpan...'
      Do bacafile
      Uselan('&nf0','&nf3','&nf1','&nf2','','')
      *
      Stor 0 to xada,xada3,tot2,totpn,totpn2
      Wkt=time()
      *
      Sele 5
      Go top
      Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
         Lks =kode_lks
         Ksp =kode_spl
         Klb =kode_klb
         Kdb =kode_brg
         Kdbx=kode_bar
         Nmb =nama_brg
         St  =satuan
         Stb =satuan_b
         Isi =isine_b
         Jml =jumlah
         Hp  =harga
         Hj  =harga_2
         Hj2 =harga_22
         Dis1=diskon
         Dis2=diskon_rp
         Ket =keterangan
         Nrx =rectran
         Ipn =id_point
         Ipn2=id_point2
         Idt =id_data
         Tot2=tot2 + ((jumlah-jumlah_rtr)*harga)
         *
         Do hitpoin
         *
         If idt=' '
            If diEdit=.f.
               xAda=xada+1
            Endi
            Sele 4
            Flok()
            Appe blan
            Repl tanggal with tgl,jam with wkt,faktur with fkt,sip with '1',kassa with ksa,kode_ksr with ksr,nama_ksr with nsls,kode_lks with lks,kode_sls with ksls,;
                 kode_lgn with plg,kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,satuan_b with stb,isine_b with isi,jumlah with jml,;
                 harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,keterangan with ket,kode_lks with lks,id_point with ipn,id_point2 with ipn2,id_data with itk
            Ul()
            xAda3=xada3+1
            *
            Sele 3
            Set order to 1
            Seek kdbx
            Rlok()
            Repl stok_k&lks with (stok_k&lks+jml),tanggal_2 with tgl
            Ul()
         Elseif idt='1' .and. plg#spac(10)
            Sele 4
            Go nrx
            Rlok()
            Repl kode_lgn with plg
            Ul()
         Endi
         *
         Sele 5
         Repl kode_ksr with '  ',nama_ksr with spac(20),id_data with ' '
         Skip
      Endd
      *
      Sele 4
      If dis31>0
         Flok()
         Appe blan
         Repl tanggal with tgl,jam with wkt,faktur with fkt,kassa with ksa,kode_lks with lks,kode_ksr with ksr,nama_ksr with nsls,kode_sls with ksls,;
              kode_lgn with plg,kode_klb with '-----',nama_brg with 'Diskon Total Penjualan-->',jumlah with 1,harga_2 with (harga_2-dis31),id_data with itk
         Ul()
      Endi
      Rlok()
      Repl rp_bayar with byr,rp_bulat with blt,rp_donasi with dns
      Ul()
      Commit
      *
      If diEdit
         Go nrbyr
         Rlok()
         Repl rp_bayar with byr,rp_bulat with blt,rp_donasi with dns
         Ul()
         Commit
      Endi
      Close
      *
      If plg#spac(10)   &&&.and. (tot-dis31)#0 .and. (tot3#0 .or. byr<(tot-dis31))
         Sele 11
         Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')
         Seek dtos(tgl)+plg+fkt
         If .not. foun()
            Flok()
            Appe blan
         Else
            Rlok()
         Endi
         Repl tanggal with tgl,tanggal_3 with (tgl+jwk),faktur with fkt,nama_ksr with nsls,kode_sls with ksls,kode_lgn with plg,nama_lgn with nsp,almt_lgn with jab,kota_lgn with kta,;
              rp_sisa with tot3,rp_total with tot,rp_pokok with tot2,rp_bayar with iif(byr>(tot3+(tot-dis31)),(tot3+(tot-dis31)),byr),diskon with dis3,rp_diskon with dis31,jml_poin with pnskr+pnskr2,pra_poin with xPoin
         Ul()
         Close
         *
      Endi
      *
      If diEdit
         Fkt=fktx
         Tgl=tglz
      Endi
      If xada#0
         Sele 6
         Loca for nomer=ksr
         Rlok()
         Repl nota with nn
         Ul()
         Nn=nn+1
         If nn>99999
            Nn=1
         Endi
         Do xnota
      Endi
      Plg=spac(10)
      Stor spac(5) to ksls
      Stor spac(25) to nsp,nsls5
      Stor ctod('  -  -    ') to tgl2,cTgljt
      Stor ' ' to idp,yt,lks,lksx
      Stor 0 to hj,dis1,dis2,dis3,byr,jwk,totpn,totpn2,pnskr,pnskr2,tot3
      Stor 1 to jml,n
      Stor .f. to isilagi,diEdit
      Jab=spac(30)
      Kta=spac(10)
      Nsls2 =nsls
      Nulise='    Terima Kasih'+spac(24)
      *Kedisple(nulise)
      *
      Kolomjl()
   Endd
   Nn=nn
   Do xnota
Endd
Set colo to
Clea
Nulise=spac(20)+spac(20)
*Kedisple(nulise)
Clos all
Rele all
Retu

Func kolomjl
Set colo to &warna5
@0,30 say iif(adatunda,'Ada Nota yg diTunda',spac(20))
Set colo to &warna1
B=7
Do whil b<=18
   @b,0 say spac(8)+tl+spac(30)+tl+spac(12)+tl+spac(10)+tl+spac(5)+tl+spac(10)
   B=b+1
Endd
B=7
@19,0 say repl(gl,8)+chr(193)+repl(gl,30)+chr(193)+repl(gl,12)+chr(193)+repl(gl,10)+chr(193)+repl(gl,5)+chr(193)+repl(gl,10)
Set colo to &warna2
@20,0  clea to 23,79
@20,1  say 'Kode:                                       '+spac(12) +'Jml.Belanja:'+spac(13)
@21,1  say 'Nama:'+spac(31)+'                    Disc:      %'+spac(13)
@22,1  say 'Almt:'+spac(31)+'                    Jml.Bayar  :'+spac(13)
@23,1  say 'Poin:'+spac(31)+'                    Donasi     :'+spac(13)
*Set colo to &warna
@20,7  say plg
@21,7  say rtri(nsp)
@22,7  say rtri(jab)
@23,7  say spac(13)
@21,63 say dis3 pict '@e 99.99'
Retu .t.

*** PROSEDUR HAPUS TRANSAKSI
Proc hpsjual
Kolomjl()
Sele 5
If ubahjml2
   Set filter to ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls2 )
Else
   Set filter to ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
Endi
Go top
If nama_ksr=spac(20)
   Set filter to
   Retu
Endi
No=1
Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
   No=no+1
   Skip
Endd
If no>18
   No=18
Endi
Skip -no
*
Kdb=''
For awal=1 to no
    Kdb=kdb+'chr(24)+'
Next
Kdb='chr(4)+&kdb'+'chr(24)'
Keyboard &kdb
If memsusul
   Keyboard chr(9)
Endi
*
Declare field[8],bts[8],frm[8]
Field[1]="left(KODE_BAR,8)"
Field[2]="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)"
Field[3]="tran(HARGA_2,'@e 99,999,999')"
Field[4]="tran(DISKON,'@e 99.99')+tl+tran(&BRS1,'@e 99,999,999')"
Frm[1]=''
Frm[2]=''
Frm[3]=''
Frm[4]=''
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Pesany='ENTER=Edit_KodeBrg    F3/F6=Hg_Ecer     F4/F5=Hg_Bakul     ESC=Selesai'
Set colo to w+/r
@5,28 say chr(254)+' KOREKSI '+chr(254)
Do pesan with pesany
Set colo to &warna3
Dbedit(7,0,18,79,field,'fhpjx',frm,'','',bts)
*
Set colo to &warna1
@5,28 say spac(11)
Kolomjl()
N=1
Stor 0 to tot,tot2,totpn,totpn2,tjml,byr,no
Set filter to
Go top
Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
   Jml =jumlah
   Hj  =harga_2
   Dis1=diskon
   Ipn =id_point
   Ipn2=id_point2
   Jhg =&brs13
   Brs2()
   *
   If jml#0
      No=no+1
   Endi
   B   =b+1
   N   =n+1
   Tot =tot+jhg
   Tot2=tot2+(jumlah*harga)
   Tjml=tjml+jumlah
   Byr =byr+(tot3+tot)
   *
   Do hitpoin
   *
   If b>18
      Scroll(7,0,18,79,1)
      B=18
   Endi
   Skip
Endd
Nmb=spac(30)
Kdb=spac(13)
Jml=1
Stor 0 to dis1,dis2
Stor .f. to hapus
Retu

Function fhpjx
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=13 .and. fds='left(KODE_BAR,8)'
   Cekvar()
   Kdb=spac(13)
   Tjml=tjml-jml
   Set curs on
   Do whil .t.
      Sele 3
      Set colo to &warna3
      @b,0 say spac(8)
      @b,0 get kdb pict '@!S8'
      Do pesan with 'BarCode?'
      Read
      If lastkey()#27
         Carikode=.t.
         Set order to 1
         Seek kdb
         If .not. foun()
            Seek rtri(kdb)
            If .not. foun()
               Carikode=.f.
               Set order to 2
               Seek rtri(kdb)
            Endi
            Nmb=kdb
            Do dafbrg
         Endi
         If lastkey()=27 .or. kdbx=spac(13)
            Sele 5
            Do pesan with pesany
            Set colo to &warna3
            Set curs off
            Retu 1
         Endi
         *
         Kdbx=kode_bar
         Kdb =kode_brg
         Ksp =kode_spl
         Klb =kode_klb
         Nmb =nama_brg
         St  =satuan
         Hp  =harga+(harga*(pajak/100))
         Hj  =harga_2
         Hj2 =harga_2
         Hjx =persen
         Dis1=diskon_1
         Dis2=diskon_rp
         Ipn =id_point
         Ipn2=id_point2
         Jml =1
         Tjml=tjml+jml
         *
         Do cekhj
         Do cariuntung
         If hp=0
            If hjx#0
               Hp=hj/((100+hjx)/100)
            Else
               Hp=hj/1.05
            Endi
         Endi
         Jhg =&brs13
         Tot =tot+&brs13
         Brs3()
         *
         Sele 5
         Repl kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2
         Commit
         *
         Sum &brs1 to tot
         Do nomor with tot,1,56
         Set colo to &warna2
         @3,71  say tran(tjml,'999 /')+tran(no,'999')
         *Set colo to &warna4
         @20,69 say tran(tot,'@e 999,999,999')
      Endi
      Exit
   Endd
   Do pesan with pesany
   Set colo to &warna3
   *
   Sele 5
   Go nr
   Jhg =&brs13
   Nulise=left(nmb,20)+tran(jml,'@e 999 ')+left(st,3)+'=Rp'+tran(jhg,'@e 99,999,999')
   *Kedisple(nulise)
   Set curs off
   Retu 1
Elseif ( (lastkey()>=48 .and. lastkey()<=57) .or. lastkey()=45 ) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 99,999,999')" .or. fds="tran(DISKON,'@e 99.99')")
   Cekvar()
   Tjml=tjml-jml
   Set curs on
   Do pesan with ''
   *
   Set key  5 to atas
   Set key 24 to bawah
   If fds="left(NAMA_BRG,30)"
      Jmlx=chr(lastkey())+spac(6)
      *
      Sele 3
      Set order to 1
      Seek kdbx
      Set key 28 to xhapusi
      @b,40 get jmlx
      If left(kdb,2)='0 '
         @b,53 get hj pict '@e 99,999,999'
      Endi
      Keyboard chr(4)
      Do pesan with 'Masukan Jml.Barang-nya, desimal pake "."   ('+ltri(tran(jml,'@e 9999.99'))+')'
      Read
      Set key 28 to
      *
      If at('.',jmlx)>0
         Jmlx=jmlx
      Else
         If len(rtri(jmlx))>4
            Jmlx=ltri(str(jml))
         Endi
      Endi
      *If val(jmlx)=0
         *Do pesan with ''
         *Do paswod2
         *If lastkey()=27 .or. xOwner=.f.
         *   Set key  5 to
         *   Set key 24 to
         *   Set curs off
         *   Do pesan with pesany
         *   Sele 5
         *   Retu 1
         *Endi
      *Endi
      *
      Jml=iif(lastkey()=27,jml,val(jmlx))
      Do cekhj
      Do cariuntung
      If left(kdb,2)='0 '
         Hp=hj/1.05
      Endi
      Tjml=tjml+jml
   Elseif fds="tran(HARGA_2,'@e 99,999,999')"
      Jmlx=chr(lastkey())+spac(9)
      *
      @b,53 get jmlx
      Keyboard chr(4)
      Do pesan with 'Masukan Harga-nya, desimal pake "."   ('+ltri(tran(hj,'@e 9,999,999.99'))+')'
      Read
      *Do pesan with ''
      *Do paswod2
      *If lastkey()=27 .or. xOwner=.f.
      *   Set key  5 to
      *   Set key 24 to
      *   Set curs off
      *   Do pesan with pesany
      *   Retu 1
      *Endi
      *
      If len(rtri(jmlx))>7
         Jmlx=ltri(str(hj))
      Endi
      Hj =iif(lastkey()=27,hj,val(jmlx))
   Else
      Jmlx=chr(lastkey())+'    '
      @b,64 get jmlx
      Keyboard chr(4)
      Do pesan with 'Masukan Diskon-nya, desimal pake "."   ('+ltri(tran(dis1,'@e 99.99'))+')'
      Read
      If len(rtri(jmlx))>4
         Jmlx=ltri(str(dis1))
      Endi
      Dis1=iif(lastkey()=27,dis1,val(jmlx))
   Endi
   Set key  5 to
   Set key 24 to
   *
   Sele 5
   If lastkey()#27
      Jhg =&brs13
      Repl jumlah with jml,harga with hp,harga_2 with hj,diskon with dis1
      Commit
      *
      Do cekedit  &&CEK APAKAH EDIT NOTA ADA YG DI-UPDATE?!
   Endi
   Sum jumlah,&brs1 to tjml,tot
   Do nomor with tot,1,56
   If diEdit=.f.
      Set colo to &warna2
      @3,71  say tran(tjml,'999 /')+tran(no-1,'999')
   Endi
   Set colo to &warna2
   @20,69 say tran(tot,'@e 999,999,999')
   *
   Do pesan with pesany
   Set colo to &warna3
   Set curs off
   *
   Go nr
   If atas
      Keyboard chr(5)
   Endi
   If bawah
      Keyboard chr(24)
   Endi
   Stor .f. to atas,bawah
   Retu 1
Elseif (lastkey()=-2 .or. lastkey()=-3) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 9,999,999')")
   ***Ecer1/Bakul1
   B  =row()
   Grs=.f.
   If lastkey()=-3
      Grs=.t.
   Endi
   *
   Cekvar()
   *
   Set curs on
   Do pesan with ''
   *
   If kdbx#spac(13)
      Sele 3
      Set order to 1
      Seek kdbx
      If grs
         Do CekHj    &&&DISAMAKAN
      Else
         Do CekhEcr
      Endi
   Endi
   *
   Sele 5
   Repl harga_2 with hj
   Commit
   *
   Do cekedit
   *
   Sum &brs1 to tot
   Do nomor with tot,1,56
   *
   Do pesan with pesany
   Set colo to &warna3
   Set curs off
   Go nr
   Jhg=&brs13
   Retu 1
Elseif (lastkey()=-4 .or. lastkey()=-5) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 9,999,999')")
   B  =row()
   Nr =recno()
   Grs=.f.
   If lastkey()=-4 .or. lastkey()=9
      Grs=.t.
   Endi
   *
   Sele 5
   Go top
   Do whil .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
      Nr  =recno()
      Nrx =rectran
      Kdbx=kode_bar
      Hj  =harga_2
      Hp  =harga
      Stor jumlah to jml,jml2
      Set curs on
      Do pesan with ''
      *
      If kdbx#spac(13)
         Sele 3
         Set order to 1
         Seek kdbx
         If grs
            Do CekHj
         Else
            Do CekHEcr
         Endi
         *
         Sele 5
         Go nr
         Repl harga_2 with hj
         *
         Do cekedit
         *
      Endi
      Skip
   Endd
   Commit
   *
   Sum &brs1 to tot
   Do nomor with tot,1,56
   *
   Do pesan with pesany
   Set curs off
   Go nr
   Jhg=&brs13
   Retu 0
Else
   Set curs off
   Retu 1
Endi
Retu 0

Proc cariuntung
If hp=0
   Hp=hj/1.1
Endi
Retu

Func cekvar
B   =row()
Kl  =col()
Nr  =recno()
Nrx =rectran
Lks =kode_lks
Kdb =kode_brg
Kdbx=kode_bar
Nmb =nama_brg
St  =satuan
Stb =satuan_b
Isi =isine_b
Hp  =harga
Hj  =harga_2
Dis1=diskon
Dis2=diskon_rp
Stor jumlah to jml,jml2
Retu(.t.)

Proc cekedit
If diEdit
   Sele 3
   Rlok()
   Repl stok_k&lks with (stok_k&lks-jml2)+jml
   Ul()
   Commit        
   *
   Sele 4
   Go nrx
   Rlok()
   Repl jumlah with jml,harga with hp,harga_2 with hj,diskon with dis1,diskon_rp with dis2
   If jml#jml2
      Repl keterangan with 'Asal: '+ltri(tran(jml2,'@e 9999 '))+left(st,3)+' /'+left(dtoc(tgl),2)+subs(dtoc(tgl),4,2)+right(dtoc(tgl),2)+'-'+left(time(),5)+'/'+ksa
   Endi
   Ul()
   Commit
   *
   Sele 5
Endi
Retu

Proc cekhj
If xKsr#'A'
   Retu
Endi
If left(kdb,2)#'0 '
   Hj =harga_2
   If (jml>=batas_3 .and. batas_3>0 .and. harga_3>0)
      Hj=harga_3
   Endi
   If (jml>=batas_4 .and. batas_4>0 .and. harga_4>0)
      Hj=harga_4
   Endi
   If (jml>=batas_5 .and. batas_5>0 .and. harga_5>0)
      Hj=harga_5
   Endi
   If hj=0
      Brs3()
      @b,53 get hj pict '@e 9,999,999'
      Read
   Endi
Endi
Retu

Proc CekHEcr
If xKsr#'A'
   Retu
Endi
Hj=iif(harga_21>0,harga_21,harga_2)
If (jml>=batas_3 .and. batas_3>0 .and. (harga_3>0 .or. harga_32>0))
   Hj=iif(harga_32>0,harga_32,harga_3)
Endi
If (jml>=batas_4 .and. batas_4>0 .and. (harga_4>0 .or. harga_42>0))
   Hj=iif(harga_42>0,harga_42,harga_4)
Endi
If (jml>=batas_5 .and. batas_5>0 .and. (harga_5>0 .or. harga_52>0))
   Hj=iif(harga_52>0,harga_52,harga_5)
Endi
Retu

Proc hitpoin
If (ipn=' ' .and. ipn2=' ') .and. rpoin>0
   Totpn =totpn  + (jml * (hj - (hj*(dis1/100))))
   Pnskr =(totpn-mod(totpn,rpoin))/rpoin
Endi
If (ipn=' ' .and. ipn2#' ') .and. rpoin2>0
   Totpn2=totpn2 + (jml * (hj - (hj*(dis1/100))))
   Pnskr2=(totpn2-mod(totpn2,rpoin2))/rpoin2
Endi
Retu

***CETAK STROK
Proc kopnota
Do cp
If lastkey()=27
   Retu
Endi
Set printer to "STRUK.TMP"
Set devi to prin
Set marg to jt
If iprn='D'
   @prow()+1,0 say chr(27)+chr(33)+chr(33)+chr(27)+'a'+'1'+aidi1+chr(27)+chr(33)+chr(5)
   @prow()+1,0 say chr(27)+'a'+'1'+ltri(rtri(alm1))
   If alm2#spac(40)
      @prow()+1,0 say ltri(rtri(alm2))
   Endi
   @prow()+1,0 say chr(27)+'a'+'0'+chr(27)+chr(33)+chr(4)+repl('-',33)
   If idcut='Y' .and. tot#0 .or. xAda2#0
      @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      @prow(),0 say chr(27)+chr(33)+chr(4)+' '
   Elseif tot=0
      @prow(),0 say ' '
   Endi
Else
   @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(32)+'TOKO SEMBAKO'
   @prow()+1,0 say aidi1+chr(27)+chr(33)+chr(4)
   @prow()+1,0 say ltri(rtri(alm1))
   If alm2#spac(40)
      @prow()+1,0 say ltri(rtri(alm2))
   Endi
   @prow()+1,0 say chr(27)+'a'+'0'+repl('-',32)
   @prow(),0   say ' '
Endi
Set devi to scre
Set printer to
KirimKePrinter("STRUK.TMP")
Retu

Proc ctknota
Do cp
If lastkey()=27
   Retu
Endi
***JIKA THERMAL
If iprn='T'
   Do ctknotath
   Retu
Endi
***
Set printer to "STRUK.TMP"
Set devi to prin
If idrw='Y' .and. byr#0 .and. ulang=.f.
   @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   @prow(),0 say chr(27)+chr(33)+chr(4)
Endi
If ulang
   @prow()+1,0 say chr(27)+chr(33)+chr(4)+'* REPRINT *'
Endi
Stor 0 to dis2,totr,totpn,totpn2
Do whil .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
   If jumlah#0 .and. uppe(left(nama_brg,6))#'DISKON'
      Jml =jumlah
      Hj  =harga_2
      Dis1=diskon
      Jhg =&brs13
      Ipn =id_point
      Ipn2=id_point2
      xAda2=xada2+1
      *
      Do hitpoin
      *
      Jml  =tran(jumlah,'@e 99,999')
      If (jumlah>0 .and. jumlah<1) .or. (jumlah<0 .and. jumlah>-1)
         Jml=tran(jumlah,'@e 999.99')
      Elseif mod(jumlah,int(jumlah))>0
         Jml=tran(jumlah,'@e 999.99')
      Endi
      @prow()+1,0 say left(nama_brg,30)
      @prow()+1,2 say ltri(jml)+' '+rtri(satuan)+iif(left(satuan,1)#' ',' X ',' ')+ltri(tran(harga_2,'@e 99,999,999'))
      If diskon#0
         @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
      Endi
      @prow(),23  say tran(jhg,'@e 99,999,999')
      If jumlah_rtr#0
         Jml3=tran(jumlah_rtr,'@e 99,999')
         If jumlah_rtr#0
            If (jumlah_rtr>0 .and. jumlah_rtr<1) .or. (jumlah_rtr<0 .and. jumlah_rtr>-1)
               Jml3=tran(jumlah_rtr,'@e 999.99')
            Elseif mod(jumlah_rtr,int(jumlah_rtr))>0
               Jml3=tran(jumlah_rtr,'@e 999.99')
            Endi
         Endi
         @prow()+1,0 say ' RETUR='+jml3+'x @'+tran(harga_2,'@e 9,999,999')
         @prow(),28 say tran(0-(jumlah_rtr*harga_2),'@e   99,999,999')
         Totr=totr+(jumlah_rtr*harga_2)
      Endi
      No=no+1
      If no>50
         @prow(),0 say ' '
         Set devi to scre
         *
         Do pesan with 'ENTER=Halaman_Selanjutnya'
         Inkey(5)
         No=1
         Do pesan with 'Tunggu, sedang diCetak'
         Set devi to prin
         @prow(),0 say chr(27)+chr(33)+chr(4)
      Endi
   Endi
   Skip
Endd
If dis2>0
   @prow()+2,0 say 'JUMLAH DISKON ITEM = ( Rp'+tran(dis2,'@e 99,999,999')+' )'
Endi
If blt#0
   @prow()+1,10 say 'PEMBULATAN  '+tran(blt,'@e 999,999,999')
Endi
*
@prow()+1,0 say repl('-',33)
If totr#0
   @prow()+1,18 say 'SUB TOTAL  '+tran(tot,'@e 99,999,999')
   @prow()+1,18 say 'RETUR      '+tran(0-totr,'@e 99,999,999')
Endi
@prow()+1,0 say ltri(str(tjml))+' itm'
@prow(),10  say 'JML.BELANJA '+tran(tot-totr, '@e 999,999,999')
If tot3#0
   @prow()+1,10 say 'KURANGAN    '+tran(tot3,'@e 999,999,999')
   @prow()+1,10 say '            -----------'
   @prow()+1,10 say 'TOTAL RP    '+tran(tot+tot3,'@e 999,999,999')
Endi
If dis31>0
   @prow()+1,7 say 'DISKON '+tran(dis3,'@e 99.99 % ')+tran(dis31,'@e 999,999,999')
Endi
If dns>0
   @prow()+1,10  say 'UNTUK DONASI'+tran(dns,'@e 999,999,999')
Endi
@prow()+1,10  say 'B A Y A R   '+tran(byr,'@e 999,999,999')
If kbl#0
   @prow()+1,10 say iif(Kbl>0,'KEMBALI     ','K U R A N G ')+tran(abs(kbl),'@e 999,999,999')
Endi
Kata=hr+', '+dtoc(tgl)+' '+tran(time(),'XX:XX')+' '+fkt+'/'+nsls
@prow()+2,0 say chr(27)+chr(33)+chr(5)+left(kata,40)
*
If plg#spac(10) &&.and. ( rpoin>0 .or. rpoin2>0 ) .and. idp=' '
   @prow()+1,0 say repl('-',40)
   @prow()+1,0 say 'Nama: '+rtri(nsp)+' ('+rtri(plg)+')'
   @prow()+1,0 say '    : '+jab
   If iPoin#'0'
      @prow()+1,0 say 'Poin: '+ltri(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+ltri(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
   Endi
Endi
*
Sele 1
Uselan('DATA.SET','','','','','')
If promo1#spac(40)
   @prow()+1,0 say repl('-',40)
   @prow()+1,0 say rtri(promo1)
Endi
If promo2#spac(40)
   @prow()+1,0 say rtri(promo2)
Endi
If promo3#spac(40)
   @prow()+1,0 say rtri(promo3)
Endi
If promo4#spac(40)
   @prow()+1,0 say rtri(promo4)
Endi
Close
*
@prow()+jn,1 say ''
Ulang=.f.
Set devi to scre
Set printer to
KirimKePrinter("STRUK.TMP")
Retu

Proc ctknotath
Set printer to "STRUK.TMP"
Set devi to prin
If lr='5'
   If idCut='Y'
      Set marg to jt
      @prow()+1,0 say chr(27)+chr(33)+chr(4)+chr(27)+'a'+'1'+'TOKO SEMBAKO DAN SNACK'
      @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(32)+ltri(aidi1)+chr(27)+chr(33)+chr(4)
      @prow()+1,0 say ltri(rtri(alm1))
      If alm2#spac(40)
         @prow()+1,0 say ltri(rtri(alm2))
      Endi
      @prow()+1,0 say chr(27)+'a'+'0'+repl('-',32)
   Endi
   *
   If idrw='Y' .and. byr#0 .and. ulang=.f.
      @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   Endi
   If ulang
      @prow()+1,0 say '* REPRINT *'
   Endi
   Stor 0 to dis2,totr,totpn,totpn2
   Adabns=.f.
   Do whil .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
      If jumlah#0 .and. uppe(left(nama_brg,6))#'DISKON'
         Jml =jumlah
         Hj  =harga_2
         Dis1=diskon
         Ipn =id_point
         Ipn2=id_point2
         Jhg =&brs13
         xAda2=xada2+1
         *
         Do hitpoin
         *
         Jml=tran(jumlah,'@e 99,999')
         If (jumlah>0 .and. jumlah<1) .or. (jumlah<0 .and. jumlah>-1)
            Jml=tran(jumlah,'@e 999.99')
         Elseif mod(jumlah,int(jumlah))>0
            Jml=tran(jumlah,'@e 999.99')
         Endi
         @prow()+1,0 say left(nama_brg,30)
         @prow()+1,4 say ltri(jml)+' '+rtri(satuan)+iif(left(satuan,1)#' ',' X ',' ')+ltri(tran(harga_2,'@e 9,999,999'))
         @prow(),22  say tran(jhg,'@e 99,999,999')
         If diskon#0
            @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
         Endi
      Endi
      Skip
   Endd
   If blt#0
      @prow()+1,10 say 'PEMBULATAN '+tran(blt,'@e 999,999,999')
   Endi
   @prow()+1,0 say repl('-',32)
   If totr#0
      @prow()+1,10 say 'SUB TOTAL  '+tran(tot,'@e 999,999,999')
      @prow()+1,10 say 'RETUR      '+tran(0-totr,'@e 999,999,999')
   Endi
   @prow()+1,0 say chr(27)+chr(33)+chr(32)+'TOTAL'+tran(tot-totr, '@e 999,999,999')+chr(27)+chr(33)+chr(4)
   If dis2>0
      @prow()+1,0  say 'DISKON ITEM Rp.'+tran(dis2,'@e 9,999,999')
   Endi
   If dis31>0
      @prow()+1,12 say 'DISKON   '+tran(dis31,'@e 999,999,999')
   Endi
   If dns>0
      @prow()+1,12 say 'DONASI   '+tran(dns,'@e 999,999,999')
   Endi
   @prow()+1,0 say tran(no,'9999 itm. ')+'  BAYAR    '+tran(byr,'@e 999,999,999')
   If kbl#0
      @prow()+1,12 say iif(kbl>0,'KEMBALI  ','KURANG    ')+tran(abs(kbl),'@e 999,999,999')
   Endi
   @prow()+1,0  say 'Kasir: '+rtri(nsls)+' ,  No: '+fkt
   @prow()+1,0  say hr+', '+dtoc(tgl)+' // '+tran(time(),'XX:XX')
   If plg#spac(10)
      @prow()+1,0 say repl('-',32)
      @prow()+1,0 say 'Nama: '+rtri(nsp)+' ('+rtri(plg)+')'
      @prow()+1,0 say '    : '+rtri(jab)
      If iPoin#'0'
         @prow()+1,0 say 'Poin: '+ltri(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+ltri(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
      Endi
   Endi   
   *
   Sele 1
   Uselan('DATA.SET','','','','','')
   If promo1#spac(40)
      @prow()+1,0 say repl('-',32)
      @prow()+1,0 say promo1
   Endi
   If promo2#spac(40)
      @prow()+1,0 say promo2
   Endi
   If promo3#spac(40)
      @prow()+1,0 say promo3
   Endi
   If promo4#spac(40)
      @prow()+1,0 say promo4
   Endi
   Close
Else
   * ============================================================
   * 80 MM THERMAL TEMPLATE - 45 CHARACTERS
   * Layout mengikuti nota thermal 80mm: header tengah,
   * nama barang di baris sendiri, qty x harga di kiri,
   * subtotal di kanan, lalu TOTAL/BAYAR dan pesan kaki.
   * ============================================================
   If idCut='Y'
      Set marg to jt
      * Header toko
      @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(4)+ltri('TOKO SEMBAKO DAN SNACK')
      @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(32)+ltri(aidi1)+chr(27)+chr(33)+chr(4)
      If aidi2#spac(40)
         @prow()+1,0 say chr(27)+'a'+'1'+ltri(rtri(aidi2))
      Endi
      @prow()+1,0 say chr(27)+'a'+'1'+ltri(rtri(alm1))
      If alm2#spac(40)
         @prow()+1,0 say chr(27)+'a'+'1'+ltri(rtri(alm2))
      Endi
      If .not. empty(alm3)
         @prow()+1,0 say chr(27)+'a'+'1'+ltri(rtri(alm3))
      Endi
      @prow()+1,0 say chr(27)+'a'+'0'+repl('-',45)
   Endi
   * Cash drawer
   If idrw='Y' .and. byr#0 .and. ulang=.f.
      @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   Endi
   If ulang
      @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(4)+'* REPRINT *'
   Endi
   Stor 0 to dis2,totr,totpn,totpn2
   * -------------------- ITEM --------------------
   Do whil .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
      If jumlah#0 .and. uppe(left(nama_brg,6))#'DISKON'
         Jml =jumlah
         Hj  =harga_2
         Dis1=diskon
         Ipn =id_point
         Ipn2=id_point2
         Jhg =&brs13
         xAda2=xAda2+1
         Do hitpoin
         * Format qty
         Jml=tran(jumlah,'@e 99,999')
         If (jumlah>0 .and. jumlah<1) .or. (jumlah<0 .and. jumlah>-1)
            Jml=tran(jumlah,'@e 999.99')
         Elseif mod(jumlah,int(jumlah))>0
            Jml=tran(jumlah,'@e 999.99')
         Endi
         * Nama barang - satu baris sendiri
         @prow()+1,0 say left(rtri(nama_brg),45)
         * Qty x satuan x harga, subtotal di kanan
         @prow()+1,0 say ' '+ltri(Jml)+' '+rtri(satuan)+iif(left(satuan,1)#' ',' X ',' ')+ltri(tran(harga_2,'@e 9,999,999'))
         @prow(),34 say tran(jhg,'@e 999,999,999')
         If diskon#0
            @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
         Endi
         If jumlah_rtr#0
            Jml3=tran(jumlah_rtr,'@e 99,999')
            If (jumlah_rtr>0 .and. jumlah_rtr<1) .or. (jumlah_rtr<0 .and. jumlah_rtr>-1)
               Jml3=tran(jumlah_rtr,'@e 999.99')
            Elseif mod(jumlah_rtr,int(jumlah_rtr))>0
               Jml3=tran(jumlah_rtr,'@e 999.99')
            Endi
            @prow()+1,0 say ' RETUR='+jml3+'x @'+tran(harga_2,'@e 9,999,999')
            @prow(),34 say tran(0-(jumlah_rtr*harga_2),'@e 999,999,999')
            Totr=totr+(jumlah_rtr*harga_2)
         Endi
         No=no+1
         If no>50
            @prow(),0 say ' '
            Set devi to scre
            Do pesan with 'ENTER=Halaman_Selanjutnya'
            Inkey(5)
            No=1
            Do pesan with 'Tunggu, sedang diCetak'
            Set devi to prin
            @prow(),0 say chr(27)+chr(33)+chr(4)
         Endi
      Endi
      Skip
   Endd
   * -------------------- TOTAL --------------------
   If dis2>0
      @prow()+1,0 say 'DISKON ITEM Rp.'+tran(dis2,'@e 99,999,999')
   Endi
   If blt#0
      @prow()+1,0 say 'PEMBULATAN '+tran(blt,'@e 999,999,999')
   Endi
   @prow()+1,0 say repl('-',45)
   If totr#0
      @prow()+1,20 say 'SUB TOTAL  '+tran(tot,'@e 999,999,999')
      @prow()+1,20 say 'RETUR      '+tran(0-totr,'@e 999,999,999')
   Endi
   @prow()+1,0 say ltri(str(no))+' itm.'
   @prow(),22 say 'TOTAL  '+tran(tot-totr,'@e 999,999,999')
   If tot3#0
      @prow()+1,20 say 'KURANGAN   '+tran(tot3,'@e 999,999,999')
      @prow()+1,20 say 'TOTAL RP   '+tran(tot+tot3,'@e 999,999,999')
   Endi
   If dis31>0
      @prow()+1,20 say 'DISKON '+tran(dis3,'@e 99.99 % ')+tran(dis31,'@e 999,999,999')
   Endi
   If dns>0
      @prow()+1,20 say 'DONASI     '+tran(dns,'@e 999,999,999')
   Endi
   @prow()+1,20 say 'BAYAR      '+tran(byr,'@e 999,999,999')
   If kbl#0
      @prow()+1,20 say iif(kbl>0,'KEMBALI    ','KURANG     ')+tran(abs(kbl),'@e 999,999,999')
   Endi
   * -------------------- TRANSAKSI --------------------
   Kata=hr+', '+dtoc(tgl)+' '+tran(time(),'XX:XX')+' '+fkt+'/'+nsls
   @prow()+1,0 say chr(27)+chr(33)+chr(4)+left(Kata,45)
   * -------------------- PELANGGAN/POIN --------------------
   If plg#spac(10)
      @prow()+1,0 say repl('-',45)
      @prow()+1,0 say 'Nama: '+rtri(nsp)+' ('+rtri(plg)+')'
      @prow()+1,0 say '    : '+rtri(jab)
      If iPoin#'0'
         @prow()+1,0 say 'Poin: '+ltri(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+ltri(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
      Endi
   Endi
   * -------------------- FOOTER --------------------
   Sele 1
   Uselan('DATA.SET','','','','','')
   If promo1#spac(40)
      @prow()+1,0 say repl('-',45)
      @prow()+1,0 say left(rtri(promo1),45)
   Endi
   If promo2#spac(40)
      @prow()+1,0 say left(rtri(promo2),45)
   Endi
   If promo3#spac(40)
      @prow()+1,0 say left(rtri(promo3),45)
   Endi
   If promo4#spac(40)
      @prow()+1,0 say left(rtri(promo4),45)
   Endi
   * Default footer mengikuti contoh nota 80mm bila promo kosong.
   If promo1=spac(40) .and. promo2=spac(40) .and. promo3=spac(40) .and. promo4=spac(40)
      @prow()+1,0 say repl('-',45)
      @prow()+1,0 say 'KOMPLAIN HARUS MEMBAWA NOTA.'
      @prow()+1,0 say 'MAKSIMAL DALAM WAKTU 1 HARI'
      @prow()+1,0 say 'TERIMA KASIH'
   Endi
Endi
@prow()+jn+1,1 say ''
If idcut='Y' .and. tot#0
   @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
   @prow(),0 say chr(27)+chr(33)+chr(4)+''
Endi
Set devi to scre
Set printer to
KirimKePrinter("STRUK.TMP")
Ulang=.f.
Retu

Proc fmtbaris
Jmlx=jumlah
St  =satuan
Nmb =rtri(nama_brg)+spac(38-len(rtri(nama_brg)))
If isine_b>1 .and. jumlah>=isine_b
   *Jmlx=jumlah/isine_b
   *St  =satuan_b
   Nmb =rtri(nama_brg)+' @'+ltri(str(isine_b))+' '+rtri(satuan)
   Nmb =nmb+spac(38-len(nmb))
Endi
Nmb=left(nmb,38)
Jml=tran(jmlx,'@e 99,999')
If jmlx#0
   If (jmlx>0 .and. jmlx<1) .or. (jmlx<0 .and. jmlx>-1)
      Jml=tran(jmlx,'@e 999.99')
   Else  &&&if mod(jmlx,int(jmlx))>0
      Jml=tran(jmlx,'@e 99,999')
   Endi
Endi
Retu

Proc atas
Atas=.t.
Keyboard chr(13)
Retu

Proc bawah
Bawah=.t.
Keyboard chr(13)
Retu

* EOF CR

Func KirimKePrinter(cFile)
Local cPrinter, nRes
cPrinter := "POS-80C"
nRes := win_PrintFileRaw(cPrinter, cFile, "Nota Kasir")
If nRes <= 0
   cPrinter := win_PrinterGetDefault()
   If !empty(cPrinter)
      win_PrintFileRaw(cPrinter, cFile, "Nota Kasir")
   Endif
Endif
Return .T.