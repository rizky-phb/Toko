* DAFTAR HUTANG
*---------------
Proc lhut1
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Go top
Tglso=tanggal_2
Sele 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
Sele 3
*
Sele 4
*
Sele 5
*
Sele 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')
Sele 7
Uselan('RETURBN.DTA','RETURBN2.NDX','','','','')
*
Tgl =tglso
Tgl2=tglx
Ada =.f.
Ksp =spac(5)
Do whil .t.
   Sele 1
   Set cent on
   Set key 28 to dafspl
   @1,0 clea to 23,79
   @2,24 say 'Suppl.:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai     00000=Global'
   If ada
      Ada=.f.
      Keyboard chr(13)+chr(13)
   Endi
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   Endi
   If ksp=spac(5)
      Do dafspl
   Endi
   If lastkey()=27
      Loop
   Endi
   If ksp='00000'
      @1,0 clea to 23,79
      Do pesan with 'Tunggu, sedang dihitung'
      Set alte on
      Set alte to HUTANG.RTF
      ?' DAFTAR HUTANG'
      ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?chr(218)+repl(gl,4)+chr(194)+repl(gl,5)+repl(chr(194)+repl(gl,25),2)+chr(194)+repl(gl,15)+chr(191)
      ?tl+' No.'+tl+'Kode '+tl+'Nama Supplier '+spac(11)+tl+'Alamat'+spac(19)+tl+'     Jumlah Rp.'+tl
      ?chr(195)+repl(gl,4)+chr(197)+repl(gl,5)+repl(chr(197)+repl(gl,25),2)+chr(197)+repl(gl,15)+chr(180)
      Tot2=0
      No  =1
      Go top
      Do whil .not. eof()
         Ksp  =kode_spl
         Nsp  =nama_spl
         Algn =almt_spl
         Stor 0 to jml,jml2,jml3
         *
         Sele 2
         Set order to 2
         Seek ksp
         Do whil .not. eof() .and. kode_spl=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Jml =jml +rp_total &&((rp_total-rp_diskon)+rp_pajak)
               Jml2=jml2+rp_bayar
            Endi
            Skip
         Endd
         Tot=jml-(jml2+jml3)
         *
         If tot#0
            ?tl+tran(no,'999.')+tl+ksp+tl+left(nsp,25)+tl+left(algn,25)+tl+tran(tot,'@e 999,999,999,999')+tl
            Tot2=tot2+tot
            No  =no+1
         Endi
         Sele 1
         Skip
      Endd
      ?chr(192)+repl(gl,78)+chr(217)
      ?spac(56)+'Total Rp'+tran(tot2,'@e 999,999,999,999')
      Set alte off
      Set alte to
      @24,0
      Memoedit(Memoread('HUTANG.RTF'),1,0,21,240,.t.,'',240)
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
      Set marg to 15
      ?chr(27)+chr(33)+chr(9)
      Type HUTANG.RTF to prin
      Set prin off
      Set devi to scre
   Else
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada kode supplier '+rtri(ksp)
         Loop
      Endi
      *
      Do bukuh
   Endi
Endd
Set cent on
Set curs off
Clos all
Retu

Proc koplhut1
Set prin on
Set devi to prin
Set marg to 10
?chr(27)+chr(33)+chr(9)
?' DAFTAR HUTANG'
?' PER TANGGAL: '+dtoc(tgl)+spac(50)+'Hal:'+tran(hal,'999')
?repl('=',80)
?'  No.  Nama Supplier                    Alamat                        Jumlah Rp.'
?repl('-',80)
N=1
Retu

Proc ebyr
@1,0 clea to 23,79
@1,0 to 22,77
Do pesan with 'ESC=Selesai   F8=Hapus'
Set cent on
Declare field[7],bts[7],frm[7],kop[7]
Field[1]='TANGGAL'
Field[2]='TANGGAL_3'
Field[3]='FAKTUR'
If klp .and. spl=.f.
   Field[4]='NAMA_LGN'
   Kop[4]='Nama Pelanggan'
Elseif spl .and. klp=.f.
   Field[4]='NAMA_SPL'
   Kop[4]='Nama Supplier'
Else
   Field[4]='NAMA_TKG'
   Kop[4]='Nama Tukang'
Endi
Field[5]='JUMLAH_BYR'
Field[6]='JENIS_BYR'
Kop[1]='Tanggal'
Kop[2]='Jt.Tempo'
Kop[3]='Nomor'
Kop[5]='Jumlah Rp.'
Kop[6]='Jenis'
Frm[1]='99-99-99'
Frm[2]='99-99-99'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@e 999,999,999'
Frm[6]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
Bts[6]=tl
Dbedit(2,1,21,76,field,'fbyr',frm,kop,gl,bts)
Retu

Function fbyr
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-7
      If klp .and. spl=.f.
         Fkt=left(faktur,5)
         Ksp=kode_lgn
         Syarat='kode_lgn=ksp'
      Elseif spl .and. klp=.f.
         Fkt=faktur
         Ksp=kode_spl
         Syarat='kode_spl=ksp'
      Else
         Ksp=kode_tkg
         Syarat='kode_tkg=ksp'
      Endi
      Tgl3=tanggal
      Jml =jumlah_byr
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

            Sele 2
            Set order to 2
            Seek ksp
            Do whil .not. eof() .and. &syarat
               If tanggal_4=tgl3 .and. faktur=left(fkt,5)
                  Rlok()
                  If (rp_bayar-jml)=0
                     Repl tanggal_4 with ctod('  -  -    ')
                  Endi
                  Repl rp_bayar with (rp_bayar-jml)
                  Ul()
                  Commit
               Endi
               Skip
            Endd

            Sele 6
         Endi
      Endi
      Do pesan with 'ESC=Selesai   F8=Hapus'
      Set curs off
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

* DAFTAR PEMBAYARAN HUT.
*-----------------------
Proc lbyrh
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
Sele 3
*
Sele 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')
*
Stor tglx to tgl,tgl2
Do whil .t.
   Ksp=spac(5)
   Sele 1
   Set cent on
   Set key 28 to dafspl
   Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,27 say 'Suppl:'get ksp pict '@!'
   @3,27 say 'Tgl..:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Supplier'
   Read
   Set key 28 to
   If lastkey()=27
      Exit
   Endi
   @4,24 to 4,59
   Stor ' ' to jlap
   @5,24 say '1=Tunai  2=BG/Cek  3=Semua  =>'get jlap pict '@!'
   Read
   @24,0
   If lastkey()=27
      Loop
   Endi
   Do case
      Case jlap='1'
         Ket="subs(jenis_byr,1,5)='TUNAI'"
      Case jlap='2'
         Ket="subs(jenis_byr,1,3)='BG-'"
      Other
         Ket="jenis_byr#spac(10)"
   Endc
   Set cent off
   Stor 0 to tot,tot2,hj
   If ksp=spac(5)
      @1,0 clea to 23,79
      @1,0 say repl(gl,8)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)+chr(194)+repl(gl,21)+chr(194)+repl(gl,12)+chr(194)+repl(gl,14)
      @2,0 say 'Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Nama Supplier'+spac(7)+tl+'  Jumlah Rp.'+tl+' Keterangan'
      @3,0 say repl(gl,8)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(193)+repl(gl,21)+chr(193)+repl(gl,12)+chr(193)+repl(gl,14)
      B=4
      Rtot2=0
      Sele 6
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      Do whil .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            @b,0 say dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_spl+tran(jumlah_byr,'@e 9,999,999,999 ')+jenis_byr
            Rtot2=rtot2+jumlah_byr
            B=b+1
            If b>20
               @b,0 say repl(gl,80)
               Do pesan with 'ESC=Selesai   SPASI=Lanjut'
               Wait''
               If lastkey()=27
                  Exit
               Endi
               B=4
            Endi
         Endi
         Skip
      Endd
      @b,0   say repl(gl,80)
      @b+1,0 clea to 23,79
      @b+1,42 say 'Total Rp'+tran(rtot2,'@e  99,999,999,999')
      Do pesan3 with 'ESC=Selesai   F8=Hapus   F10=Cetak'
      If lastkey()=-7
         Klp=.f.
         Spl=.t.
         Go nr
         Do ebyr
      Endi
      If nlp=0
         Loop
      Endi
      Do cp
      If cc=0
         Loop
      Endi
      Stor 1 to no,hal
      Stor 0 to tot,tot2
      Do koplbyrh1
      Go nr
      Do whil .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            ?'  '+dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+'  '+nama_spl+tran(jumlah_byr,'@e 9,999,999,999 ')+jenis_byr
            Tot =tot +jumlah_byr
            Tot2=tot2+jumlah_byr
            No=no+1
            If no>60
               ?repl('-',89)
               ?' Sub Total   Rp'+spac(37)+tran(tot ,'@e 99,999,999,999')
               ?repl('-',89)
               ?''
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
               Hal=hal+1
               Tot=0
               Do koplbyrh1
            Endi
         Endi
         Skip
      Endd
      If cc=0
         Loop
      Endi
      ?repl('-',89)
      ?' Sub Total   Rp'+spac(37)+tran(tot ,'@e 99,999,999,999')
      ?repl('-',89)
      ?' T o t a l   Rp'+spac(37)+tran(tot2,'@e 99,999,999,999')
      ?repl('=',89)
      ?''
      Set prin off
      Set devi to scre
   Else
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada kode supplier '+rtri(ksp)
         Loop
      Endi
      Nsp =nama_spl
      Algn=almt_spl

      @1,0 clea to 23,79
      @1,8 say 'Suppl: '+rtri(nsp)+' ('+rtri(ksp)+')'
      @2,7 say chr(218)+repl(gl,9)+chr(194)+repl(gl,9)+chr(194)+repl(gl,11)+chr(194)+repl(gl,11)+chr(194)+repl(gl,21)+chr(191)
      @3,7 say tl+' Tanggal '+tl+' Jt.Tempo'+tl+' Nomor     '+tl+' Jumlah Rp.'+tl+' Keterangan'+spac(10)+tl
      @4,7 say chr(195)+repl(gl,9)+chr(193)+repl(gl,9)+chr(193)+repl(gl,11)+chr(193)+repl(gl,11)+chr(193)+repl(gl,21)+chr(180)
      B=5
      Rtot2=0
      Sele 6
      Set filter to kode_spl=ksp
      Loca for kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Nr=recno()
      Do whil .not. eof()
         If kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            @b,7 say tl+' '+dtoc(tanggal)+'  '+dtoc(tanggal_3)+'  '+faktur+tran(jumlah_byr,'@e  999,999,999  ')+jenis_byr+tl
            Rtot2=rtot2+jumlah_byr
            B=b+1
            If b>20
               @b,7 say chr(195)+repl(gl,65)+chr(180)
               Do pesan2 with 'ESC=Selesai   SPASI=Lanjut'
               If nlp=0
                  Exit
               Endi
               B=4
            Endi
         Endi
         Skip
      Endd
      @b,7 say chr(192)+repl(gl,65)+chr(217)
      @b+1,0 clea to 23,79
      @b+1,28 say 'Total Rp'+tran(rtot2,'@e  99,999,999,999')
      Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
      Bnk=spac(5)
      Fkt=spac(10)
      Jml=0
      If lastkey()=-7
         Klp=.f.
         Spl=.t.
         Go nr
         Do ebyr
      Endi
      If nlp=0
         Loop
      Endi
      Do cp
      If cc=0
         Loop
      Endi
      Stor 1 to no,hal
      Stor 0 to tot,tot2
      Do koplbyrh2
      Go nr
      Do whil .not. eof()
         If kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2) .and. &ket
            ?'| '+dtoc(tanggal)+'| '+dtoc(tanggal_3)+'| '+faktur+'|'+tran(jumlah_byr,'@e 999,999,999| ')+jenis_byr+' |'
            Tot =tot +jumlah_byr
            Tot2=tot2+jumlah_byr
            No=no+1
            If no>55
               ?'|'+repl('-',66)+'|'
               ?''
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
               Hal=hal+1
               Do koplbyrh2
            Endi
         Endi
         Skip
      Endd
      If cc=0
         Loop
      Endi
      ?'*'+repl('=',66)+'*'
      ?' Total Rp '+tran(tot2,'@e 99,999,999,999')
      ?''
      Set prin off
      Set devi to scre
   Endi
Endd
Clos all
Set curs off
Set cent on
Retu

Proc koplbyrh1
Set prin on
Set devi to prin
Set marg to 2
?chr(27)+chr(33)+chr(9)
?' DAFTAR PEMBAYARAN HUTANG'
?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(50)+'Hal:'+tran(hal,'999')
?repl('=',89)
?'  Tanggal   Jt.Tempo  No.BG/Fakt  Nama Supplier '+spac(6)+'  Jumlah Rp.  Keterangan'
?repl('-',89)
No=1
Retu

Proc koplbyrh2
Set prin on
Set devi to prin
Set marg to 10
?chr(27)+chr(33)+chr(9)
?'  DAFTAR PEMBAYARAN HUTANG'
?'  SUPPLIER.: '+rtri(nsp)+' ('+rtri(ksp)+')'
?'             '+algn
?'  TANGGAL..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(26)+'Hal:'+tran(hal,'999')
?'*'+repl('=',66)+'*'
?'| Tanggal | Jt.Tempo| No.BG/Fakt| Jumlah Rp.|  Keterangan          |'
?'|'+repl('-',66)+'|'
No=1
Retu

* LAPORAN FAKTUR PEMBELIAN
*--------------------------
Proc fkth
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
Sele 3
*
Sele 4
Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
Sele 5
*
Sele 6
Uselan('BAYARH.DTA','BAYARH1.NDX','BAYARH2.NDX','','','')

Nklb='-'
Stor ' ' to hri,bln,thn
Stor tglx to tgl,tgl2,tgl4
Ada=.f.
Do whil .t.
   Ksp=spac(5)
   *
   Sele 1
   Set filter to
   Set cent on
   Set key 28 to dafspl
   Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,24 say 'Supl..:'get ksp pict '@!'
   Do pesan with 'F1=Daftar Supplier   ( Kode Supl. HARUS diisi Jika Mau Melakukan Pembayaran )'
   Read
   Set key 28 to
   @24,0
   If lastkey()=27
      Exit
   Endi
   Nsp='-'
   If ksp#spac(5)
      Set order to 1
      Seek ksp
      If .not. foun()
         Set order to 2
         Seek rtri(ksp)
         Do dafspl
      Endi
      If lastkey()=27
         Loop
      Endi
      Nsp=nama_spl
   Endi
   *
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'Masukan Tanggal'
   Read
   @24,0
   If lastkey()=27
      Loop
   Endi
   Stor 0 to no,tot,tot2,totb
   @22,0 to 22,79
   @1,0 say repl(repl(gl,8)+chr(194),2)+repl(gl,8)+chr(194)+repl(gl,18)+repl(chr(194)+repl(gl,12),2)+chr(194)+repl(gl,8)
   @2,0 say 'Tanggal '+tl+'Jt.Tempo'+tl+'Faktur  '+tl+'Nama Supplier     '+tl+'    Rp.Total'+tl+'    Rp.Bayar'+tl+'Tgl.Byr.'
   @3,0 say repl(repl(gl,8)+chr(197),2)+repl(gl,8)+chr(197)+repl(gl,18)+repl(chr(197)+repl(gl,12),2)+chr(197)+repl(gl,8)
   Sele 2
   If ksp=spac(5)
      Set order to 1
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek dtos(tgl)+ksp
      If .not. foun()
         Seek dtos(tgl)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp
            If .not. foun()
               Loca for (tanggal>=tgl .and. tanggal<=tgl2)
               If .not. foun()
                  Do pesan2 with 'Tidak ada Data Faktur Pembelian'
                  Loop
               Endi
            Endi
         Endi
      Endi
   Elseif ksp#spac(5)
      Ada=.t.
      Set order to 2
      Set filter to ( (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_spl=ksp )
      Seek ksp+dtos(tgl)
      If .not. foun()
         Loca for kode_spl=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. foun()
            Loca for (tanggal>=tgl .and. tanggal<=tgl2)
            If .not. foun()
               Do pesan2 with 'Tidak ada Data Faktur Pembelian'
               Loop
            Endi
         Endi
      Endi
   Endi
   Nr=recno()
   Pesanx='SPASI=Pilih   B/L=Bayar   F1=Buku Hutang   F8=Hapus   F9=Jt.Tempo   F10=Cetak'
   Do pesan with pesanx
   Set cent off
   Declare field[8],bts[8],frm[8]
   Field[1]='TANGGAL'
   Field[2]='TANGGAL_3'
   Field[3]='left(FAKTUR,8)'
   Field[4]='left(NAMA_SPL,18)'
   Field[5]='(RP_TOTAL-RP_DISKON)+RP_PAJAK'
   Field[6]='RP_BAYAR'
   Field[7]='TANGGAL_4'
   Field[8]='RP_RETUR'
   Frm[1]='99-99-99'
   Frm[2]='99-99-99'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@e 9999,999,999'
   Frm[6]='@e 9999,999,999'
   Frm[7]='99-99-999'
   Frm[8]='@e 9999,999,999'
   Bts[1]=tl
   Bts[2]=tl
   Bts[3]=tl
   Bts[4]=tl
   Bts[5]=tl
   Bts[6]=tl
   Bts[7]=tl
   Bts[8]=tl
   Dbedit(4,0,21,79,field,'ffkth',frm,'','',bts)
   *
Endd
Set curs off
Clos all
Rele ket,tgl3,ksp,jml,hj,yt,no,hal,tot,tot2,totx,toty,tots,tot2s,totxs,totys
Retu

Function ffkth
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-7
      Tgl4=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl

      Set colo to gr+*
      @row(),col() say &fds
      Stor ' ' to yt,yt2
      Set curs on
      Set colo to g
      Do pesan with 'Item Faktur DiHapus? Y/T:          Item Pemb.Brg ikut diHapus? Y/T:'
      Do whil .not. yt$'YT'
         @24,31 get yt pict '!'
         @24,74 get yt2 pict '!'
         Read
         If lastkey()=27
            Exit
         Endi
      Endd
      If yt='Y' .or. yt2='Y'
         If yt='Y'
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            Commit
            Skip
         Endi
         If yt2='Y'
            Sele 3
            Uselan('STOK.DTA','STOK.NDX','','','','')

            Sele 4
            Seek dtos(tgl)+ksp+fkt
            Do whil .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_spl=ksp .and. faktur=fkt )
               Kdb=kode_brg
               Jml=jumlah
               Isi=isine_b

               Rlok()
               Dele
               Ul()
               Commit

               Sele 3
               Seek kdb
               Rlok()
               Repl stok_m with stok_m-(jml*isi)
               Ul()
               Commit

               Sele 4
               Skip
            Endd

            Sele 3
            Close

            Sele 6
            Loca for kode_spl=ksp .and. faktur=fkt
            If foun()
               Rlok()
               Dele
               Ul()
               Commit
            Endi

         Endi
      Endi
      Do pesan with pesanx
      Set curs off
      Tgl=tgl4
      Sele 2
      Retu 2
   Case lastkey()=-8
      Nr=recno()
      Set cent on
      Set curs on
      Stor tglx to tgl3,tgl4
      Do pesan with 'Jt.Tempo Tanggal:           s/d           '
      @24,36 get tgl3
      @24,51 get tgl4
      Read
      If lastkey()#27
         Save scre to smkdb
         Do pesan with 'ESC=Menu Selanjutnya'
         Copy to JT-TEMPO.TMP for ( (tanggal_3>=tgl3 .and. tanggal_3<=tgl4) .and. tanggal_4=ctod('  -  -    ') .and. rp_bayar < ((rp_total-rp_diskon)+rp_pajak) )
         Sele 99
         Useidx('JT-TEMPO.TMP')
         Sort on tanggal_3 to JT-TEMPO.DTA

         Useidx('JT-TEMPO.DTA')
         Go top

         Stor 1 to n,no,hal
         Stor 0 to tot,tot2,totx,toty,tots,tot2s,totxs,totys
         Set curs on
         Set alte on
         Set alte to JT-TEMPO.RTF
         ?' DAFTAR FAKTUR PEMBELIAN'
         ?' JT.TEMPO TANGGAL: '+dtoc(tgl3)+' s/d '+dtoc(tgl4)
         ?repl(gl,80)
         ?' No. Jt.Tempo  Tgl.Beli  No.Faktur   Nama Supplier                    Jumlah Rp.'
         ?repl(gl,80)

         Set cent off
         Do whil .not. eof()
            Jml =((rp_total-rp_diskon)-rp_bayar)
            Totx=totx+jml
            ?tran(n,'999. ')+dtoc(tanggal_3)+'  '+dtoc(tanggal)+'  '+faktur+'  '+nama_spl+' '+kode_spl+tran(jml,'@e  999,999,999')

            N =n+1
            No=no+1
            Skip
         Endd
         ?repl(gl,80)
         ?spac(58)+'Total = Rp'+tran(totx,'@e 9999,999,999')
         ?repl(gl,80)
         ?' '
         Set alte off
         Set alte to
         Memoedit(Memoread('JT-TEMPO.RTF'),1,0,22,240,.t.,'',240)
         Do pesan3 with 'ESC=Selesai   F10=Cetak'
         If lastkey()#27
            Do cp
            If lastkey()#27
               Set prin on
               Set devi to prin
               ?chr(27)+chr(33)+chr(9)
               Set marg to 10
               Type JT-TEMPO.RTF to prin
               Set prin off
               Set devi to scre
            Endi
         Endi
         Rest scre from smkdb
      Endi
      Do pesan with pesanx
      Set cent off
      Set curs off
      Sele 2
      Go nr
      Retu 2
   Case lastkey()=-9
      Go nr
      Stor 1 to no,hal
      Stor 0 to tot,tot2,totx,toty,tots,tot2s,totxs,totys
      Set curs on
      Do cp
      If cc=0
         Retu 0
      Endi
      Do kopfkth
      Do whil .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jml=(((rp_total-rp_diskon)+rp_pajak)-rp_bayar)
            Tot =tot +((rp_total-rp_diskon)+rp_pajak)
            Tot2=tot2+rp_bayar
            Totx=totx+jml
            Tots =tots +((rp_total-rp_diskon)+rp_pajak)
            Tot2s=tot2s+rp_bayar
            Totxs=totxs+jml

            ?'| '+dtoc(tanggal)+' | '+dtoc(tanggal_3)+' | '+left(faktur,10)+'| '+left(nama_spl,20)+'|'+tran(((rp_total-rp_diskon)+rp_pajak),'@e 99,999,999,999 |')+;
             tran(rp_bayar,'@e  99,999,999,999 |')+tran(jml,'@e 99,999,999,999 |')
            No=no+1
            If no>55
               ?'|'+repl('-',104)+'|'
               ?'| Jumlah Hal ini Rp'+spac(38)+tran(tots,'@e 99,999,999,999 ')+tran(tot2s,'@e   99,999,999,999 ')+tran(totxs,'@e  99,999,999,999 |')
               ?'|'+repl('-',104)+'|'
               ?''
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
               Hal=hal+1
               Stor 0 to tots,tot2s,totxs,totys
               Do kopfkth
            Endi
         Endi
         Skip
      Endd
      If cc=0
         Do pesan with pesanx
         Set curs off
         Retu 1
      Endi
      ?'|'+repl('-',104)+'|'
      ?'| Jumlah Hal ini Rp'+spac(38)+tran(tots,'@e 99,999,999,999 ')+tran(tot2s,'@e   99,999,999,999 ')+tran(totxs,'@e  99,999,999,999 |')
      ?'|'+repl('-',104)+'|'
      ?'| Grand Total    Rp'+spac(38)+tran(tot ,'@e 99,999,999,999 ')+tran(tot2 ,'@e   99,999,999,999 ')+tran(totx ,'@e  99,999,999,999 |')
      ?'*'+repl('=',104)+'*'
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
      Go nr
      Retu 1
   Case lastkey()=13
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      Tot =0
      B   =6
      No  =1
      Save scre to sdaf
      Set colo to bg+/b
      @4,0 say dtoc(tanggal)+tl+dtoc(tanggal_3)+tl+left(faktur,8)+tl+left(nama_spl,18)+tl+tran((rp_total-rp_diskon)+rp_pajak,'@e 9999,999,999')+tl+tran(rp_bayar,'@e 9999,999,999')+tl+dtoc(tanggal_4)
      Set colo to /bg
      @2,38 say ':'+ksp
      @5,0 to 5,79 doub
      @6,0 clea to 22,79
      @20,0 to 20,79
      @21,1 say 'Diskon:'
      @22,1 say 'PPN...:'
      @21,33 say 'Bayar:'
      @22,33 say 'Retur:'
      @21,59 say 'Usr:'
      @22,59 say 'Ket:'
      Set colo to /bg
      @21,8 say tran(diskon,'@e 99.99% Rp')+tran(rp_diskon,'@e 999,999.99')
      @22,8 say tran(pajak ,'@e 99.99% Rp')+tran(rp_pajak ,'@e 999,999.99')
      @21,40 say 'Rp '+tran(rp_bayar,'@e 999,999,999')
      @22,40 say 'Rp '+tran(rp_retur,'@e 999,999,999')
      @21,63 say left(nama_ksr,15)
      @22,63 say keterangan
      Sele 4
      Seek dtos(tgl)+ksp+fkt
      Do whil .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_spl=ksp .and. faktur=fkt )
         Jml =jumlah
         Hp  =(jml*harga)
         Dis1=((diskon_1/100)*(jml*harga))
         Pjk1=((pajak/100)*(hp-dis1))
         Jhg =((hp-dis1)+pjk1)
         Tot=tot+jhg
         If jml#0
            @b,0 say tran(no,'999. ')+left(kode_brg,7)+' '+left(nama_brg,29)+tran(jml,'@e  99999.99 ')+satuan+tran(harga,'@e 9,999,999 ')+tran(jhg,'@e 9999,999,999')
            B =b+1
            No=no+1
         Endi
         If b>19
            Do pesan with 'Pgln=Lanjut'
            Wait''
            Set colo to /bg
            B=6
         Endi
         Skip
      Endd
      @b,0 clea to 19,79
      Do pesan with 'ESC=Selesai'
      Wait''
      Rest scre from sdaf
      Do pesan with pesanx
      Sele 2
      If rp_total#tot
         Rlok()
         Repl rp_total with tot
         Ul()
         Commit
      Endi
      Retu 1
   Case lastkey()=72 .or. lastkey()=104
      Do pesan with 'Tunggu, sedang diHitung'
      Totb=0
      Nr=recno()
      Set order to 1
      Sum ((rp_total-rp_diskon)+rp_pajak),rp_bayar to tot,tot2 for (tanggal>=tgl .and. tanggal<=tgl2)
      Go nr
      Set colo to gr+
      @22,34 say 'Total=Rp'+tran(tot,'@e 9,999,999,9999')+gl+gl+gl+'Bayar=Rp'+tran(tot2,'@e 99,999,999,999')
      Set colo to g
      Do pesan with pesanx
      Retu 2
   Case lastkey()=32
      Tot=((rp_total-rp_diskon)+rp_pajak)-rp_bayar
      Rlok()
      If id_bayar=' '
         Repl id_bayar with chr(251)
         Totb=totb+tot
      Elseif id_bayar#' '
         Repl id_bayar with ' '
         Totb=totb-tot
      Endi
      Ul()
      Commit
      @22,0 to 22,79
      @22,48 say 'Yang Mau diBayar:'+tran(totb,'@e 9,999,999,999')
      Do pesan with pesanx
      Retu 1
   Case lastkey()=28
      Save scre to smkdb
      Set curs on
      Tgl4=tgl
      Ksp =kode_spl
      Nr  =recno()

      Do bukuh
      Set curs off
      Rest scre from smkdb
      Do pesan with pesanx
      Sele 2
      Tgl=tgl4
      Go nr
      Retu 1
   Case lastkey()=66 .or. lastkey()=98
      If .not. ada
         Do pesan2 with 'Untuk Pembayaran Kolektif, Kode Supplier Harus diIsi'
         Do pesan with pesanx
         Sele 2
         Retu 1
      Endi
      No  =recno()
      Totb=0

      Go nr
      Do whil .not. eof() .and. kode_spl=ksp
         If id_bayar#' '
            Totb=totb+(((rp_total-rp_diskon)+rp_pajak)-rp_bayar)
         Endi
         Skip
      Endd

      Go no
      Tgl4=tgl
      Tgl =tanggal
      Ksp =kode_spl
      Fkt =faktur

      Save scre to smkdb
      Set curs on

      Sele 1
      Seek ksp
      Jmlso=rp_saldo
      Tglso=tanggal
      Stor 0 to jml,jml2
      *
      Sele 2
      xFilter=dbfilter()
      Set filter to
      Set order to 2
      Seek ksp
      Do whil .not. eof() .and. kode_spl=ksp
         If (tanggal>=tglso .and. tanggal<tgl)
            Jml=jml+((rp_total-rp_diskon-rp_retur)+rp_pajak)
         Endi
         Skip
      Endd
      *
      Sele 6
      Set order to 2
      Seek ksp
      Do whil .not. eof() .and. kode_spl=ksp
         If (tanggal>=tglso .and. tanggal<tgl)
            Jml2=jml2+jumlah_byr
         Endi
         Skip
      Endd
      Tot2=(jmlso+jml+totb)-jml2
      *
      @17,0 clea to 23,79
      @17,0 to 22,79
      Set cent on
      Ket ='Tunai'+spac(15)
      Tgl3=tglx
      Do whil .t.
         *@13,48 say 'Jumlah Hutang..=Rp'+tran(tot2,'@e 9999,999,999')
         @19,2  say 'Tanggal Pembayaran....: 'get tgl3
         @20,2  say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 9,999,999,999'
         @19,48 say 'Keterangan/Mutasi:'
         @20,48 get ket
         *
         Tot3=tot2-totb
         *@22,52 say 'Sisa Hutang Rp'+tran(tot3,'@e  999,999,999')
         Yt=' '
         Do pesan with 'Data Benar? Y/T'
         Do whil .not. yt$'YT'
            @24,48 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            Endi
         Endd
         If lastkey()=27 .or. yt#'Y'
            Exit
         Endi

         Sele 2
         Go nr
         Do whil .not. eof() .and. kode_spl=ksp
            If id_bayar#' '
               Rlok()
               Repl tanggal_4 with tgl3,id_bayar with ' '
               If totb<tot
                  Repl rp_bayar with (rp_bayar+totb),keterangan with 'TITIP, MSH ADA SISA.'
               Elseif totb>=tot
                  Repl rp_bayar with ((rp_total-rp_diskon)+rp_pajak),keterangan with 'DIBAYAR TUNAI.'
               Endi
               If totb>tot
                  Repl keterangan with ket
               Endi
               Ul()
            Endi
            Skip
         Endd
         Commit
         Go no

         Sele 6
         If totb#0
            Flok()
            Appe blan
            Repl tanggal with tgl3,tanggal_2 with dtos(tgl3),faktur with fkt,;
                 kode_spl with ksp,nama_spl with nsp,jumlah_byr with totb,jenis_byr with ket
            Ul()
            Commit
         Endi
         Exit
      Endd
      Set curs off
      Set cent off
      Rest scre from smkdb
      Do pesan with pesanx
      Sele 2
      Set filter to &xFilter
      Go no
      Tgl=tgl4
      Retu 2
   Case lastkey()=76 .or. lastkey()=108
      Tgl4=tgl
      Tgl =tanggal
      Ksp =kode_spl
      Nsp =nama_spl
      Fkt =faktur
      Totb=(((rp_total-rp_diskon-rp_retur)+rp_pajak)-rp_bayar)
      xFilter=dbfilter()
      *
      Save scre to smkdb
      Set curs on
      *
      Set colo to w+/br
      @19,0 clea to 23,79
      @19,0 to 23,79
      Set cent on
      Ket ='Nota No:'+fkt+'  '
      Tgl3=tglx
      @20,2 say 'Tanggal Pembayaran....:'get tgl3
      @21,2 say 'Tunai/Cash/Transfer Rp:'get totb pict '@e 999,999,999'
      @22,2 say 'Total Rp '+ltri(tran(rp_total-rp_diskon,'@e 999,999,999'))+' - Retur Rp '+ltri(tran(rp_retur,'@e 999,999,999'))
      @20,48 say 'Keterangan/Mutasi:'
      @21,48 get ket
      *
      Yt=' '
      Do pesan with 'Data Benar? Y/T'
      Do whil .not. yt$'YT'
         @24,48 get yt pict '!'
         Read
         If lastkey()=27
            Exit
         Endi
      Endd
      If yt='Y'
         Rlok()
         Repl rp_bayar with rp_bayar+totb,tanggal_4 with tgl3,id_bayar with ' '
         Ul()
         Commit
         *
         Sele 6
         If totb#0
            Flok()
            Appe blan
            Repl tanggal with tgl3,tanggal_2 with dtos(tgl3),faktur with fkt,;
                 kode_spl with ksp,nama_spl with nsp,jumlah_byr with totb,jenis_byr with ket
            Ul()
            Commit
         Endi
      Endi
      Set curs off
      Set cent off
      Rest scre from smkdb
      Do pesan with pesanx
      Sele 2
      Set filter to &xFilter
      Tgl=tgl4
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

Proc kopfkth
Set prin on
Set devi to prin
Set marg to 10
?chr(27)+chr(33)+chr(4)
?' DAFTAR FAKTUR PEMBELIAN'
If ksp#spac(5)
   ?' SUPPLIER: '+rtri(nsp)+' ('+rtri(ksp)+')'
Endi
?' TANGGAL..: '+dtoc(tgl)+' SAMPAI '+dtoc(tgl2)+spac(62)+'Hal:'+tran(hal,'999')
?'*'+repl('=',104)+'*'
?'| Tanggal  | Jt.Tempo | No.Faktur | Nama Supplier       |        Jumlah |          Bayar |    Kekurangan |'
?'|'+repl('-',104)+'|'
No=1
Retu

Proc bukuh
Set cent off
Sele 1
Seek ksp
Tgl4 =tgl
*
If hut
   Nsp =nama_spl
   Algn=almt_spl
   Kota=kota_spl
   Syarat='kode_spl=ksp'
   Kataph='Pembelian No: '
   Kata1=' DAFTAR HUTANG (Rupiah) '
   Kata2=' Nama Sup.: '+rtri(nsp)+'  '+ksp
   *
   Sele 7
   Uselan('RETURBN.DTA','RETURBN1.NDX','','','','')
   *
Else
   Nsp =nama_lgn
   Algn=almt_lgn
   Kota=kota_lgn
   Syarat='kode_lgn=ksp'
   Kataph='Penjualan Nota No:'
   Kata1=' DAFTAR PIUTANG (Rupiah)'
   Kata2=' Nama Plgn: '+rtri(nsp)+'  '+ksp
   *
Endi
@1,0 clea to 23,79
Do pesan with 'Tunggu, sedang dihitung...'
Stor 0 to jml,jml2,jml3,tot,tot2,tot3
Stor 1 to n,no
*
Set alte on
Set alte to BUKU-PH.RTF
?Kata1+spac(25)
?Kata2
?' Alamat...: '+rtri(algn)+' - '+kota
?' Tanggal..: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
?repl(gl,4)+chr(194)+repl(gl,9)+chr(194)+repl(gl,10)+repl(chr(194)+repl(gl,13),3)+gl
?' No.'+tl+'Tanggal  '+tl+'No.Nota   '+tl+'       Jumlah'+tl+'        Bayar'+tl+'         Sisa'
?repl(gl,4)+chr(193)+repl(gl,9)+chr(193)+repl(gl,10)+repl(chr(193)+repl(gl,13),3)+gl
Sele 2
Set order to 2
Seek ksp
If foun()
   Do whil .not. eof() .and. &syarat
      If (tanggal>=tgl .and. tanggal<=tgl2)
         Jml =(rp_total-rp_diskon)
         Jml2=rp_bayar
         Tot =tot +jml
         Tot2=tot2+jml2
         ?tran(no,'999. ')+dtoc(tanggal)+'  '+faktur+iif(hut,'','     ')+tran(jml,'@e  9,999,999,999 ')+tran(jml2,'@e 9,999,999,999 ')+tran(jml-jml2,'@e 9,999,999,999')
         No=no+1
         N =n +1
      Endi
      Skip
   Endd
Endi
*
If hut
   Sele 7
   Set filter to kode_spl=ksp
   Loca for tanggal>=tgl
   If foun()
      ?' '
      ?'     RETUR BARANG'
      ?'     ------------'
      Fkt =faktur
      Tgl3=tanggal
      Do whil .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Jml3=jml3+(jumlah*harga)
            Skip
            If faktur#fkt
               Tot3=tot3+jml3
               ?tran(no,'999. ')+dtoc(tgl3)+'  '+fkt+spac(20)+tran(jml3,'@e 9,999,999,999 ')+tran(1*-jml3,'@e 9,999,999,999')
               No  =no+1
               N   =n +1
               Fkt =faktur
               Tgl3=tanggal
               Jml3=0
            Endi
         Else
            Skip
         Endi
      Endd
   Endi
Endi
?repl(gl,68)
?spac(15)+'Total Rp   '+tran(tot,'@e 9,999,999,999 ')+tran(tot2+tot3,'@e 9,999,999,999 ')+tran(tot-(tot2+tot3),'@e 9,999,999,999')
?repl(gl,68)
?''
Set alte off
Set alte to
Set cent off
Do pesan with 'ESC=Menu Berikutnya'
Memoedit(Memoread('BUKU-PH.RTF'),1,0,22,210,.t.,'',210)
Do pesan3 with 'ESC=Selesai   F10=Cetak'
If lastkey()=-9
   Do cp
   If cc#0
      Set prin on
      Set devi to prin
      ?chr(27)+chr(33)+chr(9)
      Type BUKU-PH.RTF to print
      Set prin off
      Set devi to scre
   Endi
Endi
Tgl=tgl4
Set cent on
*
Retu

* EOF LHUT
