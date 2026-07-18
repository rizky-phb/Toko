* LAPORAN POIN
*-------------
Sele 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
Sele 2
Uselan('FAKTURP.DTA','FAKTURP2.NDX','FAKTURP1.NDX','','','')
Sele 3
Uselan('STOK.DTA','STOK.NDX','','','','')
Sele 4
*
Sele 5
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
*Tgl   =tanggal_2
*Tgl2  =tanggal_3
Close
*
Ksp=spac(10)
Stor ' ' to hri,bln,thn,xbayar
Stor tglx to tgl,tgl2
Do whil .t.
   Sele 1
   Set cent on
   Set key 28 to daflgn
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'F1=Daftar Pelanggan     ESC=Selesai'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   Endi
   Nsp='-'
   If ksp#spac(5)
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+rtri(ksp)
         Loop
      Endi
      Nsp=nama_lgn
   Endi
   Stor 0 to no,tot,tot2
   Do konvtgl
   Set cent off
   @1,0 to 1,79
   @22,0 to 22,79
   @1,0 say repl(gl,8)+chr(194)+repl(gl,5)+chr(194)+repl(gl,9)+chr(194)+repl(gl,8)+chr(194)+repl(gl,28)+chr(194)+repl(gl,11)+chr(194)+repl(gl,5)
   @2,0 say 'Tanggal '+tl+'Nota '+tl+'Kasir    '+tl+'Kode    '+tl+'Nama Pelanggan/Member       '+tl+' Jumlah Rp.'+tl+'Point'
   @3,0 say repl(gl,8)+chr(197)+repl(gl,5)+chr(197)+repl(gl,9)+chr(197)+repl(gl,8)+chr(197)+repl(gl,28)+chr(197)+repl(gl,11)+chr(197)+repl(gl,5)
   Sele 2
   If ksp#spac(10)
      Set order to 1
      Set filter to kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Seek ksp+(thn+bln+hri)
      If .not. foun()
         Seek ksp
         If .not. foun()
             Do pesan2 with 'Tidak ada Data Nota Penjualan'
             Loop
         Endi
      Endi
   Else
      Set order to 2
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek (thn+bln+hri)
      If .not. foun()
         Loca for (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. foun()
             Do pesan2 with 'Tidak ada Data Nota Penjualan'
             Loop
         Endi
      Endi
   Endi
   Nr=recno()
   Pesanx='H=Hitung_Poin     F1=Kode_Plgn     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   Set cent off
   Declare field[9],bts[9],frm[9]
   Field[1]='TANGGAL'
   Field[2]='FAKTUR'
   Field[3]='left(NAMA_KSR,9)'
   Field[4]='left(KODE_LGN,8)'
   Field[5]='left(NAMA_LGN,28)'
   Field[6]='RP_TOTAL'
   Field[7]='JML_POIN'
   Frm[1]='99-99-99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='@!'
   Frm[5]='@!'
   Frm[6]='@e 999,999,999'
   Frm[7]='9999 '
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   Bts[6]=''+tl
   Bts[7]=''+tl
   Dbedit(4,0,21,79,field,'fpoin',frm,'','',bts)
   *
Endd
Set curs off
Clos all
Retu

Function fpoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=28
      Nr=recno()
      Klb=spac(5)
      Set curs on
      Do pesan with ''
      Set colo to w+/r
      @23,32 say 'Kode Plgn.:'get klb pict '@!'
      Read
      If lastkey()#27 .and. klb#spac(5)
         Klb=rtri(klb)
         Set colo to g
         Set order to 2
         Seek klb
         If .not. foun()
           Go nr
         Endi
      Endi
      Do pesan with pesanx
      Set curs off
      Retu 2
   Case lastkey()=-7
      Tgl3=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      Set colo to gr+*
      @row(),col() say &fds
      Yt=' '
      Set curs on
      Set colo to g
      Do pesan with 'Data DiHapus? Y/T'
      Do whil .not. yt$'YT'
         @24,49 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Yt=' '
         Set curs on
         Set colo to g
         Do pesan with 'Data Penjualan ikut DiHapus? Y/T'
         Do whil .not. yt$'YT'
            @24,58 get yt pict '!'
            Read
            If lastkey()=27
               Exit
            Endi
         Endd
         If lastkey()=27
            Do pesan with pesanx
            Set curs off
            Retu 1
         Endi   
         If .not. deleted()
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            Commit
            Skip
            Set cent on
            Do konvtgl
            *
            If yt='Y'
               Do bacafile
               If .not. file('&nf0')
                  Do pesan2 with 'Tidak ada File '+&nf0
                  Do pesan with pesanx
                  Set curs off
                  Retu 1
               Endi
               *
               Sele 4
               Uselan('&nf0','&nf1','&nf2','&nf3','','')
               Seek dtos(tgl)+fkt+ksp
               Do whil .not. eof() .and. ( tanggal_2=dtos(tgl) .and. kode_lgn=ksp .and. faktur=fkt )
                  Kdb=kode_brg
                  Jml=jumlah
                  Rlok()
                  Dele
                  Ul()
                  Commit
                  *
                  Sele 3
                  Seek kdb
                  If foun()
                     Rlok()
                     Repl stok_k with (stok_k-jml)
                     Ul()
                     Commit
                  Endi
                  *
                  Sele 4
                  Skip
               Endd
               Close
            Endi
         Endi
         Tgl=tgl3
      Endi
      Do pesan with pesanx
      Set curs off
      Set cent off
      Sele 2
      Retu 2
   Case lastkey()=-9
      Stor 1 to no,hal
      Stor 0 to tot,tot2,pnskr,totpn
      Set curs on
      Do cp
      If cc=0
         Retu 0
      Endi
      Do kopoin
      Go top
      Stor kode_lgn to ksp,ksp2
      Stor nama_lgn to nsp,nsp2
      Do whil .not. eof()
         If (tanggal>=tgl .and. tanggal<=tgl2)
            Tot  =tot  +rp_total
            Tot2 =tot2 +rp_total
            Pnskr=pnskr+jml_poin
            Totpn=totpn+jml_poin
            ?ksp+' '+left(nsp,25)+' '+faktur+' '+dtoc(tanggal)+' '+jam+' '+tran(rp_total,'@e 999,999,999 ')+tran(jml_poin,'9999')
            * 
            No=no+1
            If no>50
               ?repl(gl,80)
               ?' '
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
               Do kopoin
            Endi
         Endi
         Skip
         If kode_lgn#ksp2
            Stor kode_lgn to ksp,ksp2
            Stor nama_lgn to nsp,nsp2
            ?spac(75)+tran(pnskr,'9999')
            Pnskr=0
         Elseif kode_lgn=ksp2
            Ksp=spac(10)
            Nsp=spac(30)
         Endi
      Endd
      ?repl(gl,80)
      ?'          Grand Total'+spac(34)+tran(tot2,'@e 99,999,999,999 ')+tran(totpn,'@e 9,999,999')
      ?repl(gl,80)
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set curs off
      Retu 1
   Case lastkey()=13
      Tglz=tgl
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_lgn
      B   =6
      No  =1
      Save scre to sdaf
      @4,0 say dtoc(tanggal)+tl+faktur+tl+left(nama_ksr,9)+tl+left(kode_lgn,8)+tl+left(nama_lgn,28)+tl+tran(rp_total,'@e 999,999,999')+tl+tran(jml_poin,'9999')
      Set colo to /bg
      @5,0 to 5,79
      @6,0 clea to 22,79
      @22,0 to 22,79
      Set cent on
      Do konvtgl
      Do bacafile
      If .not. file('&nf0')
         Do pesan2 with 'Tidak ada File '+nf0
         Do pesan with pesanx
         Set curs off
         Set cent off
         Retu 1
      Endi
      Stor 0 to tot,tot2,totpn,totpn2
      *
      Sele 4
      Uselan('&nf0','&nf1','','','','')
      Seek dtos(tgl)+ksp+fkt
      Do whil .not. eof() .and. ( tanggal=tgl .and. kode_lgn=ksp .and. faktur=fkt )
         Kdby=kode_bar
         Kdbx=iif(len(rtri(kdby))>=7,left(kdby,2)+'x'+right(rtri(kdby),4),left(kdby,7))
         Jml =(jumlah-jumlah_rtr)
         Hj  =harga_2
         Dis1=diskon
         Jhg =jml*hj
         Tot=tot+jhg
         *
         If (id_point=' ' .and. id_point2=' ') .and. rpoin>0
            Totpn =totpn  + (jml * (hj - (hj*(dis1/100))))
         Endi
         If (id_point=' ' .and. id_point2#' ') .and. rpoin2>0
            Totpn2=totpn2 + (jml * (hj - (hj*(dis1/100))))
         Endi
         *
         If jml#0
            @b,0 say tran(no,'999. ')+kdbx+' '+left(nama_brg,30)+tran(jml,'@e 9999.99 ')+;
                     satuan+kode_lks+tran(hj,'@e  99,999,999 ')+tran(jml*hj,'@e 999,999,999')
            B =b+1
            No=no+1
         Endi
         If b>21
            Do pesan with 'Pgln=Lanjut'
            Wait''
            Set colo to /bg
            B=6
         Endi
         Skip
      Endd
      @b,1 clea to 21,79
      Do pesan with 'ESC=Selesai'
      Close
      Wait''
      *
      Pnskr =iif(rpoin>0 ,(totpn -mod(totpn ,rpoin ))/rpoin , 0 )
      Pnskr2=iif(rpoin2>0,(totpn2-mod(totpn2,rpoin2))/rpoin2, 0 )
      Tgl  =tglz
      Rest scre from sdaf
      Do pesan with pesanx
      Set cent off
      Sele 2
      If tot#0
         Rlok()
         Repl rp_total with tot,jml_poin with (pnskr+pnskr2)
         Ul()
         Commit
      Endi
      Rest scre from sdaf
      Do pesan with pesanx
      Set cent off
      Retu 1
   Case lastkey()=72 .or. lastkey()=104
      Nr=recno()
      Yt=' '
      Set curs on
      Do pesan with '1=Hanya_hitung_saja     2=Hitung_ulang_dan_simpan_poin : '
      @24,68 get yt pict '!'
      Read
      Do pesan with 'Tunggu, sedang diHitung'
      If yt#'2' .and. lastkey()#27
         Stor 0 to tot,tot2,totpn,pnskr
         *
         Sele 1
         Seek ksp
         Ajp=aj_point
         Amp=am_point
         *
         Sele 2
         Set order to 1
         Seek ksp
         Do whil .not. eof() .and. kode_lgn=ksp
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Tot =tot +(rp_total-rp_diskon)
               Tot2=Tot2+rp_pokok
               Totpn=totpn+jml_poin
            Endi
            Skip
         Endd
         *
         Go nr
         Set colo to gr+
         @22,1  say 'Total=Rp '+ltri(tran(tot,'@e 9,999,999,999'))
         @22,25 say 'Margin=Rp '+ltri(tran(tot-tot2,'@e 999,999,999'))
         @22,50 say 'Poin= '+ltri(tran((totpn+ajp)-amp,'999,999  ( '))+ltri(tran(totpn,'@e 999999 + '))+ltri(tran(ajp,'@e 999999 - '))+ltri(tran(amp,'@e 999999 )'))
      Elseif yt='2' .and. rpoin>0
         Stor 0 to tot,tot2,totpn,pnskr
         Syarat=iif(ksp#spac(10),'kode_lgn=ksp','kode_lgn#spac(10)')
         *
         Go nr
         Do whil .not. eof() .and. &syarat
            If (tanggal>=tgl .and. tanggal<=tgl2)
               Pnskr =iif(rpoin>0 ,(rp_poin -mod(rp_poin ,rpoin ))/rpoin , 0 )
               Pnskr2=iif(rpoin2>0,(rp_poin2-mod(rp_poin2,rpoin2))/rpoin2, 0 )
               Rlok()
               Repl jml_poin with (pnskr+pnskr2)
               Ul()
            Endi
            Skip
         Endd
         Commit
         *
         Go nr
      Endi
      Set colo to g
      Set curs off
      Do pesan with pesanx
      Retu 2
  Othe
      Retu 1
Endc
Retu 0

Proc kopoin
Set prin on
Set devi to prin
Set marg to 10
?chr(27)+chr(33)+chr(9)
?' DAFTAR PEROLEHAN POINT BELANJA'
?' TANGGAL.: '+dtoc(tgl)+' S/D '+dtoc(tgl2)+spac(17)+'Hal:'+tran(hal,'999')
?repl(gl,80)
?'Kode       Nama Pelanggan            Nota/Tgl/Jam          Jumlah Rp.  Jml.Poin'
?repl(gl,80)
No=1
Retu

*LAPORAN POINT/GLOBAL
*--------------------
Proc poin2
Sele 1
Uselan('CUST.DTA','CUST2.NDX','','','','')
Sele 2
Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
Sele 3
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
Tgl   =tanggal_2
Tgl2  =tanggal_3
Close
*
Stor ' ' to hri,bln,thn
Do whil .t.
   Set cent on
   Set colo to /bg,w+/b
   @1,0 clea to 23,79
   @2,24 say 'Tanggal'get tgl
   @2,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai'
   Read
   @23,0
   If lastkey()=27
      Exit
   Endi
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'
   *
   Sele 4
   Useidx('TOP-POIN.DTA')
   Copy stru to TOP-POIN.TMP
   Close
   *
   Useidx('TOP-POIN.TMP')
   *
   Stor 0 to tot,tot2,totx,jml,jml2,pnskr,totp,tajp,tamp
   Stor 1 to hal,no,n
   Ada=.f.
   *
   Sele 1
   Go top
   Do whil .not. eof()
      Ksp =kode_lgn
      Nsp =nama_lgn
      Algn=almt_lgn
      Ajp  =aj_point
      Amp  =am_point
      Tglhp=tgl_ambil   &&&DIHITUNG MULAI TGL.AMBIL TERAKHIR
      Jamx =jam_ambil
      *
      Sele 2
      Seek ksp
      Do whil .not. eof() .and. kode_lgn=ksp
         If tanggal=tglhp .and. jam>jamx
            Jml =jml +(rp_total-rp_diskon)
            Jml2=jml2+jml_poin
            Tot =tot +(rp_total-rp_diskon)
            Tot2=tot2+rp_pokok
         Elseif tanggal>tglhp
            Jml =jml +(rp_total-rp_diskon)
            Jml2=jml2+jml_poin
            Tot =tot +(rp_total-rp_diskon)
            Tot2=tot2+rp_pokok
         Endi
         Skip
      Endd
      If jml2>0 .or. jml>0 .or. ajp>0
         Ada=.t.
         *
         Sele 4
         Appe blan
         Repl kode with ksp,nama with nsp,alamat with algn,rp_jual with jml,rp_pokok with tot2,rp_margin with jml-tot2,;
              jm_poin with jml2,aj_poin with ajp,am_poin with amp,so_poin with (jml2+ajp)
         *
         Totp =totp+jml2
         Totx =totx+(jml-tot2)
         Tajp =tajp+ajp
         No=no+1
      Endi
      Stor 0 to jml,jml2,tot2
      *
      Sele 1
      Skip
   Endd
   *
   If ada
      Sele 6
      Close
      *
      Sele 5
      Close
      *
      Sele 4
      Sort on nama       to TOP-PNMA.TMP
      Sort on rp_jual /D to TOP-PRPH.TMP
      Sort on so_poin /D to TOP-PPNT.TMP
      Close
      *
      Pesanx='N/R/P=Top_Ranking     F1=Cari_Nama    F10=Cetak'
      Sele 4
      Useidx('TOP-PNMA.TMP')
      Sele 5
      Useidx('TOP-PPNT.TMP')
      Sele 6
      Useidx('TOP-PRPH.TMP')
      *
      Set cent off
      Set colo to w/b,w+/g
      Ket='TOP NOTA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      @1,0 clea to 23,79
      @1,1 say ket
      @2,0 to 2,79 doub
      @21,0 to 21,79
      Do whil .t.
         Declare field[11],bts[11],frm[11],kop[11]
         Do pesan with pesanx
         Set colo to w/b,w+/g
         @22,0 say ' Nota   Rp'+tran(tot ,'@e 999,999,999,999')+'     Jml.Poin:'+tran(totp,'@e 9999,999')
         @23,0 say ' Margin Rp'+tran(totx,'@e 999,999,999,999')+'     Adjust..:'+tran(tajp,'@e 9999,999')+'     Sisa.:'+tran((totp+tajp)-tamp,'@e 9999,999')
         Kata1="iif(JM_POIN>0,tran(JM_POIN,'@e  999,999 '),spac(9))+iif(AJ_POIN>0,tran(AJ_POIN,'@e 999,999 '),spac(8))+iif(SO_POIN>0,tran(SO_POIN,'@e 999,999'),spac(7))"
         Field[1]="left(KODE,8)+' '+left(NAMA,23)+' '+left(ALAMAT,22)+tran(RP_JUAL,'@e 99999,999,999')+tran(RP_MARGIN,'@e 9999,999,999')"
         Field[2]="left(KODE,8)+' '+left(NAMA,23)+' '+left(ALAMAT,23)+&kata1"
         Kop[1]='Kode     Nama_Pelanggan          Alamat                      Rp_Nota   Rp_Margin'
         Kop[2]='Kode     Nama_Pelanggan          Alamat                    Jml.P  Adjust    Sisa'
         Stor '' to frm[1],frm[2],bts[1],bts[2]
         Dbedit(3,0,20,79,field,'flpoin',frm,kop,gl,bts)
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
Set cent on
Set curs off
Clos all
Retu

Function flpoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   @21,1 say repl(gl,15)
   Set colo to gr+/b
   @21,1 say ltri(str(recno()))+'//'+ltri(str(reccount()))
   Set colo to w/b,w+/g
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=spac(30)
      Set curs on
      Do pesan with ''
      Set colo to w+/r
      @24,18 say 'Urut Nama:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#spac(30)
         Sele 4
         Set colo to /bg
         Loca for nama=rtri(nmb)
         If .not. foun()
           Go nr
         Endi
      Endi
      Do pesan with pesanx
      Set colo to w/b,w+/g
      Set curs off
      Retu 2
Elseif lastkey()=78 .or. lastkey()=110
   Sele 4
   Ket='TOP NAMA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   Retu 0
Elseif lastkey()=80 .or. lastkey()=112
   Sele 5
   Ket='TOP POIN,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   Keyboard chr(4)
   Retu 0
Elseif lastkey()=82 .or. lastkey()=114
   Sele 5
   Ket='TOP NOTA,  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   @1,0
   @1,1 say ket
   Retu 0
Elseif lastkey()=-9
      Set curs on
      Pil='2'
      Do pesan with '1=Printer     2=File : '
      @24,51 get pil pict '!'
      Read
      If pil#'1'
         No =1
         Nf='D:\POIN.CSV'
         Do pesan with 'Tunggu, sedang proses '+nf
         Set alte on
         Set alte to &nf
         ?';'+aidi1
         ?';'
         ?';REKAP POIN BELANJA'
         ?';PERIODE : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
         ?';'
         ?'KODE;NAMA_PELANGGAN;ALAMAT;RP_NOTA;RP_MARGIN;JML_POIN;ADJUST;SISA'
         Do whil .not. eof()
            ?"'"+rtri(kode)+';'+rtri(nama)+';'+rtri(alamat)+';'+ltri(str(rp_jual))+';'+ltri(str(rp_margin))+';'+ltri(str(jm_poin))+';'+ltri(str(aj_poin))+';'+ltri(str(so_poin))
            No=no+1
            Skip
         Endd
         ?''
         Set alte off
         Set alte to
         Do pesan with 'Export selesai..., nama file: '+subs(nf,4,12)+' , di drive D:\'
         Wait''
         Set curs off
         Do pesan with pesanx
         Set colo to w/b,w+/g
         Retu 2
      Endi
      *
      Stor 1 to n,no,hal
      Do cp
      If cc=0
         Retu 0
      Endi
      Set prin on
      Set devi to prin
      Set marg to 5
      ?chr(27)+chr(33)+chr(4)
      ?' REKAP POIN BELANJA'
      ?' PERIODE: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(59)+'Hal:'+tran(hal,'999')
      ?repl('=',129)
      ?'    No. Kode      Nama Pelanggan                Alamat                              Rp_Nota     Rp_Margin   Jml_P  Adjust    Sisa'
      ?repl('-',129)
      Do whil .not. eof()
         ?tran(no,'@e 99,999. ')+kode+left(nama,29)+' '+left(alamat,29)+tran(rp_jual,'@e 99,999,999,999')+tran(rp_margin,'@e 99,999,999,999')+&kata1
         No=no+1
         N =n+1
         If n>50
            ?repl('-',122)
            ?' '
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
            N   =1
            Hal =hal+1
            Set prin on
            Set devi to prin
            Set marg to 5
            ?chr(27)+chr(33)+chr(4)
            ?' REKAP POIN BELANJA'
            ?' PERIODE: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(59)+'Hal:'+tran(hal,'999')
            ?repl('=',129)
            ?'    No. Kode      Nama Pelanggan                Alamat                              Rp_Nota     Rp_Margin   Jml_P  Adjust    Sisa'
            ?repl('-',129)
         Endi
         Skip
      Endd
      ?repl('-',129)
      ?spac(18)+'Grand Total'+spac(47)+tran(tot ,'@e 999,999,999,999')+tran(totx,'@e 99,999,999,999')+tran(totp,'@e 9999,999')+tran(tajp,'@e 9999,999')+tran(totp+tajp,'@e 9999,999')
      ?repl('=',129)
      ?''
      Set prin off
      Set devi to scre
      Do pesan with pesanx
      Set colo to w/b,w+/g
      Set curs off
      Retu 1
Else
   Set curs off
   Do pesan with pesanx
   Set colo to w/b,w+/g
   Retu 1
Endi
Retu 0

*LAPORAN AMBILPOIN
*-----------------
Proc lambpoin
Sele 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
Sele 2
Uselan('POINTAM.DTA','POINTAM2.NDX','POINTAM1.NDX','','','')
*
Ksp=spac(10)
Stor ' ' to hri,bln,thn,jlap
Stor tglx to tgl,tgl2
Do whil .t.
   Sele 1
   Set cent on
   Set key 28 to daflgn
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,24 say 'Plgn..:'get ksp pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 's/d'get tgl2
   Do pesan with 'ESC=Selesai     F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   Endi
   Nsp='-'
   If ksp#spac(5)
      Seek ksp
      If .not. foun()
         Do pesan2 with 'Tidak ada Kode Pelanggan '+rtri(ksp)
         Loop
      Endi
      Nsp=nama_lgn
   Endi
   Stor 0 to no,tot,tot2
   Do konvtgl
   Set cent off
   @1,0 to 1,79
   @22,0 to 22,79
   Set colo to g
   Sele 2
   If ksp#spac(5)
      Set order to 1
      Set filter to kode_lgn=ksp .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Seek ksp+dtos(tgl)
      If .not. foun()
         Seek ksp
         If .not. foun()
             Do pesan2 with 'Tidak ada Data Pengambilan Point'
             Loop
         Endi
      Endi
   Else
      Set order to 2
      Set filter to (tanggal>=tgl .and. tanggal<=tgl2)
      Seek dtos(tgl)
      If .not. foun()
         Loca for (tanggal>=tgl .and. tanggal<=tgl2)
         If .not. foun()
             Do pesan2 with 'Tidak ada Data Pengambilan Point'
             Loop
         Endi
      Endi
   Endi
   Nr=recno()
   Pesanx='ESC=Selesai     F8=Hapus     F10=Cetak'
   Do pesan with pesanx
   Set cent off
   Declare field[1],kop[1],bts[1],frm[1]
   Field[1]="dtoc(TANGGAL)+tl+JAM+tl+left(KODE_LGN,9)+tl+left(NAMA_LGN,25)+tl+tran(iif(AM_POINT>0,AM_POINT,RP_UANG),'@e 9,999,999')+tl+left(keterangan,19)"
   Kop[1]='Tanggal '+tl+'Jam  '+tl+'Kode     '+tl+'Nama Pelanggan/Member    '+tl+'Jml.Ambil'+tl+'Catatan'
   Frm[1]=''
   Bts[1]=''
   Dbedit(2,0,21,79,field,'fampoin',frm,kop,gl,bts)
   *
Endd
Set curs off
Clos all
Retu

Function fampoin
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Set colo to w+/b
   @23,0 clea to 23,79
   @23,0 say 'Point seblmnya: '+ltri(tran(so_point,'@e 999,999'))+'   Catatan: '+keterangan
   Do pesan with pesanx
   Retu 1
Elseif lastkey()=27
   Retu 0
Elseif lastkey()=-7
   Ksp=kode_lgn
   Ajp=aj_point
   Set colo to gr+*
   @row(),col() say &fds
   Yt=' '
   Set curs on
   Set colo to g
   Do pesan with 'Data DiHapus? Y/T : '
   @24,49 get yt pict '!'
   Read
   If lastkey()#27 .and. yt='Y'
      Do pesan with 'Tunggu, sedang dihapus'
      Rlok()
      Dele
      Ul()
      Commit
      Skip
      *
      Sele 1
      Seek ksp
      Rlok()
      Repl aj_point with ajp,so_point with 0,am_point with 0,tgl_ambil with ctod('  -  -    '),jam_ambil with ' ',ket_ambil with 'KOREKSI/'+dtoc(tglx)+'/'+left(time(),5)
      Ul()
      Commit
      *
   Endi
   Do pesan with pesanx
   Set cent off
   Sele 2
   Retu 2
Elseif lastkey()=-9
   Set curs on
   Pil='2'
   Do pesan with '1=Printer     2=File : '
   @24,51 get pil pict '!'
   Read
   If pil#'1'
      No =1
      Nf='D:\AMBILPN.CSV'
      Do pesan with 'Tunggu, sedang proses '+nf
      Set alte on
      Set alte to &nf
      ?';;;'+aidi1
      ?';;;'
      ?';;;DAFTAR PENGAMBILAN POIN'
      ?';;;TANGGAL : '+dtoc(tgl)+' s/d '+dtoc(tgl2)
      ?';;;'
      ?'TANGGAL;JAM;KODE;NAMA_PELANGGAN;JML_AMBIL;DARI;CATATAN'
      Do whil .not. eof()
         ?dtoc(tanggal)+';'+jam+";'"+rtri(kode_lgn)+';'+rtri(nama_lgn)+';'+ltri(str(am_point))+';'+ltri(str(so_point))+';'+rtri(keterangan)
         No=no+1
         Skip
      Endd
      ?''
      Set alte off
      Set alte to
      Do pesan with 'Export selesai..., nama file: '+subs(nf,4,12)+' , di drive D:\'
      Wait''
      Set curs off
      Do pesan with pesanx
      Retu 2
   Endi
Else
   Retu 1
Endi
Retu 0

* EOF POIN
