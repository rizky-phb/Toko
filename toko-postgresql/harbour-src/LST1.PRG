***PERHATIAN
***HARGA GROSIR: ADA YG DISKON, ADA YG @HARGA
***------------------------------------------
* DAFTAR STOK
Sele 1
Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
Sele 2
Uselan('JENIS.DTA','JENIS.NDX','JENIS2.NDX','','','')
Sele 3
Uselan('STOK.DTA','STOK.NDX','STOK2.NDX','STOK3.NDX','STOK4.NDX','STOK5.NDX')
Sele 4
*
Sele 8
Uselan('RAK.DTA','','','','','')
Sele 10
Uselan('DATA.SET','','','','','')
Ikdb=id_kodebrg
Nn =no_beli+1
Stor spac(5) to fkt,kspx
If nn>99999
   Nn=1
Endi
Do xnota
Close
*
Ksa =gete("KASSA")
Klb =spac(5)
Kdp ='   '
Nklb=spac(30)
Ndep=spac(20)
Stor ' ' to stk,lks,kdb
Stor '   ' to rak,rak2,rak3
Stor .f. to diEdit
Stor tglx to tglz,tglso
Set cent off
Do whil .t.
   Sele 3
   Ksp=spac(5)
   Kdb=spac(7)
   Nmb=spac(40)
   Isi=1
   Kdb2=spac(13)
   Idx ='4'
   Sambung='Y'
   Syaratx='NAMA_BRG#SPAC(30)'
   Stor ' ' to syarat,ytp,judulx
   Stor 'PCS  ' to st,stb
   Stor 0    to hp,hp2,hpb,hj,hj21,hj22,hj3,hj32,hj4,hj42,hj5,hj52,bts3,bts4,bts5,jml,jmlx,jml1,jml2,jml3,jmlrb,nr,hjx,pjg,xhal,no,n,ppn,pjk
   Stor tglx to tgl,tgl0,tgl1,tgl2,tglrb
   Stor .f. to keluar,adadis,hapus,xUdahada,beli,cuma1
   Stor ' ' to cNmb11,cKdb11,cKdb12,cHg11,cHg12,cHg13,cPc12,cPc13,cPc12a,cPc12b,cPc12c,cPc12d,cPc12e
   Stor ' ' to cNmb21,cKdb21,cKdb22,cHg21,cHg22,cHg23,cPc22,cPc23,cPc22a,cPc22b,cPc22c,cPc22d,cPc22e
   Stor ' ' to cNmb31,cKdb31,cKdb32,cHg31,cHg32,cHg33,cPc32,cPc33,cPc32a,cPc32b,cPc32c,cPc32d,cPc32e
   Brslabel=8
   Nfl='LABEL-'+ksa+'.DTA'
   *
   @1,0 clea to 24,79
   @1,0 say 'SEMUA KELOMPOK BARANG'
   @2,0 to 2,79 doub
   @22,0 to 22,79
   Katax='F2/F3=Sortir    F4=Tambah    F5=Edit    F6=Mutasi    F8=Hapus    F10=Cetak'
   Do pesan  with katax
   Declare field[22],kop[22],bts[22],frm[22]
   Field[1]='KODE_BAR'
   Field[2]='left(KODE_KLB,3)'
   Field[3]='left(NAMA_BRG,31)'
   Field[4]='((STOK_A+STOK_M) - (STOK_K+STOK_RB))'
   Field[5]='left(SATUAN,3)'
   Field[6]='HARGA_2'
   Field[7]='HARGA_21'
   Field[8]='HARGA_BL'
   Field[9]='HARGA'
   Field[10]='PAJAK'
   Field[11]='PERSEN'
   Field[12]='ID_POINT'
   Field[13]='ID_POINT2'
   Field[14]='left(NAMA_BRG,29)'
   Field[15]='KODE_BRG'
   Field[16]='KODE_BAR'
   Field[17]='KODE_KLB'
   Field[18]='KODE_SPL'
   Kop[1]='BarCode'
   Kop[2]='Klp'
   Kop[3]='Nama Barang'
   Kop[4]='  Sisa'
   Kop[5]='Stk'
   Kop[6]='@Hg.Bakul'
   Kop[7]=' @Hg.Ecer'
   Kop[8]='   @Pokok'
   Kop[9]='    @Beli'
   Kop[10]='PPN %'
   Kop[11]=' Margin.%'
   Kop[12]='P1'
   Kop[13]='P2'
   Kop[14]='Nama Barang'
   Kop[15]='Kode'
   Kop[16]='BarCode'
   Kop[17]='Klp.'
   Kop[18]='Supl.'
   Frm[4]='@e 999999'
   Frm[6]='@e 9,999,999'
   Frm[7]='@e 9,999,999'
   Frm[8]='@e 9,999,999.99'
   Frm[9]='@e 9,999,999.99'
   Frm[10]='@e 99.99'
   Frm[11]='@e 99999.9999'
   Bts[1]=''
   Bts[2]=' '
   Bts[5]=' '
   Stor '@!' to frm[1],frm[2],frm[3],frm[5],frm[12],frm[13],frm[14],frm[15],frm[16],frm[17],frm[18],frm[19]
   Stor tl to bts[3],bts[4],bts[6],bts[7],bts[8],bts[9],bts[10],bts[11],bts[12],bts[13],bts[14],;
              bts[15],bts[16],bts[17],bts[18],bts[19],bts[20],bts[20],bts[21],bts[22]
   Set filter to
   Set order to &idx
   Go top
   Dbedit(3,0,21,79,field,'flstx',frm,kop,gl,bts)
   If lastkey()=27 .or. ytp='X'
      Exit
   Endi
Endd
Set curs off
If diEdit
   Sele 10
   Uselan('DATA.SET','','','','','')
   Rlok()
   Repl no_beli with nn
   Ul()
   Commit
Endi
Clos all
Rele fkt,kdb,kdb2,nmb,isi,st,ksp,stb,hp,hp2,hpb,hj,hj21,hj22,hj3,hj32,hj4,hj42,hj5,hj52,bts3,bts4,bts5,nr,nn,smn,;
     stk,stkx,pjg,jml,jml1,jml2,jml3,jmlrb,hjx,tgl0,tgl1,tgl2,tglrb,keluar,adadis,hapus,xUdahada,diEdit
Rele field[4],frm[4],bts[4]
Set cent on
Retu


Proc lstx
*
Rele field[17],kop[17],frm[17],bts[17]
Retu

Function flstx
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      Set colo to w+/b
      @23,0 clea to 23,79
      @23,0 say kode_bar+' '+left(kode_klb,3)+' '+left(nama_brg,31)+tran(((STOK_A+STOK_M) - (STOK_K+STOK_RB)),'@e  999999 ')+left(satuan,3)+tran(harga_2,'@e  9,999,999')+tran(harga_21,'@e  9,999,999')
      Do pesan with katax
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=32 .and. fds='ID_POINT2'
      Rlok()
      If id_point2=' '
         Repl id_point2 with '*'
      Else
         Repl id_point2 with ' '
      Endi
      Ul()
      Commit
      Retu 1
   Case lastkey()=32 .and. fds='ID_POINT'
      Rlok()
      If id_point=' '
         Repl id_point with 'X'
      Else
         Repl id_point with ' '
      Endi
      Ul()
      Commit
      Retu 1
   Case (lastkey()>=48 .and.  lastkey()<=57) .or. (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122) .or. lastkey()=32
      Set curs on
      Syaratx=' '
      Nmb=uppe(chr(lastkey()))+spac(29)
      *Do pesan1 with ''
      Do pesan with 'Cari: '+spac(30)
      @24,28 get nmb pict '@!'
      Keyboard chr(4)
      Read
      Set curs off
      Nmb=rtri(nmb)
      Set colo to g
      If lastkey()#27
         Idx='4'
         Set order to &idx
         If left(nmb,1)=' '
            Nmb=ltri(nmb)
            Set filter to AT(nmb,nama_brg)>0
            Go top
            Set colo to /bg
            @1,50
            Set colo to w/b
            @1,77-len(nmb) say ' ?'+nmb+' '
         Else
            Set filter to
            Seek nmb
            If .not. foun()
               Idx='5'
               Set order to &idx
               Seek nmb
               If .not. foun()
                  Idx='1'
                  Set order to &idx
                  Seek nmb
                  If .not. foun()
                     Tone(1500,2)
                     Tone(2000,4)
                     Do pesan with 'Data yg diCari tidak ditemukan...!'
                     Wait''
                     Go nr
                  Endi
               Endi
            Endi
            Set colo to /bg
            @1,50
            Set colo to w/b
            @1,77-len(nmb) say ' '+nmb+' '
            Nmb=spac(30)
         Endi
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Case lastkey()=-1
      Nr  =recno()
      Klb =spac(5)
      Nklb=spac(30)
      Klp =.t.
      Set curs on
      *
      Sele 2
      Do pesan with 'Sortir, Kode Klp:      '
      @24,46 get klb pict '@!'
      Read
      If lastkey()=27
         Sele 3
         Set filter to
         Go top
      Else
         Seek klb
         If .not. foun()
            Do dafklb
         Endi
         Klb =kode_klb
         Nklb=nama_klb
         Syaratx='KODE_KLB=KLB'
         Judulx ='KELOMPOK: '+rtri(nklb)+'  ('+rtri(klb)+')'
         *
         @1,0
         Set colo to w+/b
         @1,0 say judulx
         Set colo to g
         *
         Sele 3
         Set order to 3
         Set filter to &syaratx
         Seek klb
      Endi
      *
      Do pesan with katax
      If klb=spac(5)
         @1,0
         @1,0 say 'SEMUA KELOMPOK BARANG'
      Endi
      Set curs off
      Retu 2
   Case lastkey()=-2
      Nr=recno()
      Yt=' '
      Set filter to
      Set curs on
      Stor ' ' to nmb,lks
      *Do pesan1 with ''
      Do pesan with 'Sortir Sisa Stok,  0=yg_Kosong    1=Mines    2=Semua_Stok     [0/1/2] : '
      @24,75 get yt pict '9'
      Read
      If yt='0'
         Syaratx="((STOK_A+STOK_M) - (STOK_K+STOK_RB))=0"
         Judulx ='SORTIR SISA=0'
         Set filter to &syaratx
         Loca for ((STOK_A+STOK_M) - (STOK_K+STOK_RB))=0
         Set colo to w+/r
         @1,66
         @1,66 say judulx
      Elseif yt='1'
         Jmlx=0
         Set curs on
         Do pesan with 'Sortir u/ Sisa Stok kurang dari=    '
         @24,56 get jmlx pict '9999'
         Read
         If lastkey()#27
            Syaratx="( ((STOK_A+STOK_M) - (STOK_K+STOK_RB))<=jmlx .and. ((STOK_A+STOK_M+(STOK_AJ)) - (STOK_K+STOK_RB+STOK_KR))#0 )"
            Judulx ='SORTIR SISA<'+ltri(str(jmlx))
            Set filter to &syaratx
            Loca for ( ((STOK_A+STOK_M) - (STOK_K+STOK_RB))<=jmlx .and. ((STOK_A+STOK_M+(STOK_AJ)) - (STOK_K+STOK_RB+STOK_KR))#0 )
            Set colo to w+/r
            @1,64
            @1,64 say judulx
            If jml#0
               Judulx='SORTIR SISA<='+ltri(str(jmlx))
               @1,64 say judulx
            Endi
         Endi
      Else
         Judulx =' '
         Syaratx='KODE_BRG#SPAC(30)'
         Set filter to 
         @1,60
      Endi
      If yt='1' .or. yt='2' .or. .not. foun()
         Tone(1000,3)
         Tone(1500,3)
         Tone(2000,6)
         Do pesan with 'Data Tidak diTemukan...'
         Wait''
         @1,59 say spac(21)
         Set filter to
         Go nr
      Endi
      Set colo to g
      Set curs off
      Do pesan  with katax
      Retu 2
   Case lastkey()=-3
      Dis1=recno()
      Kdb =spac(13)
      Nklb=spac(30)
      @24,0
      Set curs on
      Do mkdb
      Set order to &idx
      Go dis1
      Set curs off
      Do pesan with katax
      Retu 2
   Case lastkey()=-4
      Stor row() to b,b2
      Stor col() to kl,klm
      *Do pesan1 with ''
      Set curs on
      If fds='KODE_KLB' .or. fds='KODE_BRG'  .or. fds='KODE_SPL' .or. fds='KODE_BAR' .or. fds='ID_POINT'
         Rlok()
         @b,klm get &fds pict '@!'
         Read
         Ul()
         Commit
      Elseif fds='left(KODE_KLB,3)' .or. fds='KODE_KLB'
         Klb=kode_klb
         Save scre to smkdb
         Set colo to w
         @b,klm get klb pict '@!S4'
         Read
         Rest scre from smkdb
         If lastkey()#27
            Rlok()
            Repl kode_klb with klb
            Ul()
            Commit
         Endi
      Elseif fds='left(NAMA_BRG,31)'
         Nmb=nama_brg
         Save scre to smkdb
         Set colo to w
         @b,klm get nmb pict '@!S40'
         Read
         Rest scre from smkdb
         If lastkey()#27
            Rlok()
            Repl nama_brg with nmb
            Ul()
            Commit
         Endi
      Elseif fds='left(SATUAN,3)'
         St =satuan
         Stb=satuan_b
         Isi=isine_b
         Smn=stok_min
         B=iif(b>17,17,b)
         Klm=iif(klm>64,64,klm)
         Save scre to smkdb
         Set colo to w+/br
         @b,klm-1 clea to b+5,klm+15
         @b,klm-1 to b+5,klm+15
         @b,klm-1 say chr(194)
         @b+1,klm say 'Sat.Bsr.:'get stb pict '@!'
         @b+2,klm say 'I s i...:'get isi pict '9999'
         @b+3,klm say 'Sat.Kcl.:'get st pict '@!'
         @b+4,klm say 'StokMin.:'get smn pict '9999'
         Read
         Rest scre from smkdb
         If lastkey()#27
            If isi=0
               Isi=1
            Endi
            Rlok()
             Repl satuan with st,satuan_b with stb,stok_min with smn,;
                 isine_b with isi,harga_b with (harga*isi),harga_bl with harga
            Ul()
            Commit
         Endi
      Elseif fds='((STOK_A+STOK_M) - (STOK_K+STOK_RB))' .and. xOwner
         Jml1=stok_a
         Jml2=stok_m
         Jml3=stok_k
         Jml =stok_rb
         Save scre to smkdb
         Set colo to w+/br
         B=iif(b>17,17,b)
         @b,klm-1 clea to b+5,klm+17
         @b,klm-1 to b+5,klm+17
         @b,klm-1 say chr(194)
         @b+1,klm say 'Awal..:'get jml1 pict '@e 999999.99'
         @b+2,klm say 'Masuk.:'get jml2 pict '@e 999999.99'
         @b+3,klm say 'Keluar:'get jml3 pict '@e 999999.99'
         @b+4,klm say 'ReturB:'get jml  pict '@e 999999.99'
         Read
         Rest scre from smkdb
         If lastkey()#27
            Rlok()
            Repl stok_a with jml1,stok_m with jml2,stok_k with jml3,stok_rb with jml,kassa with ksa
            Ul()
            Commit
         Endi
      Elseif fds='HARGA_2'
         Hp  =harga
         Hj  =harga_2
         Hj21=harga_21
         Hj3 =harga_3
         Hj4 =harga_4
         Hj5 =harga_5
         Hj32=harga_32
         Hj42=harga_42
         Hj52=harga_52
         Bts3=batas_3
         Bts4=batas_4
         Bts5=batas_5
         Kl  =39
         If b>18
            B=b-5
         Endi
         Set key -4 to xadadis
         Save scre to sdaf
         Set colo to w+/br
         @b,kl clea to b+5,79
         @b,kl+1 say '@HgBakul:             Ecer:'
         @b+1,kl to b+5,79
         @b+1,kl+2 say 'Harga Grosir'
         @b+1,kl+22 say 'Pokok:'+tran(hp,'@e 9,999,999')
         @b+2,kl+2 say 'Bts.:'
         @b+3,kl+2 say 'HJ.@:'
         @b+2,kl+11  say bts3 pict ' 999'
         @b+3,kl+8  say hj3  pict '@e 9999999.99'
         @b+2,kl+22 say bts4 pict ' 999'
         @b+3,kl+19 say hj4  pict '@e 9999999.99'
         @b+2,kl+33 say bts5 pict ' 999'
         @b+3,kl+30 say hj5  pict '@e 9999999.99'
         *
         @b,kl+10 get hj   pict '@e 9,999,999' valid iif(hj>0,hj>=hp,hj=0)
         @b,kl+28 get hj21 pict '@e 9,999,999' valid iif(hj21>0,hj21>=hp,hj21=0)
         Do pesan with 'F5=Edit Harga Grosir'
         Read
         Set key -4 to
         If adadis
            Adadis=.f.
            @b+2,kl+11  get bts3 pict ' 999'
            @b+3,kl+8  get hj3  pict '@e 9999999.99'
            *@b+4,kl+8  get hj32 pict '@e 9999999.99'
            @b+2,kl+22 get bts4 pict ' 999'
            @b+3,kl+19 get hj4  pict '@e 9999999.99'
            *@b+4,kl+19 get hj42 pict '@e 9999999.99'
            @b+2,kl+33 get bts5 pict ' 999'
            @b+3,kl+30 get hj5  pict '@e 9999999.99'
            *@b+4,kl+30 get hj52 pict '@e 9999999.99'
            Read
         Endi
         Set colo to /bg
         Rest scre from sdaf
         *
         If lastkey()#27
            Rlok()
            Repl harga_2 with hj,harga_21 with hj21,persen with 0
            Repl harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,;
                 harga_32 with hj32,harga_42 with hj42,harga_52 with hj52,;
                 batas_3 with bts3,batas_4 with bts4,batas_5 with bts5
            If hp>0
               Repl persen with ((hj-hp)/hp)*100
            Endi
            Ul()
            Commit
         Endi
      Elseif fds=='HARGA'
         Hp=harga
         Hj=harga_2
         @b,klm get hp pict '@e 9,999,999.99'
         Read
         If lastkey()#27
            Rlok()
            Repl harga with hp
            If isine_b=1
               Repl harga_b with hp
            Endi
            If hp>0
               Repl persen with ((hj-hp)/hp)*100
            Endi
            Ul()
            Commit
         Endi
      Elseif fds='HARGA_BL'
         Hb=harga_bl
         @b,klm get hb pict '@e 9,999,999.99'
         Read
         If lastkey()#27
            Rlok()
            Repl harga_bl with hb
            Ul()
            Commit
         Endi
      Elseif fds='PERSEN'
         Hjx=persen
         @b,klm get hjx pict '@e 99999.9999'
         Read
         If lastkey()#27
            Rlok()
            If persen#hjx
               Repl harga_2 with harga+(harga*(hjx/100))
               Repl persen with hjx
            Endi
            Ul()
            Commit
         Endi
      Elseif fds='PAJAK'
         Rpx=pajak
         @b,klm get rpx pict '@e 99.99'
         Read
         If lastkey()#27
            Rlok()
            Repl pajak with rpx
            Ul()
            Commit
         Endi
      Elseif fds='ID_POINT'
         Rpx=id_point
         @b,klm get rpx pict '@!'
         Read
         If lastkey()#27
            Rlok()
            Repl id_point with rpx
            Ul()
            Commit
         Endi
      Elseif fds='KODE_RAK'
         Do pesan with 'F1=Daftar_Lokasi     ESC=Batal'
         Lks=' '
         Rak=kode_rak
         If fds='KODE_RAK2'
            Rak=kode_rak2
            Lks='2'
         Endi
         Klp=.f.
         Save scre to smkdb
         Set colo to w
         Set key 28 to dafklb
         Sele 8
         @b,kl get rak pict '@!'
         Read
         Set key 28 to
         Rest scre from smkdb
         *
         Sele 3
         If lastkey()#27
            Rlok()
            Repl kode_rak&lks with rak
            Ul()
            Commit
         Endi
      Endi
      Set curs off
      Do pesan  with katax
      Retu 2
   Case lastkey()=-5
      Set order to 5
      Nr  =recno()
      Kdb2=kode_bar
      Nmb2=nama_brg
      St2 =satuan
      Hp2 =harga_b
      Stor 0 to jml,jml2
      Save scre to smkdb
      Set curs on
      Set colo to gr+*
      @row(),col() say &fds
      Do whil .t.
         Kdb=spac(13)
         Do pesan with 'Masukan Jumlah Barang yg akan dikurangkan dan BarCode Tujuan'
         Set colo to w+/b,w+/r
         @21,0 clea to 23,79
         @21,0 to 21,79
         @21,32 say 'MUTASI/PECAH STOK'
         @22,1  say 'Asal Barang..: '+kdb2+' ='+rtri(nmb2)
         @22,62 say 'Qty:'get jml pict '@e 9999.99'
         @22,75 say st2
         @23,1  say 'Barang Tujuan:'get kdb pict '@!'
         Read
         If lastkey()=27
            Exit
         Endi
         Seek kdb
         If .not. foun()
            Pjg=len(rtri(kdb))
            Do whil pjg>0
               Kdb=left(kdb,pjg)
               Seek kdb
               If foun()
                  Exit
               Endi
               Pjg=pjg-1
            Endd
            If .not. foun()
               Tone(1000,2)
               Tone(1500,2)
               Tone(2000,4)
               Do pesan with 'Kode Brg Tidak DiTemukan'
               Wait''
               Loop
            Endi
            Do dafbrg
            If lastkey()=27
               Loop
            Endi
         Endi
         Do pesan with 'Masukan Jumlah yg Akan diTambahkan di Barang Tujuan'
         Set colo to w+/b,w+/r
         Kdb=kode_bar
         Nmb=nama_brg
         St =satuan
         Hp =harga
         Isi=isine_b
         @23,16 say kdb
         @23,30 say '='+rtri(nmb)
         @23,62 say 'Jml:'get jml2 pict '@e 9999.99'
         @23,75 say st
         Read
         Yt=' '
         Do pesan with 'Benar? Y/T : '
         @24,46 get yt pict '!'
         Read
         Set colo to g
         If yt='Y' .and. jml#0 .and. jml2#0
            Do konvtgl
            Rlok()
            Repl stok_m with (stok_m+jml2),harga_b with hp2,harga with (hp2/isi),tanggal_1 with tgl
            Ul()
            Commit
            *
            Seek kdb2
            Rlok()
            Repl stok_k with (stok_k+jml),tanggal_2 with tgl
            Ul()
            Commit
            *
            Sele 4
            Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')
            Flok()
            Appe blan
            Repl tanggal with tgl,tanggal_2 with dtos(tgl),kode_brg with kdb,nama_brg with nmb,satuan with st,;
                 kode_brg2 with kdb2,nama_brg2 with nmb2,satuan2 with st2,jumlah with jml,isine_b with isi
            Ul()
            Commit
            Close
            *
            Sele 3
            Exit
         Endi
      Endd   
      Set order to &idx
      Rest scre from smkdb
      Set curs off
      Do pesan with katax
      Rele nmb2,st2
      Go nr
      Retu 1
   Case lastkey()=-7
      Set curs on
      Set colo to gr+*
      @row(),col() say &fds
      *Do pesan1 with ''
      Do pesan with 'Kode: '+rtri(kode_brg)+' ->'+rtri(nama_brg)+' ('+rtri(satuan)+')'
      Set colo to w+/r
      Yt=' '
      Do whil .not. yt$'YT'
         @24,64 say 'DiHapus? Y/T'get yt pict '!'
         Read
      Endd
      Set colo to g
      If yt='Y'
         If .not. deleted()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
         Endi
      Endi
      Set curs off
      Do pesan with katax
      Retu 2
   Case lastkey()=-9
      Nr=recno()
      Set curs on
      Kdb=kode_brg
      Pil ='2'
      Pil2='2'
      Yts ='1'
      Yt  ='1'
      Syarat='kode_klb=klb'
      If syaratx#' '
         Syarat=syaratx
      Endi
      Do pesan with 'Sortir Hasil Cetak'
      Save scre to smkdb
      Set colo to w+/r
      @12,17 clea to 17,62
      @12,17 to 17,62
      @13,19 say '1 =Cetak_ke_Printer   2 =File    [1/2] :'get pil
      @14,19 say '1 =Mutasi_Stok        2 =Sisa_sj [1/2] :'get pil2
      @15,19 say '1 =Hanya_yg_ada_Stok             [1/ ] :'get yts
      @16,19 say '1 =dengan_Harga_Pokok            [1/ ] :'get yt
      Read
      Rest scre from smkdb
      If lastkey()#27 .and. pil='2'
         No =1
         Nf='D:\DAFSTOK.CSV'
         Do pesan with 'Tunggu, sedang proses '+nf
         Set order to 4
         If yts='1'
            Set filter to ((STOK_A+STOK_M)-(STOK_K+STOK_RB))#0 .and. &syaratx
         Endi
         Go top
         Set alte on
         Set alte to &nf
         If pil2='2'
            ?';;; DAFTAR STOK BARANG; ; ; ; ;'
            If judulx#' '
               ?';;; '+judulx
            Endi
            ?';;; PER TANGGAL: '+dtoc(tglx)+'; ; ; ; ;'
            ?'NO.;KODE;BARCODE; NAMA BARANG;STOK;SATUAN;@HG.POKOK;@HG.JUAL;@HG.JUAL2;@HG.JUAL3;BTS3;@HG.JUAL4;BTS4;@HG.JUAL5;BTS5;JML_POKOK'
            Do whil .not. eof()
               If nama_brg#spac(30) .and. harga#0
                  Do sudahada
                  Stk ='((STOK_A+STOK_M)-(STOK_K+STOK_RB))'
                  Jhg =&stk * hp
                  Jml =&stk   &&&iif(&stk<0,abs(&stk),&stk)
                  Stk =iif(jml=0,'- ',ltri(str(jml)))
                  Kata="ltri(str(hj3))+';'+ltri(str(bts3))+';'+ltri(str(hj4))+';'+ltri(str(bts4))+';'+ltri(str(hj5))+';'+ltri(str(bts5))+';'+ltri(str(jhg))"
                  ?ltri(str(no))+";'"+rtri(kode_brg)+";'"+rtri(kode_bar)+'; '+rtri(nmb)+';'+' '+stk+';'+rtri(satuan)+';'+ltri(str(hp))+';'+ltri(str(hj))+';'+ltri(str(hj21))+';'+&kata
                  No=no+1
               Endi
               Skip
            Endd
         Else
            ?';;; DAFTAR MUTASI BARANG; ; ; ; ;'
            If judulx#' '
               ?';;; '+judulx
            Endi
            ?';;; PER TANGGAL: '+dtoc(tglx)+'; ; ; ; ;'
            ?'NO.;KODE;BARCODE; NAMA BARANG;TANGGAL_SO;STOK_AWAL;PEMBELIAN;PENJUALAN;RETUR_PEMB;SISA_STOK'+iif(yt='1',';HG.POKOK;JUMLAH_POKOK','')
            Do whil .not. eof()
               If nama_brg#spac(30) .and. harga#0
                  Do sudahada
                  Stk ='((STOK_A+STOK_M)-(STOK_K+STOK_RB))'
                  Jhg =&stk * hp
                  Jml =&stk   &&&iif(&stk<0,abs(&stk),&stk)
                  Stk =iif(jml=0,'- ',ltri(str(jml)))
                  Kata="ltri(tran(jml1,'999999'))+';'+ltri(tran(jml2,'999999'))+';'+ltri(tran(jml3,'999999'))+';'+ltri(tran(jmlrb,'999999'))+';'+ltri(tran(&stk,'9999999'))"
                  ?ltri(str(no))+";'"+rtri(kode_brg)+";'"+rtri(kode_bar)+'; '+rtri(nmb)+';'+dtoc(tgl0)+';'+&kata+iif(yt='1',';'+ltri(tran(hp,'99999999'))+';'+ltri(tran(hp*&stk,'999999999')),'')
                  No=no+1
               Endi
               Skip
            Endd
         Endi
         ?''
         Set alte off
         Set alte to
         *
         Do pesan with 'Export selesai..., nama file: '+subs(nf,4,12)+' , di drive D:\'
         Wait''
         Set curs off
         Set filter to
         Go nr
         Do pesan with katax
         Retu 2
      Elseif lastkey()=27
         Set curs off
         Do pesan with katax
         Retu 1
      Endi
      *
      Do ctkstok
      Set filter to
      Go nr
      Do pesan with katax
      Set curs off
      Retu 1
   Case lastkey()=13
      @24,0
      Nr=recno()
      Set curs on
      Kdb=kode_brg
      Do lkdb
      Set order to &idx
      Set curs off
      Do pesan  with katax
      Retu 2
   Case lastkey()=28
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Nr=recno()
      Cf=1
      Save scre to sdaf
      Set colo to gr+
      @10,0 clea to 12,79
      @10,0 to 12,79
      @10,22 say 'Kode/BarCode yg Doubel/Kembar'
      @11,25 say 'Tunggu sedang dicari...'
      Set colo to w
      Set order to 1
      *
      Set alte on
      Set alte to DOBELKOD.RTF
      ?' DAFTAR KODE/BARCODE YANG KEMBAR'
      ?repl(gl,57)
      Go top
      Kdbx=kode_brg
      Awal=1
      Klm =12
      Do whil .not. eof()
         If kode_brg=kdbx .and. awal>1
            ?' '+kode_brg+'        '+nama_brg+tran(harga_2,'@e  9,999,999')
            *Klm=klm+len(rtri(kode_brg)+'-')
            Nr =recno()
         Endi
         Kdbx=kode_brg
         Awal=awal+1
         Skip
      Endd
      *
      Set order to 5
      Go top
      Kdbx=kode_bar
      Awal=1
      Klm =12                                           
      Do whil .not. eof()
         If kode_bar=kdbx .and. awal>1
            ?' '+kode_bar+'  '+left(nama_brg,30)+tran(harga_2,'@e  9,999,999')
            *Klm=klm+len(rtri(kode_bar)+'-')
            Nr =recno()
         Endi
         Kdbx=kode_bar
         Awal=awal+1
         Skip
      Endd
      ?repl(gl,57)
      ?' '
      Set alte to
      Set alte off
      *
      Set curs on
      @22,0 clea to 23,79
      Do pesan with 'ESC=Menu Selanjutnya'
      Layar('DOBELKOD')
      *
      Do pesan3 with 'ESC=Selesai     F10=Cetak'
      If lastkey()#27
         Do cp
         If lastkey()#27
            Set prin on
            Set devi to prin
            Set marg to 5
            ?chr(27)+chr(33)+chr(1)
            Type DOBELKOD.RTF to print
            Eject
            Set prin off
            Set devi to scre
         Endi
      Endi
      *
      Rest scre from sdaf
      Rele awal,klm
      Do pesan with katax
      Set curs off
      Set order to 1
      Go nr
      Retu 2
   Case lastkey()=9
      Save scre to sdaf
      Set colo to w
      If file('&nfl')
         Sele 99
         Useidx('&nfl')
         Go top
         No=1
         @1,0 clea to 24,79
         @1,22 say 'DAFTAR BARANG YG AKAN DI-CETAK LABEL'
         @2,0 to 2,79 doub
         B=3
         Do whil .not. eof()
            @b,0 say tran(no,'99. ')+nama_brg+' '+kode_bar+' '+kode_brg+tran(harga_2,'@e 9,999,999')+tran(batas_3,'9999/')+tran(harga_3,'@e 9,999,999')
            B=b+1
            No=no+1
            If b>21
               @b,0 to b,79
               @b,34 say 'PgDn=Lanjut'
               B=3
               Wait''
            Endi
            Skip
         Endd
         @b,0 to b,79
         @b+1,0 clea to 24,79
         Do pesan with 'ESC=Lanjut'
      Else
         Tone(2000,4)
         Tone(1500,2)
         Yt=' '
         Do pesan with 'Data Label Belum diSortir...!'
      Endi
      Wait''
      Rest scre from sdaf
      Do pesan with katax
      Sele 3
      Retu 1 
   Case lastkey()=43
      @24,0
      Nr=recno()
      Set curs on
      Set order to 3
      Beli=.t.
      Kdb =kode_brg
      Do lkdb
      Set order to 1
      Set curs off
      Do pesan with katax
      Retu 2
   Case lastkey()=-8
      Nr=recno()
      Yt=' '
      Set colo to w+/r
      @row(),col() say &fds
      Set cent off
      Set curs on
      Do pesan with 'Cetak Label, 1=Hanya_1_item : '
      @24,55 get yt pict '!'
      Read
      If lastkey()#27 .and. yt='1' 
         Cuma1=.t.
         Do ctkhg
      Elseif lastkey()#27 .and. yt#'1'
         Cuma1=.f.
         If file('&nfl')
            Sele 99
            Useidx('&nfl')
            Go top
            Do ctkhg
            *
            Set curs on
            Tone(2000,4)
            Tone(1500,2)
            Yt=' '
            Do pesan1 with ''
            Do pesan with 'Pastikan Label Sudah diCetak Semua.   Mau diHapus? Y/T : '
            @24,67 get yt pict '!'
            Read
            If yt='Y' .and. lastkey()#27
               Close
               Dele File &nfl
            Endi
         Else
            Tone(2000,4)
            Tone(1500,2)
            Yt=' '
            Do pesan with 'Data Label Belum diSortir...!'
            Wait''
         Endi
      Endi
      Sele 3
      Do pesan  with katax
      Set curs off
      Set cent on
      Retu 1
   Case lastkey()=22
      Kdb =kode_brg
      Kdb2=kode_bar
      Nmb =nama_brg
      Hj  =harga_2
      Hj3 =harga_3
      Hj4 =harga_4
      Hj5 =harga_5
      Bts3=batas_3
      Bts4=batas_4
      Bts5=batas_5
      Set curs on
      Set colo to gr+*
      @row(),col() say &fds
      *
      If .not. file('&nfl')
         Copy stru to &nfl field KODE_BRG,KODE_BAR,NAMA_BRG,HARGA_2,;
                                      HARGA_3,HARGA_4,HARGA_5,BATAS_3,BATAS_4,BATAS_5
      Endi
      Sele 99
      Useidx('&nfl')
      Loca for kode_bar=kdb2
      If foun()
         Tone(2000,2)
         Tone(1500,1)
         Yt=' '
         Do pesan1 with ''
         Do pesan with 'Data Sudah Ada.   Mau diHapus? Y/T : '
         @24,57 get yt pict '!'
         Read
         If yt='Y' .and. lastkey()#27
            Dele
         Endi
      Elseif .not. foun()
         Appe blan
         Repl kode_brg with kdb,kode_bar with kdb2,nama_brg with nmb,harga_2 with hj,;
              harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5
         Commit
         Tone(1000,1)
         Tone(1500,2)
         Do pesan with rtri(nmb)+' / '+rtri(kdb)+'   telah dimasukan ke Daftar Label'
         Inkey(1)
      Endi
      Do pesan  with katax
      Set curs off
      Sele 3
      Retu 1
   Case lastkey()=7
      Set curs on
      Tone(2000,4)
      Tone(1500,2)
      Yt=' '
      Do pesan1 with ''
      Do pesan with 'Pastikan Label Sudah diCetak Semua.   Mau diHapus? Y/T : '
      @24,67 get yt pict '!'
      Read
      If yt='Y' .and. lastkey()#27
         Sele 99
         Close
         Dele File &nfl
      Endi
      Do pesan with katax
      Set curs off
      Sele 3
      Retu 1
   Case lastkey()=390
      Do postok
      Retu 0
   Othe
      Retu 1
Endc
Retu 0

Proc mulaictk
Do pesan with 'Mulai No:        *Hal:        *Bersambung Y/T:      *SubTot:           '
@24,13 get no  pict '9999'
@24,26 get hal pict '9999'
@24,50 get sambung pict '!'
Read
If lastkey()=27
   Retu
Endi
If sambung#'Y'
   Sambung='T'
Endi
If hal>1
   @24,63 get tot2 pict '@e 999,999,999'
   Read
Endi
Retu

Proc ctkstok
Stor 1 to no,hal,b,n,jml
Stor 0 to tot,tot2
Yt =' '
Do pesan with 'Dgn Harga Pokok? Y/T  '
Do whil .not. yt$'YT'
   @24,50 get yt pict '!'
   Read
   If lastkey()=27
      Exit
   Endi
Endd
Do mulaictk
If lastkey()=27
   Retu
Endi
Do cp
If cc=0                        
   Retu
Endi
Do koplst1
If yts='Y'
   Set filter to ((STOK_A+STOK_M)-(STOK_K+STOK_RB))#0 .and. &syaratx
   Go top
Endi
Do whil .not. eof()
   If nama_brg#spac(40)
      Stkx=((STOK_A+STOK_M) - (STOK_K+STOK_RB))
      Jml=tran(stkx,'@e 9999,999 ')
      If stkx#0
         If (stkx>0 .and. stkx<1)
            Jml=tran(stkx,'@e 99999.99 ')
         Elseif mod(stkx,int(stkx))>0 
            Jml=tran(stkx,'@e 99999.99 ')
         Endi
      Endi
      *
      Tot =tot+ (stkx*harga)
      Tot2=tot2+(stkx*harga)
      If yt='T'
         @b,0 say tran(no,'9999.  ')+left(kode_bar,13)+'  '+left(nama_brg,30)+' '+jml+satuan+tran(harga_2,'@e  9,999,999 ')
      Else
         @b,0 say tran(no,'9999.  ')+left(kode_bar,13)+'  '+left(nama_brg,30)+'  '+satuan+' '+jml+;
                  tran(harga,'@e   9,999,999  ')+tran(harga_2,'@e  9,999,999 ')+tran(stkx*harga,'@e 999,999,999')
      Endi
      B=b+1
      No=no+1
      N =n+1
      If n>xhal
         If yt='T'
            @b,0 say   repl('-',81)
         Else
            @b,0 say   repl('-',105)
            @b+1,0 say spac(77)+'Jumlah Hal.ini '+tran(tot,'@e 9999,999,999')
            @b+2,0 say repl('-',105)
            @b+3,0 say spac(77)+'Sub Total   Rp.'+tran(tot2,'@e 9999,999,999')
            @b+4,0 say repl('=',105)
         Endi
         @b+5,0 say ' '
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
         Tot=0
         Do koplst1
      Endi
   Endi
   Skip
Endd
If cc=0
   Retu
Endi
If yt='T'
   @b,0 say   repl('=',81)
Else
   If hal>1
      @b,0 say   repl('-',105)
      @b+1,0 say spac(77)+'Jumlah Hal.ini '+tran(tot,'@e 9999,999,999')
      @b+2,0 say repl('-',105)
      @b+3,0 say spac(77)+'T o t a l   Rp.'+tran(tot2,'@e 9999,999,999')
      @b+4,0 say repl('=',105)
   Else
      @b,0 say repl('-',105)
      @b+1,0 say spac(77)+'T o t a l   Rp.'+tran(tot2,'@e 9999,999,999')
      @b+2,0 say repl('=',105)
   Endi
Endi
@b+5,0 say ' '
Set prin off
Set devi to scre
Retu

Proc koplst1
Set prin on
Set devi to prin
If yt='T'
   Set marg to 10
   ?chr(27)+chr(33)+chr(1)
   ?aidi1
   ?repl('-',13)
   @prow()+1,0 say 'DAFTAR BARANG'+iif(judulx#' ',', '+judulx,'')
   @prow()+1,0 say 'TANGGAL : '+dtoc(tglx)
   @prow(),68  say 'Hal:'+tran(hal,'999')
   @prow()+1,0 say repl('=',81)
   @prow()+1,0 say '  No.  Barcode        Nama Barang                         Jml.Brg      @Harga'
   @prow()+1,0 say repl('-',81)
   xHal=60
Else
   Set marg to 15
   ?chr(27)+chr(33)+chr(4)
   ?aidi1
   ?repl('-',13)
   @prow()+1,0 say 'DAFTAR BARANG'+iif(judulx#' ',', '+judulx,'')
   @prow()+1,0 say 'TANGGAL : '+dtoc(tglx)
   @prow(),97 say 'Hal:'+tran(hal,'999')
   @prow()+1,0 say repl('=',105)
   @prow()+1,0 say '  No.  Barcode        Nama Barang                     Stn.       Qty.     @Pokok       @Jual      Jumlah'
   @prow()+1,0 say repl('-',105)
   xHal=50
Endi
B=prow()+1
N=1
Retu

Proc mkdb
Set filter to
Save scre to smkdb
If .not. keluar
   Do caribaru
Endi
Set order to 5
Totx=kdb
Nmb=spac(40)
Stor 0 to jml,jml1,jml2,jml3,jmlrb,jmlb,jmlj,hp,hj,hj21,hj22,hj3,hj32,hj4,hj42,hj5,hj52,bts3,bts4,bts5
Do whil .t.
   Sele 3
   Stor .f. to ada,hapus,sama
   Tambah=.t.
   Do koplht
   Set colo to b/bg,gr+/b
   @2,17 say rtri(nklb)+' ('+rtri(klb)+')'
   @4,17 say iif(ikdb#'1',kdb,rtri(klb))
   If keluar
      @4,17 say iif(ikdb#'1',kdb,rtri(klb))
      Set order to 1
      Seek kdb
   Else
      @2,17 get nklb pict '@!'
      Do pesan with 'Masukan Kelompok/Kategori.     ESC=Selesai'
      Read
      If lastkey()=27
         Sele 3
         Exit
      Endi
      *
      Sele 2
      Set order to 2
      Set filter to
      Seek nklb
      If foun()
         Klb =kode_klb
         Nklb=nama_klb
      Elseif .not. foun()
         Klp=.t.
         If nklb#spac(30)
            Set filter to AT(rtri(nklb),nama_klb)>0
         Endi
         Go top
         Do dafklb
      Endi
      If lastkey()=27 .or. klb=spac(5)
         Loop
      Endi
      Set colo to w+/b
      @2,17 say spac(30)
      @2,17 say rtri(nklb)+' ('+rtri(klb)+')'
      Set filter to
      Set colo to /bg
      *
      Sele 3
      Set order to 5
      Kdb2=spac(13)
      @1,10 to 1,69
      Set key  5 to dataht
      *@6,29+pjg say kdb pict iif(ikdb#'1','999999','9999')
      @4,53 get kdb2 pict '@!'
      Do pesan with 'Scan Barcode-nya atau Kosongkan untuk Barang Tanpa Barcode.     ESC=Batal'
      Read
      Set key  5 to 
      Totx=kdb
      @24,0
      Set colo to b/bg,gr+/b
      If hapus
         Hapus=.f.
         Ada =.t.
         @4,17+pjg get kdb pict iif(ikdb#'1','999999','9999')
         Read
      Endi              
      If lastkey()=27
         Loop
      Endi
      *
      If kdb2=spac(13)
         Kdb2=iif(ikdb#'1',kdb,rtri(rtri(klb)+kdb)+spac(13-len(rtri(rtri(klb)+kdb))))
         Kdb2=kdb2+spac(13-len(rtri(kdb2)))
         Sama=.t.
      Endi
      If .not. ada
         Seek kdb2
      Elseif ada
         Set order to 1
         Seek iif(ikdb#'1',kdb,rtri(rtri(klb)+kdb))
         Set order to 5
      Endi
   Endi
   If foun()
      If .not. keluar
         xUdahada=.t.
         Do lkdb
         Nmb=spac(40)
         Kdb2=spac(13)
         Stor 0 to jml1,jml2,jml3,jmlrb,jmlb,jmlj,hp,hj,hj21,hj22,hj3,hj32,hj4,hj42,hj5,hj52,bts3,bts4,bts5
         Loop
      Endi
      Do sudahada
      Tambah=.f.
   Endi
   Do whil .t.
      Set colo to b/bg,gr+/b
      @4,53 say kdb2
      @5,17 get nmb pict '@!'
      @6,17 say stb
      @6,29 say isi pict '@e 9999.99'
      @6,53 say st
      If .not. keluar
         @6,17  get stb pict '@!'
         @6,29  get isi pict '@e 9999.99'
         @6,53  get st  pict '@!'
      Endi
      @7,17  get hpb pict '@e 99,999,999.99'
      @7,53  say hp  pict '@e 99,999,999.99'
      Do pesan with 'Masukan Nama Barang, Kemasan dan Harga Beli.     ESC=Ulangi_dari_awal'
      Read
      If lastkey()=27
         Exit
      Endi
      Set colo to b/bg,gr+/b
      *
      Hp =iif(isi>0,hpb/isi,hpb)
      Pjk=iif(ppn>0,hp*(ppn/100),0)
      @6,17  say stb pict '@!'
      @6,29  say isi pict '@e 9999.99'
      @6,53  say st  pict '@!'
      @7,53 say hp     pict '@e 99,999,999.99'
      @8,47 say ppn    pict '@e 99'
      @8,53 say hp+pjk pict '@e 99,999,999.99'
      *
      Do whil .t.
         @8,69 get hjx pict '@e 9999.9999'
         Do pesan with 'Masukan Persentase Margin Harga Jual, dari Harga Beli + PPN'
         Read
         If lastkey()=27
            Exit
         Endi
         Set colo to b/bg,gr+/b
         @11,17 say hj   pict '@e 99,999,999'
         @12,17 say hj21 pict '@e 99,999,999'
         @10,46 say bts3 pict ' 999'
         @10,59 say bts4 pict ' 999'
         @10,72 say bts5 pict ' 999'
         @11,41 say hj3  pict '@e 9,999,999.99'
         @11,54 say hj4  pict '@e 9,999,999.99'
         @11,67 say hj5  pict '@e 9,999,999.99'
         @15,58 say dtoc(tgl0) +'  ='+tran(jml1 ,'@e 999999.99')
         @16,58 say dtoc(tgl1) +'  ='+tran(jml2 ,'@e 999999.99')
         @17,58 say dtoc(tgl2) +'  ='+tran(jml3 ,'@e 999999.99')
         @18,58 say dtoc(tglrb)+'  ='+tran(jmlrb,'@e 999999.99')
         @20,60 say st
         @20,69 say tran((jml1+jml2)-(jml3+jmlrb),'@e 999999.99')
         *
         Do hithj
         Set key -4 to xadadis
         @11,17 get hj   pict '@e 99,999,999' valid iif(hj>0,hj>=hp,hj=0)
         @12,17 get hj21 pict '@e 99,999,999' valid iif(hj21>0,hj21>=hp,hj21=0)
         Do pesan with 'Masukan Harga Jual.     F5=Edit_Harga_Grosir'
         Read
         Set key -4 to 
         Set colo to b/bg,gr+/b
         If lastkey()=27
            Loop
         Endi
         *
         If hp>0
            Hjx=((hj-(hp+pjk))/(hp+pjk))*100
            If hjx<=0
               Hjx=0
            Endi
         Elseif (hp=0 .and. hjx#0)
            Hjx=hjx
         Endi
         @8,69 say hjx pict '@e 9999.9999'
         *
         If adadis
            Adadis=.f.
            @10,46 get bts3 pict ' 999'
            @11,41 get hj3  pict '@e 9,999,999.99'
            *@12,41 get hj32 pict '@e 9,999,999.99'
            @10,59 get bts4 pict ' 999'
            @11,54 get hj4  pict '@e 9,999,999.99'
            *@12,54 get hj42 pict '@e 9,999,999.99'
            @10,72 get bts5 pict ' 999'
            @11,67 get hj5  pict '@e 9,999,999.99'
            *@12,67 get hj52 pict '@e 9,999,999.99'
         Endi
         *
         Sele 1
         Set key 28 to dafspl
         Set key -4 to xadadis
         @16,17 get ksp pict '@!'
         Do pesan with 'F1=Daftar Supplier   F5(Supl)=Edit Stok'
         Read
         Set key 28 to
         Set key -4 to
         Seek ksp
         Set colo to b/bg,gr+/b
         @17,17 say left(nama_spl,30)
         @18,17 say left(almt_spl,30)
         If hp#0
            Hjx=((hj-(hp+pjk))/(hp+pjk))*100
            If hjx<=0
               Hjx=0
            Endi
         Endi
         @24,0
         *
         Sele 3
         If adadis .or. tambah
            If tambah
               @15,69 get jml1 pict '@e 999999.99'
            Endi
            Read
            @20,69 say tran((jml1+jml2)-(jml3+jmlrb),'@e 999999.99')
         Endi
         Read
         If lastkey()=27
            Loop
         Endi
         Exit
      Endd
      If lastkey()=27
         Loop
      Endi
      Exit
   Endd
   *
   @24,0
   Yt='Y'
   Do pesan with 'Data Benar? Y/T : '
   @24,49 get yt pict '!'
   Read
   If lastkey()=27
      If keluar
         Keluar=.f.
         Exit
      Else
         Loop
      Endi
   Endi
   If yt#'Y'
      If ada
         Kdb=spac(7)
      Endi
      Loop
   Endi
   *
   If tambah
      Flok()
      Set order to 1
      Go bott
      Kdb=kode_brg
      Do hitkdb    &&&SEBLM SIMPAN, KODE DIHITUNG LG, BRGKL ADA KOMP.LAIN YG ENTRY JG
      If sama
         Kdb2=kdb
      Endi
      *
      Appe blan
      Repl kode_spl with ksp,kode_klb with klb,kode_brg with iif(ikdb#'1',kdb,rtri(klb)+kdb),kode_bar with kdb2,;
           nama_brg with nmb,satuan with st,harga_bl with hp,harga with hp,harga_2 with hj,harga_21 with hj21,;
           harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,harga_32 with hj32,harga_42 with hj42,harga_52 with hj52,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5,;
           satuan_b with stb,harga_b with hpb,isine_b with iif(isi<=0,1,isi),persen with iif(hjx>=999.999,999.999,hjx),tanggal_3 with tglx,;
           jam with time(),kassa with ksa
      Dis1=recno()
   Else
      Rlok()
      If stok_m#jml2
         Repl tanggal_1 with tglx
      Endi
      Repl kode_spl with ksp,nama_brg with nmb,satuan_b with stb,satuan with st,isine_b with isi,;
           harga_b with hpb,persen with hjx,harga with hp,harga_bl with hp,harga_2 with hj,harga_21 with hj21,;
           harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,harga_32 with hj32,harga_42 with hj42,harga_52 with hj52,;
           batas_3 with bts3,batas_4 with bts4,batas_5 with bts5
   Endi
   Repl stok_a with jml1,stok_m with jml2,stok_k with jml3
   *
   If ada
      Ada=.f.
      Kdb=totx
   Endi
   Ul()
   Commit
   *
   Kdb=totx
   Nr =recno()
   If tambah
      Save scre to sdaf
      Set colo to gr+/r
      @12,23 clea to 14,56
      @12,23 to 14,56
      @13,28 say 'Data Sudah di-Update...'
      Do pesan with 'ENTER=Lanjutkan'
      Wait''
      Rest scre from sdaf
      Go bott
      Kdb=kode_brg
      Do hitkdb
   Endi
   *
   If keluar
      Keluar=.f.
      Exit
   Endi
   *
   Nmb =spac(40)
   Kdb2=spac(13)
   Stor 0 to jml,jml1,jml2,jml3,jmlrb,jmlb,jmlj,hp,hj,hj21,hj22,hj3,hj32,hj4,hj42,hj5,hj52,bts3,bts4,bts5
   Pjg=iif(ikdb#'1',0,len(rtri(klb)))
Endd
Rest scre from smkdb
Retu

Proc lkdb
Save scre to slkdb
Do whil .t.
   Do koplht
   Do sudahada
   Klbx=kode_klb
   Sele 2
   Set order to 1
   Seek klb
   Nklb=nama_klb
   *
   Sele 3
   Set colo to b/bg,gr+/b
   @2,17 say rtri(nklb)+' ('+rtri(klb)+')'
   @4,17 say iif(ikdb#'1',kode_brg,rtri(rtri(klb)+kdb))
   @4,53 say kdb2
   @5,17 say nmb
   @6,17 say stb
   @6,29 say isi pict '@e 9999.99'
   @6,53 say st
   @7,17 say hpb pict '@e 99,999,999.99'
   @7,53 say hp pict '@e 99,999,999.99'
   @8,47 say ppn pict '@e 99'
   @8,53 say hp+pjk pict '@e 99,999,999.99'
   @8,70 say hjx pict '@e 999.9999'                     
   @11,17 say hj   pict '@e 99,999,999'
   @12,17 say hj21 pict '@e 99,999,999'
   @10,46 say bts3 pict ' 999'
   @10,59 say bts4 pict ' 999'
   @10,72 say bts5 pict ' 999'
   @11,41 say hj3  pict '@e 9,999,999.99'
   @11,54 say hj4  pict '@e 9,999,999.99'
   @11,67 say hj5  pict '@e 9,999,999.99'
   *@12,41 say hj32 pict '@e 9,999,999.99'
   *@12,54 say hj42 pict '@e 9,999,999.99'
   *@12,67 say hj52 pict '@e 9,999,999.99'
   @15,58 say dtoc(tgl0) +'  ='+tran(jml1 ,'@e 999999.99')
   @16,58 say dtoc(tgl1) +'  ='+tran(jml2 ,'@e 999999.99')
   @17,58 say dtoc(tgl2) +'  ='+tran(jml3 ,'@e 999999.99')
   @18,58 say dtoc(tglrb)+'  ='+tran(jmlrb,'@e 999999.99')
   @20,60 say st
   @20,69 say tran((jml1+jml2)-(jml3+jmlrb),'@e 999999.99')
   *
   Sele 1
   Seek ksp
   @16,17 say ksp
   @17,17 say left(nama_spl,30)
   @18,17 say left(almt_spl,30)
   *
   Sele 8
   Loca for kode=rak
   @20,17 say rak+'='+rtri(nama)+iif(rak2#'   ',' +'+rtri(rak2),'')+iif(rak3#'   ',' +'+rtri(rak3),'')
   *
   Sele 3
   If beli
      Set cent on
      Sele 9
      Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
      *
      Jml=0
      Beli=.f.
      Do whil .t.
         Set colo to w+/b
         @16,17 say kspx
         @19,17 say fkt
         @20,17 say tgl
         *
         Set colo to gr+/bg
         @1,30 say 'Entry Data Pembelian'
         @7,2 say  'Hg.Pokok Bsr@:'
         @11,2 say 'Harga Ecer..@:'
         @12,2 say '      Bakul.@:'
         @16,2 say 'Kode Supplier:'
         @19,2 say 'No.Faktur....:'
         @20,2 say 'Tanggal Fakt.:'
         @21,2 say 'Jumlah Rp....:'
         @13,48 say 'Jumlah Pembelian...='get jml pict '@e 999999.99'
         @7,17  get hpb pict '@e 99,999,999.99'
         @8,70  get hjx pict '@e 999.9999'                     
         Do pesan with 'Jumlah Barang, Harga Pokok & %Margin ?'
         Read
         If lastkey()=27
            Exit
         Endi
         *
         Set colo to b/bg,w+/b
         Set key -4 to xadadis
         Hp=hpb/isi
         Do hithj
         @7,53  say hp   pict '@e 99,999,999.99'
         @8,70  say hjx  pict '@e 999.9999'
         @11,17 get hj   pict '@e 99,999,999' valid iif(hj>0,hj>=hp,hj=0)
         @12,17 get hj21 pict '@e 99,999,999' valid iif(hj21>0,hj21>=hp,hj21=0)
         @21,16 say tran(jml*hp ,'@e 999,999,999')
         Do pesan with 'F5=Edit Harga Grosir'
         Read
         Set key -4 to
         If lastkey()=27
            Loop
         Endi
         *
         If hp#0
            Hjx=((hj-(hp+pjk))/(hp+pjk))*100
            If hjx<=0
               Hjx=0
            Endi
         Endi
         Set colo to b/bg,w+/b
         @8,69 say hjx pict '@e 9999.9999'
         @20,69 say tran((jml+jml1+jml2)-(jml3+jmlrb),'@e 999999.99')
         *
         If adadis
            Adadis=.f.
            @10,46 get bts3 pict ' 999'
            @11,41 get hj3  pict '@e 9,999,999.99'
            *@12,41 get hj32 pict '@e 9,999,999.99'
            @10,59 get bts4 pict ' 999'
            @11,54 get hj4  pict '@e 9,999,999.99'
            *@12,54 get hj42 pict '@e 9,999,999.99'
            @10,72 get bts5 pict ' 999'
            @11,67 get hj5  pict '@e 9,999,999.99'
            *@12,67 get hj52 pict '@e 9,999,999.99'
         Endi
         *
         Sele 1
         Set key 28 to dafspl
         @16,17 get kspx pict '@!'
         Do pesan with 'F1=Daftar Supplier'
         Read
         Set key 28 to
         Seek kspx
         Set colo to b/bg,w+/b
         Nsp=nama_spl
         @17,17 say left(nsp,30)
         @19,17 get fkt
         @20,17 get tglz
         *
         Sele 3
         Yt='Y'
         Do pesan with 'Data Benar? Y/T '
         @24,48 get yt pict '!'
         Read
         If lastkey()=27
            Loop
         Endi
         If yt='Y' .and. jml#0
            Rlok()
            Repl kode_spl with kspx,harga with hp,harga_2 with hj,harga_21 with hj21,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5,;
                 harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,harga_32 with hj32,harga_42 with hj42,harga_52 with hj52,;
                 harga_b with hpb,stok_m with (stok_m+jml),persen with hjx,tanggal_1 with tglz
            Ul()
            Commit
            *
            Sele 9
            Flok()
            Appe blan
            Repl tanggal with tglz,tanggal_2 with dtos(tglz),jam with time(),faktur with fkt,kode_spl with kspx,nama_spl with nsp,kode_klb with klbx,;
                 kode_brg with kdb,kode_bar with kdb2,nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,isine_b with isi,lokasi with 'T'
            Ul()
            Commit
            Close
         Endi
         diEdit=.t.
         Exit
      Endd
      Set cent off
      *
      Sele 3
      Keluar=.f.
      Exit
   Endi
   *
   Set colo to b
   If xUdahada
      xUdahada=.f.
      Set colo to b/bg
      @4,17 say iif(ikdb#'1',kode_brg,rtri(rtri(klb)+kdb))
      Set colo to w+*/r
      Tone(1500,1)
      Tone(2000,3)
      @24,0
      @24,32 say 'Data Sudah Ada!'
      Set colo to g
      Wait''
      Dis1=recno()
      Exit
   Endi
   Do pesan with 'ENTER=Edit'
   Wait''
   If lastkey()=13
      Rest scre from slkdb
      Keluar=.t.
      Do mkdb
   Endi
   Exit
Endd
Rest scre from slkdb
Retu

Proc koplht
@1,0 clea to 24,79
Set colo to /bg
@1,0 clea to 22,79
@1,0 to 22,79
@1,2 say  'Rec:'+ltri(str(recno()))+'-'+ltri(str(reccount()))
@2,2 say  'Klp. Barang..:'
@4,2 say  'Kode Barang..:                      BarCode......:'
@5,2 say  'Nama Barang..:'
@6,2 say  'Kemasan Besar:        Isi:          Kemasan Kecil:'
@7,2 say  'Hg.Beli Bsr.@:                      Hg.Beli Kcl.@:'
@8,2 say  '                                    HB.+PPN:    %:               +          %'
@10,2 say '                             BatasJual:--->         --->         --->'
@11,2 say 'Harga Bakul.@:            --->Hg.Jual@:'
@12,2 say '      Ecer..@:            --->        :'
@15,2 say '                                              Qty.Awal:'
@16,2 say 'Kode Supplier:                                    Beli:'
@17,2 say '                                                  Jual:'
@18,2 say '                                               ReturBl:'
@20,2 say '   Lokasi/Rak:                                Sisa Stok          =='
Retu

Proc sudahada
Klb=kode_klb
Ksp=kode_spl
Nmb=nama_brg
Stb=satuan_b
St =satuan
Isi=isine_b
Hpb=harga_b
Hp2=harga_b
Hp =harga
Hj =harga_2
Hj21=harga_21
Hj3 =harga_3
Hj4 =harga_4
Hj5 =harga_5
Hj32=harga_32
Hj42=harga_42
Hj52=harga_52
Bts3=batas_3
Bts4=batas_4
Bts5=batas_5
Tgl0=tanggal_0
Tgl1=tanggal_1
Tgl2=tanggal_2
Tglrb=tanggal_rb
Kdb2 =kode_bar
Hjx  =persen
Ppn  =pajak
Pjk  =hp*(ppn/100)
Jml1 =stok_a
Jml2 =stok_m
Jml3 =stok_k
Jmlrb=stok_rb
Kspx =ksp
*Rak  =kode_rak
*Rak2 =kode_rak2
Retu

Proc hithj
If ( hpb>0 .or. hp>0 ) .and. hjx#0 
   If hjx#0
      Hj=(hp+pjk)+((hp+pjk)*(hjx/100))
   Endi
Endi
Retu

Proc hitkdb
Kdb=val(rtri(kdb))+1
If ikdb#'1'
   Kdb='000000'+ltri(str(kdb))
   Kdb=right(kdb,6)
Else
   Kdb='0000'+ltri(str(kdb))
   Kdb=right(kdb,4)
Endi
Retu

Proc ctkhg
Sambung='T'
If cuma1=.f.
   Do pesan with 'Cetak Label, kertas bersambung Y/T:      Jumlah Baris per Hal:       ESC=Batal'
   @24,37 get sambung pict '!'
   @24,64 get brslabel pict '99'
   Read
   If lastkey()=27
      Retu
   Endi
   If brslabel<=0
      Brslabel=1
   Endi
Endi
Stor 1 to no,b,n
Do cp
If cc=0                        
   Retu
Endi
Kcl ='chr(27)+chr(33)+chr(5)'
Kcl2='chr(27)+chr(33)+chr(9)'
Bsr ='chr(27)+chr(33)+chr(41)'
Set prin on
Set devi to prin
Set marg to 1
?&kcl
If cuma1
   Do itemhg
   ?repl('-',52)
   ?' '
   Set prin off
   Set devi to scre
   Cuma1=.f.
   Retu
Endi
Do whil .not. eof()
   If nama_brg#spac(40)
      Do itemhg
   Endi
   If n>brslabel
      ?repl('-',158)+&kcl
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
         Set prin on
         Set devi to prin
      Else
         N=1
         Eject
      Endi
   Endi
   Skip
Endd
If cc=0
   Retu
Endi
?repl('-',158)+&kcl
?' '
Set prin off
Set devi to scre
Retu

Proc itemhg
If no=1
   cNmb11=nama_brg
   cKdb12=kode_bar
   cHg11 =harga_2
   cHg12 =harga_3
   cHg13 =harga_4
   cPc12 =batas_3
   cPc13 =batas_4
   If cPc12>0 .and. cHg12>0
      cPc12a='HARGA  @ : '  +&bsr+tran(chg11,'@e 9,999,999')
      cPc12b=spac(29)
      cPc12c='PER'+&bsr+tran(cpc12,'999:')+tran(chg11-cHg12,'@e 9,999,999')
      cPc12d=spac(29)
      cPc12e=spac(29)
      *
      If cPc13>0 .and. cHg13>0
         cPc12e='PER'+&bsr+tran(cpc13,'999:')+tran(chg11-cHg13,'@e 9,999,999')
      Endi
   Else
      cPc12a=spac(29)
      cPc12b='HARGA  @ : '  +&bsr+tran(chg11,'@e 9,999,999')
      cPc12c=spac(29)
      cPc12d=spac(29)
      cPc12e=spac(29)
   Endi
   No=no+1
Elseif no=2
   cNmb21=nama_brg
   cKdb22=kode_bar
   cHg21 =harga_2
   cHg22 =harga_3
   cHg23 =harga_4
   cPc22 =batas_3
   cPc23 =batas_4
   If cPc22>0 .and. cHg22>0
      cPc22a=' HARGA  @ : '+&bsr+tran(chg21,'@e 9,999,999')
      cPc22b=spac(30)
      cPc22c=' PER'+&bsr+tran(cpc22,'999:')+tran(chg21-cHg22,'@e 9,999,999')
      cPc22d=spac(30)
      cPc22e=spac(30)
      *
      If cPc23>0 .and. cHg23>0
         cPc22e=' PER'+&bsr+tran(cpc23,'999:')+tran(chg21-cHg23,'@e 9,999,999')
      Endi
   Else
      cPc22a=spac(30)
      cPc22b=' HARGA  @ : '+&bsr+tran(chg21,'@e 9,999,999')
      cPc22c=spac(30)
      cPc22d=spac(30)
      cPc22e=spac(30)
   Endi
   No=no+1
Elseif no=3
   cNmb31=nama_brg
   cKdb32=kode_bar
   cHg31 =harga_2
   cHg32 =harga_3
   cHg33 =harga_4
   cPc32 =batas_3
   cPc33 =batas_4
   If cPc32>0 .and. cHg32>0
      cPc32a=' HARGA  @ : '+&bsr+tran(chg31,'@e 9,999,999')
      cPc32b=spac(30)
      cPc32c=' PER'+&bsr+tran(cpc32,'999:')+tran(chg31-cHg32,'@e 9,999,999')
      cPc32d=spac(30)
      cPc32e=spac(30)
      *
      If cPc33>0 .and. cHg33>0
         cPc32e=' PER'+&bsr+tran(cpc33,'999:')+tran(chg31-cHg33,'@e 9,999,999')
      Endi
   Else
      cPc32a=spac(30)
      cPc32b=' HARGA  @ : '+&bsr+tran(chg31,'@e 9,999,999')
      cPc32c=spac(30)
      cPc32d=spac(30)
      cPc32e=spac(30)
   Endi
   No=no+1
Endi
If no>3 .or. cuma1
   If cuma1
      @prow()+1,0 say repl('-',52)+&kcl2
      @prow()+1,0 say spac(29)+' |'+spac(30)+' |'+spac(30)+' |'+&kcl2
      @prow()+1,0 say left(cnmb11,29)+&kcl2+' |'+&kcl2
      @prow()+1,0 say ckdb12+spac(17)+'|'
      @prow()+1,0 say cPc12a+&kcl2+' |'
      @prow()+1,0 say cPc12b+&kcl2+' |'
      @prow()+1,0 say cPc12c+&kcl2+' |'
      @prow()+1,0 say cPc12d+&kcl2+' |'
      @prow()+1,0 say cPc12e+&kcl2+' |'+&kcl
      *@prow()+1,0 say spac(29)+' |'+&kcl
   Else
      No=1
      @prow()+1,0 say repl('-',158)+&kcl2
      @prow()+1,0 say spac(29)+' |'+spac(30)+' |'+spac(30)+' |'+&kcl2
      @prow()+1,0 say left(cnmb11,29)+&kcl2+' | '+left(cnmb21,29)+&kcl2+' | '+left(cnmb31,29)+&kcl2+' |'
      @prow()+1,0 say ckdb12+spac(17)+'| '+ckdb22+spac(17)+'| '+ckdb32+spac(17)+'|'
      @prow()+1,0 say cPc12a+&kcl2+' |'+cpc22a+&kcl2+' |'+cpc32a+&kcl2+' |'
      @prow()+1,0 say cPc12b+&kcl2+' |'+cpc22b+&kcl2+' |'+cpc32b+&kcl2+' |'
      @prow()+1,0 say cPc12c+&kcl2+' |'+cpc22c+&kcl2+' |'+cpc32c+&kcl2+' |'
      @prow()+1,0 say cPc12d+&kcl2+' |'+cpc22d+&kcl2+' |'+cpc32d+&kcl2+' |'
      @prow()+1,0 say cPc12e+&kcl2+' |'+cpc22e+&kcl2+' |'+cpc32e+&kcl2+' |'+&kcl
      *@prow()+1,0 say spac(29)+' |'+spac(30)+' |'+spac(30)+' |'
      N=n+1
   Endi
Endi
Retu

* EOF LST1
