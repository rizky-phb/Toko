// ============================================================
// File    : MPB.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
***URGEN: HJ TIDAK MENYIMPAN, HB BLM TERMASUK PJK...
***BRGKL ADA PERUBAHAN RECORD DI STOK.DTA, GANTI SAJA "SEEK KDB/KDBX"
* PEMBELIAN
SELECT 1
Useidx('KASSA.SET')
LOCATE FOR no_kassa=ksa
Jk=cari_kode
Close
*
SELECT 2
*
SELECT 3
*
SELECT 4
*
SELECT 5
Ksa=GetEnv("KASSA")
Nfx='BELI-'+ksa+'.DTA'
Batal=.f.
Useidx('&nfx')
If batal
   Batal=.f.
   Close all
   RETURN
ENDIF
Jdtbl=reccount()
SELECT 6
*
SELECT 7
*
SELECT 8
Uselan('DATA.SET','','','','','')
Ikdb =id_kodebrg
pExim=path_exim
Close
*
STORE .f. to ada,tambah,hapus,ambilpo,daripo,baru,diedit,isilagi,adaitem,dis_on
STORE tglx to tgl,tgl2
STORE SPACE(5) to ksp,nsp,klb,kdbx,nmbx,stm
STORE ' ' to konsi,flt
inBeli=.t.
Tgl2=ctod('  -  -    ')
Fkt =SPACE(10)
Lks ='T'
Nrf =0
Slhtbrg=''
DO WHILE .t.
   @1,0 CLEAR to 24,79
   STORE 0 to jml,jml2,hp,hb,hj,hj21,hj22,dis1,dis2,dis3,dis4,dis5,pjg,stk,pru,jwk,bts3,bts4,bts5,hj3,hj4,hj5
   STORE 0 to jhg,tot,totor,disr1,disr2,disr3,dis0,dis02,dis03,pjk,rpjk,pjk1,hbx,hbnet,nr,nrpo
   SELECT 5
   Go top
   If kode_spl#SPACE(5)
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      SET COLOR TO gr+/b
      @1,26 say 'INPUT PEMBELIAN YANG TERTUNDA'
      SET COLOR TO /bg
      @2,0 to 2,79 doub
      B =3
      No=1
      DO WHILE .not. eof() .and. kode_spl#SPACE(5)
         Hitdis()
         Jml  =jumlah
         Hp   =(jml*harga)
         Pjk1 =((pajak/100) *(hp-dis1))
         Jhg  =hbnet
         *
         @b,0 say tran(no,'99. ')+left(nama_brg,25)+' '+left(kode_brg,6)+tran(jumlah,'@e  9999.99 ')+left(satuan,4)+tran(harga,'@e 9,999,999 ')+tran(diskon_1,'@e 99.99 ')+tran(pajak,' 99=')+tran(jhg,'@e 999,999,999')
         B =b+1
         No=no+1
         If b>13
            B=3
         ENDIF
         Skip
      ENDDO
      @b,0 to b,79
      Go top
      Yt=' '
      SET COLOR TO gr+/b
      @b+1,22 CLEAR to b+5,58
      @b+1,22 to b+5,58
      @b+2,24 say 'Keterangan, Tanggal: '+dtoc(tgl)
      @b+3,24 say '            No.Fkt.: '+fkt
      @b+4,24 say '            Suplier: '+ksp
      Tone(1500,2)
      Tone(2500,4)
      DO WHILE .not. yt$'YT'
         @b+5,26 say 'Masih mau diLanjutkan? Y/T :'get yt pict '!'
         Read
      ENDDO
      If yt='T'
         Tone(1500,4)
         Tone(2500,2)
         SET COLOR TO gr+/b
         @b+1,22 CLEAR to b+5,58
         @b+1,22 to b+4,58
         @b+2,25 say 'Data Pemb. diatas akan dihapus!'
         @b+3,25 say 'Mohon di-koreksi Sisa Stok-nya.'
         SET COLOR TO /bg
         @b+4,30 say 'Tekan Sembarang Tombol'
         Wait''
         REPLACE all kode_spl with SPACE(5),rectran with 0,id_data with ' '
      Else
         STORE .t. to diEdit,tambah,isilagi
         Do mpbx
      ENDIF
   Elseif kode_spl=SPACE(5)
      REPLACE all kode_spl with SPACE(5),rectran with 0,id_data with ' '
   ENDIF
   *
   SET COLOR TO /bg
   @1,0 CLEAR to 24,79
   @22,1 to 22,78
   @1,1 say REPLACE(REPLACE(gl,8)+chr(194),2)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,12)
   @2,1 say 'Tanggal '+tl+'Jt.Tempo'+tl+'No.Faktur '+tl+'Kode '+tl+'Nama Supplier'+SPACE(17)+tl+'    Rp.Total'
   @3,1 say REPLACE(REPLACE(gl,8)+chr(197),2)+REPLACE(gl,10)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,12)
   *
   SELECT 7
   Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
   If adaitem=.f.
      Go bott
      If recno()>17
         Skip -17
      Else
         Go top
      ENDIF
   Else
      Go nrf
   ENDIF
   Nrf=recno()
   Tgl=tglx
   diPilih=.f.
   Pesanx='F1=Cari     F4=Tambah     F5=Edit_Faktur     F8=Hapus'
   Do pesan with pesanx
   SET CENTURY OFF
   DECLARE field[9],bts[9],frm[9]
   Field[1]="dtoc(TANGGAL)+tl+dtoc(TANGGAL_3)+tl+left(FAKTUR,10)+tl+KODE_SPL+tl+left(NAMA_SPL,30)+tl+tran((RP_TOTAL-RP_DISKON)+RP_PAJAK,'@e 9999,999,999')"
   Frm[1]=''
   Bts[1]=''
   DbEdit(4,1,21,78,field,'ffkt',frm,'','',bts)
   *
   If lastkey()=27 .and. (.not. tambah)
      Exit
   ENDIF
   Tambah=.f.
ENDDO
SET CURSOR OFF
SET CENTURY ON
CLOSE all
RELEASE ada,batal,adadis,ubahjml,tambah,hapus,baru,tgl2,fkt,ksp,klb,lks,slhtbrg
RELEASE kdb,nmb,jml,isi,st,stb,tot,tot2,dis1,dis2,dis3,dis4,dis5,dis0,dis02,dis03,pjk,rpjk,hj,hp,hpb,byr
RELEASE hp,hb,hj,dis,dis1,pjk1,pjg,stk,hj21,hj22,pr,pr2,hj3,hj4,hj5,bts3,bts4,bts5
RETURN

Function ffkt
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      SET COLOR TO w+/b
      @23,0
      @23,1  say LTRIM(str(recno()))+'/'+LTRIM(str(reccount()))
      @23,18 say kode_spl
      @23,26 say nama_spl+' , Fkt: '+faktur
      Do pesan with pesanx
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=28
      SET CURSOR ON
      Ksp=SPACE(5)
      @23,0
      Do pesan with 'Masukan Kode Spl:      '
      @24,46 get ksp
      Read
      If lastkey()#27
         SELECT 1
         Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
         Seek ksp
         If .not. FOUND()
            Do dafspl
         ENDIF
         *
         SELECT 7
         Set order to 2
         Seek ksp
      ENDIF
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 2
   Case lastkey()=-3 .or. lastkey()=22
      Save scre to sdafx
      SET CURSOR ON
      SET CENTURY ON
      STORE tglx to tgl,tgl2
      Tgl2=ctod('  -  -    ')
      Fkt=SPACE(10)
      STORE SPACE(5) to ksp,klb
      STORE .f. to diEdit
      Tambah=.t.
      Do mpbx
      Rest scre from sdafx
      *
   Case lastkey()=-4
      Save scre to sdafx
      SET CURSOR ON
      SET CENTURY ON
      Tgl  =tanggal
      Tgl2 =tanggal_3
      Fkt  =faktur
      Ksp  =kode_spl
      Pjk  =pajak
      Ppn  =pjk
      Dis0 =diskon
      Lks  =lokasi
      Byr  =rp_bayar
      Jwk  =tgl2-tgl
      Dis03=rp_diskont
      Nrf =recno()
      STORE .t. to diEdit,tambah,adaitem
      Do mpbx
      Rest scre from sdafx
      *
   Case lastkey()=-7
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      SET COLOR TO gr+*/b
      @row(),col() say &fds
      Yt=' '
      SET CURSOR ON
      SET COLOR TO /bg
      Do pesan with 'DiHapus? Y/T'
      DO WHILE .not. yt$'YT'
         @24,47 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         If .not. deleted()
            Do pesan with 'Tunggu, sedang dihapus'
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip
            *
            SELECT 3
            Uselan('STOK.DTA','STOK.NDX','','','','')
            SELECT 4
            Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
            Seek dtos(tgl)+ksp+fkt
            DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
               Kdb=kode_brg
               Jml=jumlah
               Isi=1  &&isine_b
               Lks=lokasi
               *
               Rlok()
               Dele
               Ul()
               *
               SELECT 3
               Seek kdb
               Rlok()
               If lks='G'
                  REPLACE stok_mg with stok_mg-(jml*isi)
               Else
                  REPLACE stok_m with stok_m-(jml*isi)
               ENDIF
               Ul()
               *
               SELECT 4
               Skip
            ENDDO
         ENDIF
      ENDIF
      SELECT 3
      COMMIT
      Close
      *
      SELECT 4
      COMMIT
      Close
      *
      Do pesan with pesanx
      SET CURSOR OFF
      SELECT 7
      RETURN 2
   Case lastkey()=13
      Tgl =tanggal
      Fkt =faktur
      Ksp =kode_spl
      Nsp =nama_spl
      B   =6
      No  =1
      Terus=.t.
      STORE 0 to tot,jdt,xhal
      Save scre to sdaf
      SET COLOR TO bg+/b
      @4,1 say dtoc(tanggal)+tl+dtoc(tanggal_3)+tl+left(faktur,10)+tl+kode_spl+tl+left(nama_spl,30)+tl+tran((rp_total-rp_diskon)+rp_pajak,'@e 9999,999,999')
      SET COLOR TO /bg
      @5,0 to 5,79 doub
      @6,0 CLEAR to 23,79
      @20,0 to 20,79
      @21,1 say 'Disc:'
      @22,1 say 'PPN.:'
      @23,1 say 'DisT:'
      @21,33 say 'Bayar: Rp'
      @22,33 say 'Retur: Rp'
      @21,59 say 'Keterangan:'
      SET COLOR TO /bg
      @21,6 say tran(diskon,'@e 99.99% Rp')+tran(rp_diskon,'@e 99,999,999.99')
      @22,6 say tran(pajak ,'@e 99.99% Rp')+tran(rp_pajak ,'@e 99,999,999.99')
      @23,6 say '       Rp'+tran(rp_diskont,'@e 99,999,999.99')
      @21,42 say tran(rp_bayar,'@e 99,999,999')
      @22,42 say tran(rp_retur,'@e 99,999,999')
      @23,33 say left(nama_ksr,15)
      @22,59 say keterangan
      *
      SELECT 4
      Uselan('BELIIN.DTA','BELIIN1.NDX','','','','')
      Seek dtos(tgl)+ksp+fkt
      Nr=recno()
      ***HITUNG JML.DATA U/ HAL
      DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
         Hitdis()
         Jml  =jumlah
         Hp   =(jml*harga)
         Pjk1 =((pajak/100) *(hp-disr1))
         Jhg  =hbnet
         Tot  =tot+jhg
         Jdt=jdt+1
         Skip
      ENDDO
      *
      Go nr
      DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
         Hitdis()
         Jml  =jumlah
         Hp   =(jml*harga)
         Pjk1 =((pajak/100) *(hp-disr1))
         Jhg  =hbnet
         *
         If terus
            Kdby=iif(len(RTRIM(kode_bar))>7,left(kode_bar,3)+'x'+right(RTRIM(kode_bar),3),left(kode_bar,7))
            @b,0 say tran(no,'999. ')+left(nama_brg,26)+' '+kdby+tran(jumlah,'@e 99999 ')+left(satuan,4)+tran(harga,'@e 9,999,999 ')+tran(harga_2,'@e 9,999,999=')+tran(jhg,'@e 99,999,999')
            B =b+1
            No=no+1
         ENDIF
         If b>19
            Do pesan with 'Pgln=Lanjut     ESC=Selesai'
            Wait''
            If lastkey()=27
               Terus=.f.
            ENDIF
            SET COLOR TO /bg
            B=6
         ENDIF
         Skip
      ENDDO
      @b,0 CLEAR to 19,79
      Do pesan with 'ESC=Selesai     P=Cetak_Nota_Kecil'
      Wait''
      If lastkey()=80 .or. lastkey()=112
         Yt  =' '
         Sambung='Y'
         STORE 1 to n,no,hal
         SET CURSOR ON
         Do cp
         If cc=0
            RETURN 0
         ENDIF
         *
         SELECT 99
         Useidx('KASSA.SET')
         LOCATE FOR no_kassa=ksa
         Idcut=id_cut
         Jn  =jark_nota
         Close
         *
         SELECT 4
         STORE 0 to jml1
         SET PRINTER ON
         SET DEVICE TO PRINTER
         Set marg to 0
         ?chr(27)+chr(33)+chr(5)
         @prow()+1,0 say ' NOTA PEMBELIAN'
         @prow()+1,0 say ' SUPPLIER: '+RTRIM(nsp)+' ('+RTRIM(ksp)+')'
         @prow()+1,0 say ' TANGGAL.: '+dtoc(tgl)+'     NO: '+fkt
         @prow()+1,0 say REPLACE('=',40)
         @prow()+1,0 say ' No.  Nama Barang              Jumlah Rp'
         @prow()+1,0 say REPLACE('-',40)
         B =prow()+1
         N =1
         *
         Go nr
         DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
               Hitdis()
               Dis1=diskon_1
               Dis2=diskon_2
               Dis3=diskon_3
               Dis4=diskon_rp
               Dis5=diskon_rp2
               Jml =jumlah
               Hp  =harga
               Hj  =harga_2
               Jml1=jml1+(jml*hp)
               *
               @b,0 say tran(n,'999.  ')+left(nama_brg,30)
               B=b+1
               @b,7 say tran(jml,'@e       99999 ')+tran(hp,'@e 9,999,999 =')+tran(jml*hp,'@e 99,999,999')
               B =b+1
               N =n+1
               Skip
         ENDDO
         @b,0    say REPLACE('-',40)
         @b+1,19 say 'Jumlah Rp:'+tran(jml1,'@e 999,999,999')
         @b+1+(2+jn),0 say ' '
         If idcut='Y'
            @prow()+1,0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
         ENDIF
         @prow()+1,0 say ' '
         SET PRINTER OFF
         SET DEVICE TO SCREEN
      ENDIF
      *
      Rest scre from sdaf
      SET CURSOR OFF
      Do pesan with pesanx
      SELECT 7
      If rp_total#tot
         Rlok()
         REPLACE rp_total with tot
         Ul()
         COMMIT
      ENDIF
      RETURN 1
   Case lastkey()=390 .and. (fds='TANGGAL' .or. fds='TANGGAL_3' .or. fds='left(FAKTUR,10)')
      B   =row()
      Kl  =col()
      Tgl2=tanggal_3
      Ksp =kode_spl
      STORE tanggal to tgl,tglz
      STORE faktur  to fkt,fktz
      SET CURSOR ON
      SET CENTURY ON
      If fds='TANGGAL_3'
         @b,kl get tgl2
         Do pesan with 'Masukan Tanggal Jt.Tempo'
         Read
         If lastkey()#27
            Rlok()
            REPLACE tanggal_3 with tgl2
            Ul()
            COMMIT
            *
         ENDIF
      Elseif fds='TANGGAL'
         @b,kl get tgl
         Do pesan with 'Masukan Tanggal Faktur'
         Read
         If lastkey()#27
            Rlok()
            REPLACE tanggal with tgl
            Ul()
            COMMIT
            *
            SELECT 4
            Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
            Set filter to kode_spl=ksp .and. faktur=fkt
            Flok()
            REPLACE all tanggal with tgl,tanggal_2 with dtos(tgl) for tanggal=tglz
            Ul()
            COMMIT
            *
            SELECT 7
         ENDIF
      Elseif fds='left(FAKTUR,7)'
         @b,kl get fkt
         Do pesan with 'Masukan Nomor Faktur'
         Read
         If lastkey()#27
            Rlok()
            REPLACE faktur with fkt
            Ul()
            COMMIT
            *
            SELECT 4
            Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
            Set order to 2
            Set filter to kode_spl=ksp .and. tanggal=tgl
            Flok()
            REPLACE all faktur with fkt for faktur=fktz
            Ul()
            COMMIT
            *
            SELECT 7
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET CENTURY OFF
      SET CURSOR OFF
      RETURN 2
   Case lastkey()=84 .or. lastkey()=116
      If fds='TANGGAL_3'
         Set order to 3
      Elseif fds='TANGGAL'
         Set order to 1
      Else
         Set order to 2
      ENDIF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE mpbx
STORE 0 to jml,tot,tot2,dis,ppn,dis1,hj,hp,hpb
STORE .f. to hapus,batal,ada,adadis,ubahjml,dis_on
DO WHILE .t.
   SET COLOR TO /bg
   @1,0 CLEAR to 24,79
   @1,0 say REPLACE(gl,13)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,5)+REPLACE(chr(194)+REPLACE(gl,8),2)+chr(194)+REPLACE(gl,11)
   @2,0 say 'PLU/BarCode  '+tl+'Nama Barang'+SPACE(19)+tl+' Qty.'+tl+'@Hg.Beli'+tl+'@Hg.Jual'+tl+' Jumlah Rp.'
   @3,0 say REPLACE(gl,13)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,5)+REPLACE(chr(197)+REPLACE(gl,8),2)+chr(197)+REPLACE(gl,11)
   B=4
   N=1
   Do kolombl
   *
   @18,9 get tgl
   @19,9 get fkt pict '@!'
   Do pesan with 'ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   ENDIF
   If fkt=SPACE(10)
      Fkt='TUNAI     '
   ENDIF
   SELECT 1
   Uselan('SUPP.DTA','SUPP.NDX','SUPP2.NDX','','','')
   *
   SET COLOR TO /bg
   @18,9 say tgl
   @19,9 say fkt
   @20,9 get ksp pict '@!'
   Do pesan with ''
   Read
   If lastkey()=27
      Loop
   ENDIF
   If ksp=SPACE(5)
      Do dafspl
   ENDIF
   Set order to 1
   Seek ksp
   If .not. FOUND()
      Set order to 2
      Seek RTRIM(ksp)
      Do dafspl
   ENDIF
   If lastkey()=27
      Loop
   ENDIF
   Set order to 1
   Nsp  =nama_spl
   Jmlso=rp_saldo
   Tglso=tanggal
   If diEdit=.f.
      Jwk=jk_waktu
   ENDIF
   *
   SET COLOR TO /bg
   @20,9 say RTRIM(nsp)+'/'+ksp
   If fkt#'TUNAI     '
      @21,9 get jwk pict '99'
      Read
   ENDIF
   @22,9  say lks pict '!'
   @20,62 say pjk pict '@e 99.99'
   *Read
   Ppn =pjk
   Tgl2=tgl+jwk
   If jwk=0
      @21,9 say 'TUNAI'+SPACE(11)
   Else
      @21,9  say jwk pict '99'
      @21,18 say tgl2
   ENDIF
   Lks=iif(lks#'G','T','G')
   If lastkey()=27
      Loop
   ENDIF
   *
   If diEdit=.f.
      Yt='Y'
      Do pesan with 'Data Benar? Y/T: '
      @24,48 get yt pict '!'
      Read
      If lastkey()=27 .or. yt#'Y'
         Loop
      ENDIF
   ENDIF
   *
   SELECT 7
   Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
   Seek dtos(tgl)+ksp+fkt
   If FOUND()
      Tot  =0
      Pjk  =pajak
      Rpjk =rp_pajak
      Dis02=rp_diskon
      Dis03=rp_diskont
      Byr  =rp_bayar
      Jwk  =tanggal_3-tanggal
      Konsi=id_konsi
   Elseif .not. FOUND()
      Flok()
      APPEND BLANK
      REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),tanggal_3 with tgl2,faktur with fkt,;
           kode_spl with ksp,nama_spl with nsp,id_konsi with konsi,lokasi with lks
      Ul()
   ENDIF
   Nrf=recno()
   Close
   *
   SELECT 3
   Uselan('STOK.DTA','STOK.NDX','STOK4.NDX','STOK3.NDX','STOK2.NDX','STOK5.NDX')
   *
   SELECT 4
   Uselan('BELIIN.DTA','BELIIN1.NDX','BELIIN2.NDX','BELIIN3.NDX','','')
   Seek dtos(tgl)+ksp+fkt
   If FOUND()
      STORE .t. to isilagi,ada
      STORE 1 to n,no
      DO WHILE (.not. eof()) .and. ( tanggal=tgl .and. kode_spl=ksp .and. faktur=fkt )
         Klb =kode_klb
         Kdb =kode_brg
         Kdb2=kode_bar
         Nmb =nama_brg
         St  =satuan
         Isi =isine_b
         Jml =jumlah
         Hb  =harga
         Hj  =harga_2
         Dis1=diskon_1
         Dis2=diskon_2
         Dis3=diskon_3
         Dis4=diskon_rp
         Dis5=diskon_rp2
         Ppn =pajak
         Nrx =recno()
         *
         Pjk1=((pajak/100)*((jumlah*harga)-dis1))
         Jhg =(((jumlah*harga)-dis1)+pjk1)
         Tot=tot+jhg
         *
         SELECT 5
         If n=1
            Go top
         ENDIF
         REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,nama_spl with nsp,kode_klb with klb,;
              kode_brg with kdb,kode_bar with kdb2,nama_brg with nmb,satuan with st,jumlah with jml,harga with hb,harga_2 with hj,isine_b with isi,;
              diskon_1 with dis1,diskon_2 with dis2,diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5,pajak with ppn,rectran with nrx,id_data with '1'
         Skip
         N =n+1
         No=no+1
         *
         SELECT 4
         Skip
      ENDDO
   ENDIF
   *
   @18,69 say tran(tot,'@e 999,999,999')
   Do kolombl
   Ppn =pjk
   Disi=.f.
   DO WHILE .t.
      Yt =' '
      Klb =SPACE(5)
      Kdb =SPACE(7)
      Kdb2=SPACE(13)
      Nmb =SPACE(40)
      Isi =1
      STORE 'PCS  ' to st,stb
      STORE 0 to jml,hp,hb,hj,hj21,hj22,dis,dis1,pjk1,pjg,stk,pru,bts3,bts4,bts4,hj3,hj4,hj5
      If isilagi
         Do hpsbeli
         Isilagi=.f.
         Jml=0
      ENDIF
      DO WHILE .t.
         SELECT 3
         Set key  -3 to xadadis  &&UNT.KODE BARU
         Set key   5 to hapus
         Set key 390 to ambilpo
         SET COLOR TO gr+/b
         @18,69 say tran(tot,'@e 999,999,999')
         SET COLOR TO /bg
         @b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(5)+REPLACE(tl+SPACE(8),2)+tl+SPACE(11)
         @b,0 get kdb2 pict '@!S13'
         Do pesan with 'ESC=Selesai     '+chr(24)+'=Koreksi     F4=Kode_Barang_Baru'
         Read
         Set key  -3 to
         Set key   5 to
         Set key 390 to
         If lastkey()=27
            Exit
         ENDIF
         If n>jdtbl
            Do pesan2 with 'Jumlah Data Tidak Boleh Lebih dari '+LTRIM(str(jdtbl))+' Baris...!'
            Loop
         ENDIF
         @24,0
         If hapus
            Edititm=.t.
            Do hpsbeli
            Hapus =.f.
            Loop
         ENDIF
         If ambilpo
            SET CENTURY OFF
            SELECT 8
            Uselan('FAKTURO.DTA','FAKTURO1.NDX','FAKTURO2.NDX','','','')
            Set filter to kode_spl=ksp
            LOCATE FOR kode_spl=ksp
            Do dafpo
            Close
            *
            SET CENTURY ON
            SELECT 3
            If lastkey()#27
               Do hpsbeli
            ENDIF
            AmbilPo=.f.
            DariPo =.t.
            Loop
         ENDIF
         If baru
            Tambah=.t.
            Kdb =SPACE(7)
            Nmb =SPACE(40)
            Isi =1
            Pru =0
            STORE 'PCS  ' to stb,st
            STORE .f. to baru,adadis
            Save scre to slhtbrg
            *
            DO WHILE .t.
               SELECT 2
               Uselan('JENIS.DTA','JENIS.NDX','JENIS2.NDX','','','')
               SET COLOR TO w+/br
               @12,17 CLEAR to 17,63
               @12,17 to 17,63
               @12,34 say 'DATA BRG.BARU'
               @13,19 say 'Kode Klp.:'get klb pict '@!'
               @14,19 say 'Kode Brg.:           BarCode:'
               @15,19 say 'Nama Brg.:'
               @16,19 say 'Kemasan..:'
               Do pesan with 'Kode Kelompok?     ENTER=Daftar_Kelompok'
               Read
               If lastkey()=27
                  Exit
               ENDIF
               If klb=SPACE(5)
                  Do dafklb
                  If lastkey()=27
                     Loop
                  ENDIF
               ENDIF
               Seek klb
               Nklb=nama_klb
               Kdp =kode_dep
               Sama=.f.
               Do caribaru
               *
               SELECT 3
               Set order to 1
               SET COLOR TO bg+/b
               @13,30 say klb+'= '+left(nklb,25)
               @14,30 say iif(ikdb#'1',kdb,RTRIM(klb)+kdb)
               Seek iif(ikdb#'1',kdb,RTRIM(RTRIM(klb)+kdb))
               If FOUND()
                  Do sdhada
                  Do lhtbrg2
                  Loop
               ENDIF
               Kdb2=iif(ikdb#'1',kdb+SPACE(13-len(RTRIM(kdb))),RTRIM(klb)+kdb+SPACE(13-len(RTRIM(klb)+RTRIM(kdb))))
               @14,49 get kdb2 pict '@!'
               Read
               Set order to 5
               Seek kdb2
               If FOUND()
                  Do sdhada
                  Do lhtbrg2
                  Loop
               ENDIF
               If RTRIM(kdb)=RTRIM(kdb2)
                  Sama=.t.
               ENDIF
               @15,30 get nmb pict '@!S33'
               @16,30 get st pict '@!'
               Read
               *
               Yt='Y'
               @17,30 say 'Data Benar? Y/T: '
               @17,47 get yt pict '!'
               Read
               If (lastkey()=27 .or. yt#'Y') .or. nmb=SPACE(40)
                  Kdb=SPACE(7)
                  Loop
               ENDIF
               Stb=st
               Isi=1
               *
               Flok()
               Set order to 1
               Go bott
               Kdb=kode_brg
               Do hitkdb
               If sama
                  Kdb2=kdb
               ENDIF
               *
               APPEND BLANK
               REPLACE kode_klb with klb,kode_brg with iif(ikdb#'1',kdb,RTRIM(klb)+RTRIM(kdb)),kode_bar with kdb2;
                    nama_brg with nmb,satuan_b with stb,isine_b with isi,satuan with st,persen with pru
               Ul()
               COMMIT
               *
               Exit
            ENDDO
            Rest scre from slhtbrg
            If lastkey()=27
               Loop
            ENDIF
            SET COLOR TO /bg
         Else
            If kdb2=SPACE(13)
               @b,14 get nmb pict '@!S30'
               Do pesan with 'Masukan Nama Barang'
               Read
               If nmb=SPACE(40)
                  Loop
               Else          
                  Carikode=.f.
                  Set order to 2
                  Seek RTRIM(nmb)
                  Do dafbrg
               ENDIF
            Else
               STORE kdb2 to kdb,kdbx,nmb
               Carikode=.t.
               Set order to 5
               Seek kdb
               If .not. FOUND()
                  Seek RTRIM(kdb)
                  If FOUND()
                     Do dafbrg
                  Elseif .not. FOUND()
                     Carikode=.f.
                     Set order to 2
                     Do dafbrg
                  ENDIF
               ENDIF
            ENDIF
            If lastkey()=27 .or. kdb=SPACE(7) .or. kdbx=SPACE(13)
               Loop
            ENDIF
            Klb=kode_klb
            Nr =recno()
            *
            SELECT 2
            Uselan('JENIS.DTA','JENIS.NDX','','','','')
            Seek klb
            Nklb=nama_klb
            Close
            *
            SELECT 3
         ENDIF
         If lastkey()=27
            Loop
         ENDIF
         Do sdhada
         Do lhtbrg
         Brs3()
         *
         Do isihj
         If lastkey()=27
            Loop
         ENDIF
         *
         Do pesan with 'ESC=Koreksi   ENTER=Simpan'
         Wait''
         Rest scre from slhtbrg
         If lastkey()=27
            Loop
         ENDIF
         *
         Brs3()
         Tot =tot + jhg
         *
         If jml#0
            Rlok()
            Totx=0
            Stkl=(stok_a+stok_m)-stok_k
            REPLACE harga_bl with hb,harga with hp,harga_b with hpb,stok_lama with stkl
            REPLACE harga_2 with hj,harga_21 with hj21,harga_22 with hj22,persen with pru
            REPLACE harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5
            Ul()
            COMMIT
            *
            SELECT 5
            If n=1
               Go top
            ENDIF
            REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),faktur with fkt,kode_spl with ksp,nama_spl with nsp,kode_klb with klb,;
                 kode_brg with kdb,kode_bar with kdb2,nama_brg with nmb,satuan with st,jumlah with jml,harga with hb,harga_2 with hj,isine_b with isi,;
                 diskon_1 with dis1,diskon_2 with dis2,diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5,pajak with pjk,id_data with ' '
            Skip
         ENDIF
         *
         B=b+1
         N=n+1
         STORE 0 to jml,dis1,pjk1,jhg
         Kdb =SPACE(7)
         Kdb2=SPACE(13)
         Nmb =SPACE(40)
         If b>16
            Scroll(4,0,16,79,1)
            B=16
         ENDIF
      ENDDO
      *
      If .not. batal
         DO WHILE .t.
            Batal=.f.
            @18,69 say tran(tot,'@e 999,999,999')
            @19,62 get dis0 pict '@e 99.99'
            Do pesan with 'Masukan Diskon'
            Read
            If dis0=0
               @19,69 get dis02 pict '@e 999,999,999'
               Read
               Dis0 =iif(tot#0,(dis02/tot)*100,0)
            Else
               Dis02=tot*(dis0/100)
            ENDIF
            Tot3=tot-dis02
            @19,62 say tran(dis0 ,'@e 99.99')
            @19,69 say tran(dis02,'@e 999,999,999')
            *
            *@20,62 get pjk pict '@e 99.99'
            *Do pesan with 'Masukan PPN'
            *Read
            *If pjk=0
            *   @20,69 get rpjk pict '@e 999,999,999'
            *   Read
            *   Pjk =iif(tot3#0,(rpjk/tot3)*100,0)
            *Else
            *   Rpjk=tot3*(pjk/100)
            *Endi
            *@21,69 get dis03 pict '@e 999,999,999'
            *Do pesan with 'Masukan Potongan Tunai'
            *Read
            *
            Tot3=((tot-dis02)+rpjk)-dis03
            Byr =0
            SET COLOR TO w+/b
            @20,62 say tran(pjk  ,'@e 99.99')
            @20,69 say tran(rpjk ,'@e 999,999,999')
            @21,69 say tran(dis03,'@e 999,999,999')
            @22,69 say tran(tot3 ,'@e 999,999,999')
            @23,69 say byr pict  '@e 999,999,999'
            *If jwk#0
            *   Byr=0
               @23,69 get byr pict '@e 999,999,999'
               Do pesan with 'Masukan Jumlah Pembayaran'
               Read
               If lastkey()=27
                  Exit
               ENDIF
            *Endi
            @24,0
            Do pesan with 'Kekurangan = Rp '+LTRIM(tran(tot3-byr,'@e 999,999,999'))+'    ESC=Koreksi   ENTER=Simpan'
            Wait''
            Exit
         ENDDO
         If lastkey()=27
            Isilagi=.t.
            Loop
         ENDIF
         Batal=.f.
         Do pesan with 'Tunggu, sedang diSimpan'
         *
         SELECT 5
         Go top
         DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_spl=ksp )
            Klb =kode_klb
            Kdb =kode_brg
            Kdb2=kode_bar
            Nmb =nama_brg
            Stb =satuan
            Jml =jumlah
            Isi =isine_b
            Hb  =harga
            Hj  =harga_2
            Dis1=diskon_1
            Dis2=diskon_2
            Dis3=diskon_3
            Dis4=diskon_rp
            Dis5=diskon_rp2
            Ppn =pajak
            Nrx =rectran
            Adaitem=.t.
            *
            If id_data=' '
               SELECT 4
               Flok()
               APPEND BLANK
               REPLACE tanggal with tgl,tanggal_2 with dtos(tgl),jam with time(),faktur with fkt,kode_spl with ksp,nama_spl with nsp,kode_klb with klb,;
                    kode_brg with kdb,kode_bar with kdb2,nama_brg with nmb,satuan with stb,jumlah with jml,harga with hb,harga_2 with hj,isine_b with isi,;
                    diskon_1 with dis1,diskon_2 with dis2,diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5,pajak with ppn,id_konsi with konsi,lokasi with lks
               Ul()
               *
               Isi=1   &&&DINETRAL-KAN LAGI JADI 1, KRN PAKE SATUAN TERKECIL
               *
               SELECT 3
               Set order to 1
               Seek kdb
               Rlok()
               REPLACE kode_spl with ksp,harga_bl with (hb*isi),harga with hb,harga_b with (hb*isine_b),diskon_1 with dis1,diskon_2 with dis2,diskon_3 with dis3,;
                    diskon_rp with dis4,diskon_rp2 with dis5,pajak with ppn,harga_2 with hj,persen with ((hj-hb)/hb)*100,stok_m with stok_m+(jml*isi),tanggal_1 with tgl
               Ul()
               *
            ENDIF
            *
            SELECT 5
            REPLACE kode_spl with SPACE(5),rectran with 0,id_data with ' '
            Skip
         ENDDO
         *
         SELECT 3
         COMMIT
         *
         SELECT 7
         Uselan('FAKTURH.DTA','FAKTURH1.NDX','FAKTURH2.NDX','FAKTURH3.NDX','','')
         Go nrf
         Nr=recno()
         Rlok()
         REPLACE tanggal_3 with tgl2,rp_total with tot,rp_bayar with byr,diskon with dis0,;
              pajak with pjk,rp_diskon with dis02,rp_pajak with rpjk,rp_diskont with dis03
         If byr>=tot3
            REPLACE tanggal_4 with tgl
         ENDIF
         Ul()
         COMMIT
         Close
      Else
         Batal=.f.
         N=1
         SELECT 5
         REPLACE all kode_spl with SPACE(5),rectran with 0,id_data with ' '
         COMMIT
         Go top
      ENDIF
      STORE 0 to jml,tot,tot2,tot3,dis0,dis02,dis03,pjk,hj
      Do kolombl
      Exit
   ENDDO
   Ksp=SPACE(5)
   Fkt=SPACE(10)
   Tgl2=ctod('  -  -    ')
   Ada=.f.
   SELECT 3
   COMMIT
   Close
   *
   SELECT 4
   COMMIT
   Close
   *
   If daripo
      Daripo=.f.
      SELECT 8
      Uselan('FAKTURO.DTA','FAKTURO1.NDX','FAKTURO2.NDX','','','')
      Go nrpo
      Ksp=kode_spl
      Npo=faktur
      Nfo='PO\'+right(RTRIM(ksp),3)+npo+'.DTA'
      *
      Rlok()
      REPLACE tanggal_3 with tgl
      Ul()
      COMMIT
      Close
      *
      SELECT 9
      Useidx('&nfo')
      Set filter to jumlah#0
      REPLACE all diambil with chr(251) for dipilih#' '
      Close
      *
   ENDIF
   *
   Exit
ENDDO
RETURN

***KOLOM JUAL/BELI
PROCEDURE kolombl
DO WHILE b<=16
   @b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(5)+REPLACE(tl+SPACE(8),2)+tl+SPACE(11)
   B =b+1                         
ENDDO
@b,0 say REPLACE(gl,13)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,5)+REPLACE(chr(193)+REPLACE(gl,8),2)+chr(193)+REPLACE(gl,11)
@18,0 say 'Tanggal:          '+SPACE(38)+'Jumlah Rp..:'+SPACE(12)
@19,0 say 'Faktur.:          '+SPACE(38)+'Disct      %'+SPACE(12)
@20,0 say 'Suplier:          '+SPACE(38)+'P P N      %'+SPACE(12)
@21,0 say 'J.Tempo:    hari, '+SPACE(38)+'Disct.Tunai:'+SPACE(12)
@22,0 say 'Lokasi.:    [G/T]         Hutang=Rp'+SPACE(21)+'Total  Rp..:'+SPACE(12)
@23,0 say '                                   '+SPACE(21)+'Bayar  Rp..:'+SPACE(12)
SET COLOR TO /bg
@18,9 say tgl
@19,9 say fkt
@20,9 say RTRIM(nsp)+'/'+ksp
@21,9 say jwk pict '99'
@21,18 say tgl2
@18,69 say tran(tot,'@e 999,999,999')
@20,62 say pjk pict '@e 99.99'
B=4
RETURN

*** PROSEDUR HAPUS TRANSAKSI 
PROCEDURE hpsbeli
SELECT 5
Set filter to ( tanggal=tgl .and. faktur=fkt .and. kode_spl=ksp )
Go top
If kode_spl=SPACE(5)
   Set filter to
   Hapus=.f.
   RETURN
ENDIF
SET COLOR TO w+/r
@2,33 say chr(254)+' KOREKSI '+chr(254)
Pesanx='ENTER=Edit Kode/Nama/Qty/Harga     F6=Diskon/All     ESC=Selesai'
Do pesan with pesanx
Hbx  ="(JUMLAH*HARGA)-DISKON_RP"
Disr1="(&HBX) * (DISKON_1/100)"
Disr2="(&HBX - &DISR1) * (DISKON_2/100)"
Disr3="(&HBX - &DISR1 - &DISR2)  * (DISKON_3/100)"
Hbnet="&HBX - ( &DISR1 + &DISR2 + &DISR3 ) - DISKON_RP2"
*
DECLARE field[8],bts[8],frm[8]
Field[1]="left(KODE_BAR,13)+tl+left(NAMA_BRG,30)+tl+tran(JUMLAH,'99999')+tl+tran(HARGA,'@e 9999,999')+tl+tran(HARGA_2,'@e 9999,999')+tl+tran(JUMLAH*HARGA,'@e 999,999,999')"
Frm[1]=''
Keyboard chr(4)+chr(4)
STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8]
DbEdit(4,0,16,79,field,'fhpb',frm,'','',bts)
*
SET COLOR TO /bg
@2,33 say SPACE(11)
Do kolombl
Tot=0
N  =1
Set filter to
Go top
DO WHILE .not. eof() .and. ( kode_spl=ksp .and. faktur=fkt )
   Hitdis()
   Jhg=hbnet
   Brs2()
   B  =b+1
   N  =n+1
   Tot=tot+jhg
   If b>16
      Scroll(4,0,16,79,1)
      B=16
   ENDIF
   Skip
ENDDO
Hapus=.f.
Nmb =SPACE(40)
Kdb =SPACE(7)
Kdb2=SPACE(13)
Jml =0
RETURN

Function fhpb
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   Hitdis()
   @17,0 to 17,79
   SET COLOR TO /bg
   @18,0 CLEAR to 23,79                                    
   @18,1  say 'BarCode..:'+SPACE(49)+'Total Rp:' &&+'Dis%1:        DisRp1:'
   @19,1  say 'Nama Brg.:'+SPACE(41) &&+'Dis%2:        DisRp2:'
   @20,1  say 'Kemasan..:'+SPACE(41) &&+'Dis%3:              '
   SET COLOR TO b/bg
   @18,12 say kode_bar+' / '+kode_brg
   @19,12 say nama_brg
   @20,12 say satuan
   *@18,58 say tran(diskon_1  ,'@e 999.99')
   *@19,58 say tran(diskon_2  ,'@e 999.99')
   *@20,58 say tran(diskon_3  ,'@e 999.99')
   *@18,73 say tran(diskon_rp ,'@e 999999')
   *@19,73 say tran(diskon_rp2,'@e 999999')
   @18,69 say tran(tot,'@e 999,999,999')
   Do pesan with pesanx
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=-4
   B   =row()
   Nrx =rectran
   Kdb =kode_brg
   Kdbx=kode_bar
   Nmb =nama_brg
   *
   SET CURSOR ON
   If fds='left(KODE_BAR,13)'
      @b,0 get kdbx pict '@!'
      Do pesan with 'Masukan Kode/BarCode'
   Else
      @b,14 get nmb pict '@!'
      Do pesan with 'Masukan Nama Barang'
   ENDIF
   Read
   *
   If lastkey()#27
      SELECT 3
      Set order to 1
      Seek kdb
      Rlok()
      REPLACE kode_bar with kdbx,nama_brg with nmb
      Ul()
      COMMIT
      *
      If diEdit
         SELECT 4
         Go nrx
         Rlok()
         REPLACE kode_bar with kdbx,nama_brg with nmb
         Ul()
         COMMIT
      ENDIF
      *
      SELECT 5
      REPLACE kode_bar with kdbx,nama_brg with nmb
      COMMIT
      *
   ENDIF
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 1
Elseif lastkey()=13
   B   =row()
   Nr  =recno()
   Nrx =rectran
   Kdb =kode_brg
   Kdb2=kode_bar
   Kdbx=kode_bar
   Nmb =nama_brg
   Nmbx=nama_brg
   Stm =stok_max
   St  =satuan
   Isi =isine_b
   Hb  =harga
   Dis1=diskon_1
   Dis2=diskon_2
   Dis3=diskon_3
   Dis4=diskon_rp
   Dis5=diskon_rp2
   Ppn =iif(pajak>0,pajak,pjk)
   Hp  =harga
   Hpb =harga*isine_b
   STORE jumlah to jml,jml2
   *
   SET CURSOR ON
   SELECT 3
   Set order to 1
   Seek kdb
   Do sdhada
   *
   Do lhtbrg
   Do isihj
   *
   Rest scre from slhtbrg
   *
   SELECT 5
   If lastkey()#27
      REPLACE jumlah with jml,harga with hb,harga_2 with hj,diskon_1 with dis1,diskon_2 with dis2,;
           diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5,pajak with ppn
      COMMIT
      Sum (jumlah * (harga-dis1+pjk1)) to tot
      *
      Do cekedit
   ENDIF
   Do pesan with pesanx
   Go nr
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-5
   B   =row()
   Nr  =recno()
   Nrx =rectran
   Dis1=0
   Yt  =' '
   *
   SET CURSOR ON
   Do pesan with 'Semua item Kolom Diskon% '+yt+' , akan di-ubah ke:'+SPACE(20)+'ESC=Batal'
   @24,28 get yt pict '9' valid yt $'123'
   @24,48 get dis1 pict '@e 999.99'
   Read
   *
   If lastkey()#27
      REPLACE all diskon_&yt with dis1
      COMMIT
      *
   ENDIF
   Do pesan with pesanx
   Go nr
   SET CURSOR OFF
   RETURN 2
Else
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

*** PROSEDUR HAPUS / EDIT ITEM
PROCEDURE xhapusi
Hapusi=.t.
Keyboard chr(13)
RETURN

PROCEDURE sdhada
Klb =kode_klb
Kdb =kode_brg
Kdb2=kode_bar
Nmb =nama_brg
Stb =satuan_b
St  =satuan
Isi =isine_b
Hb  =harga_bl
Hpb =harga_b
Hp  =harga
Pru =persen
Hj  =harga_2
Hj21=harga_21
Hj3 =harga_3
Hj4 =harga_4
Hj5 =harga_5
Bts3=batas_3
Bts4=batas_4
Bts5=batas_5
Dis1=diskon_1
Dis2=diskon_2
Dis3=diskon_3
Dis4=diskon_rp
Dis5=diskon_rp2
Ppn =iif(pajak>0,pajak,pjk)
Stk =iif(lks='G',((stok_ag+stok_mg)-stok_kg)/isi,((stok_a+stok_m)-stok_k)/isi)
RETURN

PROCEDURE isihj
STORE 0 to pjk1
@b,45 get jml pict '@e 99999'
@b,51 get hb  pict '@e 9999999.99'
Do pesan with 'Masukan Jml/Harga/Diskon Barang'
Read
If lastkey()=27
   RETURN
ENDIF
*
Hbx  =(jml*hb)-dis4
Disr1=( hbx * (dis1/100))
Disr2=( (hbx-disr1) * (dis2/100) )
Disr3=( (hbx-disr1-disr2) * (dis3/100) )
Hbnet=( hbx-(disr1+disr2+disr3)-dis5 )
*
Pjk1=hb*(ppn/100)
Hp  =hb
Jhg =hbnet
Hpl =hp
Hjl =hj
Hjl2=hj21
Hpb =hp*isi
Brs3()
SET COLOR TO b/bg
@21,11 say tran(hp,'@e 99,999,999.99')
@22,6  say tran(ppn,'@e 99')
@23,11 say tran(hp+pjk1,'@e 99,999,999.99')
*
If hj=0
   Hj=( (hp+pjk1) *( 1 +(pru/100)) )
ENDIF
Set key -4 to xadadis
@b,60 get hj pict '@e  9999999'
Do pesan with "Masukan Hg.Jual, Pokok= "+LTRIM(tran(hpl,'@e 9,999,999.99'))+"   Jual= "+LTRIM(tran(hjl,'@e 9,999,999'))+"     F5=Hg.Grosir"
Read
Set key -4 to
If hp>0 .and. hj>0
   Pru=(((hj-(hp+pjk1))/(hp+pjk1))*100)
ENDIF
SET COLOR TO b/bg
@21,37 say tran(pru,'@e 999.9999 %')
If adadis
   STORE .f. to adadis,baru
   @23,37 get hj21 pict '@e 9,999,999'
   @21,57 get bts3 pict '9999'
   @21,64 get hj3  pict '@e 9,999,999.99'
   @22,57 get bts4 pict '9999'
   @22,64 get hj4  pict '@e 9,999,999.99'
   @23,57 get bts5 pict '9999'
   @23,64 get hj5  pict '@e 9,999,999.99'
   Read
ENDIF
RETURN

PROCEDURE lhtbrg
Save scre to slhtbrg
@17,0 to 17,79
SET COLOR TO /bg
@18,0 CLEAR to 23,79                                    
@18,1  say 'BarCode..:'+SPACE(17)+'PLU:'+SPACE(20)
@19,1  say 'Nama Brg.:'+SPACE(41)
@20,1  say 'Sisa Stok:'+SPACE(46)+'-----Hg.Grosir-----'
@21,1  say 'HB.Kecil.:'+SPACE(17)+'% Margin:'+SPACE(11)+SPACE(14)+'='
@22,1  say 'PPN= 00 %:'+SPACE(17)+'Hg.Jual.:'+SPACE(11)+SPACE(14)+'='
@23,1  say 'HB + PPN.:'+SPACE(17)+'   Jual2:'+SPACE(11)+SPACE(14)+'='
SET COLOR TO b/bg
@18,12 say kdb2
@18,33 say kdb
@19,12 say nmb
@20,11 say tran(stk,'@e 999,999.99 ')+stb+' ( 1'+RTRIM(stb)+' = '+LTRIM(str(isine_b))+RTRIM(st)+' )'
@21,11 say tran(hp ,'@e 99,999,999.99')
@22,6  say tran(ppn,'@e 99')
@23,11 say tran(hp+(hp*(ppn/100)),'@e 99,999,999.99')
@21,37 say tran(pru ,'@e 999.9999 %')
@22,37 say tran(hj  ,'@e 99,999,999')
@23,37 say tran(hj21,'@e 99,999,999')
@21,57 say tran(bts3,'9999')
@22,57 say tran(bts4,'9999')
@23,57 say tran(bts5,'9999')
@21,64 say tran(hj3 ,'@e 9,999,999.99')
@22,64 say tran(hj4 ,'@e 9,999,999.99')
@23,64 say tran(hj5 ,'@e 9,999,999.99')
SET COLOR TO /bg
RETURN

PROCEDURE lhtbrg2
SET COLOR TO gr+/b
@14,49 say kdb2 
@15,30 say left(nmb,32)
@16,30 say st
Tone(1500,1)
Tone(2000,3)
@17,30 say '  Data Sudah Ada!  '
Wait''
Kdb=SPACE(7)
Nmb=SPACE(40)
Isi=1
STORE 'PCS  ' to stb,st
STORE 0 to pru
RETURN

FUNCTION brs2
Kdby=kode_bar
SET COLOR TO /bg
@b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(5)+REPLACE(tl+SPACE(8),2)+tl+SPACE(11)
@b,0  say left(kdby,13)
@b,14 say left(nama_brg,30)
@b,45 say tran(jumlah,'@e 99999')
@b,51 say tran(harga,'@e 9999,999')
@b,60 say tran(harga_2,'@e 9999,999')
@b,69 say tran(jhg,'@e 999,999,999')
RETURN(.t.)

FUNCTION brs3
Kdby=kdb2
SET COLOR TO /bg
@b,0 say SPACE(13)+tl+SPACE(30)+tl+SPACE(5)+REPLACE(tl+SPACE(8),2)+tl+SPACE(11)
@b,0 say left(kdby,13)
@b,14 say left(nmb,30)
@b,45 say tran(jml ,'@e 99999')
@b,51 say tran(hb,'@e 9999,999')
@b,60 say tran(hj,'@e 9999,999')
@b,69 say tran(jhg,'@e 999,999,999')
RETURN(.t.)

PROCEDURE cekedit
If diEdit
   SELECT 3
   Rlok()
   REPLACE harga_bl with hb,harga with hp,harga_b with hpb,stok_m with (stok_m-(jml2*isi))+(jml*isi)
   REPLACE harga_2 with hj,harga_21 with hj21,harga_22 with hj22,persen with pru
   REPLACE harga_3 with hj3,harga_4 with hj4,harga_5 with hj5,batas_3 with bts3,batas_4 with bts4,batas_5 with bts5
   Ul()
   COMMIT        
   *
   SELECT 4
   Go nrx
   Rlok()
   REPLACE jumlah with jml,harga with hb,harga_2 with hj,diskon_1 with dis1,diskon_2 with dis2,;
        diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5,pajak with ppn
   Ul()
   COMMIT
ENDIF
SELECT 5
RETURN

PROCEDURE caribaru
Ada=.f.
If ikdb#'1'
   SELECT 3
   Set order to 1
   Go bott
   Kdb=kode_brg
   Do hitkdb
   RETURN
ENDIF
SELECT 2
If .not. eof()
   Nr =recno()
   Pjg=len(RTRIM(klb))
   *
   Skip
   If eof()
      Go bott
      *
      SELECT 3
      Set order to 3
      Seek klb
      If FOUND()
         Go bott
         Kdb=SUBSTR(kode_brg,pjg+1,7)
      Elseif .not. FOUND()
         Kdb='0'
      ENDIF
      *
   Elseif .not. eof()
      DO WHILE .not. eof()
         Klbx=kode_klb
         *
         SELECT 3
         Set order to 3
         Seek klbx
         If FOUND()
            Skip -1
            Kdb=SUBSTR(kode_brg,pjg+1,7)
            Ada=.t.
            Exit
         Elseif .not. FOUND()
            Kdb='0'
         ENDIF

         SELECT 2
         Skip
      ENDDO
      *
      SELECT 2
      Go nr
      If ada=.f.
         Kdb='0'
      ENDIF
      *
      SELECT 3
   ENDIF
   Do hitkdb
Elseif eof()
   Kdb=RTRIM(klb)+SPACE(7)
   Kdb=SPACE(7-len(RTRIM(klb)))
   Pjg=len(RTRIM(klb))
ENDIF
RETURN

PROCEDURE xadadis
STORE .t. to adadis,baru
Keyboard chr(13)
RETURN

PROCEDURE hapus
Hapus=.t.
Keyboard chr(13)
RETURN

PROCEDURE dis_on
SET COLOR TO w+/r
If dis_on
   Dis_on=.f.
   @23,47 say ' Dis:OFF '
Elseif dis_on=.f.
   Dis_on=.t.
   @23,47 say ' Dis:ON '
ENDIF
SET COLOR TO /bg
RETURN

PROCEDURE ambilpo
Ambilpo=.t.
Keyboard chr(13)
RETURN

PROCEDURE dafpo
Set key 28 to
Save scre to sdaf
SET COLOR TO /bg
@1,14 CLEAR to 24,79
@22,14 to 22,79
@1,14 say REPLACE(REPLACE(gl,8)+chr(194),2)+REPLACE(gl,5)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,11)
@2,14 say 'Tanggal '+tl+'Tgl.Krm '+tl+'Nomor'+tl+'Nama Supplier'+SPACE(17)+tl+'   Rp Total'
@3,14 say REPLACE(REPLACE(gl,8)+chr(197),2)+REPLACE(gl,5)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,11)
*
Pesanx='ENTER=Ambil Semua item Barang'
Do pesan with pesanx
SET COLOR TO /bg
DECLARE field[8],bts[8],frm[8]
Field[1]='TANGGAL'
Field[2]='TANGGAL_3'
Field[3]='FAKTUR'
Field[4]='NAMA_SPL'
Field[5]='RP_TOTAL'
Frm[1]='99-99-9999'
Frm[2]='99-99-9999'
Frm[3]='@!'
Frm[4]='@!'
Frm[5]='@e 999,999,999'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=tl
DbEdit(4,14,20,79,field,'ffktox',frm,'','',bts)
*
Rest scre from sdaf
RETURN

Function ffktox
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Nrpo=recno()
      Npo=faktur
      Nfo='PO\'+right(RTRIM(ksp),3)+npo+'.DTA'
      No =0
      @1,0 
      @1,0 say 'Supl: '+RTRIM(nama_spl)+' - '+kode_spl+'   Tgl: '+dtoc(tanggal)+' / '+faktur
      *
      SELECT 9
      Useidx('&nfo')
      Set filter to jumlah#0 .and. diambil=' ' 
      Sum (jumlah*harga) to totor for dipilih#' '
      Go top
      *
      SET COLOR TO w+/r
      @2,0 to 2,79
      @2,0 say 'BarCode'
      @2,13 say chr(194)+'Nama Barang'
      @2,49 say 'Order'+chr(194)
      @2,60 say 'Harga'+chr(194)
      @2,68 say 'Jumlah Rp'+chr(194)
      @2,79 say chr(251)
      SET COLOR TO w+/b
      @23,0
      @23,54 say 'Jumlah= Rp'+tran(totor,'@e 9,999,999,999')
      SET COLOR TO /bg
      @22,0 to 22,79
      Pesanx='SPASI=Pilih item    ENTER/DEL=Pilih semua/Batal     F9=Batal_Masuk     F12=Ambil'
      Do pesan with pesanx
      DECLARE field[8],bts[8],frm[8]
      Field[1]='KODE_BAR'
      Field[2]='left(NAMA_BRG,35)'
      Field[3]='JUMLAH'
      Field[4]='HARGA'
      Field[5]='(JUMLAH*HARGA)'
      Field[6]='DIPILIH'
      Frm[1]='@!'
      Frm[2]='@!'
      Frm[3]='@e 99999'
      Frm[4]='@e 9,999,999'
      Frm[5]='@e 999,999,999'
      Frm[6]='@!'
      Bts[1]=tl
      Bts[2]=tl
      Bts[3]=tl
      Bts[4]=tl
      Bts[5]=tl
      Bts[6]=tl+' '
      Keyboard chr(4)
      DbEdit(3,0,21,79,field,'fisipo2',frm,'','',bts)
      *
      RETURN 0
   Othe
      RETURN 1
ENDCASE
RETURN 0

Function fisipo2
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=28
   If flt=' '
      Flt="dipilih#' '"
      Set filter to jumlah#0 .and. diambil=' ' .and. &flt
      LOCATE FOR &flt
   Else
      Set filter to jumlah#0 .and. diambil=' ' 
      Flt=' '
   ENDIF
   RETURN 2
Elseif lastkey()=390
   SET CURSOR ON
   Yt='Y'
   Do pesan with 'Proses? Y/T :'
   @24,47 get yt pict '!'
   Read
   If yt='Y'
      Useidx('&nfo')
      Set filter to jumlah#0 .and. diambil=' ' .and. dipilih#' '
      Do ambilitem
      *
   ENDIF
   RETURN 0
Elseif lastkey()=13
   SET CURSOR ON
   Yt='Y'
   Do pesan with 'Pilih Semua item ? Y/T :'
   @24,53 get yt pict '!'
   Read
   If yt='Y'
      REPLACE all dipilih with chr(251)
      *
   ENDIF
   Go top
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=7
   SET CURSOR ON
   Yt='Y'
   Do pesan with 'Batalkan Pilihan ? Y/T :'
   @24,53 get yt pict '!'
   Read
   If yt='Y'
      REPLACE all dipilih with ' '
      *
   ENDIF
   Go top
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=-8
   SET CURSOR ON
   Yt='Y'
   Do pesan with 'Batalkan Semua yg Masuk Pembelian ? Y/T :'
   @24,61 get yt pict '!'
   Read
   If yt='Y'
      Set filter to jumlah#0
      REPLACE all diambil with ' ',dipilih with ' '
      *
   ENDIF
   Go top
   Do pesan with pesanx
   SET CURSOR OFF
   RETURN 2
Elseif lastkey()=32
   REPLACE dipilih with iif(dipilih=' ',chr(251),' ')
   Totor=iif(dipilih=' ',totor-(jumlah*harga),totor+(jumlah*harga))
   SET COLOR TO w+/b
   @23,54 say 'Jumlah= Rp'+tran(totor,'@e 9,999,999,999')
   SET COLOR TO /bg
   Do pesan with pesanx
   RETURN 1
Else
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

PROCEDURE ambilitem
Go top
DO WHILE .not. eof()
   Klb =kode_klb
   Kdb =kode_brg
   Kdbx=kode_bar
   Nmb =nama_brg
   St  =satuan
   Isi =isine_b
   Hp  =harga
   Jml =jumlah
   Dis1=diskon_1
   Dis2=diskon_2
   Dis3=diskon_3
   Dis4=diskon_rp
   Dis5=diskon_rp2
   No  =no+1
   diEdit =.f.
   *
   SELECT 3
   Set order to 5
   Seek kdbx
   Hj=harga_2
   *
   SELECT 5
   If no=1
      Go top
   Elseif no>jdtbl
      APPEND BLANK
   ENDIF
   REPLACE tanggal with tgl,faktur with fkt,nomr_po with npo,kode_spl with ksp,kode_klb with klb,;
        kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,isine_b with isi,jumlah with jml
   REPLACE harga with hp,harga_2 with hj,diskon_1 with dis1,diskon_2 with dis2,diskon_3 with dis3,diskon_rp with dis4,diskon_rp2 with dis5
   Skip
   *
   SELECT 9
   Skip
ENDDO
SELECT 9
Close
RETURN

PROCEDURE xubahjml   && UBAH ke QTY.KECIL
Jml=jml/isi
Keyboard chr(13)
RETURN

* EOF MPB
