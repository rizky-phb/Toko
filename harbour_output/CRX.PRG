// ============================================================
// File    : CRX.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* PROSEDUR-2 BANTUAN
PROCEDURE cekdata
*
Uselan('DATA.SET','','','','','')
xPos =RTRIM(keterangan)
dTgl =day(tgl)
*
Do bacafile
Uselan('JUAL.DTA','','','','','')
If .not. file('&nf0')
   Copy stru to &nf0
   CLOSE all
   B=23
   Indx('&nf0','dtos(TANGGAL)+KODE_LGN+FAKTUR','&nf1',2)
   Indx('&nf0','KODE_BAR+dtos(TANGGAL)','&nf2',2)
   Indx('&nf0','dtos(TANGGAL)+KODE_KSR+FAKTUR','&nf3',2)
ENDIF
CLOSE all
*
Nf0='KAS-'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+right(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
If .not. file('&nf0')                 
   Useidx('KAS.DTA')
   Copy stru to &nf0
   Close all
   B=23
   Indx('&nf0','dtos(TANGGAL)+KODE_POS+ID_POS','&nf1',2)
   Indx('&nf0','KODE_POS+dtos(TANGGAL)','&nf2',1)
ENDIF
*
Brs1 ="(jumlah * (harga_2- ((harga_2*(diskon/100))+diskon_rp)))"
Brs13="(jml * (hj- ((hj*(dis1/100))+dis2)))"
RETURN

FUNCTION brs2
Kdby=kode_bar
SET COLOR TO &warna3
@b,0 say SPACE(8)+tl+SPACE(30)+tl+SPACE(12)+tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(10)
@b,0 say iif(len(RTRIM(kdby))>=9,left(kdby,3)+'x'+right(RTRIM(kdby),4),left(kdby,8))
@b,9 say left(nama_brg,30)
@b,40 say tran(jumlah,'@e 9999.99 ')+left(satuan,4)
@b,53 say tran(harga_2,'@e 99,999,999')
@b,64 say iif(diskon#0,tran(diskon,'@e 99.99'),'     ')
@b,70 say tran(jhg,'@e 99,999,999')
RETURN(.t.)

FUNCTION brs3
Kdby=kdbx
SET COLOR TO &warna3
@b,0 say SPACE(8)+tl+SPACE(30)+tl+SPACE(12)+tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(10)
@b,0 say iif(len(RTRIM(kdby))>=9,left(kdby,3)+'x'+right(RTRIM(kdby),4),left(kdby,8))
@b,9 say left(nmb,30)
@b,40 say tran(jml,'@e 9999.99 ')+left(st,4)
@b,53 say tran(hj,'@e 99,999,999')
@b,64 say iif(dis1#0,tran(dis1,'@e 99.99'),'     ')
@b,70 say tran(jhg,'@e 99,999,999')
RETURN(.t.)

PROCEDURE bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
RETURN

PROCEDURE bacaflkas
Nf0='KAS-'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.DTA'
Nf1='KAS1'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
Nf2='KAS2'+SUBSTR(dtoc(tgl),9,2)+SUBSTR(dtoc(tgl),4,2)+'.NDX'
RETURN

***Indeks File
FUNCTION Indx
PARAMETERS Arg4,arg5,arg6,arg3
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   Useidx("&arg4")
   INDEX ON &arg5 to &arg6
ENDIF
CLOSE all
RETURN .T.

PROCEDURE hari
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
ENDIF
RETURN

PROCEDURE pesan
PARAMETERS psn
SET COLOR TO &warna5
@24,0 say SPACE(80)
@24,40-(len(psn)/2) say psn
SET COLOR TO &warna1
RETURN

PROCEDURE pesan2
PARAMETERS psn
Tone(499,1)
Tone(300,1)
SET COLOR TO &warna5
@24,0 say SPACE(80)
@24,40-(len(psn)/2) say psn
SET COLOR TO &warna1
Wait''
@24,0
RETURN

PROCEDURE pesan3
PARAMETERS psn
SET COLOR TO &warna5
@24,0 say SPACE(80)
@24,40-(len(psn)/2) say psn
SET COLOR TO &warna1
I=0
DO WHILE i#-9
   I=inke()
   If i=27 .or. i=28 .or. i=-3 .or. i=-4 .or. i=-7 .or. i=70 .or. i=74 .or. i=80 .or. i=102 .or. i=106 .or. i=112
      Exit
   ENDIF
ENDDO
If i=27 .or. i=28 .or. i=-3 .or. i=-4 .or. i=-7 .or. i=70 .or. i=74 .or. i=80 .or. i=102 .or. i=106 .or. i=112
   Nlp=0
Else
   Nlp=1
ENDIF
@24,0
RETURN

***PROSES ULANGI??
Function ulangi
I=inke()
DO WHILE i#13 .and. i#84 .and. i#116
   I=inke()
   If i=27 .or. i=89 .or. i=121
      RETURN(.t.)
   ENDIF
ENDDO
RETURN(.f.)

PROCEDURE xnota
Fkt=right('00000'+LTRIM(str(nn)),5)
RETURN

PROCEDURE tampil
Save scre to stampil
Kata1=RTRIM(aidi1)
Kata2=RTRIM(aidi2)
Kata3=' '+RTRIM(nsls)+' . '+kata1+' . '+kata2+' . '+hr+', '+dtoc(tgl)+' . '+iif(rpoin#0,'1 Point = Rp '+LTRIM(tran(rpoin,'@e 999,999'))+' . ','')
Kata4=' '+kata1+' . '+kata2+' . '+hr+', '+dtoc(tgl)+' . '
Pjg =len(kata3)
Pjg2=len(kata4)
DO WHILE .t.
   SET COLOR TO &warna1
   @4,0 to 4,79 doub
   @6,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,10)
   Do nomor with tot,1,56
   Do nomor with kbl,20,53
   Do pesan with 'F3=Kas   F4=Rekap   F5=Set_Print   F10=Ctk_Ulang   F11=CekPoin   ENTER=Transaksi'
   SET CURSOR OFF
   STORE 1 to awl,jln
   I=0
   Timernd1= Val(SUBSTR(Time(), 4, 2))
   DO WHILE awl<=pjg
      Katax=SUBSTR(kata3,awl,(pjg-awl))+SUBSTR(kata3,1,awl)
      Katay=SUBSTR(kata4,awl,(pjg-awl))+SUBSTR(kata4,1,awl)
      SET COLOR TO &warna5
      @0,71  say time()
      If dow(tgl)=1
         If idis#'0'
            SET COLOR TO /bg
         ENDIF
      Elseif dow(tgl)=2
         SET COLOR TO /w
      Elseif dow(tgl)=3
         If idis#'0'
            SET COLOR TO w+/r
         ENDIF
      Elseif dow(tgl)=4
         If idis#'0'
            SET COLOR TO br+/b
         ENDIF
      Elseif dow(tgl)=5
         If idis#'0'
            SET COLOR TO w+/gr
         ENDIF
      Elseif dow(tgl)=6
         If idis#'0'
            SET COLOR TO gr+/b
         ENDIF
      Elseif dow(tgl)=7
         If idis#'0'
            SET COLOR TO gr+/br
         ENDIF
      ENDIF
      @5,0
      @5,12 say SUBSTR(katax,1,54)
      Awl=awl+1
      Jln=jln+1
      I  =inke(.3)
      If i#0
         Exit
      ENDIF
      If awl=pjg
         Awl=1
      ENDIF
      If jln>=6
         Jln=1
         Nulise=left(katay,20)+SPACE(20)
         *Kedisple(nulise)
      ENDIF
      *
      Timernd2= Val(SUBSTR(Time(), 4, 2))
      If (timernd2 - timernd1 = 3 .OR. timernd2 - timernd1 = -55)
         Save scre to sdaf
         Tampil2()
         Timernd1= Val(SUBSTR(Time(), 4, 2))
         Rest scre from sdaf
      ENDIF
   ENDDO
   If i=-9
      Fktx=Fkt
      Fkt =val(fktx)-1
      Tglz=tgl
      STORE nsls to nsls2,nsls4
      *
      SET CURSOR ON
      Do pesan with 'Tanggal:              No.Nota:         Nama Ksr:          '
      @24,19 get tgl
      @24,41 get fkt pict '99999'
      @24,59 say RTRIM(nsls2)
      Read
      Fkt=right('00000'+LTRIM(str(fkt)),5)
      If lastkey()=27
         Tgl=tglz
         Fkt=fktx
         Loop
      ENDIF
      Do bacafile
      *
      SELECT 4
      Uselan('&nf0','&nf3','','','','')
      Seek dtos(tgl)+ksr+fkt
      If FOUND()
         Ulang=.t.
         Nr   =recno()
         Plg  =kode_lgn
         Ksls =kode_sls
         Kolomjl()
         SET COLOR TO &warna1
         @4,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,10)
         @5,0 say 'Kode Brg'+tl+'Nama Barang'+SPACE(19)+tl+' Jml.Barang '+tl+'    @Harga'+tl+'@Dis%'+tl+'Jumlah Rp.'
         @6,0 say REPLACE(gl,8)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,12)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,10)
         SET COLOR TO &warna2
         @1,18 CLEAR to 3,79
         @1,63 say 'Tgl.: '
         @2,63 say 'Nota: '
         @3,63 say 'Ksr.: '
         SET COLOR TO &warna2
         @1,69 say tgl
         @2,69 say fkt+SPACE(5)
         @3,69 say left(nsls2,10)
         STORE 0 to no,tot,tjml,dis31,byr,blt,dns
         Set filter to
         DO WHILE .not. eof() .and. ( tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt )
            If left(nama_brg,6)#'Diskon'
               Jhg=&brs1
               Brs2()
               If jumlah#0
                  No =no+1
               ENDIF
               B  =b+1
               Tot=tot+jhg
               Tjml=tjml+jumlah
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               ENDIF
            Elseif left(nama_brg,6)='Diskon'
               Dis31=dis31+abs(harga_2)
            ENDIF
            Byr=byr+rp_bayar
            Blt=blt+rp_bulat
            Dns=dns+rp_donasi
            *
            Skip
         ENDDO
         SET COLOR TO &warna1
         @b,0 say SPACE(8)+tl+SPACE(30)+tl+SPACE(12)+tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(10)
         *
         Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
         Do nomor with tot,1,56
         SET COLOR TO &warna6
         If blt#0
            @19,57 say 'Pembulatan :'
            @19,75 say tran(blt,'@e 9,999')
         ENDIF
         @20,69 say tran(tot+blt,'@e 999,999,999')
         @21,69 say tran(dis31,'@e 999,999,999')
         @22,69 say tran(byr,'@e 999,999,999')
         @23,69 say iif(dns>0,tran(dns,'@e 999,999,999'),'')
         *
         STORE SPACE(30) to nsp,jab
         Kta=SPACE(10)
         If plg#SPACE(10)
            SELECT 10
            Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
            Seek dtos(tgl)+plg+fkt
            If FOUND()
               Dis3 =diskon
               Dis31=rp_diskon
               Pnskr=jml_poin
               xPoin=pra_poin
               Tot3 =rp_sisa
            Elseif .not. FOUND()
               Uselan('CUST.DTA','CUST.NDX','','','','')
               Seek plg
            ENDIF
            Nsp=nama_lgn
            Jab=almt_lgn
            Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
            *
            SET COLOR TO w+/b
            @20,7  say plg
            @21,7  say nsp
            @22,7  say left(jab,25)
            @23,7  say SPACE(13)
            @23,7  say LTRIM(tran(pnskr,'9999'))
            If tot3#0
               SET COLOR TO gr+/b
               @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
               @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
               @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
               @23,33 say 'Sisa/Kurang: '+LTRIM(tran(kbl,'@e 999,999,999'))
               SET COLOR TO /bg
            ENDIF
            Close
         ENDIF
         *
         Do pesan3 with 'F4=Nota_Batal     F5=Edit     F10=Cetak_Nota'
         If lastkey()=-3
            Do pesan with ''
            Do paswod2
            If lastkey()=27 .or. xOwner=.f.
               Rest scre from stampil
               Tgl=tglz
               Fkt=fktx
               Nsp=SPACE(30)
               Jab=SPACE(30)
               Kta=SPACE(10)
               Plg=SPACE(10)
               Ksls=SPACE(5)
               Loop
            ENDIF
            *
            Do pesan with 'Nota diatas akan DiHapus? Y/T'
            Yt=' '
            DO WHILE .not. yt$'YT'
               @24,55 get yt pict '!'
               Read
               If lastkey()=27
                  Exit
               ENDIF
            ENDDO
            If yt='Y'
               Do pesan with 'Tunggu, sedang dihapus'
               SELECT 4
               Go nr
               DO WHILE .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt)
                  Kdbx=kode_bar
                  Jml =jumlah
                  Rlok()
                  REPLACE keterangan with dtoc(date())+' '+left(time(),5)+' '+ksa+'KSR '
                  Dele
                  Ul()
                  COMMIT
                  *
                  SELECT 3
                  Set order to 1
                  Seek kdbx
                  Rlok()
                  REPLACE stok_k&lks with (stok_k&lks-jml)
                  Ul()
                  COMMIT
                  *
                  SELECT 4
                  Skip
               ENDDO
               If plg#SPACE(10)
                  SELECT 10
                  Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
                  Seek dtos(tgl)+plg+fkt
                  Rlok()
                  Dele
                  Ul()
                  COMMIT
                  Close
               ENDIF
            ENDIF
            *
         Elseif lastkey()=-4
            Do pesan with ''
            Do paswod2
            If lastkey()=27 .or. xOwner=.f.
               Rest scre from stampil
               Fkt=fktx
               Tgl=tglz
               Loop
            ENDIF
            xAda=0
            STORE 1 to n,no
            *
            SELECT 4
            Go nr
            DO WHILE .not. eof() .and. (tanggal=tgl .and. nama_ksr=nsls2 .and. faktur=fkt)
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
               REPLACE rp_bayar with 0,rp_donasi with 0,rp_bulat with 0
               Ul()
               *
               If left(nmb,6)='Diskon'
                  Rlok()
                  Dele
                  Ul()
               Elseif left(nmb,6)#'Diskon'
                  SELECT 5
                  If n=1
                     Go top
                  ENDIF
                  REPLACE tanggal with tgl,faktur with fkt,kassa with ksa,kode_ksr with ksr,nama_ksr with nsls,kode_lks with lks,kode_spl with ksp,kode_lgn with plg,;
                       kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,isine_b with isi,jumlah with jml,harga with hp,harga_2 with hj,;
                       diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2,keterangan with ket,rectran with nrx,id_data with '1'
                  Skip
                  N =n+1
                  No=no+1
               ENDIF
               *
               SELECT 4
               Skip
            ENDDO
            Lks=lksx
            STORE .t. to diEdit,isilagi
            Exit
            *
         Elseif lastkey()=-9
            SELECT 4
            Go nr
            Do ctknota
            *If iprn='D' .or. (iprn='T' .and. idCut='T')
            *   Do kopnota
            *Endi
         Else
            Ulang=.f.
         ENDIF
      ENDIF
      SELECT 4
      Close
      *
      Ulang=.f.
      Fkt =fktx
      Tot =0
      Jml =1
      Tgl =tglz
      Nsls=nsls4
      Jab =SPACE(30)
      Kta =SPACE(10)
      Plg =SPACE(10)
      Ksls=SPACE(5)
      STORE SPACE(30) to nsp,nsls5
      RELEASE tgl3,tglz,sls2,nsls3,nsls4
      Rest scre from stampil
      Loop
   Elseif i=-4
      Save scre to sdaf
      @24,0
      SET CURSOR ON
      SET COLOR TO w+/br
      @7,18 CLEAR to 23,62
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
      ENDIF
      Rest scre from sdaf
      *
      If lastkey()#27
         SELECT 99
         Useidx('KASSA.SET')
         LOCATE FOR no_kassa=ksa
         REPLACE mode_kasir with xKsr,id_prin with iif(iprn#'D','T',iprn),id_drwr with iif(idrw#'Y','T',idrw),id_cut with iif(idcut#'Y','T',idcut),id_komp with iif(ikomp#'C','S',ikomp),;
              id_data with iif(idata#'0','1',idata),id_poin with iPoin,cari_kode with iif(jk#'1','0',jk),jark_nota with jn,jark_tepi with jt,lebr_roll with iif(lr#'5','8',lr),tanggal with tgl
         COMMIT
         Close
      ENDIF
   Elseif i=-2
      Save scre to skas
      SET CURSOR ON
      Do inkas
      SET CURSOR OFF
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
      ENDIF
      *
      Pil=' '
      Save scre to skas
      SET CURSOR ON
      If xOwner .and. lastkey()#27
         DO WHILE .t.
            Do pesan with 'Jenis Lap,  1=Rekap Barang   2=Rekap Nota   3=Rekap  [1/2/3]:      ESC=Selesai'
            @24,63 get pil pict '!'
            Read
            If lastkey()=27 .or. pil='0'
               Exit
            ENDIF
            If pil='1'
               Do lpj99
            Elseif pil='2'
               Do lnota
            Else
               Do lnota2
            ENDIF
         ENDDO
      Elseif lastkey()#27
          Do lnota2
      ENDIF
      SET CURSOR OFF
      SET COLOR TO /bg,w+/r
      Rest scre from skas
      Tgl=tglz
      Tot=0
      Loop
   Elseif i=-7
      Sls2=SPACE(20)
      Do pesan with ''
      SET COLOR TO w+/b*,b/b
      @24,69 say '_'
      SET COLOR TO w+/r
      @24,53  say 'Kode Kasir/User:'get sls2 pict '@!S10'
      Read
      SET COLOR TO w+/r,w+/b
      If lastkey()=27 .or. sls2#sls
         Loop
      ENDIF
      Do cp
      SET DEVICE TO PRINTER
      SET PRINTER ON
      If idrw='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
      ENDIF
      SET DEVICE TO SCREEN
      SET PRINTER OFF
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
      Plg=SPACE(10)
      Save scre to skas
      STORE .t. to member,cekpoin,hanyaCekp
      Do cekplg
      STORE .f. to cekpoin,member,hanyaCekp
      Rest scre from skas
      Plg  =SPACE(10)
      xPoin=0
      Loop
   ENDIF
   If i=13 .or. i=27
      Exit
   ENDIF
ENDDO
Rest scre from stampil
SET COLOR TO &warna5
@0,71 say time()
SET CURSOR ON
SET COLOR TO &warna6
@1,19 CLEAR to  3,59
If diEdit
   SET COLOR TO &warna2
   @1,63 say 'Tgl.: '
   @2,63 say 'Nota: '
   @3,63 say 'Ksr.: '
   SET COLOR TO &warna6
   @1,69 say tgl
   @2,69 say fkt+SPACE(5)
   @3,69 say left(nsls2,10)
ENDIF
If lihat
   SET COLOR TO &warna3
   @12,1 say SPACE(30)+'Lihat Harga Barang'+SPACE(30)
   SET COLOR TO &warna1
ENDIF
RETURN

PROCEDURE warna
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
ENDIF
If pilwarna
   SELECT 99
   Useidx('KASSA.SET')
   LOCATE FOR no_kassa=ksa
   REPLACE id_disp with idis
   COMMIT
   Close
ENDIF
RETURN

PROCEDURE tampil2
SET COLOR TO gr+
CLEAR
STORE 1 to br,klm,awl,tjam
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
   ENDIF
   Katay=SUBSTR(kata4,awl,(pjg2-awl))+SUBSTR(kata4,1,awl)
   Tot2 =InKey(2)
   Tjam =tjam+1
   If tjam=4
      Tjam=1
      Nulise=left(katay,20)+SPACE(20)
   Else
      Nulise=left(katay,20)+SPACE(7)+left(time(),5)+SPACE(8)
   ENDIF
   *Kedisple(nulise)
   @ br, klm say SPACE(50)
Enddo
SET COLOR TO /bg
RETURN

PROCEDURE cp
DO WHILE .t.
   If .not. isprinter()
      Tone(700,1)
      Tone(499,1)
      Do pesan with 'Printer belum siap,  ESC=Batal  F10=Coba Lagi'
      I=0
      DO WHILE i#-9
         I=inke()
         If i=27
            Exit
         ENDIF
      ENDDO
      If i=27
         Exit
      Elseif i=-9
         If isprinter()
            Exit
         ENDIF
      ENDIF
   Else
      Exit
   ENDIF
ENDDO
Do pesan with 'Tunggu, sedang dicetak'
RETURN

***Buka File di LAN
FUNCTION UseLan
PARAMETERS Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   Use &arg1
   If (!NetErr())
      Exit
   ENDIF
   Do pesan with 'Tunggu, File '+&arg1 +' Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
SET INDEX TO &arg2,&arg3,&arg4,&arg5,&arg6
RETURN .T.

***Buka File u/ Reindex-Exclusive
FUNCTION UseIdx
PARAMETERS Arg1
DO WHILE .t.
   Use &arg1 exclusive
   If (!NetErr())
      Exit
   ENDIF
   Tone(700,6)
   Do pesan with 'Proses diTunda, ada yg memBuka File '+arg1+'         '
   Yt=' '
   SET COLOR TO gr+/b
   @24,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   SET COLOR TO w+/b
   If yt#'Y'
     Batal=.t.
     Exit
   ENDIF
   @24,0
   Inkey(0.5)
ENDDO
RETURN(.T.)

FUNCTION RLOK
Do while .not. Rlock()
   If rlock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
Return(.T.) 

FUNCTION FLOK
Do while .not. FLock()
   If flock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
Return(.T.)

FUNCTION UL
Unlock
Return(.T.)

PROCEDURE daflgn
If plg#SPACE(5) .and. val(RTRIM(plg))=0
   Set order to 2
   Set filter to AT(RTRIM(plg),nama_lgn)>0
Elseif plg#SPACE(5) .and. val(RTRIM(plg))>0
   Set order to 1
   Set filter to AT(RTRIM(plg),kode_lgn)>0
ENDIF
Go top
Save scre to sdaf
Do pesan with 'Cari Kode/Nama Pelanggan, tekan A--Z  atau  0--9     ENTER=Pilih'
SET COLOR TO w+/br   &&&&warna4
@1,16 CLEAR to 22,79
@1,16 to 1,79 doub
@22,16 to 22,79
DECLARE field[4],kop[4],bts[4]
Field[1]="left(NAMA_LGN,25)+tl+KODE_LGN+tl+left(ALMT_LGN,25)"
Kop[1]='Nama Pelanggan            Kode       Alamat'
Bts[1]=''
DbEdit(2,17,21,78,field,'flgn','',kop,gl,bts)
*
Set filter to
Set order to 1
Rest scre from sdaf
SET COLOR TO &warna1
SET CURSOR ON
RETURN

Function flgn
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Plg=kode_lgn
      RETURN 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122).or. (lastkey()>=48 .and.  lastkey()<=57)
      Set order to 2
      Seek uppe(chr(lastkey()))
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

PROCEDURE dafbrg
Save scre to sdaf
Do pesan with 'Pilih item Barangnya, kemudian Enter'
SET COLOR TO &warna4
Set filter to
If ikomp='C' .and. idata='0' .and. jk='0'
   SELECT 77
   @1,69 say 'STOK LOKAL'
ENDIF
*
If jk='0'
   Nmb =iif(carikode,alltrim(kdb),alltrim(nmb))
   Set filter to AT(nmb,iif(carikode,kode_bar,nama_brg))>0
   Go top
   If eof()
      Do pesan with 'DATA BARANG TIDAK DITEMUKAN...!'
   ENDIF
ENDIF
@1,0 CLEAR to 22,79
@1,0 to 1,79
@23,0 to 23,79
@1,0 say 'Cari= '+RTRIM(nmb)
DECLARE field[11],kop[11],frm[11],bts[11]
Field[1]="left(NAMA_BRG,30)+' '+KODE_BAR+tran(HARGA_2,'@e  9,999,999')+tran(iif(idp='1',HARGA_32,HARGA_3),'@e 9,999,999 / ')+LTRIM(tran(BATAS_3,'9999 '))+SATUAN"
Field[2]="left(NAMA_BRG,30)+' '+KODE_BAR+tran(HARGA_2,'@e  9,999,999')+tran(HARGA_3,'@e 9,999,999 ')+tran(((STOK_A+STOK_M) - (STOK_K+STOK_RB)),'@e 9999,999 ')+SATUAN"
Kop[1]='Nama Barang                    Barcode          @Harga   @Grosir   Sat'
Kop[2]='Nama Barang                    Barcode          @Harga   @Grosir   Sisa Stok'
Frm[1]=''
Frm[2]=''
Bts[1]=' '
Bts[2]=' '
DbEdit(2,0,22,79,field,'xkdb2',frm,kop,chr(196),bts)
*
Set filte to
Rest scre from sdaf
SET COLOR TO &warna1
RETURN

Function xkdb2
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=13
      Kdbx=kode_bar
      Nmb =nama_brg
      RETURN 0
   Case (lastkey()>=65 .and. lastkey()<=90) .or. (lastkey()>=97 .and.  lastkey()<=122)
      Seek uppe(chr(lastkey()))
      RETURN 1
   Othe
      RETURN 1
ENDCASE
RETURN 0

***NOMOR YG GEDE-2
PROCEDURE nomor
PARAMETERS tot,br,klm
SET COLOR TO &warna6
@br,klm-iif(br=20,20,38) CLEAR to br+2,klm+3
Chtot=LTRIM(str(tot))
Awl=1
Akr=len(chtot)
DO WHILE awl<=len(chtot)
   If awl=4 .or. awl=7
      @br+2,klm+1 say ' Ü'
      Klm=klm-2
   ENDIF
   Do case
      Case SUBSTR(chtot,akr,1)='1'
         @br  ,klm+2 say 'Ü'
         @br+1,klm+2 say 'Û'
         @br+2,klm+2 say 'Û'
         Klm=klm+2
      Case SUBSTR(chtot,akr,1)='2'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'ÜÜÛ'
         @br+2,klm say 'ÛÜÜ'
      Case SUBSTR(chtot,akr,1)='3'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say ' ÜÛ'
         @br+2,klm say 'ÜÜÛ'
      Case SUBSTR(chtot,akr,1)='4'
         @br  ,klm say 'Ü Ü'
         @br+1,klm say 'ÛÜÛ'
         @br+2,klm say '  Û'
      Case SUBSTR(chtot,akr,1)='5'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'ÛÜÜ'
         @br+2,klm say 'ÜÜÛ'
      Case SUBSTR(chtot,akr,1)='6'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'ÛÜÜ'
         @br+2,klm say 'ÛÜÛ'
      Case SUBSTR(chtot,akr,1)='7'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say '  Û'
         @br+2,klm say '  Û'
      Case SUBSTR(chtot,akr,1)='8'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'ÛÜÛ'
         @br+2,klm say 'ÛÜÛ'
      Case SUBSTR(chtot,akr,1)='9'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'ÛÜÛ'
         @br+2,klm say 'ÜÜÛ'
      Case SUBSTR(chtot,akr,1)='0'
         @br  ,klm say 'ÜÜÜ'
         @br+1,klm say 'Û Û'
         @br+2,klm say 'ÛÜÛ'
   ENDCASE
   Awl=awl+1
   Akr=akr-1
   Klm=klm-4
ENDDO
SET COLOR TO &warna3
RETURN

PROCEDURE xubahjml2   && UBAH QTY.AWAL
Ubahjml2=.t.
Keyboard chr(13)
RETURN

PROCEDURE xubahjml   && UBAH 1 ITEM DIATAS
Ubahjml=.t.
Keyboard chr(13)
RETURN

PROCEDURE xadadis  && ADA DISKON?
Adadis=.t.
Keyboard chr(13)
RETURN

PROCEDURE xadamem  && ADA MEMBER?
Member=.t.
Keyboard chr(13)
RETURN

PROCEDURE xlihat  && LIHAT HRG
Lihat=.t.
Save scre to slihat
SET COLOR TO gr+/b
@b+2,1 say SPACE(30)+'Lihat Harga Barang'+SPACE(30)
SET COLOR TO /bg
RETURN

PROCEDURE paswod2
xOwner=.f.
xSele=select()
SELECT 10
Uselan('DATA.SET','','','','','')
SET COLOR TO w+*/r
@24,59 say 'Password:'
SET COLOR TO w+*/b
@24,68 say '_'
@24,69 say SPACE(11)
SET COLOR TO b/b,b/b
Tone(1500,1)
Tone(1000,3)
Awal=1
DO WHILE awal<=3
   In=SPACE(15)
   @24,69 get in pict '@S11'
   Read
   If lastkey()=27 .or. in=SPACE(15)
      Exit
   ENDIF
   If faktur=left(in,10)
      xOwner=.T.
      Exit
   ENDIF
ENDDO
SET COLOR TO /bg,w+/b
Close
SELECT(xsele)
RETURN

PROCEDURE konvtgl
If len(LTRIM(str(day(tgl))))=1
   Hri='0'+LTRIM(str(day(tgl)))
Else
   Hri=LTRIM(str(day(tgl)))
ENDIF
If len(LTRIM(str(mont(tgl))))=1
   Bln='0'+LTRIM(str(mont(tgl)))
Else
   Bln=LTRIM(str(mont(tgl)))
ENDIF
Thn=LTRIM(str(year(tgl)))
RETURN

PROCEDURE nol3
Byr=byr*1000
Keyboard chr(13)
RETURN

PROCEDURE cekbulat
If file('RPBULAT.SET')
   SELECT 11
   Uselan('RPBULAT.SET','','','','','')
   Go top
   DO WHILE .not. eof()
      If (tot>=batas_1 .and. tot<=batas_2) .and. nilai_rp#0
         Blt=mod(tot,nilai_rp)
         If blt<=nilai_b
            Blt=-blt
            Tot=tot+blt
         Elseif blt>=nilai_a
            Blt=nilai_rp-blt
            Tot=tot+blt
         ENDIF
         Exit
      ENDIF
      Skip
   ENDDO
   Close
   *
   SELECT(xsele)
ENDIF
RETURN

PROCEDURE cekplg
If member .or. byr<(tot-dis31)
   If iPoin='2'
      Cekpoin=.t.
   ENDIF
   *
   Plg=SPACE(11)
   SELECT 10
   Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
   DO WHILE .t.
      Set key 28 to daflgn
      @20,7 get plg pict '@!'
      Do pesan with 'Masukan Kode Pelanggan,    F1=Daftar Pelanggan     0=Cash'
      SET COLOR TO w+/b,/w
      Read
      Set key 28 to
      If lastkey()=27
         Exit
      ENDIF
      Plgz=RTRIM(plg)
      If plg=SPACE(11)
         Plg=SPACE(10)
      Elseif plg#SPACE(11)
         If val(right(RTRIM(plg),3))#0
            *Plg=iif(val(right(rtri(plg),2))>0, idtx+right('000000'+rtri(plg),pdt-len(idtx))+spac(10-pdt) , left(plg,10))
            Plg=idtx+right('000000'+RTRIM(plg),pdt-len(idtx))+SPACE(10-pdt)
         ENDIF
      ENDIF
      @20,7 say plg
      Set order to 1
      Seek plg
      If .not. FOUND()
         *Plg=plgz
         Do daflgn
         If lastkey()=27
            Loop
         ENDIF
      ENDIF
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
      ENDIF
      STORE 0 to pnsbl,totbl,totmr
      *
      If left(plg,3)='0  '
         Plg=SPACE(10)
         STORE SPACE(30) to nsp,jab
         STORE 0 to tot3,kbl
         SET COLOR TO w+/b
         @19,0 say REPLACE(gl,8)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,13)+chr(193)+REPLACE(gl,9)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,10)
         @20,1  say 'Kode:                                       '+SPACE(12) +'Jml.Belanja:'+SPACE(13)
         @21,1  say 'Nama:'+SPACE(31)+'                    Pembulatan :'+SPACE(13)
         @22,1  say 'Almt:'+SPACE(31)+'                    Disc.:     %'+SPACE(13)
         @23,1  say 'Poin:'+SPACE(31)+'                    Jml.Bayar  :'+SPACE(13)
      ENDIF
      Plg=left(plg,10)
      SET COLOR TO w+/b
      @20,7 say plg+'  '
      @21,7 say nsp
      @22,7 say jab
      @23,7 say LTRIM(tran(pnskr,'999 + '))+LTRIM(tran(pnskr2,'999'))
      Do pesan with 'Tunggu, sedang dihitung...'
      *
      SELECT 11
      Uselan('FAKTURP.DTA','FAKTURP2.NDX','','','','')
      Seek plg
      STORE 0 to jml1,jml2
      DO WHILE .not. eof() .and. kode_lgn=plg
         If (tanggal>=tglso .and. tanggal<=tgl)
            Jml1=jml1+(rp_total-rp_diskon)
            Jml2=jml2+rp_bayar
         ENDIF
         Skip
      ENDDO
      Tot3=(jml1-jml2)
      If tot3#0
         SET COLOR TO gr+/b
         @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
         SET COLOR TO /bg
      ENDIF
      *
      If cekpoin
         Seek plg
         DO WHILE .not. eof() .and. kode_lgn=plg
            If (tanggal>=tglhp .and. tanggal<=tglb)
               If tanggal=tglhp .and. jam>jamx
                  Pnsbl=pnsbl+jml_poin
                  Totbl=totbl+(rp_total-rp_diskon)
                  Totmr=totmr+(rp_total-rp_pokok)
               Elseif tanggal>tglhp
                  Pnsbl=pnsbl+jml_poin
                  Totbl=totbl+(rp_total-rp_diskon)
                  Totmr=totmr+(rp_total-rp_pokok)
               ENDIF
            ENDIF
            Skip
         ENDDO
         xPoin=(pnsbl+ajp)
         SET COLOR TO w+/b
         *
         SELECT 10
         If hanyaCekp    &&&.and. tgl_ambil#ctod('  -  -    ')
            @23,7 say SPACE(30)
            @23,7 say LTRIM(tran(xPoin,'@e 999,999'))   &&&+'   Belanja= '+LTRIM(tran(totbl,'@e 9,999,999,999'))+'   Margin= '+LTRIM(tran(totmr,'@e 999,999,999 '))
            *
            SET COLOR TO w+/r
            @16,13 CLEAR to 19,63
            @16,13 to 19,63
            @16,27 say 'PENGAMBILAN POIN YG LALU'
            @17,15 say 'Tanggal: '+dtoc(tgl_ambil)+' / '+jam_ambil
            @18,15 say 'Jumlah.: '+LTRIM(tran(amp,'@e 999,999'))+iif(rpu#0,' / '+LTRIM(tran(rpu,'@e 999,999,999')),'')+' / '+RTRIM(ket_ambil)
            Do pesan with 'A=Ambil_Poin     ESC=Ulangi'
         Else
            Do pesan with 'ENTER=Lanjut_Transaksi     ESC=Ulangi'
         ENDIF
         Wait''
         SET COLOR TO w+/b
         @23,7 say SPACE(7)
         If lastkey()=27
            Plg=SPACE(11)
            Loop
         Elseif hanyaCekp .and. (lastkey()=65 .or. lastkey()=97)
            SET CURSOR ON
            Ketx='PENGAMBILAN POIN'
            Ket =SPACE(50)
            Tglz=tgl
            STORE 0 to jml,jml2
            *
            SELECT 11
            Uselan('POINTAM.DTA','POINTAM1.NDX','POINTAM2.NDX','','','')
            Do pesan with 'Masukan Tanggal dan Jumlah Point / Voucer (Rp) yg diAmbil'
            SET COLOR TO w+/g
            @16,10 CLEAR to 19,71
            @16,10 to 19,71
            @16,32 say ketx
            @17,12 say 'Tanggal.:'get tglz
            @17,34 say 'Catatan:'get ket pict '@!S28'
            @18,12 say 'Poin diAmbil:'get jml pict '@e 999999'
            @18,34 say '-atau- diUangkan sejmlh Rp'get jml2 pict '@e   99999999'
            Do pesan with 'Masukan Tanggal dan Jumlah Poin yg diAmbil'
            Read
            If lastkey()=27
               SELECT 10
               Exit
            ENDIF
            SET COLOR TO w+/b
            @18,25 say tran(jml,'@e 999,999')
            @18,61 say tran(jml2,'@e 99,999,999')
            Yt=' '
            Do pesan with 'Data Benar? Y/T :'
            @24,49 get yt pict '!'
            Read
            If lastkey()=27
               SELECT 10
               Exit
            ENDIF
            If ket#SPACE(50) .and. (jml1#0 .or. jml2#0)
               Flok()
               APPEND BLANK
               REPLACE tanggal with tglz,jam with time(),kode_lgn with plg,nama_lgn with nsp,rp_uang with jml2,;
                    so_point with xPoin,am_point with jml,id_point with kdb,keterangan with ket
               Ul()
               COMMIT
               Close
               *
               SELECT 10
               Rlok()
               REPLACE so_point with xPoin,aj_point with xPoin-jml,am_point with jml,rp_uang with jml2,tgl_ambil with tglz,jam_ambil with time(),ket_ambil with ket
               Ul()
               COMMIT
               *
            ENDIF
         ENDIF
         *
      Else
         If tot3#0
            SET COLOR TO gr+/b
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            SET COLOR TO /bg
         ENDIF
      ENDIF
      Close
      *
      Exit
   ENDDO
   Close
   *Dis31=(tot*(dis3/100))
   *If byr>(tot-dis31)
   *   @22,63 say dis3  pict '@e 99.99'
   *   @22,69 say dis31 pict '@e 999,999,999'
   *   Do pesan with 'ENTER=Lanjut'
   *   Wait''
   *Endi
ENDIF
RETURN

*OUTPUT KE DISPLAY/DRAWER
*-------------------------
FUNCTION kedisple
If idis='Y'
   Parameters xkata
   Xopen = Fopen("COM3",1)
   If (ferror() != 0)
      Do pesan with 'Tidak Tersambung ke OutPut--> COM1/Display'
      Inkey(1)
      RETURN(.f.)
   ENDIF
   Fwrite(xopen,xkata,40)
   Fclose(xopen)
ENDIF
RETURN(.t.)

FUNCTION bukadrw
Xopen = Fopen("LPT1",1)
If (ferror() != 0)
   Do pesan with 'Tidak Tersambung ke OutPut--> LPT1/CashDrawer'
   Inkey(1)
   RETURN(.f.)
ENDIF
*Fwrite(xopen,chr(27)+'p'+chr(0)+chr(100)+chr(250),5)      &&PRINTR EPSON, SAMSUNG->EPSON
Fwrite(xopen,chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'255',7)  &&PRINTR STAR->EPSON
Fclose(xopen)
RETURN(.t.)

***Kalkulator
PROCEDURE kalk
Private rupacalc, barcalc, kolcalc, opr, ha_opr, n_calc, mhasil, ;
      mcalc_tot, in_opr, j_opr, len_opr, hasil, pos_calc, pos_prs, ;
      minplus, nil_calc, prs_calc, scr_calc
Save scre to scr_calc
Barcalc= Row()
Kolcalc= Col()
If (Row() <= 11 .AND. Col() < 40)
   Row_calc= Row() + 2
   Col_calc= 4
ENDIF
If (Row() <= 11 .AND. Col() >= 40)
   Row_calc= Row() + 2
   Col_calc= 44
ENDIF
If (Row() > 11 .AND. Col() < 40)
   Row_calc= Row() - 8
   Col_calc= 4
ENDIF
If (Row() > 11 .AND. Col() >= 40)
   Row_calc= Row() - 8
   Col_calc= 44
Endif
SET COLOR TO w+/b
@ row_calc, col_calc clear to row_calc +6, col_calc +30
@ row_calc, col_calc to row_calc +6, col_calc +30
@ row_calc, col_calc +11 say "Calculator"
@ row_calc +2, col_calc +2 say "Operasi ="
@ row_calc +3, col_calc +2 say "Hasil   ="
@ row_calc +4, col_calc +2 say "Total   ="
@ row_calc +6, col_calc +6 say "[R]-Reset  [H]-Hasil"
STORE 0 to hasil,calc_tot
STORE '' to opr,ha_opr
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
      ENDIF
   Enddo
   If (in_opr == 27)
      exit
   ENDIF
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
   ENDIF
   ha_opr= iif(hasil = 0, "", LTrim(iif(mod(hasil, 1) = 0, Str(hasil, 17), Str(hasil, 17, 3))))
Enddo
Rest screen from scr_calc
SET COLOR TO /w,bg+/b
@ barcalc, kolcalc say ""
Scr_calc= ""
RELEASE rupacalc, barcalc, kolcalc, opr, ha_opr, n_calc, mhasil, ;
     mcalc_tot, in_opr, j_opr, len_opr, hasil, pos_calc, pos_prs, ;
     minplus, nil_calc, prs_calc, scr_calc
RETURN

***PROSES UNTUK NOTA TUNDA
*-------------------------
PROCEDURE notunda
Ditunda=.t.
Keyboard chr(13)
RETURN

PROCEDURE cektunda
If file ('&nfy')
   SELECT 99
   Useidx('&nfy')
   Pack
   If eof()
      Close
      Dele file &nfy
      Adatunda=.f.
   Elseif recno()>=1
      Adatunda=.t.
   ENDIF
ENDIF
RETURN

PROCEDURE caritunda
Save scre to sdaf
Pesanx='ENTER=Pilih     F4=Hapus Semua     F8=Hapus Item ini     ESC=Keluar'
Do pesan with pesanx
SET COLOR TO /bg
@1,4  CLEAR to 23,76
@1,4  to 1,76  doub
@23,4 to 23,76
@1,28 say'Item Barang yg Tertunda'
DECLARE field[7],kop[7],bts[7],frm[7]
Field[1]="JAM+tl+left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+SATUAN+tl+tran(HARGA_2,'@e 9,999,999 ')+tl+tran(JUMLAH*HARGA_2,'@e 99,999,999')"
Kop[1]='No.   Nama Barang                      Jml.Barang     @Harga   Jumlah Rp'
Frm[1]=''
STORE '@!' to frm[1],frm[2],frm[3]
STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7]
SET COLOR TO /bg
Go top
DbEdit(2,4,22,76,field,'ftunda',frm,kop,gl,bts)
*
Rest scre from sdaf
SET COLOR TO w+/b
SET CURSOR ON
RETURN

Function ftunda
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=13 .and. jam#'---'
   Isilagi =.t.
   Tot=1
   Wkt=jam
   DO WHILE .not. eof() .and. jam=wkt
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
      SELECT 5
      REPLACE tanggal with tgl,faktur with fkt,kode_ksr with ksr,nama_ksr with nsls,kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,;
           nama_brg with nmb,satuan with st,jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2,keterangan with ket,id_data with ' '
      Skip
      *
      SELECT 99
      Dele
      Skip
      If jam='---' .and. nama_brg=REPLACE('-',30) .and. satuan='-----'
         Dele
      ENDIF
   ENDDO
   RETURN 0
Elseif lastkey()=-3
   Do pesan with ''
   Do paswod2
   If xOwner .and. lastkey()#27
      xOwner=.f.
      Yt=' '
      SET CURSOR ON
      Do pesan with 'Hapus Semua item yg diTunda? Y/T : '
      DO WHILE .not. yt$'YT'
         @24,57 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Dele All
         COMMIT
      ENDIF
   ENDIF
   SET COLOR TO /bg
   SET CURSOR OFF
   RETURN 0
Elseif lastkey()=-7
   B =row()
   Kl=col()
   Do pesan with ''
   Do paswod2
   If xOwner .and. lastkey()#27
      xOwner=.f.
      SET COLOR TO gr+*/b
      @b,kl say &fds
      Yt= ' '
      SET CURSOR ON
      SET COLOR TO /bg
      Do pesan with 'Hapus item ini? Y/T : '
      DO WHILE .not. yt$'YT'
         @24,51 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Dele
         COMMIT
         Skip
      ENDIF
   ENDIF
   Do pesan with pesanx
   SET COLOR TO /bg
   SET CURSOR OFF
   RETURN 2
Else
   RETURN 1
ENDIF
RETURN 0

* LAPORAN TOTAL NO.NOTA
*-----------------------
PROCEDURE lnota
@1,0 CLEAR to 23,79
SET COLOR TO /bg
@9,26 CLEAR to 14,53
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
SELECT 4
Uselan('&nf0','&nf3','','','','')
Set filter to id_data=' ' .and. kode_ksr=ksr
Seek dtos(tgl)
If .not. FOUND()
   LOCATE FOR tanggal>=tgl
ENDIF
*
Nr=recno()
STORE 1 to hal,noz
STORE 0 to tot,totd,tdns,jml1,jds,jdns
Arg1='L-NOTA1.RTF'
Set alte on
Set alte to &arg1
?' '+aidi1
?' REKAP NOTA PENJUALAN'
?' TANGGAL: '+dtoc(tgl)+' // '+left(time(),5)
?' KASIR  : '+nsls
?REPLACE('=',32)
?' No. Nota  Jam   Dns   Jumlah Rp'
?REPLACE('-',32)
Fktz=faktur
Jamz=jam
Slsz=nama_ksr
Tgl3=tanggal
DO WHILE .not. eof() .and. tanggal=tgl
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
      STORE 0 to jml1,jds,jdns
   ENDIF
ENDDO
?REPLACE('-',32)
If totd#0
   ?SPACE(10)+'Jumlah Rp'+tran(tot+totd,'@e 9,999,999,999')
   ?SPACE(10)+'Diskon Rp'+tran(totd,'@e 9,999,999,999')
ENDIF
?SPACE(10)+'Total  Rp'+tran(tot,'@e 9,999,999,999')
If tdns#0
   ?SPACE(10)+'Donasi Rp'+tran(tdns,'@e 9,999,999,999')
ENDIF
?REPLACE('=',32)
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
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to jt
      ?chr(27)+chr(33)+chr(4)
      Type &arg1 to prin
      @prow()+(jn+5),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      ENDIF
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDIF
SELECT 4
Close
RELEASE fktz,jamz,slsz,noz,nz
RETURN

PROCEDURE lnota2
Do pesan with 'Rekap Penjualan'
*
@1,0 CLEAR to 23,79
SET COLOR TO /bg
@9,26 CLEAR to 14,53
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
SELECT 4
Uselan('&nf0','&nf3','','','','')
Set filter to kode_ksr=ksr
Seek dtos(tgl)
If .not. FOUND()
   LOCATE FOR tanggal>=tgl
ENDIF
*
STORE 1 to hal,noz
STORE 0 to tot,totd,totk,tdns,jml1,jml2,jmlb
Arg1='L-NOTA1.RTF'
Set alte on
Set alte to &arg1
?' '+aidi1
?' REKAP PENDAPATAN'
?' TANGGAL: '+dtoc(tgl)+' / '+left(time(),5)
?' KASIR  : '+nsls
?REPLACE('=',iif(lr='8',40,32))
?'PEMASUKAN:'
DO WHILE .not. eof() .and. tanggal=tgl
   Jml1=jml1+&brs1
   Tot =tot +&brs1
   Tdns=tdns+rp_donasi
   If id_data='K'
      Jml2=jml2+&brs1
      Jmlb=jmlb+rp_bayar
   ENDIF
   Skip
ENDDO
?'- Penjualan Barang   '+SPACE(iif(lr='8',8,0))+tran(tot,'@e 999,999,999')
If jml2#0
?'- Penjualan Kredit   '+SPACE(iif(lr='8',8,0))+'('+tran(jml2-jmlb,'@e 999,999,999')+')'
ENDIF
If tdns#0
?'- Uang Donasi        '+SPACE(iif(lr='8',8,0))+tran(tdns,'@e 999,999,999')
ENDIF
*
Do bacaflkas
*
SELECT 11
Uselan('&nf0','&nf2','','','','')
SELECT 10
Uselan('POSAKUN.DTA','POSAKUN.NDX','','','','')
Set filter to id_pos='D'
LOCATE FOR id_pos='D'
DO WHILE .not. eof()
   Kpos =kode_pos
   Nspos=nama_pos
   Ipos =id_pos
   *
   SELECT 11
   Set filter to nama_ksr=nsls .and. id_pos='D'
   Seek kpos+dtos(tgl)
   If .not. FOUND()
      Seek kpos
   ENDIF
   DO WHILE .not. eof() .and. kode_pos=kpos
      If tanggal=tgl
         ?'- '+left(keterangan,iif(lr='8',27,19))+tran(rp_total,'@e 999,999,999')
         Totd=totd+rp_total
      ENDIF
      Skip
   ENDDO
   SELECT 10
   Skip
ENDDO
Totd=totd+(tot-(jml2-jmlb))+tdns
?SPACE(iif(lr='8',29,21))+REPLACE('-',11)
?'  Jml.Pemasukan   Rp'+SPACE(iif(lr='8',9,1))+tran(totd,'@e 999,999,999')
?' '
?'PENGELUARAN:'
*
SELECT 10
Set filter to id_pos='K'
LOCATE FOR id_pos='K'
DO WHILE .not. eof()
   Kpos =kode_pos
   Kspos=kode_pos
   Ipos =id_pos
   *
   SELECT 11
   Set filter to nama_ksr=nsls .and. id_pos='K'
   Seek kpos+dtos(tgl)
   If .not. FOUND()
      Seek kpos
   ENDIF
   DO WHILE .not. eof() .and. kode_pos=kpos
      If tanggal=tgl
         ?'- '+left(keterangan,iif(lr='8',27,19))+tran(rp_total,'@e 999,999,999')
         Totk=totk+rp_total
      ENDIF
      Skip
   ENDDO
   SELECT 10
   Skip
ENDDO
?SPACE(iif(lr='8',29,21))+REPLACE('-',11)
?'  Jml.Pengeluaran Rp'+SPACE(iif(lr='8',9,1))+tran(totk,'@e 999,999,999')
?' '
?SPACE(iif(lr='8',29,21))+REPLACE('-',11)
?'  Sisa Uang Kas   Rp'+SPACE(iif(lr='8',9,1))+tran(totd-totk,'@e 999,999,999')
?SPACE(iif(lr='8',29,21))+REPLACE('-',11)
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
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to jt
      ?chr(27)+chr(33)+iif(iprn='D',chr(5),chr(4))
      Type &arg1 to print
      @prow()+(jn+2),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      ENDIF
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDIF
Close
*
RETURN

* LAPORAN PENJUALAN (TERLARIS)
*-----------------------------
PROCEDURE lpj99
*
SELECT 3
Close
*
DO WHILE .t.
   Jlap=' '
   SET COLOR TO
   @1,0 CLEAR to 24,79
   SET COLOR TO /bg,w+/b
   @9,26 CLEAR to 14,53
   @9,26 to 14,53
   @10,28 say 'Rekap Item Barang'
   @11,28 say 'Penjualan Semua Kasir'
   @13,28 say 'Tanggal Trx:'get tgl
   Do pesan with 'Masukan Tanggal yg mau diRekap'
   Read
   @24,0
   If lastkey()=27
      Exit
   ENDIF
   Do bacafile
   *
   SELECT 4
   Uselan('&nf0','&nf2','','','','')
   *
   SELECT 3
   Uselan('STOK.DTA','STOK5.NDX','','','','')
   *
   SELECT 13
   Useidx('TOP-RANK.DTA')
   Copy stru to TOP-RANK.TMP
   Close
   *
   Ada =.f.
   Awl =1
   Bln =SUBSTR(dtoc(tgl),4,2)
   *
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang Proses... Rec: '+LTRIM(str(reccount()))+'-          File: '+nf0
   SET COLOR TO w+/r
   *
   SELECT 4
   Set filter to
   *
   SELECT 3
   Go top
   DO WHILE .not. eof()
      Klb =kode_klb
      Kdb =kode_brg
      Kdbx=kode_bar
      Nmb =nama_brg
      St  =satuan
      Stk =(stok_a+stok_m)-stok_k
      Ksp =kode_spl
      STORE 0 to jml,jhg,jhp,jds
      @24,44 say LTRIM(str(awl))
      *
      SELECT 4
      Seek kdbx
      DO WHILE .not. eof() .and. kode_bar=kdbx
         If tanggal=tgl
            Jml=jml+jumlah
            Jhg=jhg+(jumlah*(harga_2-diskon_rp))
            Jhp=jhp+(jumlah*harga)
            Jds=jds+(jumlah*diskon_rp)
            If kode_spl#SPACE(5)
               Ksp=kode_spl
            ENDIF
         ENDIF
         Skip
      ENDDO
      *
      If jml#0
         Ada=.t.
         SELECT 13
         Useidx('TOP-RANK.TMP')
         APPEND BLANK
         REPLACE kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              jumlah with jml,sisa with stk,harga with jhp,harga_2 with jhg,margin with jhg-jhp,diskon with jds
      ENDIF
      *
      Awl=awl+1
      SELECT 3
      Skip
   ENDDO
   *
   Awl=1
   If ada
      SELECT 16
      Close
      *
      SELECT 15
      Close
      *
      SELECT 14
      Close
      *
      SELECT 13
      Sort on jumlah   /D to TOP-RANK.QTY
      Sort on harga_2  /D to TOP-RANK.RPH
      Sort on kode_brg to TOP-RANK.KDB
      Sort on nama_brg,jumlah to TOP-RANK.NMB
      Close
      *
      Pesanx='T=Top Ranking   F1=Nama Brg.   F10=Cetak'
      SELECT 13
      Useidx('TOP-RANK.QTY')
      SELECT 14
      Useidx('TOP-RANK.RPH')
      SELECT 15
      Useidx('TOP-RANK.NMB')
      SELECT 16
      Useidx('TOP-RANK.KDB')
      SELECT 15
      *
      Ket='TOP NAMA,  Tgl: '+dtoc(tgl)
      SET COLOR TO /bg
      @1,0 CLEAR to 23,79
      @1,1 say ket
      @2,0 to 2,79 doub
      @22,0 to 22,79
      Keyboard chr(4)
      DO WHILE .t.
         DECLARE field[10],bts[10],frm[10],kop[10]
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
         STORE '@!' to frm[1],frm[2],frm[4]
         STORE tl to bts[1],bts[2],bts[3],bts[4],bts[5],bts[6],bts[7],bts[8]
         DbEdit(3,0,21,79,field,'flpj99',frm,kop,gl,bts)
         *
         If lastkey()=27
            Exit
         ENDIF
      ENDDO
   Elseif .not. ada
      Tone(1500,3)
      Do pesan with 'Tidak Ada Data Penjualan'
      Wait''
   ENDIF
ENDDO
SELECT 16
Close
*
SELECT 15
Close
*
SELECT 14
Close
*
SELECT 13
Close
*
SELECT 3
Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
RETURN

Function flpj99
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   @22,0 say REPLACE(gl,15)
   SET COLOR TO gr+/b
   @22,0 say ' '+LTRIM(str(recno()))+'/'+LTRIM(str(reccount()))+' '
   SET COLOR TO /bg
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=28
      Nr=recno()
      Nmb=SPACE(30)
      SET CURSOR ON
      Do pesan with ''
      SET COLOR TO w+/r
      @23,18 say 'Urut Nama Brg:'get nmb pict '@!'
      Read
      If lastkey()#27 .and. nmb#SPACE(30)
         SET COLOR TO w+/w
         LOCATE FOR nama_brg=RTRIM(nmb)
         If .not. FOUND()
           Go nr
         ENDIF
      ENDIF
      Do pesan with pesanx
      SET COLOR TO w+/b
      SET CURSOR OFF
      RETURN 1
Elseif uppe(chr(lastkey()))='T' .and. fds='KODE_BAR'
      SELECT 16
      Ket='TOP KODE,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='NAMA_BRG'
      SELECT 15
      Ket='TOP NAMA,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='JUMLAH'
      SELECT 13
      Ket='TOP QTY,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)+chr(4)
      RETURN 0
Elseif uppe(chr(lastkey()))='T' .and. fds='HARGA_2'
      SELECT 14
      Ket='TOP RUPIAH,  Tgl: '+dtoc(tgl)
      @1,0
      @1,5 say ket
      Keyboard chr(4)+chr(4)+chr(4)+chr(4)
      RETURN 0
Elseif lastkey()=-9
      Go top
      STORE 1 to n,no,hal
      STORE 0 to tot,dis
      Do cp
      If lastkey()=0
         RETURN 0
      ENDIF
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Set marg to jt
      @prow()+1,0 say chr(27)+chr(33)+chr(5)
      @prow()+1,0 say 'REKAP PENJUALAN'
      @prow()+1,0 say ket+' // '+left(time(),5)
      @prow()+1,0 say 'Kode    Nama Barang           Jumlah Rp.'
      @prow()+1,0 say REPLACE('=',40)
      DO WHILE .not. eof()
         @prow()+1,0 say kode_brg+' '+left(nama_brg,30)
         @prow()+1,2 say tran(jumlah,'@e 999,999 ')+satuan+'='+tran(diskon,'@e 999,999,999 =')+tran(harga_2,'@e 999,999,999')
         Tot=tot+harga_2
         Dis=dis+diskon
         No =no+1
         N  =n+1
         Skip
      ENDDO
      @prow()+1,0  say REPLACE('-',40)
      @prow()+1,0  say 'Jumlah Rp'+tran(tot+dis,'@e 9,999,999,999')
      @prow()+1,0  say 'Diskon Rp'+tran(dis    ,'@e 9,999,999,999')
      @prow()+1,0  say 'Total  Rp'+tran(tot    ,'@e 9,999,999,999')
      @prow()+1,0 say chr(27)+chr(33)+chr(4)
      @prow()+(jn+5),1 say ' '
      If idcut='Y'
         @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      ENDIF
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      Do pesan with pesanx
      SET CURSOR OFF
      RETURN 1
Else
      SET CURSOR OFF
      Do pesan with pesanx
      RETURN 1
ENDIF
RETURN 0

PROCEDURE ceklokal
If ikomp='C' .and. idata='0' .and. jk='0'
   Do pesan with 'Tunggu, sedang mempersiapkan File '+nfc+' ke Data Lokal'
   SELECT 77
   Close
   *
   SELECT 3
   Set order to 2
   Copy to &nfc field kode_bar,nama_brg,satuan,harga,harga_2,harga_21,;
                      harga_3,harga_32,batas_3,stok_a,stok_m,stok_k,stok_rb
   *
   SELECT 77
   Useidx('&nfc')
ENDIF
RETURN

* EOF CRX
