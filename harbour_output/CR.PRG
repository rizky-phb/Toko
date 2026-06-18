// ============================================================
// File    : CR.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

***CEK KURANGAN KALO CTK.ULANG
* PENJUALAN
SET TALK OFF
// SET STAT OFF  // Note: Not supported in Harbour
// SET SCOR OFF  // Note: Not supported in Harbour
SET SAFETY OFF
SET CONSOLE OFF
SET EXCLUSIVE OFF
SET CONFIRM ON
SET DELETED ON
Set wrap on
SET FIXED ON
SET CENTURY ON
SET DECIMALS TO 0
SET DATE ITALIAN
SET PROCEDURE TO crx
SET PROCEDURE TO kas
Set key -6 to kalk
CLEAR
STORE '' to aidi1,aidi2,empunya
*
Do idents
*
Blnthn='SUBSTR(dtoc(tgl),7,4)+SUBSTR(dtoc(tgl),4,2)'
Tgl   =date()
Ksa   =GetEnv("KASSA")
STORE '' to hr,brs1,brs13,fkt,fktx,nfc,nf0,nf1,nf2,nf3,nf4,pr1,pr2,pr3,pr4,klb,xpos,slihat,tglz,tgly,;
           warna1,warna2,warna3,warna4,warna5,warna6
STORE .f. to atas,bawah,xCetak,xOwner,ada,lihat,diedit,onlen,pilwarna,memsusul,cekpoin,hanyaCekp
STORE .f. to ubahjml,ubahjml2,hapus,isilagi,ulang,batal,adadis,member,nonctk
Gd=chr(205)
Gl=chr(196)
Tl=chr(179)
Do hari
Do cekdata

SELECT 3
*
SELECT 4
*
SELECT 5
*
SELECT 6
Uselan('DATA.SET','','','','','')
Rpoin =rp_debet
Rpoin2=rp_kredit
Alm1  =almt_1
Alm2  =almt_2
Alm3  =almt_3
Tgla  =tanggal_2
Tglb  =tanggal_3
Idtx  =RTRIM(id_cust)
Pdt   =pj_cust
Close
*
SELECT 99
Useidx('KASSA.SET')
LOCATE FOR no_kassa=ksa
If .not. FOUND()
   APPEND BLANK
   REPLACE no_kassa with ksa,mode_kasir with 'A',jark_nota with 4,jark_tepi with 0,lebr_roll with '5',cari_kode with '0',;
        id_komp with 'S',id_data with '0',id_prin with 'T',id_drwr with 'T',id_cut with 'T',id_disp with '2',id_poin with '0',tanggal with tgl
   COMMIT
ENDIF   
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
STORE 0   to jml2,hj,hj2,hp,dis1,dis2,dis3,dis31,dis4,jhg,byr,tot,tot2,tot3,kbl,blt,dns,nr,nrx,nrs,nrbyr,xAda2,kl,jwk,jmlso,xhal,totpn,totpn2,pnskr,pnskr2,pnsbl,isi,xPoin
STORE ' ' to id_ctk,lks,lksx,kdb,kdbx,nmb,st,stb,rn,ket1,ket2,ipn,ipn2
STORE .f. to ubahjml,ubahjml2,hapus,isilagi,ulang,batal,adadis,adatunda,ditunda
DO WHILE .t.
   SELECT 6
   Uselan('KASIR.DTA','','','','','')
   Ksr ='  '
   Sls =SPACE(20)
   Awal=0
   SET COLOR TO
   @0,0 CLEAR to 24,79
   SET COLOR TO /bg,w/b
   @9,22 CLEAR to 14,58
   @9,22 to 14,58
   @10,24 say 'Hari, Tgl.: '+hr+', '+dtoc(tgl)
   @11,24 say 'No. Kassa.: '+ksa
   @12,24 say 'Kode Kasir: '
   @13,24 say 'Cetak Nota:    T =Tidak_Cetak_Nota'
   @12,36 get sls pict '@!' valid sls#SPACE(20)
   Do pesan with 'Masukan Kode Kasir     ESC=Selesai'
   Read
   If lastkey()=27
      Exit
   ENDIF
   LOCATE FOR kode=sls
   If .not. FOUND()
      SET COLOR TO r+/b
      @12,36 say 'x x x x x x'
      Tone(1000,3)
      Tone(1500,3)
      Tone(2000,6)
      Do pesan with 'Kode Kasir Salah.  Ulangi !'
      Wait''
      Loop
   ENDIF
   STORE nama to nsls,nsls2
   STORE jens_jual to jj,itk
   Ksr=nomer
   *
   SET COLOR TO /bg,w+/b
   SET COLOR TO /bg,gr+/b
   @12,24 say 'Nama Kasir: '+nsls
   @13,36 get id_ctk pict '@!'
   Read
   SET COLOR TO w+
   If lastkey()=27
      Loop
   ENDIF
   If id_ctk='T'
      xCetak=.f.
   Else
      xCetak=.t.
   ENDIF
   *
   If tanggal#tgl
      Rlok()
      REPLACE tanggal with tgl
      If jj=' '
         REPLACE nota with 0
      ENDIF
      Ul()
   ENDIF
   Nn=nota+1
   If nn>99999
      Nn=1
   ENDIF
   Do xnota
   Do bacafile
   *
   Nfc='C:\TEMP\BR'+SUBSTR(&blnthn,3,2)+right(&blnthn,2)+'.TMP'
   Nfx='JUAL-'+ksa+'.DTA'
   Nfy='TUNDA-'+ksa+'.TMP'
   Do cektunda
   *
   If .not. file ('&nfx') .or. (.not. file('&nf0'))
      Do pesan2 with 'Belum ada File Data untuk Kassa-'+ksa+' , Lapor ke Kantor, jalankan Re-Index'
      Loop
   Else
      SELECT 5
      Useidx('&nfx')
      If batal
         Batal=.f.
         Loop
      ENDIF
      Go top
      If nama_ksr=nsls
         Yt='Y'
         SET COLOR TO w+/r
         @15,21 CLEAR to 20,59
         @15,21 to 20,59
         @16,23 say 'Ada Nota yang Belum di-Simpan...!'
         @17,23 say 'Tanggal: '+dtoc(tanggal)+' , Nota: '+faktur
         @19,23 say 'Mau diLanjutkan unt diSimpan? Y/T'get yt pict '!'
         Read
         If yt='T'
            REPLACE all kode_ksr with '  ',nama_ksr with SPACE(20),id_data with ' '
         Else
            Isilagi=.t.
         ENDIF
      Elseif nama_ksr#nsls
         REPLACE all kode_ksr with '  ',nama_ksr with SPACE(20),id_data with ' '
      ENDIF
      SET COLOR TO w+
   ENDIF
   SELECT 3
   Uselan('STOK.DTA','STOK5.NDX','STOK4.NDX','','','')
   Do ceklokal
   *
   N  =1
   Jab=SPACE(30)
   STORE SPACE(10) to plg,kta
   STORE ctod('  -  -    ') to tgl2,cTgljt
   STORE SPACE(25) to nsp,nsls5
   STORE SPACE(5) to ksls
   STORE ' ' to idp,yt,lks,lksx
   DO WHILE .t.
      CLEAR
      SET COLOR TO &warna5
      @0,0
      @0,1 say hr+', '+dtoc(tgl)
      SET COLOR TO &warna2
      @1,0 CLEAR to 3,79
      @1,1 say right(aidi1,17)
      @2,1 say aidi2
      @3,1 say 'Kasir: '+nsls
      @2,65 say 'Nota:'
      @3,65 say 'Item:'
      SET COLOR TO &warna1
      @4,0 say REPLACE(gl,8)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,10)+chr(194)+REPLACE(gl,5)+chr(194)+REPLACE(gl,10)
      @6,0 say REPLACE(gl,8)+chr(197)+REPLACE(gl,30)+chr(197)+REPLACE(gl,12)+chr(197)+REPLACE(gl,10)+chr(197)+REPLACE(gl,5)+chr(197)+REPLACE(gl,10)
      Kolomjl()
      B=7
      @24,0
      SET COLOR TO &warna2
      @2,74 say fkt
      SET COLOR TO &warna1
      @5,0 say 'Kode Brg'+tl+'Nama Barang'+SPACE(19)+tl+' Jml.Barang '+tl+'    @Harga'+tl+'@Dis%'+tl+'Jumlah Rp.'
      If xCetak .and. (.not. isilagi)
         *If xCetak .and. nonctk=.f.  &&&.and. iprn='D' 
         *   Do kopnota
         *Elseif xCetak .and. nonctk=.f.  &&&.and. (iprn='T' .and. idCut='T') 
         *   Do kopnota
         *Else
         *   Nonctk=.f.
         *Endi
      ENDIF
      If (.not. isilagi)
         Tot3=0
         Plg =SPACE(10)
         STORE SPACE(25) to nsp,jab
         @24,0
         Do tampil
         If lastkey()=27
            Exit
         ENDIF
         B=7
         If diEdit
            SELECT 5
            Go top
            DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
               Jhg =&brs1
               Brs2()
               B  =b+1
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               ENDIF
               Skip
            ENDDO
            B=7
         ENDIF
         *
      ENDIF
      If pilwarna
         Pilwarna=.f.
         Loop
      ENDIF
      *
      STORE 0 to dis1,tot,totr,totpn,totpn2,pnskr,pnskr2,kbl,tjml,no,jmlx
      Jml =1
      vKdb=SPACE(13)
      vNmb=SPACE(30)
      Idp =' '
      If isilagi
         Do hpsjual
         Isilagi=.f.
         *If diEdit
         *   Sele 4
         *   Close
         *Endi
      ENDIF
      DO WHILE .t.
         STORE SPACE(13) to kdb,kdbx
         Ksp=SPACE(5)
         Nmb=SPACE(30)
         STORE 0 to dis1,dis2,jhg
         *
         If lihat
            Rest scre from slihat
         ENDIF
         STORE .f. to lihat,memsusul
         *
         If .not. lihat
            Set key -1 to xlihat
         ENDIF
         *
         Set key  5 to xubahjml
         *
         If jj=' '
            Set key -8 to notunda
         ENDIF
         *
         Set key 42 to xubahjml2
         Set key 43 to xubahjml2
         Set key 28 to xadamem
         Do nomor with tot,1,56
         SET COLOR TO &warna1
         @b,0 say SPACE(8)+tl+SPACE(30)+tl+SPACE(12)+tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(10)
         If jml#1
            SET COLOR TO &warna5
            @b,40  say jml pict '@e 9999.99'
         ENDIF
         SET COLOR TO &warna2
         If diEdit=.f.
            @3,71  say tran(tjml,'999 /')+tran(no,'999')
         ENDIF
         @23,7 say SPACE(4)
         @23,7 say LTRIM(tran(pnskr,'999 + '))+LTRIM(tran(pnskr2,'999'))
         SET COLOR TO &warna6
         @20,69 say tran(tot,'@e 999,999,999')
         If tot3#0
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            SET COLOR TO &warna1
         ENDIF
         *
         SELECT 3
         SET COLOR TO &warna1
         @b,0 get kdb pict '@!S8'
         Do pesan with 'BarCode?   n*=Qty   '+chr(24)+'=Koreksi   F1=Member   F2=Cek_Hrg   F9=Tunda   ESC=Selesai'
         Read
         Set key  42 to
         Set key  43 to
         Set key  28 to
         Set key   5 to
         Set key  -1 to 
         Set key  -8 to 
         SET COLOR TO &warna3
         If lastkey()=27 .and. (ubahjml=.f. .and. ubahjml2=.f.)
            Exit
         ENDIF
         SET COLOR TO &warna1
         *      
         If n>200
            Tone(2000,1)
            Tone(1500,1)
            Do pesan with 'Kode Brg untuk 1 Nota Tidak Boleh Lebih dari 200 item.   ENTER-Selesai'
            Wait''
            Exit
         ENDIF
         *
         If ubahjml .and. n>1
            Do hpsjual
            Ubahjml=.f.
            Loop
         Elseif ubahjml .and. n=1
            Ubahjml=.f.
         ENDIF
         *
         If ubahjml2
            Ubahjml2=.f.
            Jml=val(kdb)
            Loop
         ENDIF
         *
         Do cekplg
         If member
            Member=.f.
            If no>0
               Memsusul=.t.
               Do hpsjual
            ENDIF
            Loop
         ENDIF
         *
         If ditunda
            Exit
         ENDIF
         *
         SELECT 3
         If xKsr='A'
            If left(kdb,2)='0 '
               Klb ='EX-'
               Nmb =SPACE(30)
               Isi =1
               St  ='  x  '
               Ipn =' '
               Ipn2=' '
               Dis1=0
               STORE 0 to hj,hj2,hjx
               @b,9  get nmb pict '@!S30'
               @b,40 get jml pict '   9999'
               @b,48 say left(st,4)
               @b,53 get hj  pict '@e   99999999'
               Read
               Hp =hj/1.05
               Hj2=hj
            Else
               If kdb=SPACE(13)
                  Carikode=.f.
                  Nmb=SPACE(30)
                  @b,9 get nmb pict '@!S30'
                  Do pesan with 'Nama Barang?'
                  Read
                  If nmb=SPACE(30)
                     Exit
                  ENDIF
                  Nmb=RTRIM(nmb)
                  Set order to 2
                  Seek RTRIM(nmb)
                  Do dafbrg
               Else
                  Carikode=.t.
                  Set order to 1
                  Seek kdb
                  If .not. FOUND()
                     Seek RTRIM(kdb)
                     If .not. FOUND()
                        Carikode=.f.
                        Set order to 2
                        Seek RTRIM(kdb)
                     ENDIF
                     Nmb=kdb
                     Do dafbrg
                  ENDIF
               ENDIF
               Kdbx=kode_bar
               *
               SELECT 3
               Set order to 1
               Seek kdbx
               Hj  =harga_2
               If lastkey()=27 .or. kdbx=SPACE(13)
                  Loop
               ENDIF
               *
               If hj>0
                  Ada=.f.
                  SELECT 5
                  Go top
                  DO WHILE .not. eof() .and. ( tanggal=tgl .and. kode_ksr=ksr .and. faktur=fkt )
                     If lihat=.f. .and. (kode_brg=kdb .or. kode_bar=kdbx)
                        Jml=jml+jumlah
                        Nr2=recno()
                        Ada=.t.
                        Exit
                     ENDIF
                     Skip
                  ENDDO
               ENDIF
               *
               SELECT 3
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
               STORE harga_2 to hj,hj2,vHj2
               Hj =iif(idp='1',iif(harga_21>0,harga_21,harga_2),harga_2)
               *
               Brs3()
               Do cekhj
               Do cariuntung
               If hp>hj
                  Hp=hj/1.05
               ENDIF
               *
               Jhg=&brs13
               Brs3()
               If lihat
                  Lihat=.f.
                  Do pesan with 'Transaksi Terakhir Hanya untuk MeLihat Harga...'
                  Wait''
                  Rest scre from slihat
                  Loop
               ENDIF
            ENDIF
         Elseif xKsr='B' .or. xKsr='C'
            Klb ='EX-'
            Nmb ='-BELANJA-'
            Isi =1
            St  ='  x  '
            Ipn =' '
            Ipn2=' '
            Dis1=0
            STORE 0 to hj,hj2,hjx
            @b,9  say nmb
            @b,40 say jml pict '  9,999'
            @b,48 say left(st,4)
            If xKsr='B'
               @b,40 get jml pict '   9999'
            ENDIF
            @b,53 get hj  pict '@e   99999999'
            Read
            Hp =hj/1.05
            Hj2=hj
         ENDIF
         *
         @24,0
         If lastkey()=27
            Loop
         ENDIF
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
         SELECT 5
         If n=1
            Go top
         Elseif ada
            Go nr2
         ENDIF
         REPLACE tanggal with tgl,faktur with fkt,sip with '1',kode_ksr with ksr,nama_ksr with nsls,;
              kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              isine_b with isi,jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,;
              kode_lks with lks,id_point with ipn,id_point2 with ipn2,rectran with 0,recstok with 0,id_data with ' '
         Skip
         Nr=recno()
         If ada
            Ada=.f.
            STORE 0 to tot,totpn,totpn2,tjml,no
            Kolomjl()
            Go top
            DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
               Jml =jumlah
               Hj  =harga_2
               Ipn =id_point
               Ipn2=id_point2
               Dis1=diskon
               Jhg =&brs13
               If recno()=nr2
                  SET COLOR TO gr+
                  @b,40 say tran(jumlah,'@e 9999.99')
                  SET COLOR TO w+
               ENDIF
               Brs2()
               If jml#0
                  No =no+1
               ENDIF
               B  =b+1
               Tot=tot+jhg
               Tjml=tjml+jumlah
               *
               Do hitpoin
               *
               If b>18
                  Scroll(7,0,18,79,1)
                  B=18
               ENDIF
               Skip
            ENDDO
            B =b-1
            No=no-1
         ENDIF
         *
         B  =b+1
         N  =n+1
         No =no+1
         Jml=1
         Kdb=SPACE(13)
         Nmb=SPACE(30)
         If b>18
            Scroll(7,0,18,79,1)
            B=18
         ENDIF
      ENDDO
      *
      If lihat
         STORE 0 to tot,kbl
         Lihat=.f.
         Wait''
         @7,0 CLEAR to 18,79
         Loop
      ENDIF
      *
      If ditunda
         Ditunda =.f.
         Adatunda=.t.
         Wkt=SUBSTR(time(),2,1)+SUBSTR(time(),4,2)+right(time(),2)
         *
         SELECT 5
         REPLACE all jam with wkt
         Copy to TUNDA.&ksa for ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
         REPLACE all kode_ksr with '  ',nama_ksr with SPACE(20)
         Go top
         *
         SELECT 99
         Do cektunda
         If .not. file('&nfy')
            Useidx('JUAL.DTA')
            Copy stru to &nfy
         ENDIF
         Useidx('&nfy')
         Jml=reccount()
         Appe from TUNDA.&ksa
         If reccount()#jml
            APPEND BLANK
            REPLACE jam with '-----',nama_brg with REPLACE('-',30),satuan with '-----'
         ENDIF
         *
         Do caritunda
         Do cektunda
         *
         SELECT 5
         Loop
      ENDIF
      *
      STORE 0 to dis31,blt,dns
      xSele=select()
      Do cekbulat
      Byr=0
      If jj=' '
         Byr=tot3+tot
      ENDIF
      Nulise='                    '+'Jumlah Rp.'+tran(tot,'@e 99,999,999')
      *Kedisple(nulise)
      DO WHILE .t.
         Batal=.f.
         STORE .f. to batal,adadis,member,donasi
         Set key  28 to xadamem
         Set key  46 to nol3
         Set key  68 to xadadis
         Set key 100 to xadadis
         SET COLOR TO &warna6
         If blt#0
            @19,57 say 'Pembulatan :'
            @19,75 say tran(blt,'@e 9,999')
         ENDIF
         @20,69 say tran(tot,'@e 999,999,999')
         @23,7 say LTRIM(tran(pnskr,'999 + '))+LTRIM(tran(pnskr2,'999'))
         If tot3#0
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
         ENDIF
         Dis31=(tot*(dis3/100))
         @21,63 say dis3  pict '@e 99.99'
         @21,69 say dis31 pict '@e 999,999,999'
         @22,69 get byr pict '@e   999999999' &&valid byr>=tot
         Do pesan with 'Bayar?     +_=Donasi     D=Diskon     F1=Member     Total Riil =Rp '+LTRIM(tran(tot3+((tot+blt)-dis31),'@e 999,999,999'))
         SET COLOR TO &warna4
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
               ENDIF
            Else
               Dis3=dis3
            ENDIF
            Byr=tot3+(tot-dis31)
            @22,69 get byr pict '@e 999,999,999'
            Do pesan with 'Isi Jumlah Pembayaran   ( Total - Diskon = Rp '+LTRIM(tran(tot3+((tot+blt)-dis31),'@e 999,999,999'))+' )'
            Read
         ENDIF
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
            ENDIF
            Exit
         ENDIF
         *
         Itk =' '
         Kbl=abs((byr-(tot3+((tot+blt)-dis31))))
         SET COLOR TO &warna6
         @22,69 say byr pict '@e 999,999,999'
         If byr>(tot-dis31) &&.and. plg=SPACE(10)
            @20,22 CLEAR to 23,55
            @23,42 say 'UANG KEMBALIAN'
            Do nomor with kbl,20,53
         ENDIF
         *
         If byr<(tot3+(tot-dis31)) .and. (tot-dis31)#0
            SET COLOR TO gr+/b
            @19,57 say 'Sisa Lalu  :'+tran(tot3,'@e 999,999,999')
            @20,57 say 'Jml.Belanja:'+tran(tot ,'@e 999,999,999')
            @21,57 say 'Total Semua:'+tran(tot3+tot,'@e 999,999,999')
            Do pesan with '    Benar Masuk Piutang? Y/T:'
            SET COLOR TO w+/r
            @24,1 say 'Sisa/Kurang Rp '+LTRIM(tran(kbl,'@e 999,999,999'))
            Yt=' '
            DO WHILE .not. yt$'YT'
               @24,55 get yt pict '!'
               Read
               If lastkey()=27
                  Exit
               ENDIF
            ENDDO
            If lastkey()=27 .or. yt='T'
               Loop
            ENDIF
            Itk='K'
         ENDIF
         Do pesan with iif(xcetak,'ENTER=Simpan+Cetak','C=Simpan+Cetak     ENTER=Simpan/Tdk.Ctk')+iif(kbl>0,'     -+D=Donasi','')+'     ESC=Koreksi'
         Wait''
         If kbl>0 .and. (lastkey()=43 .or. lastkey()=45 .or. lastkey()=68 .or. lastkey()=100)
            Do pesan with 'Masukan Jumlah Uang untuk Donasi'
            SET COLOR TO ,/w
            @23,69 get dns pict '@e        9999'
            Read
            Kbl=(byr-(tot3+((tot+blt)-dis31)))-dns
            SET COLOR TO &warna6
            @20,22 CLEAR to 23,55
            @23,69 say dns pict '@e 999,999,999'
            @23,42 say 'UANG KEMBALIAN'
            Do nomor with kbl,20,53
            Do pesan with iif(xcetak,'ENTER=Simpan+Cetak','C=Simpan+Cetak     ENTER=Simpan/Tdk.Ctk')+'     ESC=Koreksi'
            Wait''
         ENDIF
         Exit
      ENDDO
      If lastkey()=27 .or. isilagi
         If diEdit
            Tone(1500,0.5)
            Tone(2000,2)
            Do pesan with 'Nota ini adalah Nota Edit,  Simpan dulu... lalu Edit kembali pakai F10/F5'
            Wait''
            Loop
            Exit
         ENDIF
         Isilagi=.t.
         Loop
      ENDIF
      Do pesan with 'Tunggu, sedang diCetak'
      I=uppe(chr(lastkey()))
      xAda2=0
      *
      SELECT 5
      If xCetak .and. lastkey()=13 .and. tot#0 
         Go top
         Do ctknota
      Elseif xCetak
         NonCtk=.t.
      ENDIF
      *
      SELECT 4
      Do pesan with 'Tunggu, Nota sedang diSimpan...'
      Do bacafile
      Uselan('&nf0','&nf3','&nf1','&nf2','','')
      *
      STORE 0 to xada,xada3,tot2,totpn,totpn2
      Wkt=time()
      *
      SELECT 5
      Go top
      DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
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
            ENDIF
            SELECT 4
            Flok()
            APPEND BLANK
            REPLACE tanggal with tgl,jam with wkt,faktur with fkt,sip with '1',kassa with ksa,kode_ksr with ksr,nama_ksr with nsls,kode_lks with lks,kode_sls with ksls,;
                 kode_lgn with plg,kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,satuan_b with stb,isine_b with isi,jumlah with jml,;
                 harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,keterangan with ket,kode_lks with lks,id_point with ipn,id_point2 with ipn2,id_data with itk
            Ul()
            xAda3=xada3+1
            *
            SELECT 3
            Set order to 1
            Seek kdbx
            Rlok()
            REPLACE stok_k&lks with (stok_k&lks+jml),tanggal_2 with tgl
            Ul()
         Elseif idt='1' .and. plg#SPACE(10)
            SELECT 4
            Go nrx
            Rlok()
            REPLACE kode_lgn with plg
            Ul()
         ENDIF
         *
         SELECT 5
         REPLACE kode_ksr with '  ',nama_ksr with SPACE(20),id_data with ' '
         Skip
      ENDDO
      *
      SELECT 4
      If dis31>0
         Flok()
         APPEND BLANK
         REPLACE tanggal with tgl,jam with wkt,faktur with fkt,kassa with ksa,kode_lks with lks,kode_ksr with ksr,nama_ksr with nsls,kode_sls with ksls,;
              kode_lgn with plg,kode_klb with '-----',nama_brg with 'Diskon Total Penjualan-->',jumlah with 1,harga_2 with (harga_2-dis31),id_data with itk
         Ul()
      ENDIF
      Rlok()
      REPLACE rp_bayar with byr,rp_bulat with blt,rp_donasi with dns
      Ul()
      COMMIT
      *
      If diEdit
         Go nrbyr
         Rlok()
         REPLACE rp_bayar with byr,rp_bulat with blt,rp_donasi with dns
         Ul()
         COMMIT
      ENDIF
      Close
      *
      If plg#SPACE(10)   &&&.and. (tot-dis31)#0 .and. (tot3#0 .or. byr<(tot-dis31))
         SELECT 11
         Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')
         Seek dtos(tgl)+plg+fkt
         If .not. FOUND()
            Flok()
            APPEND BLANK
         Else
            Rlok()
         ENDIF
         REPLACE tanggal with tgl,tanggal_3 with (tgl+jwk),faktur with fkt,nama_ksr with nsls,kode_sls with ksls,kode_lgn with plg,nama_lgn with nsp,almt_lgn with jab,kota_lgn with kta,;
              rp_sisa with tot3,rp_total with tot,rp_pokok with tot2,rp_bayar with iif(byr>(tot3+(tot-dis31)),(tot3+(tot-dis31)),byr),diskon with dis3,rp_diskon with dis31,jml_poin with pnskr+pnskr2,pra_poin with xPoin
         Ul()
         Close
         *
      ENDIF
      *
      If diEdit
         Fkt=fktx
         Tgl=tglz
      ENDIF
      If xada#0
         SELECT 6
         LOCATE FOR nomer=ksr
         Rlok()
         REPLACE nota with nn
         Ul()
         Nn=nn+1
         If nn>99999
            Nn=1
         ENDIF
         Do xnota
      ENDIF
      Plg=SPACE(10)
      STORE SPACE(5) to ksls
      STORE SPACE(25) to nsp,nsls5
      STORE ctod('  -  -    ') to tgl2,cTgljt
      STORE ' ' to idp,yt,lks,lksx
      STORE 0 to hj,dis1,dis2,dis3,byr,jwk,totpn,totpn2,pnskr,pnskr2,tot3
      STORE 1 to jml,n
      STORE .f. to isilagi,diEdit
      Jab=SPACE(30)
      Kta=SPACE(10)
      Nsls2 =nsls
      Nulise='    Terima Kasih'+SPACE(24)
      *Kedisple(nulise)
      *
      Kolomjl()
   ENDDO
   Nn=nn
   Do xnota
ENDDO
SET COLOR TO
CLEAR
Nulise=SPACE(20)+SPACE(20)
*Kedisple(nulise)
CLOSE all
RELEASE all
RETURN

FUNCTION kolomjl
SET COLOR TO &warna5
@0,30 say iif(adatunda,'Ada Nota yg diTunda',SPACE(20))
SET COLOR TO &warna1
B=7
DO WHILE b<=18
   @b,0 say SPACE(8)+tl+SPACE(30)+tl+SPACE(12)+tl+SPACE(10)+tl+SPACE(5)+tl+SPACE(10)
   B=b+1
ENDDO
B=7
@19,0 say REPLACE(gl,8)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,10)+chr(193)+REPLACE(gl,5)+chr(193)+REPLACE(gl,10)
SET COLOR TO &warna2
@20,0  CLEAR to 23,79
@20,1  say 'Kode:                                       '+SPACE(12) +'Jml.Belanja:'+SPACE(13)
@21,1  say 'Nama:'+SPACE(31)+'                    Disc:      %'+SPACE(13)
@22,1  say 'Almt:'+SPACE(31)+'                    Jml.Bayar  :'+SPACE(13)
@23,1  say 'Poin:'+SPACE(31)+'                    Donasi     :'+SPACE(13)
*Set colo to &warna
@20,7  say plg
@21,7  say RTRIM(nsp)
@22,7  say RTRIM(jab)
@23,7  say SPACE(13)
@21,63 say dis3 pict '@e 99.99'
RETURN .t.

*** PROSEDUR HAPUS TRANSAKSI
PROCEDURE hpsjual
Kolomjl()
SELECT 5
If ubahjml2
   Set filter to ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls2 )
Else
   Set filter to ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
ENDIF
Go top
If nama_ksr=SPACE(20)
   Set filter to
   RETURN
ENDIF
No=1
DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
   No=no+1
   Skip
ENDDO
If no>18
   No=18
ENDIF
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
ENDIF
*
DECLARE field[8],bts[8],frm[8]
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
SET COLOR TO w+/r
@5,28 say chr(254)+' KOREKSI '+chr(254)
Do pesan with pesany
SET COLOR TO &warna3
DbEdit(7,0,18,79,field,'fhpjx',frm,'','',bts)
*
SET COLOR TO &warna1
@5,28 say SPACE(11)
Kolomjl()
N=1
STORE 0 to tot,tot2,totpn,totpn2,tjml,byr,no
Set filter to
Go top
DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. kode_ksr=ksr )
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
   ENDIF
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
   ENDIF
   Skip
ENDDO
Nmb=SPACE(30)
Kdb=SPACE(13)
Jml=1
STORE 0 to dis1,dis2
STORE .f. to hapus
RETURN

Function fhpjx
Parameter mode,pointer
Private fds
Fds=field[pointer]
If mode<4
   RETURN 1
Elseif lastkey()=27
   RETURN 0
Elseif lastkey()=13 .and. fds='left(KODE_BAR,8)'
   Cekvar()
   Kdb=SPACE(13)
   Tjml=tjml-jml
   SET CURSOR ON
   DO WHILE .t.
      SELECT 3
      SET COLOR TO &warna3
      @b,0 say SPACE(8)
      @b,0 get kdb pict '@!S8'
      Do pesan with 'BarCode?'
      Read
      If lastkey()#27
         Carikode=.t.
         Set order to 1
         Seek kdb
         If .not. FOUND()
            Seek RTRIM(kdb)
            If .not. FOUND()
               Carikode=.f.
               Set order to 2
               Seek RTRIM(kdb)
            ENDIF
            Nmb=kdb
            Do dafbrg
         ENDIF
         If lastkey()=27 .or. kdbx=SPACE(13)
            SELECT 5
            Do pesan with pesany
            SET COLOR TO &warna3
            SET CURSOR OFF
            RETURN 1
         ENDIF
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
            ENDIF
         ENDIF
         Jhg =&brs13
         Tot =tot+&brs13
         Brs3()
         *
         SELECT 5
         REPLACE kode_spl with ksp,kode_klb with klb,kode_brg with kdb,kode_bar with kdbx,nama_brg with nmb,satuan with st,;
              jumlah with jml,harga with hp,harga_2 with hj,harga_22 with hj2,diskon with dis1,diskon_rp with dis2,id_point with ipn,id_point2 with ipn2
         COMMIT
         *
         Sum &brs1 to tot
         Do nomor with tot,1,56
         SET COLOR TO &warna2
         @3,71  say tran(tjml,'999 /')+tran(no,'999')
         *Set colo to &warna4
         @20,69 say tran(tot,'@e 999,999,999')
      ENDIF
      Exit
   ENDDO
   Do pesan with pesany
   SET COLOR TO &warna3
   *
   SELECT 5
   Go nr
   Jhg =&brs13
   Nulise=left(nmb,20)+tran(jml,'@e 999 ')+left(st,3)+'=Rp'+tran(jhg,'@e 99,999,999')
   *Kedisple(nulise)
   SET CURSOR OFF
   RETURN 1
Elseif ( (lastkey()>=48 .and. lastkey()<=57) .or. lastkey()=45 ) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 99,999,999')" .or. fds="tran(DISKON,'@e 99.99')")
   Cekvar()
   Tjml=tjml-jml
   SET CURSOR ON
   Do pesan with ''
   *
   Set key  5 to atas
   Set key 24 to bawah
   If fds="left(NAMA_BRG,30)"
      Jmlx=chr(lastkey())+SPACE(6)
      *
      SELECT 3
      Set order to 1
      Seek kdbx
      Set key 28 to xhapusi
      @b,40 get jmlx
      If left(kdb,2)='0 '
         @b,53 get hj pict '@e 99,999,999'
      ENDIF
      Keyboard chr(4)
      Do pesan with 'Masukan Jml.Barang-nya, desimal pake "."   ('+LTRIM(tran(jml,'@e 9999.99'))+')'
      Read
      Set key 28 to
      *
      If at('.',jmlx)>0
         Jmlx=jmlx
      Else
         If len(RTRIM(jmlx))>4
            Jmlx=LTRIM(str(jml))
         ENDIF
      ENDIF
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
      ENDIF
      Tjml=tjml+jml
   Elseif fds="tran(HARGA_2,'@e 99,999,999')"
      Jmlx=chr(lastkey())+SPACE(9)
      *
      @b,53 get jmlx
      Keyboard chr(4)
      Do pesan with 'Masukan Harga-nya, desimal pake "."   ('+LTRIM(tran(hj,'@e 9,999,999.99'))+')'
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
      If len(RTRIM(jmlx))>7
         Jmlx=LTRIM(str(hj))
      ENDIF
      Hj =iif(lastkey()=27,hj,val(jmlx))
   Else
      Jmlx=chr(lastkey())+'    '
      @b,64 get jmlx
      Keyboard chr(4)
      Do pesan with 'Masukan Diskon-nya, desimal pake "."   ('+LTRIM(tran(dis1,'@e 99.99'))+')'
      Read
      If len(RTRIM(jmlx))>4
         Jmlx=LTRIM(str(dis1))
      ENDIF
      Dis1=iif(lastkey()=27,dis1,val(jmlx))
   ENDIF
   Set key  5 to
   Set key 24 to
   *
   SELECT 5
   If lastkey()#27
      Jhg =&brs13
      REPLACE jumlah with jml,harga with hp,harga_2 with hj,diskon with dis1
      COMMIT
      *
      Do cekedit  &&CEK APAKAH EDIT NOTA ADA YG DI-UPDATE?!
   ENDIF
   Sum jumlah,&brs1 to tjml,tot
   Do nomor with tot,1,56
   If diEdit=.f.
      SET COLOR TO &warna2
      @3,71  say tran(tjml,'999 /')+tran(no-1,'999')
   ENDIF
   SET COLOR TO &warna2
   @20,69 say tran(tot,'@e 999,999,999')
   *
   Do pesan with pesany
   SET COLOR TO &warna3
   SET CURSOR OFF
   *
   Go nr
   If atas
      Keyboard chr(5)
   ENDIF
   If bawah
      Keyboard chr(24)
   ENDIF
   STORE .f. to atas,bawah
   RETURN 1
Elseif (lastkey()=-2 .or. lastkey()=-3) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 9,999,999')")
   ***Ecer1/Bakul1
   B  =row()
   Grs=.f.
   If lastkey()=-3
      Grs=.t.
   ENDIF
   *
   Cekvar()
   *
   SET CURSOR ON
   Do pesan with ''
   *
   If kdbx#SPACE(13)
      SELECT 3
      Set order to 1
      Seek kdbx
      If grs
         Do CekHj    &&&DISAMAKAN
      Else
         Do CekhEcr
      ENDIF
   ENDIF
   *
   SELECT 5
   REPLACE harga_2 with hj
   COMMIT
   *
   Do cekedit
   *
   Sum &brs1 to tot
   Do nomor with tot,1,56
   *
   Do pesan with pesany
   SET COLOR TO &warna3
   SET CURSOR OFF
   Go nr
   Jhg=&brs13
   RETURN 1
Elseif (lastkey()=-4 .or. lastkey()=-5) .and. (fds="left(NAMA_BRG,30)+tl+tran(JUMLAH,'@e 9999.99 ')+left(SATUAN,4)" .or. fds="tran(HARGA_2,'@e 9,999,999')")
   B  =row()
   Nr =recno()
   Grs=.f.
   If lastkey()=-4 .or. lastkey()=9
      Grs=.t.
   ENDIF
   *
   SELECT 5
   Go top
   DO WHILE .not. eof() .and. ( tanggal=tgl .and. faktur=fkt .and. nama_ksr=nsls )
      Nr  =recno()
      Nrx =rectran
      Kdbx=kode_bar
      Hj  =harga_2
      Hp  =harga
      STORE jumlah to jml,jml2
      SET CURSOR ON
      Do pesan with ''
      *
      If kdbx#SPACE(13)
         SELECT 3
         Set order to 1
         Seek kdbx
         If grs
            Do CekHj
         Else
            Do CekHEcr
         ENDIF
         *
         SELECT 5
         Go nr
         REPLACE harga_2 with hj
         *
         Do cekedit
         *
      ENDIF
      Skip
   ENDDO
   COMMIT
   *
   Sum &brs1 to tot
   Do nomor with tot,1,56
   *
   Do pesan with pesany
   SET CURSOR OFF
   Go nr
   Jhg=&brs13
   RETURN 0
Else
   SET CURSOR OFF
   RETURN 1
ENDIF
RETURN 0

PROCEDURE cariuntung
If hp=0
   Hp=hj/1.1
ENDIF
RETURN

FUNCTION cekvar
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
STORE jumlah to jml,jml2
RETURN(.t.)

PROCEDURE cekedit
If diEdit
   SELECT 3
   Rlok()
   REPLACE stok_k&lks with (stok_k&lks-jml2)+jml
   Ul()
   COMMIT        
   *
   SELECT 4
   Go nrx
   Rlok()
   REPLACE jumlah with jml,harga with hp,harga_2 with hj,diskon with dis1,diskon_rp with dis2
   If jml#jml2
      REPLACE keterangan with 'Asal: '+LTRIM(tran(jml2,'@e 9999 '))+left(st,3)+' /'+left(dtoc(tgl),2)+SUBSTR(dtoc(tgl),4,2)+right(dtoc(tgl),2)+'-'+left(time(),5)+'/'+ksa
   ENDIF
   Ul()
   COMMIT
   *
   SELECT 5
ENDIF
RETURN

PROCEDURE cekhj
If xKsr#'A'
   RETURN
ENDIF
If left(kdb,2)#'0 '
   Hj =harga_2
   If (jml>=batas_3 .and. batas_3>0 .and. harga_3>0)
      Hj=harga_3
   ENDIF
   If (jml>=batas_4 .and. batas_4>0 .and. harga_4>0)
      Hj=harga_4
   ENDIF
   If (jml>=batas_5 .and. batas_5>0 .and. harga_5>0)
      Hj=harga_5
   ENDIF
   If hj=0
      Brs3()
      @b,53 get hj pict '@e 9,999,999'
      Read
   ENDIF
ENDIF
RETURN

PROCEDURE CekHEcr
If xKsr#'A'
   RETURN
ENDIF
Hj=iif(harga_21>0,harga_21,harga_2)
If (jml>=batas_3 .and. batas_3>0 .and. (harga_3>0 .or. harga_32>0))
   Hj=iif(harga_32>0,harga_32,harga_3)
ENDIF
If (jml>=batas_4 .and. batas_4>0 .and. (harga_4>0 .or. harga_42>0))
   Hj=iif(harga_42>0,harga_42,harga_4)
ENDIF
If (jml>=batas_5 .and. batas_5>0 .and. (harga_5>0 .or. harga_52>0))
   Hj=iif(harga_52>0,harga_52,harga_5)
ENDIF
RETURN

PROCEDURE hitpoin
If (ipn=' ' .and. ipn2=' ') .and. rpoin>0
   Totpn =totpn  + (jml * (hj - (hj*(dis1/100))))
   Pnskr =(totpn-mod(totpn,rpoin))/rpoin
ENDIF
If (ipn=' ' .and. ipn2#' ') .and. rpoin2>0
   Totpn2=totpn2 + (jml * (hj - (hj*(dis1/100))))
   Pnskr2=(totpn2-mod(totpn2,rpoin2))/rpoin2
ENDIF
RETURN

***CETAK STROK
PROCEDURE kopnota
Do cp
If lastkey()=27
   RETURN
ENDIF
SET DEVICE TO PRINTER
Set marg to jt
If iprn='D'
   @prow()+1,0 say chr(27)+chr(33)+chr(33)+chr(27)+'a'+'1'+aidi1+chr(27)+chr(33)+chr(5)
   @prow()+1,0 say chr(27)+'a'+'1'+LTRIM(RTRIM(alm1))
   If alm2#SPACE(40)
      @prow()+1,0 say LTRIM(RTRIM(alm2))
   ENDIF
   @prow()+1,0 say chr(27)+'a'+'0'+chr(27)+chr(33)+chr(4)+REPLACE('-',33)
   If idcut='Y' .and. tot#0 .or. xAda2#0
      @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
      @prow(),0 say chr(27)+chr(33)+chr(4)+' '
   Elseif tot=0
      @prow(),0 say ' '
   ENDIF
Else
   @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(32)+'TOKO SEMBAKO'
   @prow()+1,0 say aidi1+chr(27)+chr(33)+chr(4)
   @prow()+1,0 say LTRIM(RTRIM(alm1))
   If alm2#SPACE(40)
      @prow()+1,0 say LTRIM(RTRIM(alm2))
   ENDIF
   @prow()+1,0 say chr(27)+'a'+'0'+REPLACE('-',32)
   @prow(),0   say ' '
ENDIF
SET DEVICE TO SCREEN
RETURN

PROCEDURE ctknota
Do cp
If lastkey()=27
   RETURN
ENDIF
***JIKA THERMAL
If iprn='T'
   Do ctknotath
   RETURN
ENDIF
***
SET DEVICE TO PRINTER
If idrw='Y' .and. byr#0 .and. ulang=.f.
   @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   @prow(),0 say chr(27)+chr(33)+chr(4)
ENDIF
If ulang
   @prow()+1,0 say chr(27)+chr(33)+chr(4)+'* REPRINT *'
ENDIF
STORE 0 to dis2,totr,totpn,totpn2
DO WHILE .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
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
      ENDIF
      @prow()+1,0 say left(nama_brg,30)
      @prow()+1,2 say LTRIM(jml)+' '+RTRIM(satuan)+iif(left(satuan,1)#' ',' X ',' ')+LTRIM(tran(harga_2,'@e 99,999,999'))
      If diskon#0
         @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
      ENDIF
      @prow(),23  say tran(jhg,'@e 99,999,999')
      If jumlah_rtr#0
         Jml3=tran(jumlah_rtr,'@e 99,999')
         If jumlah_rtr#0
            If (jumlah_rtr>0 .and. jumlah_rtr<1) .or. (jumlah_rtr<0 .and. jumlah_rtr>-1)
               Jml3=tran(jumlah_rtr,'@e 999.99')
            Elseif mod(jumlah_rtr,int(jumlah_rtr))>0
               Jml3=tran(jumlah_rtr,'@e 999.99')
            ENDIF
         ENDIF
         @prow()+1,0 say ' RETUR='+jml3+'x @'+tran(harga_2,'@e 9,999,999')
         @prow(),28 say tran(0-(jumlah_rtr*harga_2),'@e   99,999,999')
         Totr=totr+(jumlah_rtr*harga_2)
      ENDIF
      No=no+1
      If no>50
         @prow(),0 say ' '
         SET DEVICE TO SCREEN
         *
         Do pesan with 'ENTER=Halaman_Selanjutnya'
         Inkey(5)
         No=1
         Do pesan with 'Tunggu, sedang diCetak'
         SET DEVICE TO PRINTER
         @prow(),0 say chr(27)+chr(33)+chr(4)
      ENDIF
   ENDIF
   Skip
ENDDO
If dis2>0
   @prow()+2,0 say 'JUMLAH DISKON ITEM = ( Rp'+tran(dis2,'@e 99,999,999')+' )'
ENDIF
If blt#0
   @prow()+1,10 say 'PEMBULATAN  '+tran(blt,'@e 999,999,999')
ENDIF
*
@prow()+1,0 say REPLACE('-',33)
If totr#0
   @prow()+1,18 say 'SUB TOTAL  '+tran(tot,'@e 99,999,999')
   @prow()+1,18 say 'RETUR      '+tran(0-totr,'@e 99,999,999')
ENDIF
@prow()+1,0 say LTRIM(str(tjml))+' itm'
@prow(),10  say 'JML.BELANJA '+tran(tot-totr, '@e 999,999,999')
If tot3#0
   @prow()+1,10 say 'KURANGAN    '+tran(tot3,'@e 999,999,999')
   @prow()+1,10 say '            -----------'
   @prow()+1,10 say 'TOTAL RP    '+tran(tot+tot3,'@e 999,999,999')
ENDIF
If dis31>0
   @prow()+1,7 say 'DISKON '+tran(dis3,'@e 99.99 % ')+tran(dis31,'@e 999,999,999')
ENDIF
If dns>0
   @prow()+1,10  say 'UNTUK DONASI'+tran(dns,'@e 999,999,999')
ENDIF
@prow()+1,10  say 'B A Y A R   '+tran(byr,'@e 999,999,999')
If kbl#0
   @prow()+1,10 say iif(Kbl>0,'KEMBALI     ','K U R A N G ')+tran(abs(kbl),'@e 999,999,999')
ENDIF
Kata=hr+', '+dtoc(tgl)+' '+tran(time(),'XX:XX')+' '+fkt+'/'+nsls
@prow()+2,0 say chr(27)+chr(33)+chr(5)+left(kata,40)
*
If plg#SPACE(10) &&.and. ( rpoin>0 .or. rpoin2>0 ) .and. idp=' '
   @prow()+1,0 say REPLACE('-',40)
   @prow()+1,0 say 'Nama: '+RTRIM(nsp)+' ('+RTRIM(plg)+')'
   @prow()+1,0 say '    : '+jab
   If iPoin#'0'
      @prow()+1,0 say 'Poin: '+LTRIM(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+LTRIM(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
   ENDIF
ENDIF
*
SELECT 1
Uselan('DATA.SET','','','','','')
If promo1#SPACE(40)
   @prow()+1,0 say REPLACE('-',40)
   @prow()+1,0 say RTRIM(promo1)
ENDIF
If promo2#SPACE(40)
   @prow()+1,0 say RTRIM(promo2)
ENDIF
If promo3#SPACE(40)
   @prow()+1,0 say RTRIM(promo3)
ENDIF
If promo4#SPACE(40)
   @prow()+1,0 say RTRIM(promo4)
ENDIF
Close
*
@prow()+jn,1 say ''
Ulang=.f.
SET DEVICE TO SCREEN
RETURN

PROCEDURE ctknotath
SET DEVICE TO PRINTER
If lr='5'
   If idCut='Y'
      Set marg to jt
      @prow()+1,0 say chr(27)+chr(33)+chr(4)+chr(27)+'a'+'1'+'TOKO SEMBAKO DAN SNACK'
      @prow()+1,0 say chr(27)+'a'+'1'+chr(27)+chr(33)+chr(32)+LTRIM(aidi1)+chr(27)+chr(33)+chr(4)
      @prow()+1,0 say LTRIM(RTRIM(alm1))
      If alm2#SPACE(40)
         @prow()+1,0 say LTRIM(RTRIM(alm2))
      ENDIF
      @prow()+1,0 say chr(27)+'a'+'0'+REPLACE('-',32)
   ENDIF
   *
   If idrw='Y' .and. byr#0 .and. ulang=.f.
      @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   ENDIF
   If ulang
      @prow()+1,0 say '* REPRINT *'
   ENDIF
   STORE 0 to dis2,totr,totpn,totpn2
   Adabns=.f.
   DO WHILE .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
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
         ENDIF
         @prow()+1,0 say left(nama_brg,30)
         @prow()+1,4 say LTRIM(jml)+' '+RTRIM(satuan)+iif(left(satuan,1)#' ',' X ',' ')+LTRIM(tran(harga_2,'@e 9,999,999'))
         @prow(),22  say tran(jhg,'@e 99,999,999')
         If diskon#0
            @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
         ENDIF
      ENDIF
      Skip
   ENDDO
   If blt#0
      @prow()+1,10 say 'PEMBULATAN '+tran(blt,'@e 999,999,999')
   ENDIF
   @prow()+1,0 say REPLACE('-',32)
   If totr#0
      @prow()+1,10 say 'SUB TOTAL  '+tran(tot,'@e 999,999,999')
      @prow()+1,10 say 'RETUR      '+tran(0-totr,'@e 999,999,999')
   ENDIF
   @prow()+1,0 say chr(27)+chr(33)+chr(32)+'TOTAL'+tran(tot-totr, '@e 999,999,999')+chr(27)+chr(33)+chr(4)
   If dis2>0
      @prow()+1,0  say 'DISKON ITEM Rp.'+tran(dis2,'@e 9,999,999')
   ENDIF
   If dis31>0
      @prow()+1,12 say 'DISKON   '+tran(dis31,'@e 999,999,999')
   ENDIF
   If dns>0
      @prow()+1,12 say 'DONASI   '+tran(dns,'@e 999,999,999')
   ENDIF
   @prow()+1,0 say tran(no,'9999 itm. ')+'  BAYAR    '+tran(byr,'@e 999,999,999')
   If kbl#0
      @prow()+1,12 say iif(kbl>0,'KEMBALI  ','KURANG    ')+tran(abs(kbl),'@e 999,999,999')
   ENDIF
   @prow()+1,0  say 'Kasir: '+RTRIM(nsls)+' ,  No: '+fkt
   @prow()+1,0  say hr+', '+dtoc(tgl)+' // '+tran(time(),'XX:XX')
   If plg#SPACE(10)
      @prow()+1,0 say REPLACE('-',32)
      @prow()+1,0 say 'Nama: '+RTRIM(nsp)+' ('+RTRIM(plg)+')'
      @prow()+1,0 say '    : '+RTRIM(jab)
      If iPoin#'0'
         @prow()+1,0 say 'Poin: '+LTRIM(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+LTRIM(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
      ENDIF
   ENDIF   
   *
   SELECT 1
   Uselan('DATA.SET','','','','','')
   If promo1#SPACE(40)
      @prow()+1,0 say REPLACE('-',32)
      @prow()+1,0 say promo1
   ENDIF
   If promo2#SPACE(40)
      @prow()+1,0 say promo2
   ENDIF
   If promo3#SPACE(40)
      @prow()+1,0 say promo3
   ENDIF
   If promo4#SPACE(40)
      @prow()+1,0 say promo4
   ENDIF
   Close
Else
   If idCut='Y'
      Set marg to jt
      @prow()+1,0 say chr(27)+chr(33)+chr(4)+chr(27)+'a'+'1'+'TOKO SEMBAKO DAN SNACK'
      @prow()+1,0 say chr(27)+chr(33)+chr(32)+chr(27)+'a'+'1'+aidi1+chr(27)+chr(33)+chr(4)
      @prow()+1,0 say chr(27)+'a'+'1'+RTRIM(alm1)
      If alm2#SPACE(40)
         @prow()+1,0 say chr(27)+'a'+'1'+RTRIM(alm2)
      ENDIF
      @prow()+1,0 say chr(27)+'a'+'0'+REPLACE('-',45)
   ENDIF
   *
   If idrw='Y' .and. byr#0 .and. ulang=.f.
      @prow(),0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
   ENDIF
   If ulang
      @prow()+1,0 say chr(27)+chr(33)+chr(4)+'* REPRINT *'
   ENDIF
   STORE 0 to dis2,totr,totpn,totpn2
   DO WHILE .not. eof() .and. (faktur=fkt .and. (nama_ksr=nsls .or. nama_ksr=nsls2))
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
         ENDIF
         If jumlah=1
            @prow()+1,0 say left(RTRIM(nama_brg),25)+', 1 '+RTRIM(satuan)
            If diskon#0
               @prow()+1,4 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+LTRIM(tran((jumlah*harga_2)*(diskon/100),'@e 999,999 '))+')'
            ENDIF
            @prow(),34  say tran(jhg,'@e 999,999,999')
         Else
            @prow()+1,0 say left(nama_brg,30)
            @prow()+1,2 say LTRIM(jml)+' '+RTRIM(satuan)+iif(left(satuan,1)#' ',' X ',' ')+LTRIM(tran(harga_2,'@e 9,999,999'))
            If diskon#0
               @prow()+1,2 say 'Diskon:'+tran(diskon,'@e 99.99 % (')+tran((jumlah*harga_2)*(diskon/100),'@e 999,999 ')+')'
            ENDIF
            @prow(),34  say tran(jhg,'@e 999,999,999')
         ENDIF
         If jumlah_rtr#0
            Jml3=tran(jumlah_rtr,'@e 99,999')
            If jumlah_rtr#0
               If (jumlah_rtr>0 .and. jumlah_rtr<1) .or. (jumlah_rtr<0 .and. jumlah_rtr>-1)
                  Jml3=tran(jumlah_rtr,'@e 999.99')
               Elseif mod(jumlah_rtr,int(jumlah_rtr))>0
                  Jml3=tran(jumlah_rtr,'@e 999.99')
               ENDIF
            ENDIF
            @prow()+1,0 say ' RETUR='+jml3+'x @'+tran(harga_2,'@e 9,999,999')
            @prow(),34 say tran(0-(jumlah_rtr*harga_2),'@e   99,999,999')
            Totr=totr+(jumlah_rtr*harga_2)
         ENDIF
      ENDIF
      Skip
   ENDDO
   If dis2>0
      @prow()+2,0 say 'JUMLAH DISKON ITEM = ( Rp'+tran(dis2,'@e 99,999,999')+' )'
   ENDIF
   If blt#0
      @prow()+1,22 say 'PEMBULATAN  '+tran(blt,'@e 999,999,999')
   ENDIF
   *
   @prow()+1,0 say REPLACE('-',45)
   If totr#0
      @prow()+1,22 say 'SUB TOTAL   '+tran(tot,'@e 999,999,999')
      @prow()+1,22 say 'RETUR       '+tran(0-totr,'@e 999,999,999')
   ENDIF
   @prow()+1,0 say tran(no,'9999 itm. ')+chr(27)+chr(33)+chr(32)+'  TOTAL'+tran(tot-totr, '@e 999,999,999')+chr(27)+chr(33)+chr(4)
   If tot3#0
      @prow()+1,22 say 'KURANGAN    '+tran(tot3,'@e 999,999,999')
      @prow()+1,22 say '            -----------'
      @prow()+1,22 say 'TOTAL RP    '+tran(tot+tot3,'@e 999,999,999')
   ENDIF
   If dis31>0
      @prow()+1,7 say 'DISKON '+tran(dis3,'@e 99.99 % ')+tran(dis31,'@e 999,999,999')
   ENDIF
   If dns>0
      @prow()+1,22  say 'UNTUK DONASI'+tran(dns, '@e 999,999,999')
   ENDIF
   @prow()+1,22  say 'B A Y A R   '+tran(byr, '@e 999,999,999')
   If kbl#0
      @prow()+1,22 say iif(kbl>0,'KEMBALI     ','K U R A N G ')+tran(abs(kbl),'@e 999,999,999')
   ENDIF
   Kata=hr+', '+dtoc(tgl)+' '+tran(time(),'XX:XX')+' '+fkt+'/'+nsls
   @prow()+2,0 say chr(27)+chr(33)+chr(4)+left(kata,45)
   *
   If plg#SPACE(10) &&.and. ( rpoin>0 .or. rpoin2>0 ) .and. idp=' '
      @prow()+1,0 say REPLACE('-',42)
      @prow()+1,0 say 'Nama: '+RTRIM(nsp)+' ('+RTRIM(plg)+')'
      @prow()+1,0 say '    : '+jab
      If iPoin#'0'
         @prow()+1,0 say 'Poin: '+LTRIM(tran(pnskr+pnskr2,'@e 999,999 '))+iif(iPoin='2','   Total: '+LTRIM(tran(xPoin+(pnskr+pnskr2),'@e 999,999')),'')
      ENDIF
   ENDIF
   *
   SELECT 1
   Uselan('DATA.SET','','','','','')
   If promo1#SPACE(40)
      @prow()+1,0 say REPLACE('-',45)
      @prow()+1,0 say promo1
   ENDIF
   If promo2#SPACE(40)
      @prow()+1,0 say promo2
   ENDIF
   If promo3#SPACE(40)
      @prow()+1,0 say promo3
   ENDIF
   If promo4#SPACE(40)
      @prow()+1,0 say promo4
   ENDIF
ENDIF
@prow()+jn+1,1 say ''
If idcut='Y' .and. tot#0
   @prow(),0 say chr(27)+chr(64)+chr(27)+chr(105) &&AUTOCUT u/EPSON
   @prow(),0 say chr(27)+chr(33)+chr(4)+''
ENDIF
SET DEVICE TO SCREEN
Ulang=.f.
RETURN

PROCEDURE fmtbaris
Jmlx=jumlah
St  =satuan
Nmb =RTRIM(nama_brg)+SPACE(38-len(RTRIM(nama_brg)))
If isine_b>1 .and. jumlah>=isine_b
   *Jmlx=jumlah/isine_b
   *St  =satuan_b
   Nmb =RTRIM(nama_brg)+' @'+LTRIM(str(isine_b))+' '+RTRIM(satuan)
   Nmb =nmb+SPACE(38-len(nmb))
ENDIF
Nmb=left(nmb,38)
Jml=tran(jmlx,'@e 99,999')
If jmlx#0
   If (jmlx>0 .and. jmlx<1) .or. (jmlx<0 .and. jmlx>-1)
      Jml=tran(jmlx,'@e 999.99')
   Else  &&&if mod(jmlx,int(jmlx))>0
      Jml=tran(jmlx,'@e 99,999')
   ENDIF
ENDIF
RETURN

PROCEDURE atas
Atas=.t.
Keyboard chr(13)
RETURN

PROCEDURE bawah
Bawah=.t.
Keyboard chr(13)
RETURN

* EOF CR
