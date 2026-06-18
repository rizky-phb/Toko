// ============================================================
// File    : LPJ13.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN RINCI PENJUALAN/TKG/SLS
SELECT 1
If klp .and. hut=.f.
   Uselan('SALES.DTA','','','','','')
Else
   Uselan('TUKANG.DTA','TUKANG.NDX','TUKANG2.NDX','','','')
ENDIF

SELECT 2
Uselan('STOK.DTA','STOK.NDX','','','','')
SELECT 3
*
SELECT 4
*
SELECT 5
Uselan('FAKTURP.DTA','FAKTURP1.NDX','FAKTURP2.NDX','','','')

STORE SPACE(5) to ksp,sls,sls2,tkg,tkg2,fkt
STORE ' ' to hri,bln,thn,nsp,nsls,ntkg
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   @1,0 CLEAR to 24,79
   If klp .and. hut=.f.
      Set key 28 to dafsls
      Do pesan with 'F1=Daftar Salesman'
      @2,24 say 'Sales.:'get sls pict '@!'
      @3,24 say 'Tanggal'get tgl
      @3,43 say 'sampai'get tgl2
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      ENDIF
      Nsls='-'
      If sls#SPACE(5)
         LOCATE FOR kode_sls=sls
         If .not. FOUND()
            Do pesan2 with 'Tidak ada Salesman '+RTRIM(sls)
            Loop
         ENDIF
         Nsls=nama_sls
      ENDIF
   Else
      Set key 28 to daftkg
      Do pesan with 'F1=Daftar Tukang'
      @2,24 say 'Tukang:'get tkg pict '@!'
      @3,24 say 'Tanggal'get tgl
      @3,43 say 'sampai'get tgl2
      Read
      Set key 28 to
      @24,0
      If lastkey()=27
         Exit
      ENDIF
      Nsp='-'
      If tkg#SPACE(5)
         Set order to 1
         Seek tkg
         If .not. FOUND()
            Do pesan2 with 'Tidak ada Tukang '+RTRIM(tkg)
            Loop
         ENDIF
         Ntkg=nama_tkg
      ENDIF
   ENDIF
   STORE 0 to no,tot,tot2,totx,jhg,jml1,jml2
   @1,0 CLEAR to 24,79
   Do pesan with 'Tunggu, sedang dihitung'
   Do bacafile
   If .not. file('&nf0')
      Do pesan2 with 'Tidak ada File '+nf0
      Loop
   ENDIF
   Beda=.f.
   If mont(tgl)#mont(tgl2)
      Beda=.t.
   ENDIF

   SELECT 4
   Uselan('PEMBLTAN.DTA','PEMBLTAN.NDX','','','','')
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      Jml1=jml1+ nilai_b
      Jml2=jml2+ nilai_a
      Skip
   ENDDO
   Close

   Do bukafjual1

   STORE 1 to cf,hal,jml
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpj11
   SET CENTURY OFF

   DO WHILE .t.
      SET CENTURY OFF
      Nr=recno()
      Fkt =faktur
      Nsls=kode_lgn
      Nsp =kode_tkg
      Nklb=nama_ksr
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         If diskon=0
            ?dtoc(tanggal)+'  '+SUBSTR(nama_brg,1,30)+jml+satuan+tran(harga_2,'@e  9,999,999       ')+tran(&brs1,'@e 999,999,999')
         Else
            ?dtoc(tanggal)+'  '+SUBSTR(nama_brg,1,30)+jml+satuan+tran(harga_2,'@e  9,999,999 ')+tran(diskon_rp,'@e 99999 ')+tran(&brs1,'@e 999,999,999')
         ENDIF
         Skip
         If faktur#fkt
            ?SPACE(68)+REPLACE(gl,12)
            ?' Plgn..:'+nsls+' / '+nsp+'   Fkt: '+fkt+'   Ksr:'+left(nklb,16)+' Jumlah Rp.='+tran(jhg,'@e 999,999,999')
            ?REPLACE(gl,80)
            Fkt =faktur
            Nsls=kode_lgn
            Nsp =kode_tkg
            Nklb=nama_ksr
            Jhg =0
            No  =no+1
            If no>=20
               ?SPACE(50)+'SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
               ?''
               STORE 0 to no,totx
               Hal=hal+1
               Cf =cf+1
               Cfx=LTRIM(str(cf))
               Arg1='L-JUAL&cfx'+'.RTF'
               Set alte to &arg1
               Do koplpj11
            ENDIF
         ENDIF
      ENDDO
      If beda
         SET CENTURY ON
         Do carifljual
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            Do bukafjual1
         ENDIF
      Elseif beda=.f.
         Exit
      ENDIF

   ENDDO
   ?REPLACE(gl,80)
   If xOwner
      ?SPACE(30)+'POKOK  Rp'+tran(tot2    ,'@e 999,999,999')+'      SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
      ?SPACE(30)+'Margin Rp'+tran(tot-tot2,'@e 999,999,999')+'      T O T A L Rp'+tran(tot, '@e  999,999,999')
   Else
      ?SPACE(56)+'SUB TOTAL Rp'+tran(totx,'@e  999,999,999')
      ?SPACE(56)+'T O T A L Rp'+tran(tot, '@e  999,999,999')
   ENDIF
   If jml1#0 .or. jml2#0
       ?SPACE(35)+'Pembulatan keBawah='+tran(jml1,'@e 9999,999')+' , keAtas='+tran(jml2,'@e 9999,999')
   ENDIF   
   Set alte off
   Set alte to
   @24,0
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner ) .and. beda=.f.
      Do hpj
      Loop
   Elseif lastkey()=27
      Loop
   ENDIF
   Do cp
   If cc=0
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot,tot2
   Tgl3=tgl
   *
   Do koplpj112
   Go nr
   SET CENTURY OFF
   Fkt =faktur
   Ksp =kode_lgn
   Tgl =tanggal

   SELECT 5
   Seek dtos(tgl)+ksp+fkt
   Nsp=nama_lgn

   SELECT 3
   DO WHILE .not. eof() .and. (tanggal>=tgl3 .and. tanggal<=tgl2)
      Do fmtjml
      Tgl =tanggal
      Jhg =&brs1
      Tot =tot +jhg
      Tot2=tot2+jhg
      ?' '+dtoc(tanggal)+'  '+left(nama_ksr,10)+' '+faktur+'  '+left(kode_brg,7)+'    '+nama_brg+jml+satuan+tran(harga_2,'@e 9,999,999 ')+tran(diskon_rp,'@e  99,999 ')+tran(jhg,'@e 999,999,999')
      No=no+1
      Skip
      If faktur#fkt
         ?' '+REPLACE('-',47)+'Nama: '+left(nsp,25)+' Kode: '+ksp+'  Jumlah='+tran(tot2,'@e 999,999,999')
         ?''
         No=no+2
         Fkt =faktur
         Ksp =kode_lgn
         SELECT 5
         Seek dtos(tgl)+ksp+fkt
         Nsp=nama_lgn

         SELECT 3
         Tot2=0
      ENDIF
      If no>55
         ?REPLACE('-',99)+'bersambung...'
         ?''
         Eject
         SET PRINTER OFF
         SET DEVICE TO SCREEN
         Do pesan3 with gtkertas
         If nlp=0
            Cc=0
            Exit
         ENDIF
         Do cp
         If cc=0
            Exit
         ENDIF
         No =1
         Hal=hal+1
         Do koplpj112
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?REPLACE('-',112)
   ?' T o t a l     '+SPACE(82)+'Rp.'+tran(tot,'@e 999,999,999')
   ?REPLACE('=',112)
   ?''
   Tgl=tgl3
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RELEASE ksp,sls,sls2,tkg,tkg2,fkt,hri,bln,thn,nsp,nsls,ntkg,tot,tot2,jhg
RETURN

PROCEDURE koplpj11
If klp .and. hut=.f.
   ?' Sales: '+nsls+'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
Else
   ?' Tkng.: '+ntkg +'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
ENDIF
?REPLACE(gd,9)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,12)+chr(209)+REPLACE(gd,8)+chr(209)+REPLACE(gd,6)+chr(209)+REPLACE(gd,10)
?'Tanggal  '+tl+'Nama Barang'+SPACE(19)+tl+'  Qty.Sat.  '+tl+'  @Harga'+tl+'Dis.Rp'+tl+'    Jumlah'
?REPLACE(gl,9)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,8)+chr(193)+REPLACE(gl,6)+chr(193)+REPLACE(gl,10)
RETURN

PROCEDURE koplpj112
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 10
?chr(27)+chr(33)+chr(4)
If klp .and. hut=.f.
   ?' LAPORAN PENJUALAN PER SALES'
   If sls#SPACE(5)
      ?' SALESMAN: '+RTRIM(nsls)+' - '+sls
   ENDIF
Else
   ?' LAPORAN PENJUALAN PER TUKANG'
   If tkg#SPACE(5)
      ?' TUKANG   : '+RTRIM(ntkg)+' - '+tkg
   ENDIF
ENDIF
If tgl=tgl2
   ?' TANGGAL: '+dtoc(tgl3)+SPACE(86)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl3)+' s/d '+dtoc(tgl2)+SPACE(73)+'Hal:'+tran(hal,'999')
ENDIF
?REPLACE('=',112)
?' Tanggal   Kasir      Nota   Kode       Nama Barang                      Jml.Brg.    @Harga  Dis.Rp   Jumlah Rp.'
?REPLACE('-',112)
No=1
RETURN

PROCEDURE bukafjual1
SELECT 3
Uselan('&nf0','&nf1','&nf2','&nf3','','')
If klp .and. hut=.f.
   Set filter to kode_sls#SPACE(5)
   If sls#SPACE(5)
      Set filter to kode_sls=sls
   ENDIF
Else
   Set filter to kode_tkg#SPACE(5)
   If tkg#SPACE(5)
      Set filter to kode_tkg=tkg
   ENDIF
ENDIF
Seek dtos(tgl)
If .not. FOUND()
   LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
ENDIF
RETURN

* EOF LPJ13
