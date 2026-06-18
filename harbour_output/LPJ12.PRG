// ============================================================
// File    : LPJ12.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN PENJUALAN/KREDIT
SELECT 1
Uselan('CUST.DTA','CUST.NDX','CUST2.NDX','','','')
SELECT 2
Uselan('STOK.DTA','STOK.NDX5','','','','')
SELECT 3
*
SELECT 4
*
SELECT 5
Uselan('FAKTURP.DTA','FAKTURP1.NDX','','','','')
*
STORE SPACE(10) to ksp,fkt
STORE ' ' to hri,bln,thn,jlap
STORE tglx to tgl,tgl2
DO WHILE .t.
   SELECT 1
   SET CENTURY ON
   Set key 28 to daflgn
   @1,0 CLEAR to 23,79
   @2,24 say 'Plgn....:'get ksp pict '@!'
   @3,24 say 'Tanggal.:'get tgl
   @3,45 say 'sampai'get tgl2
   Do pesan with 'F1=Daftar Pelanggan'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   Tgl4=tgl
   STORE '-' to nsls,nsp,jab
   If ksp#SPACE(10)
      Seek ksp
      If .not. FOUND()
         Do pesan2 with 'Tidak ada Pelanggan '+RTRIM(ksp)
         Loop
      ENDIF
      Nsp=nama_lgn
      Jab=almt_lgn
   ENDIF
   STORE 0 to no,tot,tot2,totx,totr,jhg,jml1,jml2
   @1,0 CLEAR to 23,79
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
   *
   Do bukafjual2
   *
   STORE 1 to cf,hal,jml
   Cfx =LTRIM(str(cf))
   Arg1='L-JUAL&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpj12
   SET CENTURY OFF
   *
   DO WHILE .t.
      SET CENTURY OFF
      Nr=recno()
      Tglz=tanggal
      Fkt =faktur
      Nsls=kode_lgn
      Nklb=nama_ksr
      *
      Do namalgn
      ?chr(254)+' '+RTRIM(nsp)+' / '+RTRIM(jab)+' / '+RTRIM(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
      No=no+1
      *
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Kdb =kode_brg
         Nmb =nama_brg
         St  =satuan
         Kata="kdb+' '+iif(jumlah_rtr=0,left(nama_brg,30)+jml+satuan,left(nama_brg,28)+jml+left(satuan,3)+'+'+tran(jumlah_rtr,'999'))+tran(harga_2,'@e  9,999,999 ')+iif(diskon_rp>0,tran(diskon_rp,'@e 999,999 '),SPACE(8))+tran(&brs1,'@e 999,999,999')"
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         No  =no+1
         ?&kata
         Skip
         If faktur#fkt .or. nama_ksr#nklb .or. kode_lgn#nsls
            If jhg>0
               ?SPACE(68)+REPLACE(gl,12)
               ?SPACE(42)+'Ksr:'+left(nklb,10)+'  Jumlah Rp.='+tran(jhg,'@e 999,999,999')
               ?REPLACE(gl,80)
               No=no+3
            ENDIF
            Tglz=tanggal
            Fkt =faktur
            Nsls=kode_lgn
            Nklb=nama_ksr
            *
            Do namalgn
            ?chr(254)+' '+RTRIM(nsp)+' / '+RTRIM(jab)+' / '+RTRIM(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
            *
            No=no+1
            Jhg =0
         ENDIF
         If no>200
            ?REPLACE(gl,80)
            ?SPACE(53)+'SUB TOTAL Rp'+tran(totx,'@e 999,999,999,999')
            ?''
            STORE 0 to no,totx
            Hal=hal+1
            Cf =cf+1
            Cfx=LTRIM(str(cf))
            Arg1='L-JUAL&cfx'+'.RTF'
            Set alte to &arg1
            Do koplpj12
         ENDIF
      ENDDO
      If beda
         SET CENTURY ON
         Do carifljual
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            Do bukafjual2
         ENDIF
      Elseif beda=.f.
         Exit
      ENDIF

   ENDDO
   ?REPLACE(gl,80)
   If xOwner
      ?SPACE(27)+'POKOK  Rp'+tran(tot2    ,'@e 99,999,999,999')+'       JUMLAH Rp'+tran(tot+totr,'@e 99,999,999,999')
      ?SPACE(27)+'Margin Rp'+tran(tot-tot2,'@e 99,999,999,999')+'       RETUR  Rp'+tran(totr    ,'@e 99,999,999,999')
      ?SPACE(27)+'TOTAL  Rp'+tran(tot     ,'@e 99,999,999,999')
   ENDIF
   ?' '
   Set alte off
   Set alte to
   Tgl=tgl4
   *
   Layar('L-JUAL')
   *
   Do pesan3 with 'ESC=Selesai     F4=Hapus/Tgl     F8=Koreksi     F10=Cetak'
   If lastkey()=27
      Loop
   Elseif lastkey()=-7 .and. ( Owner .or. xOwner ) .and. beda=.f.
      Do hpj
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      SET CENTURY ON
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @23,65 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp
         Ul()
         COMMIT
         Do pesan with 'Faktur Penjualan tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
         Yt=' '
         DO WHILE .not. yt$'YT'
            @23,69 get yt pict '!'
            Read
         ENDDO
         If yt='Y'
            SELECT 5
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2) .and. kode_lgn=ksp
            Ul()
            COMMIT
         ENDIF
      ENDIF
      Loop
   ENDIF
   Sambung='Y'
   Do pesan with 'Pake kertas bersambung? Y/T :  '
   @23,54 get sambung pict '!'
   Read
   Do cp
   If cc=0
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot,tot2
   Do koplpj122
   Go nr
   DO WHILE .t.
      SET CENTURY OFF
      Nr=recno()
      Tglz=tanggal
      Fkt =faktur
      Nsls=kode_lgn
      Nklb=nama_ksr
      *
      Do namalgn
      ?'* '+RTRIM(nsp)+' / '+RTRIM(jab)+' / '+RTRIM(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
      No=no+1
      *
      DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
         Do fmtjml
         Kdb =kode_brg
         Nmb =nama_brg
         St  =satuan
         Kata="kdb+' '+iif(jumlah_rtr=0,left(nama_brg,30)+jml+satuan,left(nama_brg,28)+jml+left(satuan,3)+'+'+tran(jumlah_rtr,'999'))+tran(harga_2,'@e  9,999,999 ')+iif(diskon_rp>0,tran(diskon_rp,'@e 999,999 '),SPACE(8))+tran(&brs1,'@e 999,999,999')"
         Jhg =jhg +&brs1
         Tot =tot +&brs1
         Totx=totx+&brs1
         Tot2=tot2+&brs14
         No  =no+1
         ?&kata
         Skip
         If faktur#fkt .or. nama_ksr#nklb .or. kode_lgn#nsls
            If jhg>0
               ?SPACE(42)+'Ksr:'+left(nklb,10)+'  Jumlah Rp.='+tran(jhg,'@e 999,999,999')
               ?' '
               No=no+2
            ENDIF
            Tglz=tanggal
            Fkt =faktur
            Nsls=kode_lgn
            Nklb=nama_ksr
            *
            Do namalgn
            ?'* '+RTRIM(nsp)+' / '+RTRIM(jab)+' / '+RTRIM(nsls)+' / Tgl: '+dtoc(tglz)+' / '+fkt
            *
            No=no+1
            Jhg =0
         ENDIF
         If no>55
            ?'--bersambung...'
            ?''
            If sambung='Y'
               Eject
            Else
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
            ENDIF
            No =1
            Hal=hal+1
            Do koplpj122
         ENDIF
      ENDDO
      If beda
         SET CENTURY ON
         Do carifljual
         If tgl>tgl2
            Exit
         ENDIF
         If file('&nf0')
            Do bukafjual2
         ENDIF
      Elseif beda=.f.
         Exit
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?REPLACE('-',80)
   ?SPACE(50)+'Total Rp.'+tran(tot,'@e 99,999,999,999')
   ?REPLACE('=',80)
   ?''
   Tgl=tgl4
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplpj12
?' Plgn: '+nsp+'   Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+'   Hal:'+tran(hal,'999')
?REPLACE(gl,7)+chr(194)+REPLACE(gl,30)+chr(194)+REPLACE(gl,12)+chr(194)+REPLACE(gl,8)+chr(194)+REPLACE(gl,8)+chr(194)+REPLACE(gl,10)
?'Kode   '+tl+'Nama Barang'+SPACE(19)+tl+'  Qty.Sat.  '+tl+'  @Harga'+tl+' Dis.Rp '+tl+'    Jumlah'
?REPLACE(gl,7)+chr(193)+REPLACE(gl,30)+chr(193)+REPLACE(gl,12)+chr(193)+REPLACE(gl,8)+chr(193)+REPLACE(gl,8)+chr(193)+REPLACE(gl,10)
RETURN

PROCEDURE koplpj122
SET PRINTER ON
SET DEVICE TO PRINTER
Set marg to 5
?chr(27)+chr(33)+chr(1)
?' LAPORAN PENJUALAN'
If ksp#SPACE(10)
   ?' PLGN...: '+RTRIM(nsp)+' - '+ksp
ENDIF
If tgl=tgl2
   ?' TANGGAL: '+dtoc(tgl)+SPACE(86)+'Hal:'+tran(hal,'999')
Else
   ?' TANGGAL: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(40)+'Hal:'+tran(hal,'999')
ENDIF
?REPLACE('=',80)
?'Kode    Nama Barang                      Jml.Brg.     @Harga  Dis.Rp   Jumlah Rp'
?REPLACE('-',80)
No=1
RETURN

PROCEDURE bukafjual2
SELECT 3
Uselan('&nf0','&nf1','&nf2','&nf3','','')
Do cekjlap2
Seek dtos(tgl)
If .not. FOUND()
   LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
ENDIF
RETURN

PROCEDURE cekjlap2
If ksp#SPACE(10)
   Set filter to kode_lgn=ksp
Elseif ksp=SPACE(10)
   Set filter to kode_lgn#SPACE(10)
ENDIF
RETURN

PROCEDURE namalgn
xSele=select()
SELECT 1
Seek nsls
Nsp=left(nama_lgn,25)
Jab=left(almt_lgn,25)
SELECT(xsele)
RETURN

* EOF LPJ12
