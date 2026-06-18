// ============================================================
// File    : LPCH.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
* LAPORAN PEMECAHAN STOK
*------------------------
SELECT 1
Uselan('STOK.DTA','STOK.NDX','STOK4.NDX','','','')
SELECT 2
Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')

STORE tglx to tgl,tgl2
DO WHILE .t.
   Kdb=SPACE(13)
   SELECT 1
   SET CENTURY ON
   Set key 28 to dafbrg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 CLEAR to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai  F1=Daftar Kode Barang'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   ENDIF
   If kdb#SPACE(13)
      Seek kdb
      If .not. FOUND()
         Seek RTRIM(kdb)
      ENDIF
   ENDIF
   STORE 0 to no,tot,tot2
   STORE SPACE(5) to st,st2
   @1,0 CLEAR to 23,79
   Do pesan with 'Tunggu, sedang dihitung'

   SELECT 2
   Set filter to
   If kdb#SPACE(13)
      Set filter to kode_brg=kdb
   ENDIF
   Seek dtos(tgl)
   If .not. FOUND()
      LOCATE FOR (tanggal>=tgl .and. tanggal<=tgl2)
   ENDIF
   Nr=recno()
   STORE 1 to cf,hal
   Cfx =LTRIM(str(cf))
   Arg1='L-PCH&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpch
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(nama_brg,30)+tl+left(nama_brg2,30)+tran(jumlah,'@e 99999 ')+left(satuan2,5)
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      Skip
      No  =no+1
      If no>=200
         No =0
         Cf =cf+1
         Cfx=LTRIM(str(cf))
         Arg1='L-PCH&cfx'+'.RTF'
         Set alte to &arg1
         Do koplpch
      ENDIF
   ENDDO
   ?REPLACE(chr(196),80)
   ?SPACE(60)+'TOTAL='+tran(tot,'@e 9,9999,999 ')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=LTRIM(str(lihat))
       Arg1='L-PCH&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,22,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       ENDIF
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hpch
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      SET CENTURY ON
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      DO WHILE .not. yt$'YT'
         @23,67 get yt pict '!'
         Read
      ENDDO
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2)
         Ul()
         COMMIT
      ENDIF
      Loop
   Elseif lastkey()=27
      Loop
   ENDIF
   Do cp
   If cc=0
      Loop
   ENDIF
   STORE 1 to no,hal,b
   STORE 0 to jhg,tot
   Do koplpch1
   Go nr
   SET CENTURY OFF
   DO WHILE .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+'  '+nama_brg+'  '+tran(jumlah*isine_b,'9999 ')+satuan+'  '+left(kode_brg2,7)+'  '+nama_brg2+tran(jumlah,'@e 9999.99 ')+satuan2
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      No=no+1
      Skip
      If no>60
         ?REPLACE('-',114)
         ?SPACE(94)+'bersambung...'
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
         Do koplpch1
      ENDIF
   ENDDO
   If cc=0
      Loop
   ENDIF
   ?SPACE(42)+'TOTAL='+tran(tot2,'@e 999,999 ')+SPACE(43)+tran(tot,'@e 999,999.99 ')
   ?REPLACE('=',114)
   ?' '
   Eject
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
SET CURSOR OFF
CLOSE all
RETURN

PROCEDURE koplpch
?' Kode: '+RTRIM(kdb)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+SPACE(26)+'Hal:'+tran(hal,'999')
?REPLACE(gd,9)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,30)+chr(209)+REPLACE(gd,9)
?'Tanggal  '+tl+'Nama Barang'+SPACE(19)+tl+'Asal Barang'+SPACE(19)+tl+'  Jml.Brg'
?REPLACE(gl,9)+chr(193)+REPLACE(gl,30)+chr(197)+REPLACE(gl,30)+chr(193)+REPLACE(gl,9)
RETURN

PROCEDURE koplpch1
SET PRINTER ON
SET DEVICE TO PRINTER
?chr(27)+chr(33)+chr(5)
Set marg to 15
?' LAPORAN MUTASI/PECAH STOK'
If kdb#SPACE(13)
   ?' Kode.: '+kdb
ENDIF
?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+SPACE(74)+'Hal:'+tran(hal,'999')
?REPLACE('=',114)
?'Tanggal   Nama Barang                               Jml.Brg    Asal Barang                                Jml.Brg'
?REPLACE('-',114)
No=1
RETURN

PROCEDURE hpch
@1,0 CLEAR to 23,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai  F8=Hapus'
Do pesan with katax
DECLARE field[9],kop[9],frm[9],bts[9]
Field[1]='TANGGAL'
Field[2]='left(NAMA_BRG,30)'
Field[3]='left(NAMA_BRG2,30)'
Field[4]='JUMLAH'
Field[5]='left(SATUAN,4)'
Field[6]='ISINE_B'
Field[7]='SATUAN2'
Field[8]='KODE_BRG'
Field[9]='KODE_BRG2'
Kop[1]='Tanggal'
Kop[2]='Nama Barang'
Kop[3]='Asal Barang'
Kop[4]='Jml.'
Kop[5]='Brg.'
Kop[6]=' Isi'
Kop[7]=''
Kop[8]='Kode'
Kop[9]='Kode Asal'
Frm[1]='99-99-99'
Frm[2]='@!'
Frm[3]='@!'
Frm[4]='9999'
Frm[5]='@!'
Frm[6]='@e 9999'
Frm[7]='@!'
Frm[8]='@!'
Frm[9]='@!'
Bts[1]=tl
Bts[2]=tl
Bts[3]=tl
Bts[4]=tl
Bts[5]=' '
Bts[6]=tl
Bts[7]=' '
Bts[8]=tl
Bts[9]=tl
Go nr
DbEdit(2,0,21,79,field,'fpch',frm,kop,chr(196),bts)
*
SET CURSOR ON
RETURN

Function fpch
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
      If tanggal<tgl .or. tanggal>tgl2
         Do case
            Case lastkey()=18  &&&-PgUp
               Tone(700,2)
               Skip 18
            Case lastkey()=3   &&&-PgDn
               Tone(700,2)
               Skip -18
            Case lastkey()=5   &&&-Up
               Tone(700,2)
               Skip 1
            Case lastkey()=24   &&&-Dn
               Tone(700,2)
               Skip -1
         ENDCASE
      ENDIF
      RETURN 1
   Case lastkey()=27
      RETURN 0
   Case lastkey()=-7
      SET COLOR TO gr+*
      @row(),col() say &fds
      SET COLOR TO g
      Do pesan with 'DiHapus? Y/T'
      SET CURSOR ON
      Ht=' '
      DO WHILE .not. ht$'YT'
         @23,47 get ht pict '!'
         Read
      ENDDO
      If ht='Y'
         Kdb =kode_brg
         Kdb2=kode_brg2
         Jml =jumlah
         Isi =isine_b
         If .not. dele()
            Rlok()
            Dele
            Ul()
            COMMIT
            Skip

            SELECT 1
            Seek kdb
            Rlok()
            REPLACE stok_m with (stok_m-(jml*isi))
            Ul()
            COMMIT

            Seek kdb2
            Rlok()
            REPLACE stok_k with (stok_k-jml)
            Ul()
            COMMIT
            Skip
            
            SELECT 2
         ENDIF
      ENDIF
      Do pesan with katax
      SET CURSOR OFF
      RETURN 2
   Othe
      RETURN 1
ENDCASE
RETURN 0

* EOF LTFB
