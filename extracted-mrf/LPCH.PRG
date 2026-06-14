* LAPORAN PEMECAHAN STOK
*------------------------
Sele 1
Uselan('STOK.DTA','STOK.NDX','STOK4.NDX','','','')
Sele 2
Uselan('PECAHIN.DTA','PECAHIN1.NDX','','','','')

Stor tglx to tgl,tgl2
Do whil .t.
   Kdb=spac(13)
   Sele 1
   Set cent on
   Set key 28 to dafbrg
   *Tgl=ctod('01-'+ltri(str(mont(tgl)))+'-'+ltri(str(year(tgl))))
   @1,0 clea to 23,79
   @2,24 say 'Kode..:'get kdb pict '@!'
   @3,24 say 'Tanggal'get tgl
   @3,43 say 'sampai'get tgl2
   Do pesan with 'ESC=Selesai  F1=Daftar Kode Barang'
   Read
   Set key 28 to
   @23,0
   If lastkey()=27
      Exit
   Endi
   If kdb#spac(13)
      Seek kdb
      If .not. foun()
         Seek rtri(kdb)
      Endi
   Endi
   Stor 0 to no,tot,tot2
   Stor spac(5) to st,st2
   @1,0 clea to 23,79
   Do pesan with 'Tunggu, sedang dihitung'

   Sele 2
   Set filter to
   If kdb#spac(13)
      Set filter to kode_brg=kdb
   Endi
   Seek dtos(tgl)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
   Endi
   Nr=recno()
   Stor 1 to cf,hal
   Cfx =ltri(str(cf))
   Arg1='L-PCH&cfx'+'.RTF'
   Set alte on
   Set alte to &arg1
   Do koplpch
   Set cent off
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
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
         Cfx=ltri(str(cf))
         Arg1='L-PCH&cfx'+'.RTF'
         Set alte to &arg1
         Do koplpch
      Endi
   Endd
   ?repl(chr(196),80)
   ?spac(60)+'TOTAL='+tran(tot,'@e 9,9999,999 ')
   Set alte off
   Set alte to
   @23,0
   Ada=.f.
   For Lihat=1 to cf
       Cfx=ltri(str(lihat))
       Arg1='L-PCH&cfx'+'.RTF'
       Do pesan with &gtlayar
       Set key 13 to xit
       Memoedit(Memoread('&arg1'),1,0,22,210,.t.,'',210)
       Set key 13 to
       If lastkey()=27 .and. (.not. ada)
          Exit
       Endi
       Ada=.f.
   Next
   Do pesan3 with 'ESC=Selesai   F4=Hapus/Tgl   F8=Koreksi   F10=Cetak'
   If lastkey()=-7 .and. ( Owner .or. xOwner )
      Do hpch
      Loop
   Elseif lastkey()=-3 .and. ( Owner .or. xOwner )
      Set cent on
      Do pesan with 'Transaksi tgl '+dtoc(tgl)+' s/d '+dtoc(tgl2)+' diHapus? Y/T'
      Yt=' '
      Do whil .not. yt$'YT'
         @23,67 get yt pict '!'
         Read
      Endd
      If yt='Y'
         Do pesan with 'Tunggu, sedang dihapus'
         Set filter to
         Flok()
         Dele for (tanggal>=tgl .and. tanggal<=tgl2)
         Ul()
         Commit
      Endi
      Loop
   Elseif lastkey()=27
      Loop
   Endi
   Do cp
   If cc=0
      Loop
   Endi
   Stor 1 to no,hal,b
   Stor 0 to jhg,tot
   Do koplpch1
   Go nr
   Set cent off
   Do whil .not. eof() .and. (tanggal>=tgl .and. tanggal<=tgl2)
      ?dtoc(tanggal)+'  '+left(kode_brg,7)+'  '+nama_brg+'  '+tran(jumlah*isine_b,'9999 ')+satuan+'  '+left(kode_brg2,7)+'  '+nama_brg2+tran(jumlah,'@e 9999.99 ')+satuan2
      St  =satuan
      St2 =satuan2
      Tot =tot +jumlah
      Tot2=tot2+(jumlah*isine_b)
      No=no+1
      Skip
      If no>60
         ?repl('-',114)
         ?spac(94)+'bersambung...'
         ?''
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
         No =1
         Hal=hal+1
         Do koplpch1
      Endi
   Endd
   If cc=0
      Loop
   Endi
   ?spac(42)+'TOTAL='+tran(tot2,'@e 999,999 ')+spac(43)+tran(tot,'@e 999,999.99 ')
   ?repl('=',114)
   ?' '
   Eject
   Set prin off
   Set devi to scre
Endd
Set curs off
Clos all
Retu

Proc koplpch
?' Kode: '+rtri(kdb)+'  Tgl: '+dtoc(tgl)+' s/d '+dtoc(tgl2)+spac(26)+'Hal:'+tran(hal,'999')
?repl(gd,9)+chr(209)+repl(gd,30)+chr(209)+repl(gd,30)+chr(209)+repl(gd,9)
?'Tanggal  '+tl+'Nama Barang'+spac(19)+tl+'Asal Barang'+spac(19)+tl+'  Jml.Brg'
?repl(gl,9)+chr(193)+repl(gl,30)+chr(197)+repl(gl,30)+chr(193)+repl(gl,9)
Retu

Proc koplpch1
Set prin on
Set devi to prin
?chr(27)+chr(33)+chr(5)
Set marg to 15
?' LAPORAN MUTASI/PECAH STOK'
If kdb#spac(13)
   ?' Kode.: '+kdb
Endi
?' Tgl..: '+dtoc(tgl)+' sampai '+dtoc(tgl2)+spac(74)+'Hal:'+tran(hal,'999')
?repl('=',114)
?'Tanggal   Nama Barang                               Jml.Brg    Asal Barang                                Jml.Brg'
?repl('-',114)
No=1
Retu

Proc hpch
@1,0 clea to 23,79
@1,0 to 1,79 doub
@22,0 to 22,79
Katax='ESC=Selesai  F8=Hapus'
Do pesan with katax
Declare field[9],kop[9],frm[9],bts[9]
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
Dbedit(2,0,21,79,field,'fpch',frm,kop,chr(196),bts)
*
Set curs on
Retu

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
         Endc
      Endi
      Retu 1
   Case lastkey()=27
      Retu 0
   Case lastkey()=-7
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Ht=' '
      Do whil .not. ht$'YT'
         @23,47 get ht pict '!'
         Read
      Endd
      If ht='Y'
         Kdb =kode_brg
         Kdb2=kode_brg2
         Jml =jumlah
         Isi =isine_b
         If .not. dele()
            Rlok()
            Dele
            Ul()
            Commit
            Skip

            Sele 1
            Seek kdb
            Rlok()
            Repl stok_m with (stok_m-(jml*isi))
            Ul()
            Commit

            Seek kdb2
            Rlok()
            Repl stok_k with (stok_k-jml)
            Ul()
            Commit
            Skip
            
            Sele 2
         Endi
      Endi
      Do pesan with katax
      Set curs off
      Retu 2
   Othe
      Retu 1
Endc
Retu 0

* EOF LTFB
