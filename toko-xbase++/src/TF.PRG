Procedure TF
* TRANSFER DARI OFFLINE KE SERVER
Set talk off
Set stat off
Set scor off
Set safe off
Set cons off
Set exclusive off
Set conf on
Set dele on
Set wrap on
Set fixe on
Set cent on
Set deci to 0
Set date ital
Empunya='YONEV Computer(0283)3300984TEGALSARI MULYA - BREBES'
Aidi1=subs(empunya,33,10)
Aidi2=subs(empunya,46,6)
Stor 0 to tgl,tgl2
Do While (tgl< Len(empunya))
   Tgl = tgl + 1
   Tgl2= tgl2+ asc(subs(empunya, tgl, 1))
EndDo
If (tgl2 # len(empunya) * 69 + 46)
   Set colo to br+
   @09,32 clea to 15,46
   @09,32 to 15,46 doub
   Set colo to r
   @10,35 say '  ³\,/³'
   @11,35 say ' \ ë ë /'
   @12,35 say ' (  ­  )'
   @13,35 say '   \ /  '
   @14,35 say '//  ß  \\'
   Set colo to r+
   @11,38 say 'ë'
   @11,40 say 'ë'
   @13,39 say 'Ü'
   Set colo to r+*
   @14,39 say 'ß'
   @15,0  say ''
   @16,0  say ''
Return
EndIf
Stor '' to thn,bln,hri,nf0,nf1,nf2,nf3,nf4
Blnthn='subs(dtoc(tgl),7,4)+subs(dtoc(tgl),4,2)'
Tglx  =date()
Gd=chr(205)
Gl=chr(196)
Tl=chr(179)

Clea
If .not. file('F:DATA.SET')
   Tone(1000,3)
   Tone(1500,6)
   Do pesan with 'Komputer Ini Belum Tersambung ke Server.  Ulangi...'
   Wait''
   Wait''
   Clea
Return
EndIf

Sele 1
Uselan('F:STOK.DTA','F:STOK.NDX','','','','')
Sele 2
*
Sele 3
*
Sls=spac(20)
Stor ' ' to hri,bln,thn
Stor tglx to tgl,tgl2
Do While .t.
   Set cent on
   Clea
   @1,0 clea to 23,79
   @2,16 say 'Data Penjualan Off-Line yg Akan di-Transfer ke Server:'
   @3,15 to 3,67
   @4,16 say 'Tanggal'get tgl
   @4,35 say 'sampai dengan'get tgl2
   Read
   @23,0
   If lastkey()=27
      Exit
EndIf

   Jdt=0
   Awl=1
   Do bacafile
   If .not. file('&nf0')
      Tone(1000,1)
      Tone(1500,2)
      Do pesan with 'Tidak ada File Data Penjualan '+&nf0
      Wait''
      Loop
EndIf

   Do pesan with 'Tunggu, sedang dicari...'
   Sele 2
   Uselan('C:&nf0','C:&nf3','','','','')
   Seek (thn+bln+hri)
   If .not. foun()
      Loca for (tanggal>=tgl .and. tanggal<=tgl2)
      If .not. foun()
         Tone(1000,1)
         Tone(1500,2)
         Do pesan with 'Tidak Ada Data Transaksi untuk Tanggal diAtas.  Tanggal diKoreksi...'
         Wait''
         Loop
EndIf
EndIf
   Nr=recno()
   Copy to JUAL.TMP for (tanggal>=tgl .and. tanggal<=tgl2)
   Close

   Uselan('F:&nf0','F:&nf3','F:&nf1','F:&nf2','','')

   Sele 3
   Useidx('JUAL.TMP')

   @1,0 clea to 23,79
   @1,0 to 1,79 doub
   @22,0 to 22,79
   @0,1 say 'Data Penjualan di Komputer Ini, Tanggal: '+dtoc(tgl)+' s/d '+dtoc(tgl2)
   Katax='ESC-Selesai   ENTER-Edit   F5-CariNota   F8-Hapus   F10-Transfer'
   Do pesan with katax
   Declare field[11],kop[11],frm[11],bts[11]
   Field[1]='TANGGAL'
   Field[2]='FAKTUR'
   Field[3]='KASSA'
   Field[4]='left(NAMA_KSR,15)'
   Field[5]='left(NAMA_BRG,25)'
   Field[6]='JUMLAH'
   Field[7]='HARGA_2'
   Field[8]='HARGA'
   Field[9]='DISKON'
   Field[10]='KODE_BRG'
   Field[11]='KODE_KLB'
   Kop[1]='Tanggal'
   Kop[2]='Nota'
   Kop[3]='Ks.'
   Kop[4]='Nama Kasir'
   Kop[5]='Nama Barang'
   Kop[6]='  Qty.'
   Kop[7]='   @Harga'
   Kop[8]='   @Pokok'
   Kop[9]='Disc.'
   Kop[10]='Kode Brg.'
   Kop[11]='Klp.'
   Frm[1]='99-99-99'
   Frm[2]='@!'
   Frm[3]='@!'
   Frm[4]='@!' 
   Frm[5]='@!'
   Frm[6]='@e 9999.99'
   Frm[7]='@e 9,999,999'
   Frm[8]='@e 9,999,999'
   Frm[9]='@e 99999'
   Frm[10]='@!'
   Frm[11]='@!'
   Bts[1]=''+tl
   Bts[2]=''+tl
   Bts[3]=''+tl
   Bts[4]=''+tl
   Bts[5]=''+tl
   Bts[6]=''+tl
   Bts[7]=''+tl
   Bts[8]=''+tl
   Bts[9]=''+tl
   Bts[10]=''+tl
   Bts[11]=''+tl
   Go nr
   Dbedit(2,0,21,79,field,'ftf',frm,kop,gl,bts)
   *
   Set curs on
EndDo
Close all
Return

Return

Function ftf
Parameter mode,pointer
Private fds
Fds=field[pointer]
Do case
   Case mode<4
Return 1
   Case lastkey()=27
Return 0
   Case lastkey()=-4
      Set curs on
      Fkt=spac(5)
      Do pesan with 'No.Nota:     '
      @23,41 get fkt pict '@!'
      Read
      If lastkey()=27 .or. fkt=spac(5)
         Do pesan with katax
         Set curs off
Return 1
EndIf
      Seek (thn+bln+hri)+sls+fkt
      Do pesan with katax
      Set curs off
Return 1
   Case lastkey()=-7
      Set colo to gr+*
      @row(),col() say &fds
      Set colo to g
      Do pesan with 'DiHapus? Y/T'
      Set curs on
      Ht=' '
Do While .not. ht$'YT'
         @23,47 get ht pict '!'
         Read
EndDo
      If ht='Y'
         If .not. dele()
            Rlok()
            Dele
            Ul()
            Commit
            Skip
EndIf
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Case lastkey()=-9
      Do pesan with 'Mulai Transfer? Y/T  '
      Set curs on
      Ht=' '
Do While .not. ht$'YT'
         @23,49 get ht pict '!'
         Read
EndDo
      If ht='Y'
         Sele 3
         Go bott
         Jdt=recno()
         Go top
Do While .not. eof()
            Do pesan with 'Tunggu, sedang di-Transfer... '+ltri(str(awl))+' - '+ltri(str(jdt))
            Tgla=tanggal
            Tglb=tanggal_2
            Ksa =kassa
            Nsls=nama_ksr
            Fkt=faktur
            Klb=kode_klb
            Kdb=kode_brg
            Nmb=nama_brg
            St =satuan
            Jml=jumlah
            Hp =harga
            Hj =harga_2
            Dis2=diskon

            Sele 2
            Flok()
            Appe blan
            Repl tanggal with tgla,tanggal_2 with tglb,faktur with fkt,kassa with ksa,;
                 nama_ksr with nsls,kode_klb with klb,kode_brg with kdb,nama_brg with nmb,;
                 satuan with st,jumlah with jml,harga with hp,harga_2 with hj,diskon with dis2
            Ul()
    
            Sele 1
            Seek kdb
            If harga_2>0
               Rlok()
               Repl stok_k with (stok_k+jml),tanggal_2 with tgl
               Do rstok
               Ul()
EndIf

            Awl=awl+1
            Sele 3
            Dele
            Skip
EndDo

         Sele 1
         Commit

         Sele 2
         Commit
         Close
         If awl>1
            Do pesan with 'Tunggu, sedang Hapus Data yang Lama'
            Uselan('C:&nf0','C:&nf3','C:&nf1','C:&nf2','','')
            Flok()
            Dele for (tanggal>=tgl .and. tanggal<=tgl2)
            Ul()
            Commit
            Close
EndIf
EndIf
Return 0   
   Case lastkey()=13
      Set curs on
      If field[pointer]='JUMLAH'
         Rlok()
         @row(),col() get &fds pict '@e 9999.99'
         Read
         Ul()
         Commit
      Elseif left(field[pointer],5)='HARGA'
         Rlok()
         @row(),col() get &fds pict '@e 9,999,999'
         Read
         Ul()
         Commit
      Elseif field[pointer]='KODE_KLB' .or. field[pointer]='KODE_BRG'
         Rlok()
         @row(),col() get &fds pict '@!'
         Read
         Ul()
         Commit
EndIf
      Do pesan with katax
      Set curs off
Return 2
   Othe
Return 1
Endc
Return 0
Procedure pesan
Parameters psn
Set colo to /r
@23,0 say spac(80)
Set colo to w+/r
@23,40-(len(psn)/2) say psn
Set colo to g
Return
Procedure bacafile
Nf0='J-'+&blnthn+'.DTA'
Nf1='J2'+&blnthn+'.NDX'
Nf2='J3'+&blnthn+'.NDX'
Nf3='J4'+&blnthn+'.NDX'
Return

***Buka File di LAN
Function UseLan
Parameters Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   Use &arg1
   If (!neterr())
      Exit
EndIf
   Do pesan with 'Tunggu, File '+&arg1 +' Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
EndDo
Set inde to &arg2,&arg3,&arg4,&arg5,&arg6
Return .T.

***Buka File u/ Reindex-Exclusive
Function UseIdx
Parameters Arg1
Do While .t.
   Use &arg1 exclusive
   If (!neterr())
      Exit
EndIf
   Tone(700,6)
   Do pesan with 'Proses diTunda, ada yg memBuka File '+arg1+'         '
   Yt=' '
   Set colo to g+*
   @23,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   Set colo to g
   If yt#'Y'
     Batal=.t.
     Exit
EndIf
   @23,0
   Inkey(0.5)
EndDo
Return(.T.)
Function RLOK
Do while .not. Rlock()
   If rlock()
      Exit
EndIf
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
EndDo
Return(.T.) 
Function FLOK
Do while .not. FLock()
   If flock()
      Exit
EndIf
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
EndDo
Return(.T.)
Function UL
Unlock
Return(.T.)
Procedure rstok
Repl stok with (stok_a+stok_m)-stok_k
Return

* EOF TF
