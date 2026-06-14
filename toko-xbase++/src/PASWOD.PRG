*** PASWOD
Procedure pm
Uselan('PSWD.SET','','','','','')
Stor .f. to Super,Owner,xOwner,Orglain
Priv in,x1,x2,ex
X3=1
Set colo to g+
@24,0
@24,25 say 'Password :'
Set colo to bg+*/b+
@24,36 say '_'
@24,37 say spac(15)
Set colo to b/b,b/b
Do While x3<=3
   Stor spac(15) to in,ex
   @24,37 get in
   Read
   Do arti
   If ex=id1
      Super=.T.
      Exit
   Elseif ex=id2
      Owner=.T.
      Exit
   Elseif ex=id3
      xOwner=.T.
      Exit
   Elseif ex=id4
      Orglain=.T.
      Exit
   Else
      X3=x3+1
      Tone(3500, 0.5)
      Tone(3000, 0.5)
      InKey(0.5)
      Tone(3500, 0.5)
      Tone(3000, 0.5)
      InKey(0.5)
      Tone(3500, 0.5)
      Tone(3000, 0.5)
      InKey(0.5)
EndIf
EndDo
If x3>3
   Hasil=.f.
EndIf
Set colo to
Clos all
Return
Procedure pu
Set deli on
Set deli to '[]'
Sele 1
Uselan('PSWD.SET','','','','','')
Priv in,x1,x2,ex
Stor spac(15) to in,ex
X3=1
Set colo to n++
@1,0 clea to 23,79
B=1
Do While b<=23
   @b,0 say repl(chr(219),80)
   B=b+1
EndDo
@4,20 clea to 11,61
Set colo to /bg,/bg
@3,18 clea to 10,58
@3,18 to 10,59
@5,20 say 'Password Lama   :'
@7,20 say 'Password Baru   :'
@8,20 say 'Ulangi Pswd.Baru:'
@5,38 get in
Read
If lastkey()=27
   Set deli off
Return
EndIf
Pesanx='Salah, tidak bisa ganti dg Password Baru'
Do arti
If Super .and. ex#id1
   Do pesan2 with pesanx
Return
EndIf
If Owner .and. ex#id2
   Do pesan2 with pesanx
Return
EndIf
If xOwner .and. ex#id3
   Do pesan2 with pesanx
Return
EndIf
Stor spac(15) to in,in2
Do While .t.
   Set colo to /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=spac(15)
      Exit
EndIf
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
EndIf
   Do arti
   Yt=' '
Do While .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
EndDo
   If yt='T'
      Loop
EndIf
   Rlok()
   If Super
      Repl id1 with ex
EndIf
   If Owner
      Repl id2 with ex
EndIf
   If xOwner
      Repl id3 with ex
EndIf
   If Orglain
      Repl id4 with ex
EndIf
   Ul()
   Commit
   *
   *DITARUH DI PU2
   *If xOwner
   *   Sele 2
   *   Uselan('DATA.SET','','','','','')
   *   Rlok()
   *   Repl faktur with in
   *   Ul()
   *   Commit
   *Endi   
   Exit
EndDo
Set deli off
Clos all
Return
Procedure pu2
Set deli on
Set deli to '[]'
Uselan('DATA.SET','','','','','')
Priv in,x1,x2,ex
Ex=faktur
In=spac(10)
X3=1
Set colo to n++
@1,0 clea to 23,79
B=1
Do While b<=23
   @b,0 say repl(chr(219),80)
   B=b+1
EndDo
@4,20 clea to 11,61
Set colo to /bg,/bg
@3,18 clea to 10,58
@3,18 to 10,59
@3,31 say 'Password Penjualan'
@5,20 say 'Password Lama   :'
@7,20 say 'Password Baru   :'
@8,20 say 'Ulangi Pswd.Baru:'
@5,38 get in
Read
If lastkey()=27
   Set deli off
Return
EndIf
Pesanx='Salah, tidak bisa ganti dg Password Baru'
If ex#in
   Do pesan2 with pesanx
Return
EndIf
Stor spac(10) to in,in2
Do While .t.
   Set colo to /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=spac(10)
      Exit
EndIf
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
EndIf
   Yt=' '
Do While .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
EndDo
   If yt='T'
      Loop
EndIf
   Rlok()
   Repl faktur with in
   Ul()
   Commit
   Exit
EndDo
Set deli off
Clos all
Return
Procedure arti
X1=1
X2=len(rtri(in))
Stor ' ' to i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15
Do While x1<=x2
   Zz=ltri(str(x1))
   Do case
      Case subs(in,x1,1)='A'
         I&zz='j'
      Case subs(in,x1,1)='a'
         I&zz='k'
      Case subs(in,x1,1)='~'
         I&zz='l'
      Case subs(in,x1,1)='B'
         I&zz=';'
      Case subs(in,x1,1)='b'
         I&zz="'"
      Case subs(in,x1,1)='!'
         I&zz='5'
      Case subs(in,x1,1)='C'
         I&zz='4'
      Case subs(in,x1,1)='c'
         I&zz='3'
      Case subs(in,x1,1)='@'
         I&zz='2'
      Case subs(in,x1,1)='D'
         I&zz='1'
      Case subs(in,x1,1)='d'
         I&zz='`'
      Case subs(in,x1,1)='#'
         I&zz='n'
      Case subs(in,x1,1)='E'
         I&zz='m'
      Case subs(in,x1,1)='e'
         I&zz=','
      Case subs(in,x1,1)='$'
         I&zz='.'
      Case subs(in,x1,1)='F'
         I&zz='/'
      Case subs(in,x1,1)='f'
         I&zz='z'
      Case subs(in,x1,1)='%'
         I&zz='x'
      Case subs(in,x1,1)='G'
         I&zz='c'
      Case subs(in,x1,1)='g'
         I&zz='v'
      Case subs(in,x1,1)='^'
         I&zz='b'
      Case subs(in,x1,1)='H'
         I&zz='h'
      Case subs(in,x1,1)='h'
         I&zz='g'
      Case subs(in,x1,1)='&'
         I&zz='f'
      Case subs(in,x1,1)='I'
         I&zz='d'
      Case subs(in,x1,1)='i'
         I&zz='s'
      Case subs(in,x1,1)='*'
         I&zz='a'
      Case subs(in,x1,1)='J'
         I&zz='='
      Case subs(in,x1,1)='j'
         I&zz='-'
      Case subs(in,x1,1)='('
         I&zz='0'
      Case subs(in,x1,1)='K'
         I&zz='9'
      Case subs(in,x1,1)='k'
         I&zz='8'
      Case subs(in,x1,1)=')'
         I&zz='7'
      Case subs(in,x1,1)='L'
         I&zz='6'
      Case subs(in,x1,1)='l'
         I&zz='u'
      Case subs(in,x1,1)='_'
         I&zz='i'
      Case subs(in,x1,1)='M'
         I&zz='o'
      Case subs(in,x1,1)='m'
         I&zz='p'
      Case subs(in,x1,1)='+'
         I&zz='['
      Case subs(in,x1,1)=' '
         I&zz=']'
      Case subs(in,x1,1)='N'
         I&zz='q'
      Case subs(in,x1,1)='n'
         I&zz='w'
      Case subs(in,x1,1)='}'
         I&zz='e'
      Case subs(in,x1,1)='O'
         I&zz='r'
      Case subs(in,x1,1)='o'
         I&zz='t'
      Case subs(in,x1,1)='{'
         I&zz='y'
      Case subs(in,x1,1)='P'
         I&zz='J'
      Case subs(in,x1,1)='p'
         I&zz='K'
      Case subs(in,x1,1)=':'
         I&zz='L'
      Case subs(in,x1,1)='Q'
         I&zz=':'
      Case subs(in,x1,1)='q'
         I&zz='"'
      Case subs(in,x1,1)='"'
         I&zz='%'
      Case subs(in,x1,1)='R'
         I&zz='$'
      Case subs(in,x1,1)='r'
         I&zz='#'
      Case subs(in,x1,1)='?'
         I&zz='@'
      Case subs(in,x1,1)='S'
         I&zz='!'
      Case subs(in,x1,1)='s'
         I&zz='~'
      Case subs(in,x1,1)='>'
         I&zz='N'
      Case subs(in,x1,1)='T'
         I&zz='M'
      Case subs(in,x1,1)='t'
         I&zz='<'
      Case subs(in,x1,1)='<'
         I&zz='>'
      Case subs(in,x1,1)='U'
         I&zz='?'
      Case subs(in,x1,1)='u'
         I&zz='Z'
      Case subs(in,x1,1)=','
         I&zz='X'
      Case subs(in,x1,1)='V'
         I&zz='C'
      Case subs(in,x1,1)='v'
         I&zz='V'
      Case subs(in,x1,1)='.'
         I&zz='B'
      Case subs(in,x1,1)='W'
         I&zz='H'
      Case subs(in,x1,1)='w'
         I&zz='G'
      Case subs(in,x1,1)='/'
         I&zz='F'
      Case subs(in,x1,1)='X'
         I&zz='D'
      Case subs(in,x1,1)='x'
         I&zz='S'
      Case subs(in,x1,1)="'"
         I&zz='A'
      Case subs(in,x1,1)='Y'
         I&zz='^'
      Case subs(in,x1,1)='y'
         I&zz='&'
      Case subs(in,x1,1)=';'
         I&zz='*'
      Case subs(in,x1,1)='Z'
         I&zz='('
      Case subs(in,x1,1)='z'
         I&zz=')'
      Case subs(in,x1,1)='['
         I&zz='_'
      Case subs(in,x1,1)=']'
         I&zz='+'
      Case subs(in,x1,1)='='
         I&zz='U'
      Case subs(in,x1,1)='-'
         I&zz='I'
      Case subs(in,x1,1)='0'
         I&zz='O'
      Case subs(in,x1,1)='9'
         I&zz='P'
      Case subs(in,x1,1)='8'
         I&zz='{'
      Case subs(in,x1,1)='7'
         I&zz='}'
      Case subs(in,x1,1)='6'
         I&zz='Y'
      Case subs(in,x1,1)='5'
         I&zz='T'
      Case subs(in,x1,1)='4'
         I&zz='R'
      Case subs(in,x1,1)='3'
         I&zz='E'
      Case subs(in,x1,1)='2'
         I&zz='W'
      Case subs(in,x1,1)='1'
         I&zz='Q'
      Case subs(in,x1,1)='`'
         I&zz=' '
      Case subs(in,x1,1)='\'
         I&zz='|'
      Case subs(in,x1,1)='|'
         I&zz='\'
   Endc
   X1=x1+1
EndDo
Ex =i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11+i12+i13+i14+i15
Return
Procedure konvtgl
Hri=left(dtoc(tgl),2)
Bln=subs(dtoc(tgl),4,2)
Thn=ltri(str(year(tgl)))
Return

***Indeks File
Function Indx
Parameters Arg4,arg5,arg6,arg3
@24,0 say 'File: '+spac(12)
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   @24,6 say arg6
   Useidx("&arg4")
   Inde on &arg5 to &arg6
EndIf
@24,b say repl(chr(219),arg3)
B=b+arg3
Clos all
Return .T.

***Buka File di LAN
Function UseLan
Parameters Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   Use &arg1
   If (!neterr())
      Exit
EndIf
   Do pesan with 'Tunggu, File '+&arg1 +'Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
EndDo
Set inde to &arg2,&arg3,&arg4,&arg5,&arg6
Return(.T.)

***Buka File u/ Reindex-Exclusive
Function UseIdx
Parameters Arg1
Do While .t.
   Use &arg1 exclusive
   If (!neterr())
      Exit
EndIf
   Tone(700,6)
   Do pesan with 'Ada User (sama) yg Sudah On-Line. ('+arg1+')'
   Yt=' '
   Set colo to g+*
   @24,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   Set colo to g
   If yt#'Y'
     Batal=.t.
     Exit
EndIf
   @24,0
   Inkey(0.5)
EndDo
Return(.T.)
Function RLOK
Do while .not. RLock()
   If rlock()
      Exit
EndIf
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Enddo
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
Enddo
Return(.T.)
Function Ul
Unlock
Return(.T.)
Procedure fmtjml
J1=jumlah
J2=jumlah_rtr
Jml=tran(j1-j2,'@e 99,999 ')
If j1-j2#0
   If (j1-j2>0 .and. j1-j2<1) .or. (j1-j2<0 .and. j1-j2>-1)
      Jml=tran(j1-j2,'@e 999.99 ')
   Elseif mod(j1-j2,int(j1-j2))>0
      Jml=tran(j1-j2,'@e 999.99 ')
EndIf
EndIf
Rele j1,j2
Return
Procedure fmtjml2
Jml=tran(xjml,'@e 99,999')
If xjml#0
   If (xjml>0 .and. xjml<1) .or. (xjml<0 .and. xjml>-1)
      Jml=tran(xjml,'@e 999.99')
   Elseif mod(xjml,int(xjml))>0
      Jml=tran(xjml,'@e 999.99')
EndIf
EndIf
Return
Procedure dataht
Hapus=.t.
Keyboard chr(13)
Return

* EOF PASWOD
