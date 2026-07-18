*** PASWOD
Proc pm
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
Do whil x3<=3
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
   Endi
Endd
If x3>3
   Hasil=.f.
Endi
Set colo to
Clos all
Retu

Proc pu
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
Do whil b<=23
   @b,0 say repl(chr(219),80)
   B=b+1
Endd
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
   Retu
Endi
Pesanx='Salah, tidak bisa ganti dg Password Baru'
Do arti
If Super .and. ex#id1
   Do pesan2 with pesanx
   Retu
Endi
If Owner .and. ex#id2
   Do pesan2 with pesanx
   Retu
Endi
If xOwner .and. ex#id3
   Do pesan2 with pesanx
   Retu
Endi
Stor spac(15) to in,in2
Do whil .t.
   Set colo to /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=spac(15)
      Exit
   Endi
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
   Endi
   Do arti
   Yt=' '
   Do whil .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
   Endd
   If yt='T'
      Loop
   Endi
   Rlok()
   If Super
      Repl id1 with ex
   Endi
   If Owner
      Repl id2 with ex
   Endi
   If xOwner
      Repl id3 with ex
   Endi
   If Orglain
      Repl id4 with ex
   Endi
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
Endd
Set deli off
Clos all
Retu

Proc pu2
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
Do whil b<=23
   @b,0 say repl(chr(219),80)
   B=b+1
Endd
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
   Retu
Endi
Pesanx='Salah, tidak bisa ganti dg Password Baru'
If ex#in
   Do pesan2 with pesanx
   Retu
Endi
Stor spac(10) to in,in2
Do whil .t.
   Set colo to /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=spac(10)
      Exit
   Endi
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
   Endi
   Yt=' '
   Do whil .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
   Endd
   If yt='T'
      Loop
   Endi
   Rlok()
   Repl faktur with in
   Ul()
   Commit
   Exit
Endd
Set deli off
Clos all
Retu

Proc arti
X1=1
X2=len(rtri(in))
Stor ' ' to i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15
Do whil x1<=x2
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
Endd
Ex =i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11+i12+i13+i14+i15
Retu

Proc konvtgl
Hri=left(dtoc(tgl),2)
Bln=subs(dtoc(tgl),4,2)
Thn=ltri(str(year(tgl)))
Retu

***Indeks File
Func Indx
Para Arg4,arg5,arg6,arg3
@24,0 say 'File: '+spac(12)
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   @24,6 say arg6
   Useidx("&arg4")
   Inde on &arg5 to &arg6
Endi
@24,b say repl(chr(219),arg3)
B=b+arg3
Clos all
Retu .T.

***Buka File di LAN
Func UseLan
Para Arg1,arg2,arg3,arg4,arg5,arg6
Local cAlias
Do while (.T.)
   cAlias=HBALIAS(arg1)
   Use (arg1) Alias (cAlias)
   If (!neterr())
      Exit
   Endi
   Do pesan with 'Tunggu, File '+&arg1 +'Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Endd
Set inde to &arg2,&arg3,&arg4,&arg5,&arg6
Retu(.T.)

***Buka File u/ Reindex-Exclusive
Func UseIdx
Para Arg1
Local cAlias
Do whil .t.
   cAlias=HBALIAS(arg1)
   Use (arg1) Alias (cAlias) exclusive
   If (!neterr())
      Exit
   Endi
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
   Endi
   @24,0
   Inkey(0.5)
Endd
Retu(.T.)
Func HBALIAS
Para cFile
Local cAlias,nPos
cAlias=upper(cFile)
nPos=rat('\',cAlias)
If nPos>0
   cAlias=substr(cAlias,nPos+1)
Endi
nPos=rat('/',cAlias)
If nPos>0
   cAlias=substr(cAlias,nPos+1)
Endi
nPos=rat('.',cAlias)
If nPos>0
   cAlias=left(cAlias,nPos-1)
Endi
cAlias=strtran(cAlias,'-','_')
cAlias=strtran(cAlias,'.','_')
cAlias=strtran(cAlias,' ','_')
Retu cAlias
Func RLOK
Do while .not. RLock()
   If rlock()
      Exit
   Endi
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Enddo
Retu(.T.) 

Func FLOK
Do while .not. FLock()
   If flock()
      Exit
   Endi
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Enddo
Return(.T.)

Func Ul
Unlock
Retu(.T.)

Proc fmtjml
J1=jumlah
J2=jumlah_rtr
Jml=tran(j1-j2,'@e 99,999 ')
If j1-j2#0
   If (j1-j2>0 .and. j1-j2<1) .or. (j1-j2<0 .and. j1-j2>-1)
      Jml=tran(j1-j2,'@e 999.99 ')
   Elseif mod(j1-j2,int(j1-j2))>0
      Jml=tran(j1-j2,'@e 999.99 ')
   Endi
Endi
Rele j1,j2
Retu

Proc fmtjml2
Jml=tran(xjml,'@e 99,999')
If xjml#0
   If (xjml>0 .and. xjml<1) .or. (xjml<0 .and. xjml>-1)
      Jml=tran(xjml,'@e 999.99')
   Elseif mod(xjml,int(xjml))>0
      Jml=tran(xjml,'@e 999.99')
   Endi
Endi
Retu

Proc dataht
Hapus=.t.
Keyboard chr(13)
Retu

* EOF PASWOD
