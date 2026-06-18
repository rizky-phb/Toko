// ============================================================
// File    : PASWOD.PRG
// Converted from CA-Clipper to Harbour
// Converter: auto-conversion script
// Note    : Requires Harbour 3.x + hbct lib for DbEdit()
//           Ensure DBF files use DBFCDX RDD
// ============================================================
*** PASWOD
PROCEDURE pm
Uselan('PSWD.SET','','','','','')
STORE .f. to Super,Owner,xOwner,Orglain
Priv in,x1,x2,ex
X3=1
SET COLOR TO g+
@24,0
@24,25 say 'Password :'
SET COLOR TO bg+*/b+
@24,36 say '_'
@24,37 say SPACE(15)
SET COLOR TO b/b,b/b
DO WHILE x3<=3
   STORE SPACE(15) to in,ex
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
   ENDIF
ENDDO
If x3>3
   Hasil=.f.
ENDIF
SET COLOR TO
CLOSE all
RETURN

PROCEDURE pu
Set deli on
Set deli to '[]'
SELECT 1
Uselan('PSWD.SET','','','','','')
Priv in,x1,x2,ex
STORE SPACE(15) to in,ex
X3=1
SET COLOR TO n++
@1,0 CLEAR to 23,79
B=1
DO WHILE b<=23
   @b,0 say REPLACE(chr(219),80)
   B=b+1
ENDDO
@4,20 CLEAR to 11,61
SET COLOR TO /bg,/bg
@3,18 CLEAR to 10,58
@3,18 to 10,59
@5,20 say 'Password Lama   :'
@7,20 say 'Password Baru   :'
@8,20 say 'Ulangi Pswd.Baru:'
@5,38 get in
Read
If lastkey()=27
   Set deli off
   RETURN
ENDIF
Pesanx='Salah, tidak bisa ganti dg Password Baru'
Do arti
If Super .and. ex#id1
   Do pesan2 with pesanx
   RETURN
ENDIF
If Owner .and. ex#id2
   Do pesan2 with pesanx
   RETURN
ENDIF
If xOwner .and. ex#id3
   Do pesan2 with pesanx
   RETURN
ENDIF
STORE SPACE(15) to in,in2
DO WHILE .t.
   SET COLOR TO /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=SPACE(15)
      Exit
   ENDIF
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
   ENDIF
   Do arti
   Yt=' '
   DO WHILE .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
   ENDDO
   If yt='T'
      Loop
   ENDIF
   Rlok()
   If Super
      REPLACE id1 with ex
   ENDIF
   If Owner
      REPLACE id2 with ex
   ENDIF
   If xOwner
      REPLACE id3 with ex
   ENDIF
   If Orglain
      REPLACE id4 with ex
   ENDIF
   Ul()
   COMMIT
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
ENDDO
Set deli off
CLOSE all
RETURN

PROCEDURE pu2
Set deli on
Set deli to '[]'
Uselan('DATA.SET','','','','','')
Priv in,x1,x2,ex
Ex=faktur
In=SPACE(10)
X3=1
SET COLOR TO n++
@1,0 CLEAR to 23,79
B=1
DO WHILE b<=23
   @b,0 say REPLACE(chr(219),80)
   B=b+1
ENDDO
@4,20 CLEAR to 11,61
SET COLOR TO /bg,/bg
@3,18 CLEAR to 10,58
@3,18 to 10,59
@3,31 say 'Password Penjualan'
@5,20 say 'Password Lama   :'
@7,20 say 'Password Baru   :'
@8,20 say 'Ulangi Pswd.Baru:'
@5,38 get in
Read
If lastkey()=27
   Set deli off
   RETURN
ENDIF
Pesanx='Salah, tidak bisa ganti dg Password Baru'
If ex#in
   Do pesan2 with pesanx
   RETURN
ENDIF
STORE SPACE(10) to in,in2
DO WHILE .t.
   SET COLOR TO /bg,/bg
   @7,38 get in
   @8,38 get in2
   Read
   If lastkey()=27 .or. in=SPACE(10)
      Exit
   ENDIF
   If in#in2
      Do pesan2 with 'Password Baru & Konfirmasi Harus Sama'
      Loop
   ENDIF
   Yt=' '
   DO WHILE .not. yt$'YT'
      @10,23 say 'Password Baru Benar ? Y/T'get yt pict '!'
      Read
   ENDDO
   If yt='T'
      Loop
   ENDIF
   Rlok()
   REPLACE faktur with in
   Ul()
   COMMIT
   Exit
ENDDO
Set deli off
CLOSE all
RETURN

PROCEDURE arti
X1=1
X2=len(RTRIM(in))
STORE ' ' to i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15
DO WHILE x1<=x2
   Zz=LTRIM(str(x1))
   Do case
      Case SUBSTR(in,x1,1)='A'
         I&zz='j'
      Case SUBSTR(in,x1,1)='a'
         I&zz='k'
      Case SUBSTR(in,x1,1)='~'
         I&zz='l'
      Case SUBSTR(in,x1,1)='B'
         I&zz=';'
      Case SUBSTR(in,x1,1)='b'
         I&zz="'"
      Case SUBSTR(in,x1,1)='!'
         I&zz='5'
      Case SUBSTR(in,x1,1)='C'
         I&zz='4'
      Case SUBSTR(in,x1,1)='c'
         I&zz='3'
      Case SUBSTR(in,x1,1)='@'
         I&zz='2'
      Case SUBSTR(in,x1,1)='D'
         I&zz='1'
      Case SUBSTR(in,x1,1)='d'
         I&zz='`'
      Case SUBSTR(in,x1,1)='#'
         I&zz='n'
      Case SUBSTR(in,x1,1)='E'
         I&zz='m'
      Case SUBSTR(in,x1,1)='e'
         I&zz=','
      Case SUBSTR(in,x1,1)='$'
         I&zz='.'
      Case SUBSTR(in,x1,1)='F'
         I&zz='/'
      Case SUBSTR(in,x1,1)='f'
         I&zz='z'
      Case SUBSTR(in,x1,1)='%'
         I&zz='x'
      Case SUBSTR(in,x1,1)='G'
         I&zz='c'
      Case SUBSTR(in,x1,1)='g'
         I&zz='v'
      Case SUBSTR(in,x1,1)='^'
         I&zz='b'
      Case SUBSTR(in,x1,1)='H'
         I&zz='h'
      Case SUBSTR(in,x1,1)='h'
         I&zz='g'
      Case SUBSTR(in,x1,1)='&'
         I&zz='f'
      Case SUBSTR(in,x1,1)='I'
         I&zz='d'
      Case SUBSTR(in,x1,1)='i'
         I&zz='s'
      Case SUBSTR(in,x1,1)='*'
         I&zz='a'
      Case SUBSTR(in,x1,1)='J'
         I&zz='='
      Case SUBSTR(in,x1,1)='j'
         I&zz='-'
      Case SUBSTR(in,x1,1)='('
         I&zz='0'
      Case SUBSTR(in,x1,1)='K'
         I&zz='9'
      Case SUBSTR(in,x1,1)='k'
         I&zz='8'
      Case SUBSTR(in,x1,1)=')'
         I&zz='7'
      Case SUBSTR(in,x1,1)='L'
         I&zz='6'
      Case SUBSTR(in,x1,1)='l'
         I&zz='u'
      Case SUBSTR(in,x1,1)='_'
         I&zz='i'
      Case SUBSTR(in,x1,1)='M'
         I&zz='o'
      Case SUBSTR(in,x1,1)='m'
         I&zz='p'
      Case SUBSTR(in,x1,1)='+'
         I&zz='['
      Case SUBSTR(in,x1,1)=' '
         I&zz=']'
      Case SUBSTR(in,x1,1)='N'
         I&zz='q'
      Case SUBSTR(in,x1,1)='n'
         I&zz='w'
      Case SUBSTR(in,x1,1)='}'
         I&zz='e'
      Case SUBSTR(in,x1,1)='O'
         I&zz='r'
      Case SUBSTR(in,x1,1)='o'
         I&zz='t'
      Case SUBSTR(in,x1,1)='{'
         I&zz='y'
      Case SUBSTR(in,x1,1)='P'
         I&zz='J'
      Case SUBSTR(in,x1,1)='p'
         I&zz='K'
      Case SUBSTR(in,x1,1)=':'
         I&zz='L'
      Case SUBSTR(in,x1,1)='Q'
         I&zz=':'
      Case SUBSTR(in,x1,1)='q'
         I&zz='"'
      Case SUBSTR(in,x1,1)='"'
         I&zz='%'
      Case SUBSTR(in,x1,1)='R'
         I&zz='$'
      Case SUBSTR(in,x1,1)='r'
         I&zz='#'
      Case SUBSTR(in,x1,1)='?'
         I&zz='@'
      Case SUBSTR(in,x1,1)='S'
         I&zz='!'
      Case SUBSTR(in,x1,1)='s'
         I&zz='~'
      Case SUBSTR(in,x1,1)='>'
         I&zz='N'
      Case SUBSTR(in,x1,1)='T'
         I&zz='M'
      Case SUBSTR(in,x1,1)='t'
         I&zz='<'
      Case SUBSTR(in,x1,1)='<'
         I&zz='>'
      Case SUBSTR(in,x1,1)='U'
         I&zz='?'
      Case SUBSTR(in,x1,1)='u'
         I&zz='Z'
      Case SUBSTR(in,x1,1)=','
         I&zz='X'
      Case SUBSTR(in,x1,1)='V'
         I&zz='C'
      Case SUBSTR(in,x1,1)='v'
         I&zz='V'
      Case SUBSTR(in,x1,1)='.'
         I&zz='B'
      Case SUBSTR(in,x1,1)='W'
         I&zz='H'
      Case SUBSTR(in,x1,1)='w'
         I&zz='G'
      Case SUBSTR(in,x1,1)='/'
         I&zz='F'
      Case SUBSTR(in,x1,1)='X'
         I&zz='D'
      Case SUBSTR(in,x1,1)='x'
         I&zz='S'
      Case SUBSTR(in,x1,1)="'"
         I&zz='A'
      Case SUBSTR(in,x1,1)='Y'
         I&zz='^'
      Case SUBSTR(in,x1,1)='y'
         I&zz='&'
      Case SUBSTR(in,x1,1)=';'
         I&zz='*'
      Case SUBSTR(in,x1,1)='Z'
         I&zz='('
      Case SUBSTR(in,x1,1)='z'
         I&zz=')'
      Case SUBSTR(in,x1,1)='['
         I&zz='_'
      Case SUBSTR(in,x1,1)=']'
         I&zz='+'
      Case SUBSTR(in,x1,1)='='
         I&zz='U'
      Case SUBSTR(in,x1,1)='-'
         I&zz='I'
      Case SUBSTR(in,x1,1)='0'
         I&zz='O'
      Case SUBSTR(in,x1,1)='9'
         I&zz='P'
      Case SUBSTR(in,x1,1)='8'
         I&zz='{'
      Case SUBSTR(in,x1,1)='7'
         I&zz='}'
      Case SUBSTR(in,x1,1)='6'
         I&zz='Y'
      Case SUBSTR(in,x1,1)='5'
         I&zz='T'
      Case SUBSTR(in,x1,1)='4'
         I&zz='R'
      Case SUBSTR(in,x1,1)='3'
         I&zz='E'
      Case SUBSTR(in,x1,1)='2'
         I&zz='W'
      Case SUBSTR(in,x1,1)='1'
         I&zz='Q'
      Case SUBSTR(in,x1,1)='`'
         I&zz=' '
      Case SUBSTR(in,x1,1)='\'
         I&zz='|'
      Case SUBSTR(in,x1,1)='|'
         I&zz='\'
   ENDCASE
   X1=x1+1
ENDDO
Ex =i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11+i12+i13+i14+i15
RETURN

PROCEDURE konvtgl
Hri=left(dtoc(tgl),2)
Bln=SUBSTR(dtoc(tgl),4,2)
Thn=LTRIM(str(year(tgl)))
RETURN

***Indeks File
FUNCTION Indx
PARAMETERS Arg4,arg5,arg6,arg3
@24,0 say 'File: '+SPACE(12)
If arg4#' '
   Arg4='&arg4'+'.DTA'
   Arg6='&arg6'+'.NDX'
   @24,6 say arg6
   Useidx("&arg4")
   INDEX ON &arg5 to &arg6
ENDIF
@24,b say REPLACE(chr(219),arg3)
B=b+arg3
CLOSE all
RETURN .T.

***Buka File di LAN
FUNCTION UseLan
PARAMETERS Arg1,arg2,arg3,arg4,arg5,arg6
Do while (.T.)
   Use &arg1
   If (!NetErr())
      Exit
   ENDIF
   Do pesan with 'Tunggu, File '+&arg1 +'Sedang Dipakai'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
ENDDO
SET INDEX TO &arg2,&arg3,&arg4,&arg5,&arg6
RETURN(.T.)

***Buka File u/ Reindex-Exclusive
FUNCTION UseIdx
PARAMETERS Arg1
DO WHILE .t.
   Use &arg1 exclusive
   If (!NetErr())
      Exit
   ENDIF
   Tone(700,6)
   Do pesan with 'Ada User (sama) yg Sudah On-Line. ('+arg1+')'
   Yt=' '
   SET COLOR TO g+*
   @24,62 say 'Coba Lagi? Y/T'get yt pict '!'
   Read
   SET COLOR TO g
   If yt#'Y'
     Batal=.t.
     Exit
   ENDIF
   @24,0
   Inkey(0.5)
ENDDO
RETURN(.T.)

FUNCTION RLOK
Do while .not. RLock()
   If rlock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /rl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Enddo
RETURN(.T.) 

FUNCTION FLOK
Do while .not. FLock()
   If flock()
      Exit
   ENDIF
   Do pesan with 'Tunggu... File Sedang diPakai User lain /fl'
   Tone(100, 0.5)
   Tone(150, 0.5)
   InKey(0.5)
Enddo
Return(.T.)

FUNCTION Ul
Unlock
RETURN(.T.)

PROCEDURE fmtjml
J1=jumlah
J2=jumlah_rtr
Jml=tran(j1-j2,'@e 99,999 ')
If j1-j2#0
   If (j1-j2>0 .and. j1-j2<1) .or. (j1-j2<0 .and. j1-j2>-1)
      Jml=tran(j1-j2,'@e 999.99 ')
   Elseif mod(j1-j2,int(j1-j2))>0
      Jml=tran(j1-j2,'@e 999.99 ')
   ENDIF
ENDIF
RELEASE j1,j2
RETURN

PROCEDURE fmtjml2
Jml=tran(xjml,'@e 99,999')
If xjml#0
   If (xjml>0 .and. xjml<1) .or. (xjml<0 .and. xjml>-1)
      Jml=tran(xjml,'@e 999.99')
   Elseif mod(xjml,int(xjml))>0
      Jml=tran(xjml,'@e 999.99')
   ENDIF
ENDIF
RETURN

PROCEDURE dataht
Hapus=.t.
Keyboard chr(13)
RETURN

* EOF PASWOD
