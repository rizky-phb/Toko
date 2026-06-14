set talk off
set scor off
clea
set prin on
set devi to prin
a=1
do whil a<=60
   b=ltri(str(a))
   c='chr(27)+chr(33)+chr(&b)'
   ?&c
   ?'&c'
   a=a+1
endd
set prin off
set devi to scre

*@prow()+1,0 say chr(27)+'@'
*@prow()+1,0 say chr(27)+'p'+'0'+chr(0)+chr(15)
*@prow()+1,0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'255'
*@prow()+1,0 say chr(27)+chr(64)+chr(27)+'p'+'0'+'5'+'   '
*@prow()+1,0 say chr(27)+'p'+chr(1)+chr(29)+chr(73)
*@prow()+1,0 say chr(16)+chr(4)+chr(1)
