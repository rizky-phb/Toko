@Echo Off
Set KASSA=1
Net Use Lpt1 /d
Net Use Lpt1 \\kasir\POS-80C
Echo.
Echo.Tunggu sebentar...
Echo.
Cr.Exe
