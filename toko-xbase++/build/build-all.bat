@Echo Off
SetLocal
Set KASSA=1
Cd /D "%~dp0..\src"

If Not Exist "..\..\toko-64-bit" Md "..\..\toko-64-bit"

Where pbuild >Nul 2>Nul
If ErrorLevel 1 (
   Echo ERROR: pbuild.exe tidak ditemukan.
   Echo Install Alaska Xbase++ lalu pastikan pbuild.exe, xpp.exe, alink.exe, dan arc.exe ada di PATH.
   Exit /B 1
)

Echo Building CR.EXE...
pbuild ..\build\cr.xpj
If ErrorLevel 1 Exit /B 1

Echo Building STOK.EXE...
pbuild ..\build\stok.xpj
If ErrorLevel 1 Exit /B 1

Echo Building INDEK.EXE...
pbuild ..\build\indek.xpj
If ErrorLevel 1 Exit /B 1

Echo.
Echo Output folder: %~dp0..\..\toko-64-bit
EndLocal
