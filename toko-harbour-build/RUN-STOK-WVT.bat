@echo off
cd /d "%~dp0"
in /n "POS-80C"
set PATH=C:\hb30\bin;C:\hb30\comp\mingw\bin;%PATH%
start "" "%~dp0stok-dpj-wvt.exe"
