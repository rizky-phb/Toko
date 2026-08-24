@echo off
cd /d "%~dp0"
in /n "POS-80C"
set PATH=C:\hb30\bin;C:\MinGW\bin;C:\hb30\comp\mingw\bin;%PATH%
set KASSA=1
start "" "%~dp0cr-wvt.exe"
