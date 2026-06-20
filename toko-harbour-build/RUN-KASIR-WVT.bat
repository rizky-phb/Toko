@echo off
cd /d "%~dp0"
set PATH=C:\hb30\bin;C:\hb30\comp\mingw\bin;%PATH%
set KASSA=1
start "" "%~dp0cr-wvt.exe"
