@echo off
cd /d "%~dp0"
<<<<<<< Updated upstream
rem Program kasir mencetak langsung ke LPT1.
rem Pastikan printer Windows sudah dibagikan dengan nama: POS-80C
net use LPT1: /delete /y >nul 2>&1
net use LPT1: "\\%COMPUTERNAME%\POS-80C" /persistent:no
if errorlevel 1 (
  echo.
  echo Printer "POS-80C" tidak dapat dipetakan ke LPT1.
  echo Pastikan printer sudah di-share dengan nama tersebut lalu jalankan ulang file ini.
  pause
  exit /b 1
)
=======
in /n "POS-80C"
>>>>>>> Stashed changes
set PATH=C:\hb30\bin;C:\hb30\comp\mingw\bin;%PATH%
set KASSA=1
start "" "%~dp0cr-wvt.exe"