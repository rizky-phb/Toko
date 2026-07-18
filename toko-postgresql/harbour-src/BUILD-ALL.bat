@echo off
setlocal
cd /d "%~dp0"
set "HBMK=C:\hb30\bin\hbmk2.exe"
set "OUT=%~dp0..\harbour-build"

if not exist "%HBMK%" (
  echo Harbour tidak ditemukan: %HBMK%
  exit /b 1
)
if not exist "%OUT%" mkdir "%OUT%"

"%HBMK%" build-entrypoints\MAIN_CR.PRG HARBOUR_COMPAT.PRG -gtwvt -ocr-dbf-wvt || exit /b 1
move /y cr-dbf-wvt.exe "%OUT%\cr-dbf-wvt.exe" >nul || exit /b 1

"%HBMK%" build-entrypoints\MAIN_STOK.PRG STOK_COMPAT.PRG -gtwvt -ostok-dbf-wvt || exit /b 1
move /y stok-dbf-wvt.exe "%OUT%\stok-dbf-wvt.exe" >nul || exit /b 1

"%HBMK%" build-entrypoints\MAIN_INDEK.PRG -gtwvt -oindek-dbf-wvt || exit /b 1
move /y indek-dbf-wvt.exe "%OUT%\indek-dbf-wvt.exe" >nul || exit /b 1

"%HBMK%" sql\pg_connection.prg -lhbwin -opg-connection-test || exit /b 1
move /y pg-connection-test.exe "%OUT%\pg-connection-test.exe" >nul || exit /b 1

echo Build Harbour selesai: %OUT%
exit /b 0
