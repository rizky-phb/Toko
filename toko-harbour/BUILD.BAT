@echo off
setlocal
cd /d "%~dp0src"

echo ============================================================
echo   BUILD TOKO HARBOUR - POS-80C AND F2 CUSTOMER ENHANCED
echo ============================================================
echo.

set "PATH=C:\hb30\bin;C:\MinGW\bin;C:\hb30\comp\mingw\bin;%PATH%"

where hbmk2.exe >nul 2>&1
if errorlevel 1 (
    echo ERROR: Compiler Harbour hbmk2.exe tidak ditemukan di C:\hb30\bin
    echo Pastikan Harbour 3.0 sudah terpasang di C:\hb30
    exit /b 1
)

echo [1/3] Compiling Kasir - cr.exe...
hbmk2 -gtwvt -lhbwin -lhbct -comp=mingw CR.PRG HARBOUR_COMPAT.PRG -ocr
if errorlevel 1 (
    echo ERROR: Gagal kompilasi CR.PRG
    exit /b 1
)

echo [2/3] Compiling Stok - stok.exe...
hbmk2 -gtwvt -lhbwin -lhbct -comp=mingw STOK.PRG -ostok
if errorlevel 1 (
    echo ERROR: Gagal kompilasi STOK.PRG
    exit /b 1
)

echo [3/3] Compiling Indek - indek.exe...
hbmk2 -gtwvt -lhbwin -lhbct -comp=mingw INDEK.PRG -oindek
if errorlevel 1 (
    echo ERROR: Gagal kompilasi INDEK.PRG
    exit /b 1
)

echo.
echo ============================================================
echo   BUILD SELESAI SUKSES!
echo   Executable baru telah dibuat di: toko-harbour\src\
echo   - cr.exe    (Kasir + Print POS-80C + Shortcut F2)
echo   - stok.exe  (Manajemen Stok)
echo   - indek.exe (Reindex Data)
echo ============================================================
echo.
