@echo off
cd /d "%~dp0"
if exist "%~dp0stok.exe" (
    "%~dp0stok.exe"
) else if exist "%~dp0stok-wvt.exe" (
    "%~dp0stok-wvt.exe"
) else (
    echo [ERROR] File stok.exe tidak ditemukan!
    pause
)
if errorlevel 1 pause
