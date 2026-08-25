@echo off
cd /d "%~dp0"
chcp 850 > nul
if exist "%~dp0stok-wvt.exe" (
    "%~dp0stok-wvt.exe"
) else if exist "%~dp0stok.exe" (
    "%~dp0stok.exe"
) else (
    echo File stok-wvt.exe tidak ditemukan!
    pause
)
