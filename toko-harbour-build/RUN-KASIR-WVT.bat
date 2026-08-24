@echo off
cd /d "%~dp0"
set KASSA=1
if exist "%~dp0cr-wvt.exe" (
    "%~dp0cr-wvt.exe"
) else if exist "%~dp0cr.exe" (
    "%~dp0cr.exe"
) else (
    echo [ERROR] File cr-wvt.exe atau cr.exe tidak ditemukan!
    pause
)
if errorlevel 1 pause
