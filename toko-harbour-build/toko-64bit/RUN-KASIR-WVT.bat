@echo off
cd /d "%~dp0"
chcp 850 > nul
set KASSA=1
if exist "%~dp0cr-wvt.exe" (
    "%~dp0cr-wvt.exe"
) else if exist "%~dp0cr.exe" (
    "%~dp0cr.exe"
) else (
    echo File cr-wvt.exe tidak ditemukan!
    pause
)



