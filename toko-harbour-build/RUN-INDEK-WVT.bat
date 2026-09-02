@echo off
cd /d "%~dp0"
chcp 850 > nul
chcp 437
mode con cp select=437
if exist "%~dp0indek-dpj-fix-wvt.exe" (
    "%~dp0indek-dpj-fix-wvt.exe"
) else if exist "%~dp0indek.exe" (
    "%~dp0indek.exe"
) else (
    echo File indek.exe tidak ditemukan!
    pause
)
