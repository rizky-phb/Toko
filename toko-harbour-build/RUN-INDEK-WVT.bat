@echo off
cd /d "%~dp0"
chcp 850 > nul
if exist "%~dp0indek-dpj-fix-wvt.exe" (
    "%~dp0indek-dpj-fix-wvt.exe"
) else if exist "%~dp0indek.exe" (
    "%~dp0indek.exe"
) else (
    echo File indek.exe tidak ditemukan!
    pause
)
