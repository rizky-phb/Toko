@echo off
cd /d "%~dp0"
if exist "%~dp0indek.exe" (
    "%~dp0indek.exe"
) else if exist "%~dp0indek-wvt.exe" (
    "%~dp0indek-wvt.exe"
) else (
    echo [ERROR] File indek.exe tidak ditemukan!
    pause
)
if errorlevel 1 pause
