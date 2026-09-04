@echo off
REM ---------------------------------------------------------------------------
REM update_greenbook.cmd -- monthly Green Book refresh
REM
REM Wrapper for Windows Task Scheduler. Runs R/04_monthly_update.R from the
REM project root and appends everything to a dated log, so a failed run leaves
REM evidence rather than vanishing.
REM
REM FDA publishes the Green Book monthly but not on a fixed day, so schedule
REM this for the 5th or later to avoid running before the new edition lands.
REM ---------------------------------------------------------------------------

setlocal

REM Project root is the parent of this script's directory.
set "PROJECT=%~dp0.."
set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"

if not exist "%RSCRIPT%" (
  echo [%date% %time%] Rscript not found at "%RSCRIPT%" - update R version in this script.
  exit /b 1
)

cd /d "%PROJECT%" || exit /b 1
if not exist "logs" mkdir "logs"

set "LOG=logs\update_%date:~-4%-%date:~4,2%.log"

echo. >> "%LOG%"
echo ======================================================== >> "%LOG%"
echo [%date% %time%] starting monthly update >> "%LOG%"

"%RSCRIPT%" R/04_monthly_update.R >> "%LOG%" 2>&1
set "RC=%ERRORLEVEL%"

if "%RC%"=="0" (
  echo [%date% %time%] update finished OK >> "%LOG%"
) else (
  echo [%date% %time%] update FAILED with exit code %RC% >> "%LOG%"
)

endlocal & exit /b %RC%
