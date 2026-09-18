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

rem FDA shortage and discontinued lists. A failure here is logged but does not
rem fail the update: the previous list stays in place with its check date.
"%RSCRIPT%" R/05_availability.R >> "%LOG%" 2>&1
if errorlevel 1 echo [%date% %time%] shortage check FAILED - previous list kept >> "%LOG%"

rem Regulatory flags, FDA recalls and letters, and openFDA adverse events.
rem Each keeps its previous output if it fails.
"%RSCRIPT%" R/06_drug_flags.R >> "%LOG%" 2>&1
if errorlevel 1 echo [%date% %time%] drug flags FAILED >> "%LOG%"
"%RSCRIPT%" R/07_safety_alerts.R >> "%LOG%" 2>&1
if errorlevel 1 echo [%date% %time%] recall and letter check FAILED - previous kept >> "%LOG%"
"%RSCRIPT%" R/08_adverse_events.R >> "%LOG%" 2>&1
if errorlevel 1 echo [%date% %time%] adverse event refresh FAILED - previous kept >> "%LOG%"

if "%RC%"=="0" (
  echo [%date% %time%] update finished OK >> "%LOG%"
) else (
  echo [%date% %time%] update FAILED with exit code %RC% >> "%LOG%"
)

endlocal & exit /b %RC%
