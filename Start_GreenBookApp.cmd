@echo off
REM ---------------------------------------------------------------------------
REM Start_GreenBookApp.cmd -- double-click to run the app and open a browser
REM
REM Exists so the app can be demonstrated without typing R commands. It runs
REM the Shiny server on a FIXED port (7792) so the address is always the same
REM and can be written down in advance, and leaves this window open, because
REM closing it stops the app.
REM
REM The browser is opened by a short detached PowerShell command rather than by
REM a nested cmd loop: nested double quotes inside `cmd /c "..."` terminate the
REM outer string early, which silently broke an earlier version of this file.
REM ---------------------------------------------------------------------------

setlocal
set "PORT=7792"
set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"

title Green Book Drug Finder - keep this window open

cd /d "%~dp0"

if not exist "%RSCRIPT%" (
  echo.
  echo   Could not find R at:
  echo     %RSCRIPT%
  echo.
  echo   R may have been updated. Edit this file and correct the RSCRIPT path.
  echo.
  pause
  exit /b 1
)

if not exist "data\processed\search_index.rds" (
  echo.
  echo   The drug data has not been built yet. Run these once:
  echo     Rscript R\01_fetch_adafda.R
  echo     Rscript R\02_tidy_greenbook.R
  echo.
  pause
  exit /b 1
)

echo.
echo   Green Book Drug Finder
echo   ======================
echo.
echo   Starting up. Your browser will open automatically in a few seconds.
echo.
echo   Address:  http://127.0.0.1:%PORT%
echo.
echo   KEEP THIS WINDOW OPEN while you are using the app.
echo   Close it, or press Ctrl+C, to stop.
echo.

REM Open the browser shortly after the server has had time to bind the port.
start "" /b powershell -NoProfile -WindowStyle Hidden -Command "Start-Sleep -Seconds 12; Start-Process 'http://127.0.0.1:%PORT%'"

"%RSCRIPT%" -e "shiny::runApp('app', port=%PORT%, host='127.0.0.1', launch.browser=FALSE)"

echo.
echo   The app has stopped.
pause
endlocal
