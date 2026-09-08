@echo off
REM ---------------------------------------------------------------------------
REM Preview_StaticSite.cmd -- unzip the CI build and serve it locally
REM
REM The static site cannot be opened by double-clicking index.html. It runs R
REM in the browser through a service worker, and service workers are refused
REM on file:// URLs, so the page loads and then sits there. It has to be served
REM over http, which is what this does.
REM
REM Serving is done with R's httpuv rather than "python -m http.server",
REM because Python is not installed on this machine and R is.
REM
REM Usage: double-click, or drag the downloaded zip onto this file.
REM ---------------------------------------------------------------------------

setlocal
set "PORT=8080"
set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"
set "WORK=%~dp0build\preview"

REM Take the zip passed in, else the usual download location.
set "ZIP=%~1"
if "%ZIP%"=="" set "ZIP=%USERPROFILE%\Downloads\static-site.zip"

title Green Book static site preview - keep this window open

if not exist "%RSCRIPT%" (
  echo.
  echo   Could not find R at "%RSCRIPT%".
  echo   Edit this file and correct the RSCRIPT path.
  pause & exit /b 1
)

if not exist "%ZIP%" (
  echo.
  echo   Could not find the downloaded build:
  echo     %ZIP%
  echo.
  echo   Download it from the Actions run, under Artifacts ^> static-site,
  echo   then run this again - or drag the zip onto this file.
  pause & exit /b 1
)

echo.
echo   Unpacking %ZIP%
if exist "%WORK%" rmdir /s /q "%WORK%"
mkdir "%WORK%" 2>nul
powershell -NoProfile -Command "Expand-Archive -LiteralPath '%ZIP%' -DestinationPath '%WORK%' -Force"
if errorlevel 1 (
  echo   Could not unpack the zip. & pause & exit /b 1
)

REM The artifact may unpack with index.html at the top or one level down.
set "ROOT=%WORK%"
if not exist "%ROOT%\index.html" (
  for /d %%D in ("%WORK%\*") do if exist "%%D\index.html" set "ROOT=%%D"
)
if not exist "%ROOT%\index.html" (
  echo   No index.html found inside the zip. & pause & exit /b 1
)

echo.
echo   Green Book Drug Finder - static site preview
echo   ===========================================
echo.
echo   Serving: %ROOT%
echo   Address: http://127.0.0.1:%PORT%
echo.
echo   The browser opens by itself. The FIRST load downloads the R runtime
echo   and takes 30-60 seconds - that wait is what real visitors will see.
echo.
echo   KEEP THIS WINDOW OPEN. Close it, or press Ctrl+C, to stop.
echo.

start "" /b powershell -NoProfile -WindowStyle Hidden -Command "Start-Sleep -Seconds 4; Start-Process 'http://127.0.0.1:%PORT%'"

"%RSCRIPT%" -e "httpuv::runStaticServer('%ROOT:\=/%', port=%PORT%, browse=FALSE)"

echo.
echo   Preview stopped.
pause
endlocal
