@echo off
rem Runs the whole pipeline, logging each script. "run_pipeline.cmd smoke" runs the two-species smoke test.
cd /d %~dp0
if /i "%1"=="smoke" (set RW_SMOKE=1) else (set RW_SMOKE=0)
set RSCRIPT=D:\R_Studio\R-4.6.1\bin\Rscript.exe
for %%s in (01_species 02_download 03_predictors 04_thin 05_brt 06_arha 07_web 08_render 09_build_wallpaper) do (
  echo [%date% %time%] %%s
  "%RSCRIPT%" R\%%s.R > logs\%%s.log 2>&1
  if errorlevel 1 (echo FAILED at %%s, see logs\%%s.log & exit /b 1)
)
echo [%date% %time%] done
