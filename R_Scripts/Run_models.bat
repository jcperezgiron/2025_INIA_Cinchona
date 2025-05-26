@echo off
setlocal enabledelayedexpansion


:: Ruta del ejecutable Rscript y scripts
set "rutaR=C:/Program Files/R/R-4.5.0/bin"
set "script=C:/SCIENCE/2025_INIA_Cinchona/R_scripts/SDMs.R"
set "output=C:/SCIENCE/2025_INIA_Cinchona/SDM_output"

:: Mostrar el comando que se ejecutará (para depuración)
echo Ejecutando: cd /d "%rutaR%" & Rscript.exe "%script%" > "%output%/Cinchona.log" 2>&1

:: Ejecutar el comando en un nuevo proceso
start "" cmd /c "cd /d "%rutaR%" & Rscript.exe "%script%" > "%output%/Cinchona.log" 2>&1"