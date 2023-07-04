@echo off
rem This launches QGIS 3.32 (without GUI) to run the QSWAT+ plugin with default settings
rem usage: run_qswatplus.bat [path to config JSON file] [path to QGIS install directory]

rem all output is written to the parent directory of the first argument 
rem on my workstation, second argument is "C:\Program Files\QGIS 3.32.0"

set RSWAT_JSON=%1
set OSGEO4W_ROOT=%~2
rem tilde removed the double quotes
call "%OSGEO4W_ROOT%\bin\o4w_env.bat"

rem next chunk is copied directly from the batch file %OSGEO4W_ROOT%\bin\python-qgis.bat
@echo off
path %OSGEO4W_ROOT%\apps\qgis\bin;%PATH%
set QGIS_PREFIX_PATH=%OSGEO4W_ROOT:\=/%/apps/qgis
set GDAL_FILENAME_IS_UTF8=YES
rem Set VSI cache to be used as buffer, see #6448
set VSI_CACHE=TRUE
set VSI_CACHE_SIZE=1000000
set QT_PLUGIN_PATH=%OSGEO4W_ROOT%\apps\qgis\qtplugins;%OSGEO4W_ROOT%\apps\qt5\plugins
set PYTHONPATH=%OSGEO4W_ROOT%\apps\qgis\python;%PYTHONPATH%

rem -u flag for unbuffered output stdout
%OSGEO4W_ROOT%\apps\Python39\python -u run_qswatplus.py %RSWAT_JSON%