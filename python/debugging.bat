rem Launch Pyscripter to run python interpreter in OSGeo4W environment
rem use me to step through helper scripts like run_qswatplus.py

@echo off
set PYSCRIPTER_PATH=C:\Program Files\PyScripter\PyScripter.exe
set OSGEO4W_ROOT=C:\Program Files\QGIS 3.32.0
call "%OSGEO4W_ROOT%\bin\o4w_env.bat"

rem next chunk is copied directly from the batch file "python-qgis.bat"
@echo off
path %OSGEO4W_ROOT%\apps\qgis\bin;%PATH%
set QGIS_PREFIX_PATH=%OSGEO4W_ROOT:\=/%/apps/qgis
set GDAL_FILENAME_IS_UTF8=YES
rem Set VSI cache to be used as buffer, see #6448
set VSI_CACHE=TRUE
set VSI_CACHE_SIZE=1000000
set QT_PLUGIN_PATH=%OSGEO4W_ROOT%\apps\qgis\qtplugins;%OSGEO4W_ROOT%\apps\qt5\plugins
set PYTHONPATH=%OSGEO4W_ROOT%\apps\qgis\python;%PYTHONPATH%

rem launch PyQGIS with Pyscripter...
start "QGIS" /B "%PYSCRIPTER_PATH%" --python39 --pythondllpath="%OSGEO4W_ROOT%\apps\Python39"

rem ...or launch Python in a shell
rem %OSGEO4W_ROOT%\apps\Python39\python %*
