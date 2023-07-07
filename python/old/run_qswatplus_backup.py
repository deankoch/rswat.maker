# Generate a SWAT+ project from reference database and geometry inputs.
# Based on: SWAT+AW v.1.0.4 (retrieved 09/09/2020).
# Updated: July, 2023
# Reference: https://celray.github.io/docs/swatplus_aw/introduction.html
#
# Command line argument 1 is the absolute path to the config JSON file
# for the project. This is a flat list containing at least:
#
# name              value
# ---------------------------------------------------------------------------
# name              name for the project
# dem               path to the DEM raster (GeoTIFF, in metres)
# outlet            path to ESRI shapefile (.shp) of inlets/outlets to define
# soil              path to the soil mukeys raster (GeoTIFF, integers)
# landuse           path to the land use raster (GeoTIFF, integer)
# landuse_lookup    path to CSV with SWAT+ codes for the land use values
# lake_threshold    integer minimum percent cell coverage to qualify as lake
# ---------------------------------------------------------------------------

import sys
import os.path
import subprocess
import json
import shutil                   # delete nonempty directory
import re                       # regular expressions
from osgeo import gdal          # spatial libraries
from pathlib import Path        # Windows path handler


'''---------- initialize QGIS and import QSWAT+ dependencies  -----------'''

# first argument is the config file path and parent directory for project tree
cfg_path = Path('D:/rswat_data/yellowstone/split/lava_creek/qswat/qswat_input.json')
#cfg_path = Path(sys.argv[1])
print('creating new QSWAT project in ' + str(cfg_path.parent))

# I have been unable to get the 'Processing' module loaded properly without
# first doing initQgis(). We also find the location of the QSWATPlus3_64
# installation directory using QtCore.QStandardPaths, so this must be loaded
# before the import calls that follow.

# PyQGIS modules
print('\n>> loading PyQGIS and QSWATPlus')
from qgis.core import QgsApplication, QgsProject
from PyQt5.QtCore import QSettings, QFileInfo, QStandardPaths
#from qgis.gui import QgsMapCanvas

# initialize the QGIS environment object (second arg suppresses GUI)
qgs = QgsApplication([], False)
qgs.initQgis()

# add QGIS processing module to path, import and initialize
QGIS_plugins_path = Path(qgs.prefixPath()) / 'python/plugins'
sys.path.append(str(QGIS_plugins_path))
from processing.core.Processing import Processing
Processing.initialize()

# QSWAT+ gets installed to a platform dependent, user-specific location
user_path = QStandardPaths.writableLocation(QStandardPaths.AppDataLocation)

# add this to path then import what is needed from QSWAT+
QSWAT_user_relpath = 'QGIS/QGIS3/profiles/default/python/plugins/'
sys.path.append(str(Path(os.path.dirname(user_path)) / QSWAT_user_relpath))
from QSWATPlus3_9.QSWATPlus.QSWATPlusMain import QSWATPlus, Parameters
from QSWATPlus3_9.QSWATPlus.delineation import Delineation
from QSWATPlus3_9.QSWATPlus.hrus import HRUs
from QSWATPlus3_9.QSWATPlus.QSWATUtils import QSWATUtils, FileTypes

# uncomment to disable MS MPI parallel processing
# QSettings().setValue('/QSWATPlus/NumProcesses', 0)

'''---------- project configuration and directories  --------------'''

# read in the JSON config file
with open(cfg_path) as f:
    cfg = json.load(f)

# output file, a list of paths related to the project
json_path = cfg_path.parent / str('qswat_output.json')

# input files, in the order we need them in this workflow
dem_src = cfg['dem'][0]
outlets_src = cfg['outlet'][0]
landlu_src = cfg['landuse_lookup'][0]
land_src = cfg['landuse'][0]
soils_src = cfg['soil'][0]

# input parameters
project_name = cfg['name'][0]
channel_threshold = cfg['channel_threshold'][0]
stream_threshold = cfg['stream_threshold'][0]
lake_threshold = cfg['lake_threshold'][0]
snap_threshold = cfg['snap_threshold'][0]

# set project name according to the JSON
project_path = cfg_path.parent / str(project_name)
print('\nsetting QSWAT project directory to ' + str(project_path))

# remove existing project directory and make new empty directory
shutil.rmtree(project_path, ignore_errors=True)
project_path.mkdir(exist_ok=True)


'''----------------- create project file  ------------------------'''

# initialize the QSWAT3 plugin object by passing QGIS environment object
plugin = QSWATPlus(qgs)

# define and initialize the QGIS project file in the new project folder
proj_file = str(project_path / project_name) + '.qgs'
proj = QgsProject.instance()
proj.setFileName(proj_file)
proj.write()

# set up a new QSWAT3 project, setting parameters and creating directories
plugin.setupProject(proj, True)


'''----------------- delineation pt1: DEM file  -----------------'''

# initialize watershed delineation object
print('\n>> starting delineation\n')
delin = Delineation(plugin._gv, False)
delin.init()

# set global variable in QSWAT and "push" the ok button
print('copying DEM from ' + str(dem_src))
delin._gv.demFile = str(dem_src)
delin._dlg.selectDem.setText(str(dem_src))
delin.btnSetDEM()


'''----------- delineation pt2: inlets and outlets  --------------'''

# copy inlets/outlets shapefile (and its babies) the same way
print('copying outlets from ' + str(outlets_src))
delin._gv.outletFile = str(outlets_src)
delin._dlg.selectOutlets.setText(str(outlets_src))
delin.btnSetOutlets()

'''--------------- delineation pt3: run TauDEM  ----------------'''

# set threshold parameters
delin._dlg.numCellsCh.setText(str(int(channel_threshold)))
delin._dlg.numCellsSt.setText(str(int(stream_threshold)))
delin._dlg.snapThreshold.setText(str(int(snap_threshold)))

# monkey-patch the iface method to avoid errors with setActiveLayer()
delin._gv.iface.setActiveLayer = lambda *args: True

# makes stream reach network and subbasins shapefile, and other stuff
print('\n>> running TauDEM\n')
delin.runTauDEM2()

print('\nfinishing delineation')
delin.finishDelineation()


'''------------ HRUs pt 1: assign input layers  --------------'''

# initialize HRUs object
hrus = HRUs(plugin._gv, plugin._odlg.reportsBox)
hrus.init()

# parameters for workflow (use SSURGO, set lake threshold, make HRUs file)
hrus._dlg.SSURGOButton.setChecked(True)
hrus._dlg.reservoirThreshold.setValue(int(lake_threshold))
hrus._dlg.generateFullHRUs.setChecked(True)

print('\n>> processing soil and land-use data\n')

# set raster paths in QSWAT
hrus.landuseFile = str(land_src)
hrus.soilFile = str(soils_src)
hrus._dlg.selectLanduse.setText(str(land_src))
hrus._dlg.selectSoil.setText(str(soils_src))

# copy to rasters directory
print('copying land use from ' + str(land_src))
print('copying soils from ' + str(soils_src))
hrus.getLanduseFile()
hrus.getSoilFile()

# open land-use lookup table and add it to database
names = hrus._db.landuseTableNames
landlu_table = hrus._db.readCsvFile(str(landlu_src), 'landuse', names)

# select the table we just loaded
hrus._dlg.selectLanduseTable.setCurrentIndex(names.index(landlu_table))
hrus.landuseTable = landlu_table
hrus.setToReadFromMaps()

# anotherpatch for an attempted palette assignment below
#FileTypes.colourLanduses = lambda *args: True

# patch iface method again to avoid errors on pushMessage() calls:
class dummyClass:
    def __init__(self, dummyFun=lambda msg, level, duration: None):
        self.pushMessage = dummyFun
hrus._iface.messageBar = dummyClass

# import and process land-use and soils rasters
hrus.readFiles()

# make subbasin and watershed shapefiles
print('\n>> calculating HRUs\n')
hrus.calcHRUs()

hrus._gv

# save results so far
hrus._dlg.close()
proj.write()



##
##
### save and reload project (fixes a database issue?)
##plugin._gv.db.conn.close()
##plugin._gv.db.connectToProjectDatabase()
##proj.write()
##
###proj.read(proj_file)
##proj.write()


#plugin._gv.db.conn.close()

# copy some important paths
json_data = {

    # derived from TauDEM analysis
    'channel':Path(plugin._gv.channelFile),
    'stream':Path(plugin._gv.streamFile),
    'sub':Path(plugin._gv.subbasinsFile),

    # derived from HRUs analysis
    'outlet':Path(plugin._gv.snapFile),
    'lake':Path(plugin._gv.lakeFile),
    'hru':Path(plugin._gv.actHRUsFile),
    'lsu':Path(plugin._gv.actLSUsFile),
    'hru_full':Path(plugin._gv.fullHRUsFile),
    'lsu_full':Path(plugin._gv.fullLSUsFile),

    # database file for the project passed to editor executable
    'sql':Path(plugin._gv.db.dbFile),

    # SWAT+ simulator files
    'txt':Path(plugin._gv.resultsDir).parent / str('TxtInOut'),

    # we use the editor CLI later on to populate TxtInOut
    'editor_exe':Path(plugin._gv.findSWATPlusEditor())
    #TODO: find the SWAT+ simulator executable
}

# coerce to strings with forward slashes for readability
for key in json_data.keys():
    json_data[key] = str(json_data[key]).replace(os.sep, '/')

# write to JSON
print('\nwriting ' + str(json_path))
with open(str(json_path), 'w') as outfile:
    json.dump(json_data, outfile, indent=1)
    outfile.write('\n')

# finished with QSWAT+ and PyQGIS
print('\n>> finished running QSWAT+')

plugin._odlg.accept()
#plugin._dlg.close()
proj.write()
plugin.finish()

##plugin = QSWATPlus(qgs)
##plugin._gv.db.connectToProjectDatabase()

qgs.exitQgis()

##proj = plugin.existingProject(proj_file)
##plugin.setupProject(proj, False)
##proj.write()
##plugin.finish()

sys.exit(0)
