#' Run QSWAT+ with default settings to create a SWAT+ project for the catchment
#' 
#' This executes a minimal QSWAT+ workflow (delineation, HRUs, etc) using the input files
#' created by `save_qswat`. This is Windows-only and has some external dependencies
#' - see below. Note that when `overwrite=TRUE`,  any existing files in the 'qswat/project/'
#' sub-directory of `data_dir` will be deleted.
#' 
#' All output is written to the sub-directory "qswat": QSWAT+/SWAT+ files go in the
#' "project" directory tree, and a set of essential input and output paths for the project
#' are written to "qswatplus_input.json" and "qswatplus_output.json". The function returns
#' the contents of "qswatplus_output.json" as an R list:
#' 
#' * exe: path to the SWAT+ Editor executable (needed for creating the files in "TxtInOut")
#' * sql: path to the project SQLite database file
#' * sub: path to the project sub-basins shape file (relevant features have `Subbasin != 0`)
#' * txt: path to the project "TxtInOut" directory (configuration files for SWAT+ simulator)
#' 
#' Before calling this function you will need to install QGIS 3.32.0 and the latest SWAT+
#' bundle, including QSWAT+ and SWAT+ Editor. You will also need to set `osgeo_dir` to point
#' to your QGIS installation directory. By default this is set to 'C:/Program Files/QGIS 3.32.0',
#' which should be the correct path for many (but not all) Windows users. Unfortunately I don't
#' know an easy way to discover this path automatically from within R.
#' 
#' The function calls a Windows batch file that launches Python3 for QGIS3 and runs the
#' script 'run_qswatplus.py'. This script uses the QSWAT+ plugin to complete the essential
#' SWAT+ model creation steps, including: delineation with TauDEM, loading soils and land use
#' to define HRUs using "Dominant HRU" method, and creating a database to link everything.
#' A second function call to SWAT+ Editor produces plaintext SWAT+ config files (in "TxtInOut") 
#'
#' @param data_dir character, path to the directory for input/output files
#' @param overwrite logical, whether to write the output or just return the file paths
#' @param osgeo_dir character, path to the QGIS 3 installation directory (AKA "OSGEO4W_ROOT")
#' @param lake_threshold integer > 0, the percent overlap for a cell to become (part of) a lake
#' @param channel_threshold  numeric > 0, channel creation threshold as fraction of basin area 
#' @param stream_threshold numeric > 0, stream creation threshold as fraction of basin area
#' @param snap_threshold integer > 0, maximum distance (metres) to snap outlets to flow lines
#'
#' @return list of paths related to the created QSWAT+ project
#' @export
run_qswat = function(data_dir,
                     overwrite = FALSE,
                     osgeo_dir = NULL,
                     lake_threshold = 50L,
                     channel_threshold = 1e-3,
                     stream_threshold = 1e-2,
                     snap_threshold = 300L) {
  
  # location of the batch file that runs Python3
  batch_name = 'run_qswatplus.bat'
  batch_dir = path.package('rswat.uyr') |> file.path('python')
  
  # it would be nice to be able to discover this automatically somehow...
  if( is.null(osgeo_dir) ) osgeo_dir = 'C:/Program Files/QGIS 3.32.0'
  
  # expected paths of input files
  input_path = qswat_path = save_qswat(data_dir)
  dest_dir = input_path |> head(1) |> dirname()
  msg_help = '\nHave you run `save_qswat` on this `data_dir` yet?'
  if( !dir.exists(dest_dir) ) stop('destination directory not found: ', dest_dir, msg_help)
  
  # output filenames (project directory must be listed first)
  out_nm = c(qswat='project',
             input='qswat_input.json', 
             output='qswat_output.json', 
             subbasin='subs.geojson')
  
  # output paths to (over)write
  dest_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # remove any existing output files ("project" directory is removed later by python script)
  is_over = file.exists(dest_path[-1])
  if( any(is_over) ) unlink(dest_path[-1][is_over])
  
  # number of pixels in the DEM
  n_pixel = input_path[['dem']] |> terra::rast() |> terra::ncell()

  # list of information needed by QSWAT+
  json_list = list(info = paste('configuration file created by rswat on', Sys.Date()),
                   name = basename(data_dir),
                   dem = input_path[['dem']],
                   outlet = input_path[['outlet']],
                   landuse_lookup = input_path[['land_lookup']],
                   landuse = input_path[['land']],
                   soil = input_path[['soil']],
                   lake_threshold = lake_threshold,
                   channel_threshold = ceiling(channel_threshold * n_pixel),
                   stream_threshold = ceiling(stream_threshold * n_pixel),
                   snap_threshold = snap_threshold)  
  
  # write to disk as JSON
  json_list |> jsonlite::toJSON(pretty=TRUE) |> write(dest_path[['input']])
  
  # build shell command to change directory then execute QSWAT+ setup
  cd_string = paste('pushd', paste0('"', normalizePath(batch_dir), '" &&'))
  call_string = paste0(batch_name, ' "', dest_path[['input']], '" "', osgeo_dir, '"')
  paste(cd_string, call_string) |> shell()
  
  # read the output JSON (paths)
  qswat_output = readLines(dest_path[['output']]) |> jsonlite::fromJSON()
  return(qswat_output)
}