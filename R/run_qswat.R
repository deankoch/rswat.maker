#' Run QSWAT+ with default settings to create a SWAT+ project for the catchment
#' 
#' This executes a minimal QSWAT+ workflow (delineation, HRUs, etc) using the input files
#' created by `save_qswat`. This is Windows-only and has some external dependencies (see Details).
#' Note that when `overwrite=TRUE`, any existing files in the 'qswat/project/' sub-directory of
#' `data_dir` will be deleted.
#' 
#' All output is written to the "qswat" directory of `data_dir`. A list of input paths and
#' parameters can be found in "qswatplus_input.json". This file parametrizes a Python script
#' that runs the QSWAT+ workflow: A project tree for QSWAT+ is written to sub-directory
#' `name`, then SWAT+ Editor is called to populate the "TxtInOut" directory with SWAT+
#' configuration files.
#' 
#' When setup is finished, a set of important output paths are stored in the file
#' "qswatplus_output.json". The function returns its contents in a list:
#' 
#' * dir: path to the project directory (`name`)
#' * sql: path to the project SQLite database file (needed by SWAT+ Editor)
#' * txt: path to the project "TxtInOut" directory (configuration files for SWAT+ simulator)
#' 
#' * channel: path to the project channels shape file 
#' * stream: path to the project streams shape file
#' * outlet: path to the project outlets shape file (after snapping)
#' * lake: path to the project lakes shape file 
#' * hru, hru_full: paths to the project HRUs shape files
#' * lsu, lsu_full: paths to the project LSUs shape files
#' 
#' * editor_exe: path to the SWAT+ Editor executable (needed for creating the files in "TxtInOut")
#' * simulator_dir: directory containing SWAT+ executable (runs the model defined in "TxtInOut")
#' 
#' When `do_check=TRUE`, the function appends '_test' to `name` before running QSWAT+.
#' The prompts QSWAT+ to run some internal consistency checks that are helpful for catching
#' delineation or snapping issues. Note that these checks happen whenever `name` contains
#' the string "test" (even if `do_check=FALSE`).
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
#' A system call to the SWAT+ Editor CLI produces plaintext SWAT+ config files in "TxtInOut". 
#'
#' @param data_dir character, path to the directory for input/output files
#' @param overwrite logical, whether to write the output or just return the file paths
#' @param name character, name of the sub-directory to use for project files
#' @param osgeo_dir character, path to the QGIS 3 installation directory (AKA "OSGEO4W_ROOT")
#' @param lake_threshold integer > 0, the percent overlap for a cell to become (part of) a lake
#' @param channel_threshold  numeric > 0, channel creation threshold as fraction of basin area 
#' @param stream_threshold numeric > 0, stream creation threshold as fraction of basin area
#' @param snap_threshold integer > 0, maximum distance (metres) to snap outlets to flow lines
#' @param do_check logical, if `TRUE` the function appends '_test' to `name` (see details) 
#'
#' @return list of paths related to the created QSWAT+ project
#' @export
run_qswat = function(data_dir,
                     overwrite = FALSE,
                     name = basename(data_dir),
                     osgeo_dir = NULL,
                     lake_threshold = 50L,
                     channel_threshold = 1e-3,
                     stream_threshold = 1e-2,
                     snap_threshold = 300L,
                     do_check = TRUE,
                     quiet = FALSE) {
  
  # location of the batch file that runs Python3
  batch_name = 'run_qswatplus.bat'
  batch_dir = path.package('rswat.uyr') |> file.path('python')
  
  # it would be nice to be able to discover this automatically somehow...
  if( is.null(osgeo_dir) ) osgeo_dir = 'C:/Program Files/QGIS 3.32.0'
  
  # expected paths of input files
  input_path = save_qswat(data_dir)
  dest_dir = input_path[['outlet']] |> dirname()
  msg_help = '\nHave you run `save_qswat` on this `data_dir` yet?'
  if( !dir.exists(dest_dir) ) stop('destination directory not found: ', dest_dir, msg_help)
  
  # output filenames (project directory must be listed first)
  out_nm = c(qswat=paste0(name, ifelse(do_check, '_test', '')),
             input='qswat_input.json', 
             output='qswat_output.json',
             log='qswat_log.txt')
  
  # output paths to overwrite
  dest_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # remove any existing output files (project directory is removed later by python script)
  is_over = file.exists(dest_path[-1])
  if( any(is_over) ) unlink(dest_path[-1][is_over])
  
  # number of pixels in the DEM
  n_pixel = input_path[['dem']] |> terra::rast() |> terra::ncell()

  # list of information needed by QSWAT+
  json_list = list(info = paste('configuration file created by rswat on', Sys.Date()),
                   name = out_nm['qswat'],
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
  
  # expected path of project file (will be created at the beginning of setup)
  qgs_path = dest_dir |> file.path(json_list[['name']], json_list[['name']]) |> paste0('.qgs')
  
  # build shell command to change directory then execute QSWAT+ setup
  message('running QSWAT+ to create project: ', out_nm['qswat'])
  cd_string = paste('pushd', paste0('"', normalizePath(batch_dir), '" &&'))
  call_string = paste0(batch_name, ' "', dest_path[['input']], '" "', osgeo_dir, '"')
  shell_result = paste(cd_string, call_string) |> 
    shell(intern=TRUE, mustWork=NA) |>
    suppressWarnings()
  
  # write stdout from setup, captured by shell(), to log file
  writeLines(shell_result, dest_path['log'])

  # report any lines beginning with ERROR (errors reported to QgsMessageLog)
  is_error = any(grepl('^Traceback', shell_result))
  if( any(is_error) ) {
    
    info_1 = 'Check the log for errors and try opening the project in QGIS:'
    info_2 = paste('\nlog file:', dest_path['log'])
    info_3 = paste('\nQGIS project file:', qgs_path)
    msg_warn = paste('There was a problem running QSWAT+ setup.', info_1, info_2, info_3)
    if(do_check) stop(msg_warn)
    warning(msg_warn)
  }

  # read the output JSON (paths)
  qswat_output = NULL
  if( file.exists(dest_path[['output']]) ) {
    
    qswat_output = readLines(dest_path[['output']]) |>
      jsonlite::fromJSON() |> 
      unlist()
  }
  
  return(qswat_output)
}

#' Create a set of dummy weather files positioned at sub-basin centroids 
#'
#' This writes empty weather files for the specified date range. All data values are
#' set to -99. If you have weather generators set up for the project, this will cause
#' SWAT+ to generate (ie randomly draw) weather for the period `from` - `to` during
#' simulations.
#' 
#' A weather station is created at the centroid of each sub-basin from the QSWAT+ project
#' in `data_dir` (or at `pts`, if supplied), and a corresponding data file containing the
#' date range `from` - `to` in daily steps is written to  the "weather" sub-directory of
#' the project root.
#' 
#' If either `from` or `to` is missing, the function sets a 7 day period to start/end at
#' the supplied date. If both are missing, `to` is assigned `Sys.Date()`.
#' 
#' `pts` is used internally to avoid opening the sub-basin shape file many times in a loop.
#' We use it to pass the sub-basin centroids, but any set of points can be supplied
#' (overriding what's in `data_dir`) as long as they have a coherent 'Elevation' (m),
#' and a (1-indexed) 'Subbasin' field mapping to the project's sub-basins keys. 
#'
#' @param data_dir character, path to the directory for input/output files
#' @param overwrite logical, whether to write the output or just return the file paths
#' @param var_nm character vector, variable names to write 
#' @param to Date, final date in the daily time series
#' @param from Date, initial date in the daily time series
#' @param pts sf data frame, with points geometry and 'Subbasin' key (integer column)
#'
#' @return vector of file paths
#' @export
make_weather = function(data_dir, overwrite=FALSE, var_nm=NULL, to=NULL, from=NULL, pts=NULL) {
  
  # number of digits to write after decimal place
  n_digit = 3L
  
  # value to write in place of NAs
  na_value = -99L
  
  # the sub-directory name to use in the project directory
  weather_nm = 'weather_template'
  
  # check that the expected QSWAT output file is there and read it
  config_path = run_qswat(data_dir)['output']
  msg_help = '\nHave you called `run_swat` yet on this `data_dir`?'
  if( !file.exists(config_path) ) stop('file not found: ', config_path, msg_help)
  qswat_path = config_path |> readLines() |> jsonlite::fromJSON()
  
  # set default variable names
  var_nm_default = c('pcp', 'tmp', 'rh', 'wind', 'solar')
  if( is.null(var_nm) ) var_nm = var_nm_default
  
  # validity check
  var_nm = var_nm[ var_nm %in% var_nm_default ]
  msg_nm = paste(var_nm_default, collapse=', ')
  if( length(var_nm) == 0 ) stop('Valid options for var_nm are ', msg_nm)
  
  # set default dates
  if( length(c(to, from)) == 0 ) to = Sys.Date()
  if( length(from) == 0 ) from = to - 7L
  if( length(to) == 0 ) to = from + 7L
  
  # validity check
  if( !all( sapply(list(to, from), \(x) is(x, 'Date')) ) ) stop('to/from must be Date class objects')
  if( from > to ) stop('from cannot be later than to!')
  
  # open the sub-basins and extract centroids 
  if( is.null(pts) ) {
    
    # key 0 means the sub-basin was discarded during delineation
    subs = qswat_path[['sub']] |> sf::st_read(quiet=TRUE) |> dplyr::filter(Subbasin > 0)
    pts_geometry = subs['Subbasin'] |> sf::st_geometry() |> sf::st_centroid()
    pts = sf::st_sf(subs['Subbasin'], geometry=pts_geometry)
    
    # get elevations from DEM
    dem = terra::rast(save_qswat(data_dir)['dem'])
    pts[['Elevation']] = terra::extract(dem, pts)[[names(dem)[1]]]
    pts = sf::st_sf(pts)
  }
  
  # loop for vectorized calls (set pts to avoid st_read every time)
  names(var_nm) = var_nm
  if( length(var_nm) > 1 ) {
    
    path_out = lapply(var_nm, \(nm) make_weather(data_dir,
                                                 overwrite = overwrite, 
                                                 var_nm = nm, 
                                                 to = to, 
                                                 from = from,
                                                 pts = pts) )
                                      
    return(invisible(path_out))
  }

  # define the paths to write
  n_pts = nrow(pts)
  weather_dir = run_qswat(data_dir)[['qswat']] |> file.path(weather_nm)
  station_file = paste0(var_nm, '.txt')
  data_file = paste0(var_nm, seq(n_pts), '.txt')
  path_out = list(station = file.path(weather_dir, station_file), 
                  data = file.path(weather_dir, data_file))  
  
  if(!overwrite) return(path_out)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(weather_dir) ) dir.create(weather_dir, recursive=TRUE)
  lapply(path_out, \(p) {
    
    is_over = file.exists(p)
    if( any(is_over) ) unlink(p[is_over])
  })

  # stations data frame
  longlat = pts |> sf::st_transform(4326) |> sf::st_coordinates() |> as.data.frame()
  station_df = data.frame(ID = pts[['Subbasin']],
                          NAME = tools::file_path_sans_ext(data_file),
                          LAT = longlat[['Y']],
                          LONG = longlat[['X']],
                          ELEVATION = round(pts[['Elevation']]))
  
  # convert to text and write to disk
  station_df |> write.csv(path_out[['station']], row.names=FALSE, quote=FALSE)

  # weather data text line by line
  n_data = as.integer(to-from) + 1L
  data_string = round(na_value, n_digit) |> as.character()
  if(var_nm == 'tmp') data_string = paste(rep(data_string, 2), collapse=',')
  data_txt = paste0('\n', data_string) |> rep(n_data) |> paste(collapse='')
  
  # concatenate and write to files
  weather_txt = paste0(format(from, '%Y%m%d'), data_txt)
  for(i in seq(n_pts) ) weather_txt |> writeLines(path_out[['data']][i])
  
  return( invisible(path_out))
}

#' Run SWAT+ Editor to create SWAT+ simulator config files in "TxtInOut" 
#'
#' @return vector of file paths
#' @export
run_editor = function(data_dir, overwrite=FALSE) {

  # check that the expected QSWAT output file is there and read it
  config_path = run_qswat(data_dir)['output']
  msg_help = '\nHave you called `run_swat` yet on this `data_dir`?'
  if( !file.exists(config_path) ) stop('file not found: ', config_path, msg_help)
  qswat_path = config_path |> readLines() |> jsonlite::fromJSON()
  
  # check that weather files are in expected location
  weather_path = unlist( make_weather(data_dir) )
  is_weather_valid = weather_path |> file.exists()
  n_miss = sum(!is_weather_valid)
  if( n_miss > 0 ) {
    
    msg_help = '\nHave you called `make_weather` yet on this `data_dir`?'
    msg_miss = weather_path[!is_weather_valid] |> head(1)
    if( n_miss > 1 ) msg_miss = msg_miss |> paste('and', n_miss-1, 'other(s)')
    stop('weather file(s) not found:\n', msg_miss, msg_help)
  }
  
  # output filenames
  out_nm = c(txt=qswat_path[['txt']],
             log='editor_log.txt')
  
  # output paths to overwrite
  dest_path = dirname(config_path) |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # remove any existing log
  if( file.exists(dest_path['log']) ) unlink(dest_path['log'])
  
  # CLI arguments for SWAT+ Editor
  import_format = 'old'
  import_wgn = 'database'
  db_path =  qswat_path[['sql']]
  editor_path = qswat_path[['editor_exe']]
  editor_dir = dirname(editor_path)
  editor_file = basename(editor_path)
  weather_dir = weather_path |> head(1) |> dirname()
  

  # helper function formats directory strings for NT shell
  dir2shell = \(d) normalizePath(d) |> dQuote(q=FALSE)
  cli_args = c('--cmd-only',
               '--weather-dir' |> paste(dir2shell(weather_dir)),
               '--weather-import-format' |> paste(import_format),
               '--wgn-import-method' |> paste(import_wgn))
  
  # build shell command to change directory
  message('running SWAT+ Editor')
  # qswat_path['txt']
  cd_string = paste('pushd', dir2shell(editor_dir), '&&')
  call_string = paste(cd_string, 
                      editor_file, 
                      dir2shell(db_path), 
                      paste(cli_args, collapse=' '))
  
  # collect error messages for tidier output
  shell_result = paste(cd_string, call_string) |> 
    shell(intern=TRUE) |> 
    suppressWarnings()
  
  # write log file to disk
  shell_result |> writeLines(dest_path['log'])
    
  # report any problems relayed by shell()
  if( any(grepl('^(Traceback)', shell_result)) ) {
    
    info_1 = 'Check the log for errors:'
    info_2 = ifelse(log_exists, paste('\nlog file:', dest_path['log']), '')
    msg_warn = paste('There was a problem running SWAT+ Editor setup.', info_1, info_2)
    if(do_check) stop(msg_warn)
    warning(msg_warn)
  }

  return( invisible(dest_path) )
}



