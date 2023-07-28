#' Run SWAT+ Editor to create SWAT+ simulator config files in "TxtInOut" 
#' 
#' This uses `shell` to call the SWAT+ Editor executable to construct the model.
#' This creates the plaintext files in "TxtInOut" that parametrize the SWAT+ simulator.
#' With default `overwrite=FALSE` the function returns the file paths that would be
#' modified, but does not run the workflow.
#' 
#' When `overwrite=TRUE` the function runs the workflow and returns the paths to the
#' log file with standard output from shell, and to the "TxtInOut" directory. 
#' 
#' This uses the command line interface for SWAT+ Editor to import weather files using
#' the "old" SWAT2012 format (at present it is better documented than the new format).
#' It also uses the "database" weather generator import option, which requires users
#' to have installed the optional databases bundled with SWAT+ (enabled by default
#' when doing a full install). 
#' 
#' Users with their own SWAT2012 weather files import them by setting `weather_dir`
#' to point to their parent directory. If `weather_dir=NULL` the function creates a
#' set of stations located at the sub-basin centroids and sets their values to the
#' missing data flag. Without further modification, these files will have no effect
#' on your simulation (SWAT+ will just generate simulated weather as thought you
#' had no observed weather stations).
#' 
#' @param data_dir character, path to the project (sub)directory for the (sub)catchment
#' @param weather_dir character, optional path to user-supplied weather files
#' @param overwrite logical, whether to write the output or just return the file paths
#'
#' @return vector of file paths
#' @export
#' 
#' @examples
#' # Error if called without having called `run_swat` first
#' # run_editor('')
run_editor = function(data_dir, weather_dir=NULL, overwrite=FALSE) {
  
  # check that the expected QSWAT output file is there and read it
  config_path = run_qswat(data_dir)['output']
  msg_help = '\nHave you called `run_swat` yet on this `data_dir`?'
  if( !file.exists(config_path) ) stop('file not found: ', config_path, msg_help)
  qswat_path = config_path |> readLines() |> jsonlite::fromJSON()
  
  # add default weather stations at sub-basin centroids
  if( is.null(weather_dir) ) {
    
    # makes dummy weather files and returns paths in list
    weather_path = make_weather(data_dir, overwrite=TRUE) |> unlist()
    weather_dir = weather_path |> head(1) |> dirname()
  }

  # check that weather directory is valid
  if( !dir.exists(weather_dir) ) stop('weather directory not found: ', weather_dir)

  # output paths to overwrite
  dest_path = c(txt=qswat_path[['txt']], log=file.path(dirname(config_path), 'editor_log.txt'))
  if( !overwrite ) return(dest_path)
  
  # remove any existing log
  if( file.exists(dest_path['log']) ) unlink(dest_path['log'])
  
  # CLI arguments for SWAT+ Editor
  dir2shell = \(d) normalizePath(d) |> dQuote(q=FALSE)
  import_format = 'old'
  import_wgn = 'database'
  db_path =  qswat_path[['sql']]
  editor_path = qswat_path[['editor_exe']]
  editor_dir = dirname(editor_path)
  editor_file = basename(editor_path)
  weather_dir = dir2shell(weather_dir)
  
  # helper function formats directory strings for NT shell
  cli_args = c('--cmd-only',
               '--weather-dir' |> paste(weather_dir),
               '--weather-import-format' |> paste(import_format),
               '--wgn-import-method' |> paste(import_wgn))
  
  # shorten path by leaving out data_dir
  qswat_txt_str = qswat_path['txt'] |> substr(1+nchar(data_dir), nchar(qswat_path['txt']))
  message('running SWAT+ Editor to populate ', qswat_txt_str)
  
  # build shell command to change directory
  cd_string = paste('pushd', dir2shell(editor_dir), '&&')
  call_string = paste(cd_string, 
                      editor_file, 
                      dir2shell(db_path), 
                      paste(cli_args, collapse=' '))
  
  # collect error messages for tidier output and write log file to disk
  shell_result = paste(cd_string, call_string) |> shell(intern=TRUE) |> suppressWarnings()
  shell_result |> writeLines(dest_path['log'])
  
  # report any problems relayed by shell()
  if( any(grepl('^(Traceback)', shell_result)) ) {
    
    info_1 = 'Check the log for errors:'
    info_2 = ifelse(log_exists, paste('\nlog file:', dest_path['log']), '')
    msg_warn = paste('There was a problem running SWAT+ Editor setup.', info_1, info_2)
    if(do_test) stop(msg_warn)
    warning(msg_warn)
    return(NULL)
  }
  
  return( invisible(dest_path) )
}


#' Create a set of dummy weather files positioned at sub-basin centroids 
#'
#' This writes empty weather files for the specified date range. All data values are
#' set to -99. If you have weather generators set up for the project, this will cause
#' SWAT+ to generate weather (ie randomly draw values from empirical climatic distributions)
#' for the period `from` - `to` during simulations.
#' 
#' A weather station is created at the centroid of each sub-basin from the QSWAT+ project
#' in `data_dir` (or at `pts`, if supplied), and a corresponding data file containing the
#' date range `from` - `to` in daily steps is written to  the "weather" sub-directory of
#' the project root.
#' 
#' If either `from` or `to` is missing, the function sets a one year period to start/end
#' at the supplied date. If both are missing, `to` is assigned `Sys.Date()`.
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
#' 
#' @examples
#' # Error if called without having called `run_swat` first
#' # make_weather('')
make_weather = function(data_dir, overwrite=FALSE, var_nm=NULL, to=NULL, from=NULL, pts=NULL) {
  
  # default time period length (days)
  n_default = 365L
  
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
  if( length(from) == 0 ) from = to - n_default
  if( length(to) == 0 ) to = from + n_default
  
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
