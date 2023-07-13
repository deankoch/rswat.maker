#' Run QSWAT+ with default settings to create a SWAT+ project for the catchment
#' 
#' This executes a minimal QSWAT+ workflow (delineation, HRUs, etc) using the input files
#' created by `save_qswat`. This is Windows-only and has some external dependencies (see Details).
#' Note that when `overwrite=TRUE`, the function first deletes all existing files in the output
#' QGIS project sub-directory `name`. 
#' 
#' Output is written to the "qswat" directory of `data_dir`. A list of input paths and
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
#' The function uses `shell(intern=TRUE)` run QSWAT+ and captures stdout in the file
#' "qswatplus_log.txt". Errors are detected by scanning this log file after the `shell` call,
#' and communicated to the user as warnings by default (or as errors, when `do_test=TRUE`).
#' 
#' After QSWAT+ setup is finished the function runs `check_qswat` to identify delineation
#' issues and attempts to fix them automatically by re-positioning outlets and calling
#' `run_qswat` again. This is repeated until the check is passed or `nudge_nmax` iterations
#' is reached (set to `0` to disable checking).
#' 
#' The re-positioned outlet shape files from all iterations are stored in the sub-directory
#' "qswat/nudge_outlet" of `data_dir` and the "qswatplus_input.json" file is always updated
#' to point to the one currently in use. The default `nudge_clear=TRUE` deletes this directory
#' at the beginning of the function call to avoid a build-up of old garbage.
#' 
#' When `do_test=TRUE`, the function also appends '_test' to `name` before running QSWAT+.
#' The prompts QSWAT+ to run some internal consistency checks that are helpful for catching
#' delineation or snapping issues. These checks can produce false positives so they are
#' disabled by default. Note however that the checks will happen whenever a QSWAT+ project
#' `name` contains the string "test".
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
#' @param data_dir character, path to the project (sub)directory for the (sub)catchment 
#' @param overwrite logical, whether to write the output or just return the file paths
#' @param name character, name of the sub-directory to use for project files
#' @param osgeo_dir character, path to the QGIS 3 installation directory (AKA "OSGEO4W_ROOT")
#' @param min_ncell integer > 0, minimum number of cells to define a channel drainage
#' @param channel_threshold  numeric > 0, channel creation threshold as fraction of basin area 
#' @param stream_threshold numeric > 0, stream creation threshold as fraction of basin area
#' @param snap_threshold integer > 0, maximum distance (metres) to snap outlets to flow lines
#' @param lake_threshold integer > 0, the percent overlap for a cell to become (part of) a lake
#' @param do_test logical, if `TRUE` the function appends '_test' to `name` (see details) 
#' @param dem_path character, path to DEM to use as input (instead of default in `data_dir`) 
#' @param outlet_path character, path to outlets to use as input (instead of default in `data_dir`)  
#' @param nudge_dist numeric > 0 or NULL (for default pixel width), distance to reposition points
#' @param nudge_nmax integer, the maximum number of iterations of nudging allowed
#' @param nudge_nm character, sub-directory name for the nudged outlet file(s)
#'
#' @return list of paths related to the created QSWAT+ project
#' @export
run_qswat = function(data_dir,
                     overwrite = FALSE,
                     name = basename(data_dir),
                     osgeo_dir = NULL,
                     lake_threshold = 50L,
                     min_ncell = 16L,
                     channel_threshold = 1e-3,
                     stream_threshold = 1e-2,
                     snap_threshold = 300L,
                     do_test = FALSE,
                     dem_path = NULL,
                     outlet_path = NULL,
                     nudge_clear = TRUE,
                     nudge_dist = NULL,
                     nudge_nmax = 5,
                     nudge_plot = TRUE,
                     nudge_nm = 'outlet_moved') {
  
  # validity checks
  msg_dir = 'data_dir was not character'
  if( !is.character(data_dir) ) {
    
    is_sf = any( class(data_dir) %in% c('sf', 'sfc') )
    if(is_sf) msg_dir = msg_dir |> paste0('. Did you means to call run_rqswat?')
    stop(msg_dir)
  }
  if( !dir.exists(data_dir) ) stop('project directory not found: ', data_dir)
  
  # location of the batch file that runs Python3
  batch_name = 'run_qswatplus.bat'
  batch_dir = path.package('rswat.uyr') |> file.path('python')
  
  # it would be nice to be able to discover this automatically somehow 
  if( is.null(osgeo_dir) ) osgeo_dir = 'C:/Program Files/QGIS 3.32.0'
  
  # expected paths of input files
  input_path = save_qswat(data_dir)
  dest_dir = input_path[['outlet']] |> dirname()
  msg_help = '\nHave you run `save_qswat` on this `data_dir` yet?'
  if( !dir.exists(dest_dir) ) stop('destination directory not found: ', dest_dir, msg_help)
  if(is.null(outlet_path)) outlet_path = input_path[['outlet']]
  
  # output filenames (project directory must be listed first)
  out_nm = c(qswat=paste0(name, ifelse(do_test, '_test', '')),
             input='qswat_input.json', 
             output='qswat_output.json',
             log='qswat_log.txt')
  
  # output paths to overwrite
  nudge_dir = dest_dir |> file.path(nudge_nm)
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
                   dem = ifelse(is.null(dem_path), input_path[['dem']], dem_path),
                   outlet = outlet_path,
                   landuse_lookup = input_path[['land_lookup']],
                   landuse = input_path[['land']],
                   soil = input_path[['soil']],
                   lake_threshold = lake_threshold,
                   channel_threshold = pmax(min_ncell, ceiling(channel_threshold * n_pixel)),
                   stream_threshold = pmax(min_ncell, ceiling(stream_threshold * n_pixel)),
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
  
  # write stdout from setup captured by shell() to log file
  writeLines(shell_result, dest_path['log'])

  # validity checks
  message('running delineation checks')
  check_result = check_qswat(data_dir, make_plot=nudge_plot)
  nudge_dist = check_result[['nudge_dist']] 
 
  # this case needs manual intervention
  if( !check_result[['output_exists']] & !check_result[['snap_problem']] ) {
    
    # skip further tests
    passed_check = FALSE
    nudge_max = 0
    nudge = data.frame()
    
  } else { nudge = check_result[['nudge']] }

  # end of error checking
  if( is.null(nudge) ) nudge = data.frame()
  passed_check = !check_result[['snap_problem']] & ( nrow(nudge) == 0 )
  
  # run outlet re-positioning loop
  if(nudge_nmax > 0) {

    # run repair
    if( !passed_check) {
      
      # for snap failures, run again with bigger snap radius
      if( check_result[['snap_problem']] ) {

        # run QSwAT+ without clearing nudge history
        snap_threshold_new = snap_threshold + nudge_dist
        message('')
        message('increasing snap threshold to: ', round(snap_threshold_new), ' m')
        message(nudge_nmax-1, ' setup attempts remaining')
        return(run_qswat(data_dir,
                         overwrite = overwrite,
                         name = name,
                         osgeo_dir = osgeo_dir,
                         lake_threshold = lake_threshold,
                         min_ncell = min_ncell,
                         channel_threshold = channel_threshold,
                         stream_threshold = stream_threshold,
                         snap_threshold = snap_threshold_new,
                         do_test = do_test,
                         dem_path = dem_path,
                         outlet_path = outlet_path,
                         nudge_clear = FALSE,
                         nudge_dist = nudge_dist,
                         nudge_nmax = nudge_nmax-1,
                         nudge_nm = nudge_nm)) 
      }
      
      # delete/create the nudged points directory as needed
      if( dir.exists(nudge_dir) & nudge_clear ) unlink(nudge_dir, recursive=TRUE)
      if( !dir.exists(nudge_dir) ) dir.create(nudge_dir)
      
      # load the existing outlets file and print the path to the new one
      outlet = outlet_path |> sf::st_read(quiet=TRUE)
      new_outlet_path = nudge_dir |> file.path(basename(tempfile('outlet_'))) |> paste0('.shp')
      message('writing new outlets file to: ', file.path(nudge_nm, basename(new_outlet_path)))

      # replace the affected points and write to disk
      id_replace = nudge[['ID']]
      nm_outlet = names(outlet)
      outlet[match(id_replace, outlet[['ID']]), nm_outlet] = nudge[nm_outlet]
      outlet |> sf::st_write(new_outlet_path, quiet=TRUE)

      # run QSwAT+ using new outlet file (and don't clear nudge directory)
      message('')
      message(nudge_nmax-1, ' outlet repositioning attempt(s) remaining')
      return(run_qswat(data_dir,
                       overwrite = overwrite,
                       name = name,
                       osgeo_dir = osgeo_dir,
                       lake_threshold = lake_threshold,
                       min_ncell = min_ncell,
                       channel_threshold = channel_threshold,
                       stream_threshold = stream_threshold,
                       snap_threshold = snap_threshold,
                       do_test = do_test,
                       dem_path = dem_path,
                       outlet_path = new_outlet_path,
                       nudge_clear = FALSE,
                       nudge_dist = nudge_dist,
                       nudge_nmax = nudge_nmax-1,
                       nudge_nm = nudge_nm))    
    }
  }
  
  # report any unsolved issues
  msg_fail = 'Try increasing nudge_nmax, changing thresholds, or inspect the points in QSWAT'
  if( passed_check ) {
    
    # report total distance for any moved points
    result_moved = report_moved(data_dir)
    message('QSWAT+ completed and passed checks')
    
  } else { 
    
    warning('unresolved delineation errors. ', msg_fail)
    result_moved = NULL
  }

  # # read the output JSON (paths)
  # qswat_output = NULL
  # if( file.exists(dest_path[['output']]) ) {
  #   
  #   qswat_output = readLines(dest_path[['output']]) |>
  #     jsonlite::fromJSON() |> 
  #     unlist()
  # }
  
  return(result_moved)
}


#' Load shape files from a QSWAT+ project
#' 
#' This loads the channels, sub-basins, and outlets shape-files created
#' during QSWAT+ setup and returns them as sf geometry data frames, in a list
#' along with a character vector of standard output from the `shell` call to QSWAT+.
#' 
#' This is meant for projects created using `run_qswat`; The function expects an
#' input JSON file in the "qswat" directory (parent of the QGIS project directory)
#' which it uses to find the correct input DEM path. The output JSON in this directory
#' (if found) is used to locate the output  QSWAT+ "Shapes" directory. If QSWAT+
#' completes delineation but halts afterwards (eg due to failed checks or database
#' issues), the function will attempt load the shape files anyway by guessing their
#' path in the `data_dir` directory tree.
#' 
#' The function returns only the subset of sub-basin polygons with positive 'Subbasin'
#' keys, and the channels mapping to them. The returned outlets have coordinates snapped
#' to channels already (by QSWAT+).
#' 
#'
#' @param data_dir character path to "qswat" subdirectory 
#' @param quiet logical if `TRUE` a warning is suppressed
#'
#' @return list with geometries 'sub', 'channel', 'outlet', as well as 'log', 'nudge_default'
#' @export
load_qswat = function(data_dir, quiet=FALSE) {
  
  # initialize output list
  load_list = list(sub = NULL,
                   channel = NULL,
                   outlet = NULL,
                   log = '')
  
  # get paths to output files from QSWAT+ setup (if it completed)
  output_json = run_qswat(data_dir)[['output']]
  log_path = run_qswat(data_dir)[['log']]
  
  # read log file if it exists
  log_txt = ''
  if(file.exists(log_path)) log_txt = readLines(log_path)
  
  # read QSWAT+ outputs
  if( file.exists(output_json) ) {
    
    # the output file paths
    qswat_path = output_json |> readLines() |> jsonlite::fromJSON()
    qswat_dir = dirname(qswat_path[['sub']])
    
  } else {
    
    # get input parameters for QSWAT+ setup
    input_json = run_qswat(data_dir)[['input']]
    if( !file.exists(input_json) ) {
      
      if(!quiet) warning('file not found: ', input_json, '\nHave you called `run_qswat` yet?')
      return(load_list)
    }
    
    # if setup didn't complete, try guessing the paths
    input_path = input_json |> readLines() |> jsonlite::fromJSON()
    qswat_dir = dirname(input_json) |> file.path(input_path[['name']], 'Watershed/Shapes')
    qswat_path = list(channel=file.path(qswat_dir, 'demchannel/demchannel.shp'),
                      sub=file.path(qswat_dir, 'demsubbasins.shp'),
                      outlet=file.path(qswat_dir, 'outlet_snap.shp'))
  }
  
  # shorten path by leaving out data_dir
  qswat_dir_str = qswat_dir |> substr(1+nchar(data_dir), nchar(qswat_dir))
  if( !quiet ) message('loading QSWAT+ model geometries from ', qswat_dir_str)
  
  # load relevant geometries
  load_geom = \(p) if( file.exists(p) ) sf::st_read(p, quiet=TRUE) else NULL
  swat_sub = qswat_path[['sub']] |> load_geom()
  swat_channel = qswat_path[['channel']] |> load_geom()
  swat_outlet = qswat_path[['outlet']] |> load_geom()
  
  # tidy using available information
  if( !is.null(swat_outlet) ) swat_outlet = swat_outlet |> dplyr::arrange(INLET)
  if( !is.null(swat_sub) ) swat_sub = swat_sub |> dplyr::filter(Subbasin > 0)
  if( !is.null(swat_channel) & !is.null(swat_sub) ) {
    
    basin_no = swat_sub |> dplyr::pull(PolygonId) |> sort()
    swat_channel = swat_channel |> dplyr::filter(BasinNo %in% basin_no)
  }

  return( list(outlet = swat_outlet,
               sub = swat_sub,
               channel = swat_channel,
               log = log_txt) )
}


