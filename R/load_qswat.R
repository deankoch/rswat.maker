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
#' Change `what` from its default 'all' to one of the output element names to return
#' only that element (unlisted).  Set `sub=TRUE` to load results from all sub-catchments
#' (subdirectories of "split") in a loop and return the results in a list. If `what` is
#' not 'all', the function column-binds the result, adding a key to identify the
#' sub-catchment and project. The path to the project associated with the geometry can
#' later be constructed using `file.path(data_dir, 'split', split_dir)`
#'
#' @param data_dir character path to parent of "qswat" subdirectory 
#' @param what character, either 'all' (default) the name of the element to return
#' @param sub logical if `TRUE` the function loops over sub-catchments in "split"
#' @param quiet logical if `TRUE` a warning is suppressed
#'
#' @return either of 'sub', 'channel', 'outlet', 'log' or all of them in a list
#' @export
load_qswat = function(data_dir, what='all', sub=FALSE, quiet=FALSE) {
  
  # loop to load all sub-catchments
  if(sub) {
    
    # sub-catchment directories
    subs = save_split(data_dir)[['sub']]
    sub_nm = basename(subs)
    
    # validity checks
    sub_exists = dir.exists(subs)
    msg_miss = paste(subs[!sub_exists], collapse=', ')
    if( any(!sub_exists) ) stop('missing catchment(s): ', msg_miss)
    
    # load everything
    load_result = lapply(subs, \(p) load_qswat(p, what=what, quiet=quiet)) |> stats::setNames(sub_nm)
    
    # collect all sub-basin geometries from sub-catchments
    if( what %in% c('sub', 'channel', 'outlet') ) {
      
      # append keys
      load_list = seq_along(subs) |> lapply(\(s) cbind(load_result[[s]],
                                                       data_dir = data_dir, 
                                                       split_dir = sub_nm[s]))
                                                         
      # join into a single data frame
      load_result = do.call(rbind, load_list)
    }
    
    return(load_result)
  }

  # initialize output list
  load_list = list(sub = NULL,
                   channel = NULL,
                   outlet = NULL,
                   log = '')
  
  # get paths to output files from QSWAT+ setup (if it completed)
  output_json = run_qswat(data_dir)[['output']]
  log_path = run_qswat(data_dir)[['log']]
  
  # read log file if it exists
  load_list[['log']] = ''
  if(file.exists(log_path)) load_list[['log']] = readLines(log_path)
  if(what == 'log') return(load_list[['log']])
  
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
      if( what != 'all' ) load_list = load_list[[what]]
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

  # copy to output list
  load_list[['outlet']] = swat_outlet
  load_list[['sub']] = swat_sub
  load_list[['channel']] = swat_channel
  
  if( what != 'all' ) load_list = load_list[[what]]
  return( load_list )
}