#' Get weather data for a SWAT+ model from `wxArchive`
#' 
#' This is a helper function for `wxArchive` users. It returns a data frame of
#' daily weather data for sub-basin(s) in the QSWAT+ model in `data_dir`. Note
#' that this requires having an up-to-date `wxArchive` project whose extent
#' covers the requested sub-basin(s). 
#' 
#' This function simply loads existing output from `wxArchive` in a convenient
#' format. Before calling this function, users must call `save_aoi` to write
#' "aoi_export.geojson" in `wx_dir`, then run the "downscale" and "extract"
#' operations in `wxArchive` to compute the output time series. See
#' `?wxArchive::workflow_downscale` and `?wxArchive::workflow_extract` for details.
#' 
#' Optional argument `subbasin_id` specifies the QSWAT+ "Subbasin" code of a
#' particular set of sub-basins. Leave this at its default `NULL` to return data
#' for all sub-basins.
#' 
#' The options for `var_nm` are designed to serve as drop-in replacements for
#' the standard SWAT+ weather station data files - eg values from `var_nm='pcp_mean'`
#' can be copied to ".pcp" files using `rswat_open` and `rswat_write`.
#'
#' @param wx_dir character path to the data directory for `wxArchive`
#' @param data_dir character path to the `rqswat` data directory for the (sub)catchment
#' @param var_nm character, see choices by calling with `var_nm=NULL`
#' @param sub_dir character, the sub-catchment directory name
#' @param subbasin_id integer key from QSWAT+ identifying the sub-basin 
#' @param from integer or `NULL`, the year to start from (this and all later years copied)
#' @param quiet logical, suppresses console messages
#'
#' @return list with 'values' (the data), 'date' (mapping to rows), 'poly' (mapping to columns)
#' @export
load_wx = function(wx_dir, 
                   data_dir, 
                   var_nm = NULL,
                   sub_dir = NULL, 
                   subbasin_id = NULL,
                   from = NULL,
                   quiet = FALSE) {
  
  # the expected file mapping sub-basins to CSV files
  aoi_file = 'aoi_export.geojson'
  
  # load the dependency
  is_loaded = requireNamespace(package='wxArchive')
  if( is_loaded ) {
    
    # load the export polygons from `wxArchive` storage directory
    export_poly = file.path(wx_dir, aoi_file)
    if( file.exists(export_poly) ) export_poly = sf::st_read(export_poly, quiet=TRUE)
    msg_miss = '\nCreate the file then run the "downscale" and "extract" workflows in `wxArchive`'
    if( is.character(export_poly) ) stop('file not found: ', export_poly, msg_miss) 
    
    # check that export polygons have expected fields, and requested project is listed
    msg_proj = 'did not have the expected fields'
    if( !all( c('project', 'split') %in% names(export_poly) ) ) stop(msg_proj)
    if( !( basename(data_dir) %in% export_poly[['project']] ) ) stop(msg_proj)
    
    # get list of available variable names
    output_nm = wxArchive:::.nm_export
    var_nm_options = wxArchive:::.var_daily
    if( is.null(var_nm) ) return(var_nm_options)
    msg_var = paste0('\nSet `var_nm` to one of: "', paste(var_nm_options, collapse='", "'), '"')
    if( length(var_nm) > 1 ) stop('var_nm had length > 1', msg_var)
    if( !(var_nm %in% var_nm_options) ) stop('unrecognized variable name ', var_nm, msg_var)
    
    # get list of available CSV data files
    output_dir = wxArchive::file_wx('nc', wx_dir, output_nm, var_nm)
    output_csv = wxArchive::nc_chunk(output_dir, file_ext='csv')
    
    # filter to requested years
    output_year = output_csv |> regmatches(regexpr('(\\d{4})', output_csv, perl=T)) |> as.integer()
    msg_year = head(output_year, 1) |> c( tail( output_year, 1) ) |> paste(collapse='-')
    if( !is.null(from) ) output_csv = output_csv[output_year > from]
    if( length(output_csv) == 0 ) stop('invalid `from`. Set to NULL or select a year in ', msg_year)

    # initialize index of relevant polygons (default all)
    is_sub = rep(TRUE, nrow(export_poly))
    if( !is.null(sub_dir) ) is_sub = export_poly[['split']] %in% basename(sub_dir)
    if( !is.null(sub_dir) ) is_sub = export_poly[['split']] %in% basename(sub_dir)
    is_proj = export_poly[['project']] %in% basename(data_dir)
    
    # get index in the CSV data file for each "Subbasin" key
    csv_col = which(is_proj & is_sub)
    if( length(csv_col) == 0 ) stop('no matches for this name and subbasin')
    
    # polygons and map (to columns) for the data being imported (as matrix)
    import_poly = data.frame(j=csv_col) |> cbind(export_poly[csv_col,])
    
    # read the relevant CSV file(s) into a list (first column is date)
    if( !quiet ) message('loading ', var_nm, ' from ', length(output_csv), ' year(s)')
    csv_result = do.call(rbind, lapply(output_csv, \(p) read.csv(p)[ c(1, 1 + csv_col) ]))
    
    # split output into index, dates, matrix data
    return( list(name = var_nm,
                 poly = import_poly,
                 date = csv_result[['date']], 
                 values = unname(as.matrix(csv_result[, -1]))) )
  }
  
  stop('the wxArchive package could not be loaded. Have you installed it?')
}

#' Write weather data from `load_wx` to SWAT+ weather station data files
#'
#' Unfinished 
#' 
#' @param sub_dir character path to the (sub)catchment directory
#' @param wx_list list returned from `load_wx` for `sub_dir`
#' @param overwrite logical, if FALSE the function returns the file paths but writes nothing
#' @param quiet logical, suppresses console messages
#'
#' @return vector of file paths modified
#' @export
write_wx = function(sub_dir, wx_list, overwrite=FALSE, quiet=FALSE) {

  # load the dependency
  is_loaded = requireNamespace(package='rswat')
  if( is_loaded ) {
    
    # find the SWAT+ directory and check it exists
    txt_dir = run_editor(sub_dir)['txt']
    msg_txt = 'TxtInOut not found. Have you called `run_editor` yet?'
    if( is.null(txt_dir) ) stop(msg_txt)
    if( !file.exists(txt_dir) ) stop(msg_txt)

    # load the project with rswat
    if( !quiet ) message('loading ', basename(sub_dir), ' project in rswat')
    rswat::rswat(txt_dir, include='basic', quiet=TRUE)
    
    # we follow this file to find weather data files in a SWAT+ project
    climate_prefix = wx_list[['name']] |> switch('tmp_min'='tmp',
                                                 'tmp_max' = 'tmp',
                                                 'hum_mean' = 'hmd',
                                                 'pcp_mean' = 'pcp',
                                                 'wnd_mean' = 'wnd', 
                                                 'unknown')
    
    # load the climate file
    if(climate_file=='unknown') stop('unknown weather variable name "', wx_list[['name']], '"')
    weather_file = rswat::rswat_open( paste0(climate_prefix, '.cli'))[['filename']]
    
    # map to sub-basin IDs with regex on file names
    weather_id = paste0('\\.*', climate_prefix) |> 
      gsub('', basename(weather_file), perl=TRUE) |> 
      as.integer()
    
    # overwrite all files in a loop
    if( !quiet )  message('overwriting ', length(weather_id), ' ', climate_prefix, ' files')
    for(i in seq_along(weather_id)) {
      
      # column to copy from input matrix in wx_list
      j = wx_list[['poly']][['j']][ match(weather_id[i], wx_list[['poly']][['Subbasin']]) ]
      
      # open the existing weather file and copy the station data frame 
      weather_list = rswat::rswat_open(weather_file[i])
      station_df = weather_list[[1]]
      
      # build replacement for existing data frame
      data_df = as.Date(wx_list[['date']]) |> 
        rswat::rswat_date_conversion() |>
        cbind(wx_list[['values']][,j]) |>
        stats::setNames(c('year', 'jday', climate_prefix))

      # copy rswat attributes back
      weather_attr = attributes(weather_list[[2]])
      weather_attr = weather_attr[ startsWith(names(weather_attr), 'rswat') ]
      for(nm in names(weather_attr)) attr(data_df, nm) = weather_attr[[nm]]

      # update station info
      station_df[['nbyr']] = length(unique(data_df[['year']]))
      
      # overwrite the file
      list(station_df, data_df) |> rswat::rswat_write()
      
      
    }
    
    #weather_file = data.frame(path=)
    
    
    
    
    
    # get info about weather files
    rswat::rswat_files(include='weather')
    rswat::rswat_files(include='climate')
    
    wx_list |> str()
    
    
    dest_path = txt_dir
    
    
    if( !overwrite ) return(dest_path)
    
  }
  
  stop('the rswat package could not be loaded. Have you installed it?')
}


#' Save sub-basins file for wxArchive
#' 
#' This collects all (QSWAT+) sub-basin polygons from an `rqswat` project
#' and writes them (with metadata) in WGS84 coordinates to `dest_path`.
#' 
#' Normally the file name is 'aoi_export.geojson' and the parent directory is
#' the `project_dir` from a `wxArchive` project. This prompts `wxArchive` to
#' aggregate down-scaled weather data at the sub-basin level, suitable for
#' use in SWAT+.
#' 
#' This function should be called at some point before `load_wx` (see `?load_wx`)
#'
#' @param data_dir character path to the rqswat project directory
#' @param dest_path character, the file path to write
#'
#' @return the result of `sf::st_write`
#' @export
save_aoi = function(data_dir, dest_path, overwrite=FALSE, quiet=FALSE) {
  
  sub_df = load_qswat(data_dir, what='sub', sub=TRUE, quiet=quiet)
  sub_geo_df = sub_df |> sf::st_transform(4326)
  sub_geo_df |> sf::st_write(dest_path, quiet=quiet)
}
















