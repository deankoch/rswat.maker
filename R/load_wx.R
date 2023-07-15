#' Get weather data for a SWAT+ model from `wxArchive`
#' 
#' This is a helper function for `wxArchive` users. It returns a data frame of
#' daily weather data for sub-basin(s) in the QSWAT+ model in `data_dir`. Note
#' that this requires having an up-to-date `wxArchive` project whose extent
#' covers the requested sub-basin(s). 
#' 
#' Before calling this function, users must first save the output of
#' `load_qswat(data_dir, 'sub', sub=TRUE)` to "aoi_export.geojson" in `wx_dir`,
#' then run the "downscale" and "extract" operations in `wxArchive` to compute
#' the time series. See `?wxArchive::workflow_downscale` and
#' `?wxArchive::workflow_extract` for details.
#' 
#' Optional argument `subbasin_id` specifies the QSWAT+ "Subbasin" code of a
#' particular set of sub-basins. Leave this at its default `NULL` to return data
#' for all sub-basins.
#' 
#' The five options for `var_nm` are designed to serve as drop-in replacements for
#' the standard SWAT+ weather station data files - eg values from `var_nm='pcp_mean'`
#' can be copied to ".pcp" files using `rswat_open` and `rswat_write`.
#'
#' @param wx_dir character path to the data directory for `wxArchive`
#' @param var_nm character, one of "tmp_max", "tmp_min", "hum_mean", "pcp_mean", "wnd_mean"
#' @param subbasin_id integer key from QSWAT+ identifying the sub-basin 
#' @param data_dir character path to the `rqswat` data directory for the (sub)catchment
#' @param from integer or `NULL`, the year to start from (this and all later years copied)
#'
#' @return list with 'values' (the data), 'date' (mapping to rows), 'poly' (mapping to columns)
#' @export
load_wx = function(wx_dir, 
                   data_dir, 
                   sub_dir = NULL, 
                   var_nm = 'pcp_mean', 
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
    return( list(poly = import_poly,
                 date = csv_result[['date']], 
                 values = unname(as.matrix(csv_result[, -1]))) )
  }
  
  stop('the wxArchive package could not be loaded. Have you installed it?')
}



#' Write weather data from `load_wx` to SWAT+ weather station data files
#'
#' @param sub_dir character path to the (sub)catchment directory
#' @param wx_list list returned from `load_wx` for `sub_dir`
#' @param overwrite logical, if FALSE the function returns the file paths but writes nothing
#' @param quiet logical, suppresses console messages
#'
#' @return vector of file paths modified
#' @export
save_wx = function(sub_dir, wx_list=NULL, overwrite=FALSE, quiet=FALSE) {

  # load the dependency
  is_loaded = requireNamespace(package='rswat')
  if( is_loaded ) {
    
    # find the SWAT+ directory and check it exists
    txt_dir = run_editor(sub_dir)['txt']
    msg_txt = 'TxtInOut not found. Have you called `run_editor` yet?'
    if( is.null(txt_dir) ) stop(msg_txt)
    if( !file.exists(txt_dir) ) stop(msg_txt)

    # TODO: load the project with rswat
    
    dest_path = txt_dir
    if( !overwrite ) return(dest_path)
    
  }
  
  stop('the rswat package could not be loaded. Have you installed it?')
}


#' Save sub-basins file TODO
#'
#' @param data_dir 
#' @param name 
#' @param overwwrite 
#'
#' @return
#' @export
#'
#' @examples
save_aoi = function(data_dir, name='aoi_export', overwrite=FALSE, quiet=FALSE) {
  
  
  sub_df = load_qswat(data_dir, what='sub', sub=TRUE)
  # sub_geo_df = sub_df |> sf::st_transform(4326)
  # aoi_path = 'G:/aoi_export.geojson'
  # sub_geo_df |> sf::st_write(aoi_path)
  
}
















