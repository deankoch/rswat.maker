#' Fetch records of point data from stations at the National Water Information System (NWIS)
#' 
#' This calls `dataRetrieval::whatNWISdata` to get a list of available service records,
#' before calling `nwis_data` in a loop to download all records relevant to the query.
#' 
#' Currently this only retrieves daily values (having `data_type_cd='dv'`) but
#' other types of records may be listed in nwis_info_all. Records with fewer than n_min
#' observations are skipped.s
#' 
#' Specify your variables of interest by supplying a vector of parameter codes in
#' `param_code`. A lookup table for these codes is lazy-loaded by this package in object
#' `usgs_lookup` - get a table of definitions for your query by calling:
#' 
#' `usgs_lookup |> dplyr::filter(parm_cd %in% param_code)`
#' 
#' For more information on accessing NWIS from R see the help pages and vignettes at
#' https://cran.r-project.org/package=dataRetrieval
#' 
#' @param data_dir character path to the directory to use for output files 
#' @param n_min integer > 0, the minimum number of records to include a station
#' @param param_code character, the 5-digit NWIS parameter code (see `?usgs_lookup`)
#' @param force_overwrite logical, causes the function to download a fresh table of service records
#'
#' @return list of data frame
#' @export
get_nwis = function(data_dir, param_code=c(flow_m='30208', flow_ft='00060'), n_min=10) {
  
  # # print info on requested variables
  # usgs_lookup |> 
  #   dplyr::filter(parm_cd %in% param_code) |> 
  #   dplyr::select(c('parm_cd', 'parm_nm', 'parm_unit'))
  
  # input and output paths
  boundary_path = save_catch(data_dir, overwrite=FALSE)['boundary']
  bbox_path = save_dem(data_dir, overwrite=FALSE)[['bbox']]
  nwis_raw_dir = save_nwis(data_dir, overwrite=FALSE)[['raw_dir']]

  # request updated directory of records from NWIS
  message('requesting service records from NWIS')
  bbox_geo = bbox_path |> sf::st_read() |> sf::st_bbox() |> sprintf(fmt='%8f')
  nwis_info_all = dataRetrieval::whatNWISdata(bBox=bbox_geo)
  
  # filter to relevant records
  nwis_info = nwis_info_all |>
    dplyr::filter(parm_cd %in% param_code) |> 
    dplyr::filter(count_nu >= n_min) |> 
    dplyr::filter(data_type_cd == 'dv')
  
  # convert to sf points data frame with some additional fields
  nwis_pt = nwis_info |> nwis_points(param_code)
  
  # crop to 
  boundary = sf::st_read(boundary_path)
  sf::st_crop(nwis_pt, boundary)
  
  
  
  
  

  
  
  
  
}


#' Save the output of `get_nwis` to disk
#' 
#' The function writes a copy of `nwis` to geoJSON. The contents of the "raw"
#' sub-directory are written by `get_nwis`.
#' 
#' When `overwrite=TRUE` the function writes ..., and when `overwrite=FALSE` the function
#' writes nothing but returns the file paths that would be written.
#' 
#'
#' @param data_dir character path to the directory to use for output files
#' @param nwis list returned from `get_nwis`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_nwis('/example')
save_nwis = function(data_dir, nwis=NULL, overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(nwis) & overwrite ) {
    
    warning('overwrite=TRUE but nwis was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'nwis')
  
  # output filenames
  dest_fname = c(station = 'station.geojson',
                 record = 'station.csv',
                 raw_dir = 'raw')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)

  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)

  # remove any existing output files (but not "raw", which is a directory)
  if( any(is_over) ) unlink(dest_path[is_over], recursive=FALSE)
  
  # TODO:
  # writing
  # message('writing to ', path_out[['nwis_records']])
  # nwis_info_all |> write.csv(path_out[['nwis_records']], row.names=FALSE)
  
  
  # save the data
  return(dest_path)
}


#' Wrapper for `dataRetrieval::whatNWISdata` to return metadata about NWIS points
#' 
#' The function returns an sf data frame summarizing the NWIS metadata returned by 
#' `dataRetrieval::whatNWISdata` on hydrological point data. Points are in WGS84
#' coordinates and fields include site metadata and data availability. 
#' 
#' This reshapes the data frame to have one row per site. Since a site may
#' produce multiple records, the function adds a field (column) for each of the
#' requested variables (in `param_code`) indicating the latest date for which a
#' record is available at the site (or `NA` if no records are available).
#' 
#' `param_code` can be `NULL` to return info on all available variables. It it is
#' specified and its entries are named, these names are re-used in the output.
#' For example if `param_code=c(flow_m='30208')` then the output data frame will
#' include a column named `flow_m` with the latest available date (or `NA`).
#' 
#' The 5-digit `stat_code` by default fetches mean values ('00003'). This makes
#' sense for our use-case of daily stream flow data, but not necessarily for other
#' applications. Other options include '00011' (instantaneous), '00001' (max),
#' '00002' (min). 
#' 
#' Sites with none of the requested variables are omitted from the results with
#' the default `na_rm=TRUE`. Set `na_rm=FALSE` to keep them. 
#' 
#' See also also the help pages at https://help.waterdata.usgs.gov/
#'
#' @param nwis_info subset of data frame returned by `dataRetrieval::whatNWISdata`
#' @param param_code named character vector, parameter codes (see `?usgs_lookup`)
#' @param stat_code character the statistic code for the variable (default is mean)
#' @param na_rm logical if `TRUE`, sites with no applicable records are omitted
#'
#' @return data frame with a row for each distinct station site
#' @export
nwis_points = function(nwis_info, param_code=NULL, stat_code='00003', na_rm=TRUE) {
  
  # param_code should be a named vector of parameter code strings eg. c(foo='00010')
  nwis_info = nwis_info |> dplyr::filter( !is.na(parm_cd) )
  if( is.null(param_code) ) param_code = stats::setNames(nm=unique(nwis_info[['parm_cd']]))
  if( is.null(names(param_code)) ) names(param_code) = param_code
  
  # apply filters (stat_code='00003' requests mean)
  site_fetch = nwis_info |> 
    dplyr::filter( parm_cd %in% param_code ) |>
    dplyr::filter( is.na(stat_cd) | (stat_cd %in% stat_code) )
  
  # make a points data-frame for each each station
  message('aggregating ', nrow(site_fetch), ' service records')
  site_list = site_fetch |> split(site_fetch[['site_no']]) |> lapply(\(s) {
    
    # geometry point for the station
    epsg_i = datum_to_epsg(s[['dec_coord_datum_cd']][1])
    point_i = s[ c('dec_long_va', 'dec_lat_va') ] |>
      head(1) |>
      as.numeric() |>
      sf::st_point() |>
      sf::st_sfc(crs=epsg_i)
    
    # which variables are offered and when was the latest record
    end_i = s[['end_date']][ match(param_code, s[['parm_cd']]) ] |> 
      as.list() |> 
      as.data.frame() |> 
      stats::setNames( names(param_code) )
    
    # append metadata for the station
    s[1L, c('site_no',
            'station_nm',
            'agency_cd',
            'site_tp_cd',
            'huc_cd')] |> cbind(end_i) |> sf::st_sf(geometry=point_i)
  })
  
  # merge them all together into a data frame
  message('found ', length(site_list), ' station sites')
  site_df = do.call(rbind, site_list)
  rownames(site_df) = NULL
  
  # remove points that have no records of the variables in param_code
  if( na_rm ) {
    
    is_rm = site_df[names(param_code)] |> apply(1, \(x) all(is.na(x)))
    site_df = site_df[!is_rm, ]
  }
  
  return(site_df)
}