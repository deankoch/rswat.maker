#' This will run the whole workflow
#'
#' @param data_dir 
#'
#' @return something
#' @export
get_nwis = function(data_dir) {
  
  # get stations list
  nwis = list_nwis(data_dir)
  data_dir |> save_nwis(nwis, overwrite=TRUE)
  
}


#' Save the output of `get_nwis` to disk
#' 
#' The function writes a copy of `nwis` to geoJSON. The contents of the "raw"
#' sub-directory are written by `get_nwis`.
#' 
#' When `overwrite=TRUE` the function writes ..., and when `overwrite=FALSE` the function
#' writes nothing but returns the file paths that would be written.
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
                 record = 'raw/all_station.csv',
                 raw_dir = 'raw')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directories if necessary and remove any existing output files
  if( !dir.exists(dest_path[['raw_dir']]) ) dir.create(dest_path[['raw_dir']], recursive=TRUE)
  is_over = file.exists(dest_path)
  
  # remove any existing output files (but not "raw", which is a directory)
  if( any(is_over) ) unlink(dest_path[is_over], recursive=FALSE)
  
  # write raw station metadata table
  if( !is.null(nwis[['all']]) ) nwis[['all']] |> write.csv(dest_path[['record']], row.names=FALSE)
  
  # write points data frame for stations of interest
  if( !is.null(nwis[['catch']]) ) nwis[['catch']] |> sf::st_write(dest_path[['station']])
  return( invisible(dest_path) )
}


#' Load NWIS data from a file on disk, or return the date range within, or the file path
#' 
#' This function organizes the storage of bulk data downloads from NWIS. The data are
#' stored in CSV files in the "raw" sub-directory of `data_dir`. File names are constructed
#' by joining the site, parameter and statistic codes (in that order) separated by an
#' underscore.
#' 
#' With the default `output` set to 'path', the function returns the expected file path for
#' the CSV. With 'date', the function attempts to open the file and returns its date range
#' or NULL. With 'data' the function returns the whole contents of the file as a data frame.
#'
#' @param data_dir character path to the directory to look for output files 
#' @param output character, either 'path', 'date' , or 'values' 
#' @param site character, the site code
#' @param param_code, the parameter code
#' @param stat_code, the statistic code
#'
#' @return either the file path (character), date range (length-2 POSIXct vector), or data frame
#' @export
load_nwis = function(data_dir, site, output='path', param_code='00060', stat_code='00003') {
  
  # unique name for the this series
  output_nm = c(site, param_code, stat_code) |> paste(collapse='_') |> paste0('.csv') 
  
  # build file path from "raw"
  output_path = save_nwis(data_dir, overwrite=FALSE)['raw_dir'] |> file.path(output_nm)
    
  # return from path mode then initialize date mode
  if(output=='path') return(output_path)
  output_date = as.POSIXct(NULL) |> list() |> rep(2) |> stats::setNames(c('start', 'end'))
  
  # check for existence (NULL for file not found)
  if( !file.exists(output_path) ) return( output_date ) 
  
  # else load the file to get dates
  output_df = read.csv(output_path)
  dates = read.csv(output_path)[['date']]
  date_range = c(head(dates, 1), tail(dates, 1)) |> as.POSIXct(tz='UTC')
  if(output=='date') return(date_range)
  
  # convert date column to expected class
  output_df[['date']] = output_df[['date']] |> as.POSIXct(tz='UTC')
  return(output_df)
}




#' Update 
#'
#' @param data_dir 
#' @param from 
#' @param param_code 
#' @param stat_code 
#'
#' @return
#' @export
#'
#' @examples
update_nwis = function(data_dir, from=NULL, param_code='00060', stat_code='00003') {

  # get site codes and destination paths
  dest_path = load_nwis_site(data_dir, output='path', param_code=param_code, stat_code=stat_code)
  site_fetch = names(dest_path)
  
  # get station records data frame 
  nwis = save_nwis(data_dir)['record'] |> read.csv(colClasses='character')
  
  # download and write to disk in a loop over sites
  for(site in site_fetch) {
    
    # download and open the time series (relabel parameter code column with alphabetic name)
    message('updating site ', site, ' (', match(site, site_fetch), ' of ', length(site_fetch), ')')
    site_df = data_nwis(nwis, site, from, param_code, stat_code)
    names(site_df)[ names(site_df) == param_code ] = 'value'
    if( nrow(site_df) == 0 ) next
    
    # merge with existing file data
    site_path = dest_path[[site]]
    if( file.exists(site_path) ) {
      
      # make sure read.csv sets the right column classes
      col_classes = site_df |> lapply(class) |> lapply(\(x) x[1])
      existing_site_df = read.csv(site_path, colClasses=col_classes)
      
      # discard any stale data, rbind, then sort by date 
      is_over = existing_site_df[['date']] %in% site_df[['date']]
      site_df = rbind(existing_site_df[!is_over], site_df) |> dplyr::arrange('date')
    }
    
    # write to disk
    site_df |> write.csv(dest_path[[site]], row.names=FALSE)
    message('')
  }
}


#' Fetch records of point data from stations at the National Water Information System (NWIS)
#' 
#' This calls `dataRetrieval::whatNWISdata` to get a list of available service records
#' for the area of interest in `data_dir`.
#' 
#' Currently this only checks for daily values (having `data_type_cd='dv'`). Records with
#' fewer than `n_min` observations are skipped.
#' 
#' The function retuns a list with two data frames: 'all' the output from
#' `dataRetrieval::whatNWISdata` before filtering for observation count and variable name;
#' and 'catch' is an sf points data frame describing the station sites.
#' 
#' Specify the statistic of interest by its stat code `stat_code` (see `?points_nwis`).
#' Specify the variables of interest by supplying `param_code`. A lookup table for these
#' codes is lazy-loaded by this package in object `usgs_lookup` - check the definition
#' of your query by calling:
#' 
#' `usgs_lookup |> dplyr::filter(parm_cd == param_code)`
#' 
#' For more information on accessing NWIS from R see the help pages and vignettes at
#' https://cran.r-project.org/package=dataRetrieval
#' 
#' @param data_dir character path to the directory to use for output files 
#' @param param_code character, the 5-digit NWIS parameter code (see `?usgs_lookup`)
#' @param stat_code character the statistic code for the variable (default is mean)
#' @param n_min integer > 0, the minimum number of records to include a station
#'
#' @return list with data frames 'catch' and 'all'
#' @export
list_nwis = function(data_dir, param_code='00060', stat_code='00003', n_min=10) {
  
  # set the name(s) of the variable if there are none 
  if( is.null(names(param_code)) ) names(param_code) = paste0('nwis_', param_code)
  
  # input and output paths
  boundary_path = save_catch(data_dir, overwrite=FALSE)['boundary']
  bbox_path = save_dem(data_dir, overwrite=FALSE)[['bbox']]
  nwis_raw_dir = save_nwis(data_dir, overwrite=FALSE)[['raw_dir']]
  
  # load the catchment boundary
  boundary = boundary_path |> sf::st_read(quiet=TRUE)
  
  # request updated directory of records from NWIS
  message('requesting service records from NWIS')
  bbox_geo = bbox_path |> sf::st_read(quiet=TRUE) |> sf::st_bbox() |> sprintf(fmt='%8f')
  nwis_info_all = dataRetrieval::whatNWISdata(bBox=bbox_geo, service='dv', statCd=stat_code)
  
  # filter to relevant variables and record lengths
  nwis_info = nwis_info_all |> 
    dplyr::filter(parm_cd == param_code) |>  
    dplyr::filter(count_nu >= n_min)
  
  # convert to sf points data frame with some additional fields
  nwis_pt = nwis_info |> points_nwis(param_code, stat_code=stat_code)
  
  # crop results to catchment
  is_in = sf::st_intersects(nwis_pt, boundary, sparse=FALSE)
  nwis_catch_pt = nwis_pt[is_in,]
  message( paste(sum(is_in), 'station(s) in catchment for', basename(data_dir)) )
  
  return( list(catch=nwis_catch_pt, all=nwis_info_all) )
}


#' Download daily observation data for a particular NWIS site and variable
#' 
#' This uses the result of `dataRetrieval::whatNWISdata` to construct API calls
#' to waterservices.usgs.gov/rest and download daily records for the variable
#' coded in `param_code` at site `site`, starting from date `from`.
#' 
#' If `from` is not supplied, the function downloads all available dates.
#' 
#' It is important to respect rate limits when making requests to this API.
#' The default `n_sec` sets a half-second pause in between subsequent requests,
#' when the site has multiple distinct records. You should program a similar delay
#' to happen in between calls to `data_nwis` (for example using `Sys.sleep`).
#'
#' @param nwis data frame returned by `dataRetrieval::whatNWISdata`
#' @param site character, the site number (must appear in `site_no` column of `nwis`)
#' @param from Date, the earliest date to fetch 
#' @param param_code character, the 5-digit NWIS parameter code (see `?usgs_lookup`)
#' @param stat_code character the 5-digit statistic code for the variable (default is mean)
#' @param n_sec numeric >= 0, the number of seconds to wait between requests
#'
#' @return data frame with 'date' column and the requested values
#' @export
data_nwis = function(nwis, site, from=NULL, param_code='00060', stat_code='00003', n_sec=0.5) {
  
  # list of all relevant records for this site
  record_df = nwis |> 
    dplyr::filter( site_no == site ) |> 
    dplyr::filter( parm_cd == param_code ) |>
    dplyr::filter( data_type_cd == 'dv' ) |>
    dplyr::filter( stat_cd == stat_code ) 
  
  # message about site
  msg_site = paste0(record_df[['station_nm']][1], ' (', site, ')')
  message('processing ', nrow(record_df), ' record(s) at ', msg_site)
  
  # loop over distinct records for the site
  result_all = vector(mode='list', length=nrow(record_df))
  t_req = proc.time()
  for(i in seq(nrow(record_df))) {
    
    # limit requests to n_sec/second
    sec_wait = pmax(0, n_sec - (proc.time() - t_req)['elapsed'])
    Sys.sleep(sec_wait)
    
    # arguments for waterservices.usgs.gov/rest 
    args_i = list(sites = record_df[['site_no']][i],
                  service = record_df[['data_type_cd']][i],
                  parameterCd = record_df[['parm_cd']][i],
                  startDate = record_df[['begin_date']][i],
                  statCd = record_df[['stat_cd']][[i]])
    
    # validate `from` argument (if supplied) and append to arguments
    if( !is.null(from) ) {
     
      from = from |> as.Date()
      end_date = record_df[['end_date']] |> as.Date()
      if( from <= end_date ) args_i[['startDate']] = as.character(from)
    }

    # print progress report to console 
    var_msg_i = paste0(args_i[['service']], '-', args_i[['parameterCd']])
    message('NWIS has ', record_df[['count_nu']][i], ' observation(s) of ', var_msg_i)
    
    # attempt to download and import record data
    t_req = proc.time()
    result_i = tryCatch({

      # download all station data for this variable/service combination 
      result_df = dataRetrieval::readNWISdata(args_i)
      nwis_nm = names(result_df)
      
      # regex for stats code 
      stat_code = args_i[['statCd']]
      stat_suffix = paste0('[_', stat_code, ']*$')
      
      # expected variable name regex (based on ?dataRetrieval::readNWISdata)
      var_regex = c('^X.*', paste0('_', args_i[['parameterCd']]), stat_suffix) |> paste(collapse='')
      var_nm = nwis_nm[ grep(var_regex, names(result_df)) ]
      if( !( 'dateTime' %in% nwis_nm ) | ( length(var_nm) != 1 ) ) stop('unexpected column names')
      
      # copy output columns with tidier names
      output_df = result_df[c('dateTime', var_nm)] |> stats::setNames(c('time', var_nm))
  
      # append some constant columns to index this time series later
      output_df[['site_no']] = site
      output_df = output_df[c('site_no', 'time', var_nm)]
      
      # omit NA rows from output
      is_incomplete = output_df |> apply(1, anyNA)
      if( all(is_incomplete) ) stop('all rows had NAs')
      message(sum(!is_incomplete), ' day(s) downloaded')
      output_df[!is_incomplete, ] |> stats::setNames(c('site_no', 'date', param_code))
      
    }, error=identity)
    
    # copy results to storage unless there was an error
    if( !is(result_i, 'error') ) result_all[[i]] = result_i
  } 
  
  # warn of failed downloads
  is_fail = result_all |> sapply(is.null)
  record_complete = record_df[!is_fail,]
  if( any(is_fail) ) {
    
    message(sum(is_fail), ' request(s) failed!')
    print(record_df[is_fail,])
    if( all(is_fail) ) return(list()) 
  }
  
  # success message
  
  
  # return all records in a single data frame
  do.call(rbind, result_all[!is_fail]) |> dplyr::arrange('time')
}


#' Summarize output from `dataRetrieval::whatNWISdata` as a sf points data frame 
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
#' Otherwise the `param_code` itself is the column name.
#' 
#' The `count` field in the output is the sum of the counts for all of the requested
#' variables - ie if you request one variable, then `count` is the number of days of
#' records, but if you request multiple variables `count` is the sum of these numbers.
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
#' @param param_code character vector, parameter code(s) (see `?usgs_lookup`)
#' @param stat_code character the 5-digit statistic code for the variable (default is mean)
#' @param na_rm logical if `TRUE`, sites with no applicable records are omitted
#'
#' @return data frame with a row for each distinct station site and three columns
#' @export
points_nwis = function(nwis_info, param_code=NULL, stat_code='00003', na_rm=TRUE) {
  
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
    
    # count all variable-days of records
    count_i = data.frame(count=sum(s[['count_nu']], na.rm=TRUE) )
    
    # append metadata for the station
    station_i = s[1L, c('site_no', 'station_nm', 'agency_cd', 'site_tp_cd', 'huc_cd')]
    station_i |> cbind(end_i, count_i) |> sf::st_sf(geometry=point_i)
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
  
  # transform to WGS84 
  return(sf::st_transform(site_df, 4326))
}