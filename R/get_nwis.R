#' Create/update a database of NWIS daily mean values
#' 
#' This uses the `dataRetrieval` package to manage downloads of data from the
#' National Water Information Service (NWIS). By default this constructs and/or
#' updates a database of mean daily stream flow in ft3.
#' 
#' Station sites of interest are selected by filtering results to points within
#' the catchment boundary polygon created by `get_catch`. A GeoJSON file locating
#' these points is written to a subdirectory of "nwis/". To get the full path do
#' `save_nwis(data_dir, nwis_nm)`. This file is written once only when `refresh=FALSE`.
#' Set `refresh=TRUE` to create it again - note that this updates all fields and checks
#' for new station locations relevant to the whole query.
#' 
#' By default records from all time periods are returned. If `from` is set, the
#' function filters results to stations having a record on/after `from`.
#' 
#' Outputs from individual stations are cached in the "raw" sub-directory of the
#' `nwis_nm` directory in `data_dir`, where they are updated/appended as needed.
#' After initializing, subsequent calls will update starting from the last recorded
#' date in the "raw" output files. To start from scratch, delete these files.
#' 
#' `from_initial` and `n_min` apply only to selecting relevant stations, so it
#' affects the output when initializing a new database, or when updating station
#' info with `refresh=TRUE`, but not when running an ordinary daily values update
#' with `refresh=FALSE`.
#' 
#' Specify a different variable of interest by changing the parameter and
#' statistic codes `param_code` and  `stat_code` (see also `?data_nwis`, and
#' `?dataRetrieval::whatNWISdata`). 
#' 
#' When changing `from_initial`, `param_code`, or `stat_code`, you should either
#' change/delete the output directory (`nwis_nm`), or else set `refresh=TRUE` to
#' rebuild the stations file - otherwise the function will only be aware of
#' previously seen stations. Note that the default `nwis_nm` only makes sense for
#' the default `param_code`. The function will not warn about nonsensical name choices!
#'
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param param_code character, the parameter code to update
#' @param stat_code character, the statistic code to update
#' @param n_min integer > 0, the minimum number of observations to include a record  
#' @param from_initial Date, results are filtered to records occurring on or after this date
#' @param refresh logical if TRUE a fresh list of station metadata is requested from NWIS 
#'
#' @return character vector, the file paths of the output
#' @export
get_nwis = function(data_dir,
                    nwis_nm = 'flow_ft',
                    param_code = '00060',
                    stat_code = '00003', 
                    n_min = 10,
                    from_initial = NULL,
                    refresh = FALSE) {

  # get stations list and write to "raw"
  nwis_path = save_nwis(data_dir, nwis_nm)[c('station', 'record')]
  if( !all(file.exists(nwis_path)) | refresh ) {
    
    # this requests up-to-date list from NWIS
    nwis = list_nwis(data_dir, nwis_nm, param_code=param_code, stat_code=stat_code, n_min=n_min)
    
    # prune the results when `from_initial` is supplied
    if( !is.null(from_initial) ) {
      
      # index stations having new enough records
      from_initial = from_initial |> as.Date()
      latest_existing = nwis[['catch']][[paste0('nwis_', param_code)]] |> as.Date()
      is_recent = latest_existing >= from_initial
      if( sum(is_recent) == 0 ) stop('no results. Try relaxing from_initial?')
      
      # remove redundant site records
      msg_count = paste(sum(is_recent), 'of', length(is_recent))
      message('filtering to ', msg_count, ' sites with records on/after ', from_initial)
      nwis[['catch']] = nwis[['catch']][is_recent, ]
    }

    # overwrites existing result on disk
    save_nwis(data_dir, nwis_nm, nwis, overwrite=TRUE)
  }

  # load list of sites of interest
  site_fetch = sf::st_read(nwis_path['station'], quiet=TRUE)[['site_no']]
  
  # update/initialize stations data (writes to "raw")
  update_nwis(data_dir, nwis_nm, from=from_initial, param_code=param_code, stat_code=stat_code)
  
  # TODO: check for split and distribute copies 
  # split_nwis
  
  # # load all station data
  # output_path = save_nwis(data_dir, nwis_nm)['data']
  # message('updating records from ', length(site_fetch), ' stations in ', output_path)
  # data_list = site_fetch |> lapply(\(s) {
  #   
  #     load_nwis(site = s,
  #               data_dir,
  #               nwis_nm,
  #               output = 'data',
  #               param_code = param_code,
  #               stat_code = stat_code)
  # })
  # 
  # # combine into a single data-frame and remove duplicates
  # data_df = do.call(rbind, data_list)

  # return file paths written above
  message('up to date')
  return(save_nwis(data_dir, nwis_nm, overwrite=FALSE))
}


#' Save NWIS files to disk
#' 
#' This writes the output of `list_nwis` (to geoJSON) or reports the files that would be written.
#' 
#' When `overwrite=TRUE` the function writes non-NULL objects in `nwis_list` to disk. When
#' `overwrite=FALSE` (the default) the function writes nothing but returns the file paths
#' that would be written.
#' 
#' All files written by this function go in the directory `data_dir |> file.path('nwis', nwis_nm)`.
#' Note that these are point location files, whereas the data values themselves are stored in the
#' "raw" subdirectory (and they are written by a different function, `update_nwis`) 
#' 
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param nwis_list list returned from `list_nwis`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_nwis('/example')
save_nwis = function(data_dir, nwis_nm='flow_ft', nwis_list=NULL, overwrite=FALSE) {

  # catch invalid calls and switch to file list mode
  if( is.null(nwis_list) & overwrite ) {
    
    warning('overwrite=TRUE but nwis_list was NULL')
    overwrite = FALSE
  }
  
  # output filenames and paths
  dest_fname = c(station = 'station.geojson', record = 'raw/all_station.csv')
  dest_path = file.path(data_dir, 'nwis', nwis_nm, dest_fname) |> stats::setNames(names(dest_fname))
  
  if( !overwrite ) return(dest_path)
  
  # make the directories if necessary and remove any existing output files
  raw_dir = dest_path['record'] |> dirname()
  if( !dir.exists(raw_dir) ) dir.create(raw_dir, recursive=TRUE)
  is_over = file.exists(dest_path[['record']])
  
  # write raw station metadata table
  if( !is.null(nwis_list[['all']]) ) {
    
    # first remove existing output file
    if( file.exists(dest_path[['record']]) ) unlink( dest_path[['record']] )
    nwis_list[['all']] |> write.csv(dest_path[['record']], row.names=FALSE)
  }

  # write points data frame for stations of interest
  if( !is.null(nwis_list[['catch']]) ) {
    
    # first remove existing output file
    if( file.exists(dest_path[['station']]) ) unlink( dest_path[['station']] )
    nwis_list[['catch']] |> sf::st_write(dest_path[['station']])
  }
  
  return( invisible(dest_path) )
}


#' Distribute copies of NWIS data files to the relevant sub-catchments
#' 
#' This is a helper for the `get_split` workflow, where sub-catchment data is saved
#' to sub-directories of `file.path(data_dir, 'split')`. This function copies data from
#' gages located at/near the outlet of each catchment, as well as any inlets, to the 'nwis'
#' sub-directory for that catchment.
#' 
#' The function returns a vector of paths to the directories modified. Once the
#' function has been called, users can pass any of these sub-directories to
#' `update_nwis` to update only the gages associated with that catchment, or
#' pass `data_dir` to update them all.
#'
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param param_code character, the parameter code to update
#' @param stat_code character, the statistic code to update
#'
#' @return character vector, paths to the directories modified
#' @export
split_nwis = function(data_dir, nwis_nm, param_code='00060', stat_code='00003') {
  
  # input all-station csv path
  record_path = save_nwis(data_dir, nwis_nm)['record']

  # load input gage file
  gage_path = save_split(data_dir, param_code=param_code, stat_code=stat_code)[['gage']]
  if( !file.exists(gage_path) ) stop('file not found: ', gage_path)
  gage = gage_path |> sf::st_read(quiet=TRUE)
  dest_dir = file.path(data_dir, 'split', unique(gage[['dir_name']]))
  
  # destination sub-catchment paths
  sub_path = save_split(data_dir, param_code=param_code, stat_code=stat_code)[['sub']]
  is_valid = dest_dir %in% sub_path
  if( any(!is_valid) ) stop('missing directory: ', paste(dest_dir[!is_valid], collapse='\n'))
  
  # loop over sub-catchments
  for( i in seq_along(dest_dir) ) {
    
    # input/output files 
    record_in = save_nwis(data_dir, nwis_nm)['record']
    record_out = save_nwis(dest_dir[i], nwis_nm)['record']
    station_out = save_nwis(dest_dir[i], nwis_nm)['station']
    
    # directory tree
    raw_dir = dirname(record_out)
    if( !dir.exists(raw_dir) ) dir.create(raw_dir, recursive=TRUE)
    
    # load and join input outlets and inlets files 
    point_in = save_catch(dest_dir[i], extra=TRUE)[c('outlet', 'inlet')]
    station = do.call(rbind, lapply(point_in, \(p) sf::st_read(p, quiet=TRUE))) |> sf::st_sf()
    
    # find source paths for data files matching these site codes
    site_in = station[['site_no']] |> 
      sapply( \(s) load_nwis(s, data_dir, nwis_nm, param_code=param_code, stat_code=stat_code) )
    
    # and the destination paths
    site_out = station[['site_no']] |> 
      sapply( \(s) load_nwis(s, dest_dir[i], nwis_nm, param_code=param_code, stat_code=stat_code) )
    
    # helper for copying
    my_copy = function(x, y) if(!file.exists(x)) stop('file not found: ', x) else x |> file.copy(y)

    # copy files (delete any existing)
    is_over = c(site_out, record_out) |> file.exists()
    if( any(is_over) ) unlink( c(site_out, record_out)[is_over] )
    Map(my_copy, x=c(site_in, record_in), y=c(site_out, record_out) )

    # save station data
    if( file.exists(station_out) ) unlink( station_out )
    station |> sf::st_write(station_out, quiet=TRUE)
  }
  
  return(dest_dir)
}


#' Update NWIS data files on disk by downloading latest dates
#'
#' This loops over the site codes in the file `save_nwis(data_dir, nwis_nm)['station']`,
#' calling `data_nwis` to download all new records starting from the date `from`, and
#' storing the results on disk in CSV files in the "raw" sub-directory.
#' 
#' If `from` is NULL and there is no existing file on disk, the function downloads all
#' available dates and creates the file. If there is a file on disk already and `from`
#' is NULL, the function sets `from` to the day after the latest date in the file.
#' 
#' The function will overwrite existing files without warning, but existing values in
#' the file on dates prior to `from` are not modified. New dates are added to the file
#' and existing dates with new values are overwritten.
#' 
#' Specify the variable and statistic of interest with `param_code` and `stat_code`
#' (see `?list_nwis`). `n_sec` should be left alone unless you know what you are doing
#' and plan to respect the Water Service API rate limits (see also `?data_nwis`).
#'
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param from Date (or any other object coercible Date) from which to start update
#' @param param_code character, the parameter code to update
#' @param stat_code character, the statistic code to update
#' @param n_sec numeric >= 0, the number of seconds to wait between requests
#'
#' @return returns nothing but possibly writes to CSV files in "raw" subdirectory
#' @export
update_nwis = function(data_dir, nwis_nm='flow_ft', from=NULL,
                       param_code='00060', stat_code='00003', n_sec=0.5) {

  # get site codes from station site points data frame
  pts = save_nwis(data_dir, nwis_nm)['station'] |> sf::st_read(quiet=TRUE)
  site_fetch = pts[['site_no']]
  
  # get station records data frame 
  nwis = save_nwis(data_dir, nwis_nm)['record'] |> read.csv(colClasses='character')
  
  # download and write to disk in a loop over sites
  t_req = proc.time()
  for(site in site_fetch) {
    
    # limit requests to n_sec/second
    sec_wait = pmax(0, n_sec - (proc.time() - t_req)['elapsed'])
    Sys.sleep(sec_wait)
    t_req = proc.time()
    
    # path to the CSV on disk
    site_path = load_nwis(site, data_dir, nwis_nm, output='path',
                          param_code=param_code, stat_code=stat_code)

    # set up starting date for update with `from` is NULL
    site_start = from
    if( file.exists(site_path) & is.null(site_start) ) {
      
      # update starts from the day after the latest existing date in the file
      day_as_int = 1
      site_start = load_nwis(site, data_dir, nwis_nm,
                             output = 'date', 
                             param_code = param_code,
                             stat_code = stat_code)[2] + 1
    }
    
    # download and open the time series 
    message('updating site ', site, ' (', match(site, site_fetch), ' of ', length(site_fetch), ')')
    site_df = data_nwis(nwis, site, from=site_start, param_code=param_code, stat_code=stat_code)
    
    # relabel parameter code column with alphabetic name
    names(site_df)[ names(site_df) == param_code ] = 'value'
    
    # if no results from NWIS, data_nwis returns empty list (and we skip to next site)
    if( length(site_df) == 0 ) next
    
    # merge with any existing file data
    if( file.exists(site_path) ) {
      
      # make sure read.csv sets the right column classes
      col_classes = site_df |> lapply(class) |> lapply(\(x) x[1])
      existing_site_df = read.csv(site_path, colClasses=col_classes)
      
      # discard any stale data, rbind, then sort by date 
      is_over = existing_site_df[['date']] %in% site_df[['date']]
      site_df = rbind(existing_site_df[!is_over,], site_df) |> dplyr::arrange('date')
    }
    
    # write to disk
    site_df |> write.csv(site_path, row.names=FALSE)
    message('')
  }
}


#' Load NWIS data from a file on disk, or return the date range within, or the file path
#' 
#' This function organizes the storage of bulk data downloads from NWIS. The data are
#' stored in CSV files in the "raw" sub-directory of `nwis_nm`. File names are constructed
#' by joining the site, parameter and statistic codes (in that order) separated by an
#' underscore.
#' 
#' With the default `output` set to 'path', the function returns the expected file path for
#' the CSV. With 'date', the function attempts to open the file and returns its date range
#' or NULL. With 'data' the function returns the whole contents of the file as a data frame.
#'
#' @param site character, the site code to fetch
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param output character, either 'path', 'date' , or 'values' 
#' @param param_code character, the parameter code to update
#' @param stat_code character, the statistic code to update
#'
#' @return either the file path (character), date range (length-2 POSIXct vector), or data frame
#' @export
load_nwis = function(site, data_dir, nwis_nm='flow_ft', output='path',
                     param_code='00060', stat_code='00003') {
  
  # unique name for the this series
  output_nm = c(site, param_code, stat_code) |> paste(collapse='_') |> paste0('.csv') 
  
  # build file path from "raw"
  raw_dir = save_nwis(data_dir, nwis_nm)['record'] |> dirname()
  output_path = file.path(raw_dir, output_nm)
  
  # return from path mode then initialize date mode
  if(output=='path') return(output_path)
  output_date = as.POSIXct(NULL)
  
  # check for existence (NULL for file not found)
  if( !file.exists(output_path) ) return( output_date ) 
  
  # else load the file to get dates
  output_df = read.csv(output_path)
  dates = read.csv(output_path)[['date']]
  output_date = c(head(dates, 1), tail(dates, 1)) |> as.Date()
  if(output=='date') return(output_date)
  
  # convert date column to expected class
  output_df[['date']] = output_df[['date']] |> as.Date()
  return(output_df)
}


#' Fetch records of point data from stations at the National Water Information System (NWIS)
#' 
#' This calls `dataRetrieval::whatNWISdata` to get a list of available service records
#' for the area of interest. This is defined by the catchment boundary file at
#' `save_catch(data_dir)['boundary']`, extended outwards by `buff_m` metres.
#' 
#' Currently this only checks for daily values (having `data_type_cd='dv'`). Records with
#' fewer than `n_min` observations are skipped.
#' 
#' The function retuns a list with two data frames: 'all' is the output from
#' `dataRetrieval::whatNWISdata` before filtering for observation count and variable name;
#' and 'catch' is an sf points data frame describing the station sites of interest.
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
#' @param data_dir character path to the output files directory
#' @param nwis_nm character name of the output data sub-directory in "nwis"
#' @param param_code character, the 5-digit NWIS parameter code (see `?usgs_lookup`)
#' @param stat_code character the statistic code for the variable (default is mean)
#' @param n_min integer > 0, the minimum number of records to include a station
#' @param buff_m numeric > 0, the length in metres to buffer the catchment boundary (see details)
#'
#' @return list with data frames 'catch' and 'all'
#' @export
list_nwis = function(data_dir, nwis_nm='flow_ft', 
                     param_code='00060', stat_code='00003', n_min=10, buff_m=100) {
  
  # set the name(s) of the variable if there are none 
  if( is.null(names(param_code)) ) names(param_code) = paste0('nwis_', param_code)
  
  # input and output paths
  outlet_path = save_catch(data_dir, overwrite=FALSE)['outlet']
  boundary_path = save_catch(data_dir, overwrite=FALSE)['boundary']
  
  # set UTM zone for computations
  crs_utm = outlet_path |> sf::st_read(quiet=TRUE) |> to_utm() |> suppressMessages()
  
  # load the catchment bounding box and add padding
  boundary_utm = boundary_path |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)
  boundary_pad = boundary_utm |> sf::st_buffer(units::set_units(buff_m, m)) |> sf::st_transform(4326)
  
  # request updated directory of records from NWIS
  message('requesting service records from NWIS')
  bbox_geo = boundary_pad |> sf::st_bbox() |> sprintf(fmt='%8f')
  nwis_info_all = dataRetrieval::whatNWISdata(bBox=bbox_geo, service='dv', statCd=stat_code)

  # filter to relevant variables and record lengths
  nwis_info = nwis_info_all |> 
    dplyr::filter(parm_cd == param_code) |> 
    dplyr::filter(count_nu >= n_min)
  
  # convert to sf points data frame with some additional fields
  nwis_pt = nwis_info |> points_nwis(param_code, stat_code=stat_code)
  
  # crop results to catchment
  is_in = nwis_pt |> sf::st_transform(crs_utm) |> sf::st_intersects(boundary_utm, sparse=FALSE)
  nwis_catch_pt = nwis_pt[is_in,]
  message( paste(sum(is_in), 'station(s) in catchment for', nwis_nm) )
  
  return( list(catch=nwis_catch_pt, all=nwis_info_all) )
}


#' Download daily observation data for a particular NWIS site and variable
#' 
#' This uses the result of `dataRetrieval::whatNWISdata` to construct API calls
#' for waterservices.usgs.gov/rest to download daily records.
#' 
#' This processes one variable at a time. Specify the variable code in `param_code`,
#' along with the site code `site`, and the starting date `from`. If `from` is not
#' supplied, the function downloads all available dates.
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
    t_req = proc.time()
    
    # arguments for waterservices.usgs.gov/rest 
    args_i = list(sites = record_df[['site_no']][i],
                  service = record_df[['data_type_cd']][i],
                  parameterCd = record_df[['parm_cd']][i],
                  startDate = record_df[['begin_date']][i],
                  statCd = record_df[['stat_cd']][[i]])
    
    # append `from` (if supplied) to arguments
    if( !is.null(from) ) args_i[['startDate']] = from |> as.Date() |> as.character()

    # print progress report to console 
    var_msg_i = paste0(args_i[['service']], '-', args_i[['parameterCd']])
    message('NWIS reports ', record_df[['count_nu']][i], ' observation(s) of ', var_msg_i)
    
    # attempt to download and import record data
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
      
      # copy output columns with tidier names and add site code
      output_df = result_df[c('dateTime', var_nm)] |> stats::setNames(c('date', var_nm))
      output_df[['site_no']] = site
      output_df = output_df[c('site_no', 'date', var_nm)]
      
      # convert POSIXct to Date
      if( nrow(output_df) > 0 ) output_df[['date']] = output_df[['date']] |> as.Date()
      
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
    
    message(sum(is_fail), ' request(s) failed or returned no new results')
    message('')
    if( all(is_fail) ) return(list()) 
  }
  
  # return all records in a single data frame
  do.call(rbind, result_all[!is_fail]) |> dplyr::arrange('date')
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
  message('aggregating service records')
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
  if( length(site_list) == 0 ) stop('no results. Try a different param_code?')
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

