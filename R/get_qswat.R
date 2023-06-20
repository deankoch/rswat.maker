#' Split a catchment into sub-catchments with outlet(s) at NWIS gages
#'
#' For more on `s2` see `?get_upstream`
#'
#' @param data_dir character path to the directory to use for output files
#' @param gage sf points data frame, point of interest at which to split the catchment
#' @param fast logical if `TRUE` only the 'boundary', 'gage', ''
#' @param snap_dist numeric with units, distance within which to snap an `outlet` point
#' @param s2 logical if `FALSE` sets `sf::sf_use_s2(FALSE)` during function evaluation
#'
#' @return something
#' @export
split_catch = function(data_dir, 
                       gage = NULL, 
                       fast = FALSE, 
                       snap_dist = units::set_units(100, m), 
                       s2 = FALSE) {

  # turn off spherical approximation
  if(!s2) {
    
    # for less chance of failure with corrupt geometries
    if( sf::sf_use_s2() ) on.exit( sf::sf_use_s2(TRUE) |> suppressMessages() )
    sf::sf_use_s2(FALSE) |> suppressMessages()
  }
  
  # look for default USGS stream gage stations found by `get_nwis`
  if( is.null(gage) ) {

    gage_path = save_nwis(file.path(data_dir, 'nwis/flow_ft'))['station']
    message('using points from ', gage_path)
    gage = sf::st_read(gage_path, quiet=TRUE)
  }
  
  # load original outlet point (input to `get_catch`) and match it against `gage` points
  outlet_main = save_catch(data_dir)['outlet'] |> sf::st_read(quiet=TRUE)
  dist_usgs = sf::st_distance(outlet_main, gage)
  is_usgs = dist_usgs < snap_dist
  
  # if there are any matches at this snapping distance, select the closest
  if( any(is_usgs) ) {
    
    idx_selected = which(is_usgs)[ which.min(dist_usgs[is_usgs]) ]
    gage[['main_outlet']] = seq_along(is_usgs) %in% idx_selected
    
    } else {
    
    # get a dummy row with NAs
    new_row = gage[0,] |> sf::st_drop_geometry()
    new_row[1,] = NA
    
    # add it to the `gage` points and mark as main 
    gage = sf::st_sf(new_row, geometry=sf::st_geometry(outlet_main)) |> rbind(gage)
    gage[['main_outlet']] = seq_along(is_usgs) == 1
  }
  
  # load boundary polygon and check validity of gage
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE)
  is_valid = sf::st_intersects(gage, boundary, sparse=FALSE) |> suppressMessages()
  msg_invalid = paste(which(!is_valid), collapse=', ')
  if( !all(is_valid) ) stop('point(s) at index ', msg_invalid, ' lie outside catchment boundary')
  if( !is.data.frame(gage) ) stop('gage must be a data frame, or NULL')
  
  # load sub-catchment polygons and edge data frame (as character to avoid COMID as integer)
  catchment = save_catch(data_dir)['catchment'] |> sf::st_read()
  edge = save_catch(data_dir)['edge'] |> read.csv(colClasses='character')

  # drop non-geometry info and call catchment finder in a loop
  n_gage = nrow(gage)
  message('finding catchments for gage locations')
  gage_result = seq(n_gage) |> lapply(\(i) get_upstream(gage[i,], edge, catchment, fast=TRUE))
  
  # create sf data frame of sub-catchment boundary polygons (likely overlapping)
  sub_poly = do.call(c, lapply(gage_result, \(x) x[['boundary']]))
  
  # for each gage point, copy COMID to gage data frame and find nearest downstream neighbour
  message('calculating stream order')
  gage[['comid']] = gage_result |> sapply(\(x) x[['comid']])
  gage[['downstream']] = seq(n_gage) |> sapply(\(i) {
    
    # find all downstream COMIDs (in order) and return the first nontrivial match
     data.frame(comid = comid_down(gage[['comid']][i], edge)) |>
      dplyr::filter(comid %in% gage[['comid']]) |>
      dplyr::filter(comid != gage[['comid']][i]) |>
      dplyr::pull(comid) |> 
      head(1)
  })

  # flag headwaters sub-catchments
  gage[['headwater']] = !( gage[['comid']] %in% gage[['downstream']] )
  
  # copy NWIS site key for later use 
  gage_lookup = gage[c('site_no', 'comid', 'station_nm')] |> sf::st_drop_geometry()
  
  # make sf data frame from polygons, removing (any) duplicate sub-catchments
  sub_sf = gage |>
    sf::st_sf(geometry=do.call(c, lapply(gage_result, \(x) x[['boundary']]))) |>
    dplyr::arrange( dplyr::desc(count) ) |>
    dplyr::select(c('station_nm', 'comid', 'downstream', 'headwater', 'main_outlet')) |>
    dplyr::distinct(comid, .keep_all=TRUE) |>
    dplyr::arrange(main_outlet, !headwater)
  
  # add name in snail case - slightly simpler (but still long) version of station_nm
  sub_sf[['snail_name']] = gsub('[^A-z]+', '_', sub_sf[['station_nm']], perl=TRUE) |> tolower()

  # clip overlap from sub-catchments to make a partition of the basin
  comid = sub_sf[['comid']][ sub_sf[['main_outlet']] ]
  is_pending = sub_sf[['downstream']] %in% comid
  while( any(is_pending) ) {

    # replace the sub-catchment with clipped version
    is_shrinking = sub_sf[['comid']] %in% comid
    sf::st_geometry(sub_sf[is_shrinking,]) = sub_sf[is_shrinking,] |> 
      sf::st_geometry() |> 
      sf::st_difference(sf::st_union(sub_sf[is_pending,])) |> suppressMessages()
    
    # update comid and list of pending sub-catchments
    comid = sub_sf[['comid']][is_pending]
    is_pending = sub_sf[['downstream']] %in% comid
  }
  
  # initialize output list (st_sf puts sets geometry column properly)
  result_list = list(boundary = sub_sf |> sf::st_sf(),
                     gage = gage |> sf::st_sf(),
                     gage_lookup = gage_lookup)
  
  if(fast) return(result_list)
  
  
  
  
  # TODO:
  # add function `snap_outlet` to find true outlets using intersections with flow,
  # create main stem using comid_down
  # take subsets of lakes, flow, etc
}

