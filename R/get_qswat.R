#' Split a catchment into sub-catchments with an NWIS gage at each outlet 
#'
#' This partitions the catchment in `data_dir` into a set of sub-catchments
#' matching the layout of upstream NWIS gages.
#' 
#' The inlets and outlets linking sub-catchments are placed on the same NHD
#' catchment polygons as the `gage` points. This ensures that each gage is
#' associated with the outlet for a single sub-catchment, and that the inlets
#' for this sub-catchment (if any) are themselves situated at `gage` points.
#' 
#' Sub-catchments inherit name and ID fields from row of `gage` mapping to its
#' outlet. If multiple elements in `gage` map to the same sub-catchment, the one
#' with the highest 'count' field (most records) is used, and the others discarded.
#' To find the sub-catchment associated with a `gage` element that was discarded,
#' use the output data frame `gage_lookup`.
#' 
#' If the main outlet of the catchment is not found within snapping distance
#' `snap_dist` of a point in `gage`, the function appends the outlet point
#' automatically. This ensures the set of output sub-catchment polygons always
#' forms a partition of the whole catchment.
#' 
#' The "true" outlet point for a sub-catchment (under the NHD model) lies at the
#' intersection of the boundary of the NHD polygon for the outlet COMID, and its
#' flow line. These points are calculated and returned in `outlet`. Note that the
#' `gage` points will often be located slightly upstream of the true outlet, but
#' always within the outlet NHD polygon.
#' 
#' See also `split_catch`, which does most of the work of following flow-lines and
#' building boundaries with set operations.
#'
#'
#' @param data_dir character path to the directory to use for output files
#' @param gage sf points data frame, point of interest at which to split the catchment
#' @param snap_dist numeric with units, snapping distance to set "main" outlet
#'
#' @return something
#' @export
get_split = function(data_dir, 
                     gage = NULL, 
                     snap_dist = units::set_units(100, m)) {
  
  # look for default USGS stream gage stations found by `get_nwis`
  if( is.null(gage) ) {
    
    gage_path = save_nwis(file.path(data_dir, 'nwis/flow_ft'))['station']
    message('loading gage points in ', gage_path)
    gage = sf::st_read(gage_path, quiet=TRUE)
  }
  
  # local UTM projection for computations
  crs_temp = gage[1,] |> sf::st_geometry() |> to_utm() |> suppressMessages()
  
  # load original outlet point (input to `get_catch`) and match it against `gage` points
  outlet_main = save_catch(data_dir)['outlet'] |> sf::st_read(quiet=TRUE)
  dist_usgs = sf::st_distance(outlet_main, gage)
  is_usgs = dist_usgs < snap_dist
  
  # if there are any matches at this snapping distance, select the closest
  if( any(is_usgs) ) {
    
    idx_selected = which(is_usgs)[ which.min(dist_usgs[is_usgs]) ]
    gage[['main_outlet']] = seq_along(is_usgs) %in% idx_selected
    
  } else {
    
    # make a dummy row with mostly NA fields
    new_row = gage[0,] |> sf::st_drop_geometry()
    new_row[1,] = NA
    new_row[['count']] = 0
    new_row[['station_nm']] = 'main outlet created by rswat'
    
    # add it to the `gage` points and mark as the main 
    gage = sf::st_sf(new_row, geometry=sf::st_geometry(outlet_main)) |> rbind(gage)
    gage[['main_outlet']] = seq_along(is_usgs) == 1
  }
  
  # load boundary polygon and check validity of gage
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE)
  is_valid = sf::st_intersects(gage, boundary, sparse=FALSE)
  
  # sanity checking
  msg_invalid = paste(which(!is_valid), collapse=', ')
  if( !all(is_valid) ) stop('point(s) at index ', msg_invalid, ' lie outside catchment boundary')
  if( !is.data.frame(gage) ) stop('gage must be a data frame, or NULL')
  
  # load sub-catchment polygons and edge data frame (as character to avoid COMID as integer)
  catchment = save_catch(data_dir)['catchment'] |> sf::st_read(quiet=TRUE)
  edge = save_catch(data_dir)['edge'] |> read.csv(colClasses='character')
  
  # run splitting subroutine and return from fast mode calls
  result_list = split_catch(gage, edge, catchment)
  if(fast) return(result_list)
  
  # load flow lines (slow)
  flow = save_catch(data_dir)['flow'] |> sf::st_read(quiet=TRUE)
  
  # compute true outlet locations and bind with metadata from nearest gage
  message('snapping outlets')
  outlet = result_list[['boundary']] |> find_outlet(edge, flow)
  
  # create a main stem LINESTRING for this set of sub-catchments
  comid_stem = result_list[['boundary']][['comid']] |> comid_down(edge)
  main_stem = flow[ flow[['COMID']] %in% comid_stem, ] |> 
    sf::st_geometry() |> 
    sf::st_transform(crs_temp) |> 
    sf::st_union() |>
    sf::st_transform(4326)
  
  # load lakes
  lake = save_catch(data_dir)['lake'] |> sf::st_read(quiet=TRUE)
  
  
  

  
  
  
  
  
  result_list[['boundary']] |> 
    sf::st_geometry() |> 
    plot(col='lightblue', border='white')
  
  plot(boundary, add=T)
  plot(main_stem, add=T, col='darkblue', lwd=2)

  outlet |> sf::st_geometry() |> plot(add=T, pch=16)
  outlet |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.5, col='red')

  # take subsets of lakes, flow, etc
}

                       

#' Find outlet points for a set of sub-catchments
#'
#' Helper function for `get_split`. This finds the intersection of the catchment
#' boundaries in `catch` with the flow line in `flow` corresponding to the outlet.
#' The function checks both the stream segment associated with the 'comid' field
#' and its immediate downstream neighbour.
#' 
#' If there is no intersection, the function snaps the endpoint of the incomplete
#' stream segment to the boundary and returns that point.
#' 
#' Identifiers in the returned data frame (like 'site_no') are copied from `catch`,
#' so they describe the nearest gage site, which may be located 
#'
#' @param catch sf data frame of sub-catchment polygons with 'comid' field
#' @param edge data frame of flow line directed edges ("PlusFlow" from "NHDPlusAttributes")
#' @param flow sf data frame of flow lines ("NHDFlowline" from "NHDSnapshot")
#'
#' @return sf data frame of outlet points with fields copied from `catch`
#' @export
find_outlet = function(catch, edge, flow) {

  # expect this to be complete
  comid = catch[['comid']]
  n_comid = length(comid)
  
  # scalar case
  if( length(comid) == 1 ) {
    
    # project to UTM for computations
    crs_temp = flow[1,] |> sf::st_geometry() |> to_utm() |> suppressMessages()
    
    # include first downstream COMID
    comid_check = comid |> comid_down(edge, first_only=TRUE) |> c(comid)
    
    # build line segments of interest
    boundary = catch |> sf::st_geometry() |> sf::st_transform(crs_temp) |> sf::st_boundary()
    out_line = flow[ flow[['COMID']] %in% comid_check, ] |> 
      sf::st_geometry() |> 
      sf::st_transform(crs_temp) |>
      sf::st_union()

    # deal with non-intersecting lines separately
    is_complete = sf::st_intersects(out_line, boundary, sparse=FALSE)
    out_point = if(is_complete) { sf::st_intersection(out_line, boundary, sparse=FALSE) } else {
      
      out_line |> sf::st_nearest_points(boundary) |> sf::st_cast('POINT') |> tail(1)
    }

    # transform back to WGS84
    return( sf::st_transform(out_point, 4326) )
  }
  
  # vector case: loop over outlet IDs to get point locations then append data frame from catch
  point_geom = do.call(c, lapply(seq(n_comid), \(i) find_outlet(catch[i,], edge, flow) ) )
  out_point = catch |> sf::st_sf(geometry=point_geom)
  return( out_point )
}


#' Split a catchment into sub-catchments with outlets on the points in `gage`
#'
#' This finds a partition of the union of the polygons in `catchment` based on the
#' locations of points of interest in `gage`, and the directed edges representing flow
#' lines in `edge`. The output has one sub-catchment for each point in `gage`, with
#' `gage` positioned on the outlet polygon.
#' 
#' Arguments `edge` and `catchment` can be generated by passing a main outlet point to
#' `get_upstream` (which uses `nhdR::nhd_plus_load` to fetch and load the data). The
#' function expects `gage` to include both this main outlet and at least one other point.
#' 
#' For each point in `gage`, the function finds the overlying polygon in `catchment`,
#' then merges it with all upstream polygons to form a larger polygon representing the
#' drainage of the point. If a different `gage` point lies upstream, then its drainage
#' polygon is differenced from the result to form the sub-catchment polygon.
#' 
#' This is done recursively starting from the main outlet, resulting in a set of disjoint
#' polygons ("sub-catchments") covering the entire catchment. Each has a unique outlet
#' in `gage`, and possibly one or more inlets (the upstream elements of `gage`).
#' 
#' The output list has two elements
#' 
#' * `boundary` is an sf data frame of sub-catchment polygons and flow directions
#' * `gage_lookup` is a data frame mapping points from `gage` to polygons in `boundary`
#' 
#' Sub-catchments in `boundary` are assigned identifiers from their associated `gage`
#' ('station_nm' and 'comid'). If multiple `gage` points lie on the same `catchment`
#' polygon, then they will share a common sub-catchment polygon in `boundary`. If this
#' happens, the function copies the name and COMID from the station with the highest
#' 'count' field (ie the most records). 
#' 
#' `s2` is used internally and should remain `FALSE` unless you know what you are doing
#' (see `?get_upstream`)
#'
#' @param gage sf points data frame, point of interest at which to split the catchment
#' @param edge data frame of flow line directed edges ("PlusFlow" from "NHDPlusAttributes")
#' @param catchment sf data frame of sub-catchment polygons ("Catchment" from "NHDPlusCatchment")
#' @param s2 logical if `FALSE` sets `sf::sf_use_s2(FALSE)` during function evaluation
#'
#' @return list with elements 'boundary', 'gage', 'gage_lookup'
#' @export
split_catch = function(gage, edge, catchment, s2=FALSE) {

  # turn off spherical approximation
  if(!s2) {
    
    # for less chance of failure with corrupt geometries
    if( sf::sf_use_s2() ) on.exit( sf::sf_use_s2(TRUE) |> suppressMessages() )
    sf::sf_use_s2(FALSE) |> suppressMessages()
  }
  
  # progress messages for the loop
  n_gage = nrow(gage)
  message('computing catchment areas for ', n_gage, ' input gage locations')
  if(n_gage > 1) pb = txtProgressBar(0, n_gage, style=3)

  # call catchment finder in a loop
  gage_result = seq(n_gage) |> lapply(\(i) {
    
    if(n_gage > 1) setTxtProgressBar(pb, i-1)
    get_upstream(gage[i,], edge, catchment, fast=TRUE) |> suppressMessages()
  })
  
  # tidier end to the progress bar loop
  if(n_gage > 1) {
    
    setTxtProgressBar(pb, n_gage)
    close(pb)
  }
  
  # copy COMID to gage data frame 
  gage[['comid']] = gage_result |> sapply(\(x) x[['comid']])
  
  # create sf data frame of sub-catchment boundary polygons (likely overlapping)
  message('computing stream flow order for COMIDs: ', paste(gage[['comid']], collapse=', '))
  sub_poly = do.call(c, lapply(gage_result, \(x) x[['boundary']]))
  
  # for each gage point, find nearest downstream neighbour
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
  
  # copy NWIS site key for later finding catchment of a duplicate record
  lu_nm = c('station_nm', 'site_no', 'comid')
  gage_lookup = gage[lu_nm] |> sf::st_drop_geometry()
  
  # make sf data frame from polygons, removing (any) duplicate sub-catchments
  sub_sf = gage |>
    sf::st_sf(geometry=do.call(c, lapply(gage_result, \(x) x[['boundary']]))) |>
    dplyr::arrange( dplyr::desc(count) ) |>
    dplyr::select(c(lu_nm, 'downstream', 'headwater', 'main_outlet')) |>
    dplyr::distinct(comid, .keep_all=TRUE) |>
    dplyr::arrange(main_outlet, !headwater)
  
  # message about dropped stations
  n_drop = nrow(gage) - nrow(sub_sf)
  if( n_drop > 0 ) message('removed ', n_drop, ' duplicate station site(s)')
  
  # add name in snail case - slightly simpler (but still long) version of station_nm
  sub_sf[['snail_name']] = gsub('[^A-z]+', '_', sub_sf[['station_nm']], perl=TRUE) |> tolower()

  # clip overlap from sub-catchments to make a partition of the basin
  message('clipping catchment polygons to form ', nrow(sub_sf), ' sub-catchments')
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
  
  # st_sf puts sets geometry column properly
  return(result_list = list(boundary = sub_sf |> sf::st_sf(),
                            gage_lookup = gage_lookup))
}




