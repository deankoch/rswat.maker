#' Split a catchment into sub-catchments with a gage point at each outlet 
#'
#' This splits the catchment in `data_dir` into a set of sub-catchments according
#' to the layout of upstream `gage` points. It returns a nested list of sub-catchment
#' geometries, one for each unique gage.
#' 
#' As in QSWAT+ delineation, the drainage areas of inlet points are clipped from
#' the downstream sub-catchment area. This results in a partition of the main outlet
#' catchment into sub-catchment polygons linked to one another by inlet/outlet points.
#' In this scheme each unique gage is the main outlet of one (and only one) of the
#' sub-catchments. A sub-catchment may have inlets, and each of these corresponds
#' to the main outlet of a different sub-catchment in the set.
#' 
#' Sub-catchments inherit the 'site_no' and 'station_nm' fields (name and ID) of
#' the row of `gage` mapping to their main outlets. 'site_no' must be unique for each
#' `gage` point. An integer 'count' field is also expected, but is only used for
#' breaking ties related to positional duplication (see below) 
#' 
#' It is possible to have multiple elements in `gage` mapping to the same NHDPlus
#' polygon (ie COMID). For example you might have two different sets of coordinates
#' for the same location due to positional errors, or simply two different gages that
#' happen to lie in the same NHDPlus polygon. Duplicate situations like these are dealt
#' with by keeping only the gage point with the highest 'count' field - ie the most
#' records. In the output, element 'gage' will include all relevant input `gage`
#' points (including duplicates), but 'outlet' and 'inlet' will have duplicates
#' removed, and only one sub-catchment will be created for each set of duplicate
#' outlets.
#' 
#' If the main outlet of the catchment is not found within snapping distance
#' `snap_main` of a point in `gage`, the function appends a new main outlet
#' point automatically. This ensures the set of output sub-catchment polygons always
#' form a partition of the whole catchment. Depending on `snap_main` and the
#' layout of the NHDPlus model in your study area, this can result in the creation of
#' an additional sub-catchment named "main outlet created by rswat".
#' 
#' The "true" outlet point for a sub-catchment under the NHD model lies at the
#' intersection of the boundary of the NHD polygon for the outlet COMID and its
#' flow line. These points are calculated and returned in `outlet` and `inlet`.
#' 
#' Note that a `gage` point (from NWIS) will often be located slightly upstream of the
#' outlet associated with its COMID (see `?get_catch`). Thus the function returns two
#' versions of the boundary polygon for a sub-catchment: `boundary` is the partition;
#' and `boundary_outer` is a copy where the NHPlus polygons for all inlets/outlets have
#' been joined to their downstream sub-catchment boundaries (introducing overlap). 
#' 
#' See also `split_catch`, which does most of the work of following flow-lines and
#' building boundaries with logical set operations.
#'
#' @param data_dir character path to the directory to use for output files
#' @param gage sf points data frame, with fields 'site_no', 'station_nm', and 'count'
#' @param snap_main numeric with units, snapping distance to set "main" outlet
#'
#' @return a list with one element per sub-catchment
#' @export
get_split = function(data_dir, 
                     gage = NULL, 
                     snap_main = units::set_units(100, m)) {
  
  # look for default USGS stream gage stations found by `get_nwis`
  if( is.null(gage) ) {
    
    gage_path = save_nwis(data_dir, 'flow_ft')['station']
    message('loading gage points in ', gage_path)
    gage = sf::st_read(gage_path, quiet=TRUE)
  }
  
  # input paths in data_dir
  outlet_path = save_catch(data_dir)['outlet']
  boundary_path = save_catch(data_dir)['boundary']
  catch_path = save_catch(data_dir)['catchment']
  edge_path = save_catch(data_dir)['edge']
  
  # local UTM projection for computations
  crs_utm = gage[1,] |> sf::st_geometry() |> get_utm() |> suppressMessages()
  gage_utm = gage |> sf::st_geometry() |> sf::st_transform(crs_utm)
  
  # load original outlet point (input to `get_catch`) and match it against `gage` points
  outlet_main = outlet_path |> sf::st_read(quiet=TRUE)
  dist_usgs = outlet_main |> sf::st_transform(crs_utm) |> sf::st_distance(gage_utm)
  is_usgs = dist_usgs < snap_main
  
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
    gage[['main_outlet']] = seq(nrow(gage)) == 1
    gage_utm = gage |> sf::st_geometry() |> sf::st_transform(crs_utm)
  }
  
  # load boundary polygon and check validity of gage
  boundary_utm = boundary_path |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)
  is_valid = gage_utm |> sf::st_intersects(boundary_utm, sparse=FALSE)
  
  # sanity checking
  msg_invalid = paste(which(!is_valid), collapse=', ')
  if( !all(is_valid) ) stop('gage point(s) at index ', msg_invalid, ' lie outside catchment boundary')
  if( !is.data.frame(gage) ) stop('gage must be a data frame, or NULL')
  
  # load sub-catchment polygons and edge data frame (as character to avoid COMID as integer)
  catch = catch_path |> sf::st_read(quiet=TRUE)
  edge = edge_path |> read.csv(colClasses='character')
  
  # run splitting subroutine 
  split_result = split_catch(gage, edge, catchment=catch)
  
  # load boundary and flow lines (slow)
  message('collecting sub-catchment features')
  flow_utm = save_catch(data_dir)['flow'] |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)
  flow_utm[['COMID']] = as.character(flow_utm[['COMID']])
  
  # compute true outlet locations and bind with metadata from nearest gage
  outlet = split_result[['boundary']] |> find_outlet(edge, flow=flow_utm)

  # load lakes
  lake_utm = save_catch(data_dir)['lake'] |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)

  # split remaining features at sub-catchments
  result_by_catch = nrow(outlet) |> seq() |> lapply(\(i) {

    # avoid slow spatial query by following the COMIDs
    comid = outlet[['comid']][i]
    
    # initialize with first immediate downstream object then add all upstream ones
    comid_check = comid_down(comid, edge, first_only=TRUE) |> c(comid_up(comid, edge))
    
    # two boundaries: "outer" includes inlet polygon(s), so it encloses "inner"
    boundary_inner_i = split_result[['boundary']][i,] |> sf::st_transform(crs_utm)
    boundary_outer_i = boundary_inner_i
    
    # initialize data frame of inlets
    inlet_i = outlet[0,]
    
    # skip on headwater catchments
    if( !outlet[['headwater']][i] ) {

      # find inlets (uses %in% instead of == to turn NA into FALSE)
      inlet_comid = outlet[['comid']][ outlet[['downstream']] %in% comid ]
      inlet_i = outlet[outlet[['comid']] %in% inlet_comid, ]
      
      # exclude all objects upstream of inlets but include the inlet objects themselves
      comid_check = comid_check[ !( comid_check %in% comid_up(inlet_comid, edge) ) ]
      comid_check = c(inlet_comid, comid_check)
      
      # append inlet polygon(s) to boundary
      poly_add = catch[ catch[['FEATUREID']] %in% inlet_comid, ] |> sf::st_transform(crs_utm)
      poly_new = poly_add |> 
        sf::st_geometry() |> 
        sf::st_make_valid() |>
        c(sf::st_geometry(boundary_outer_i)) |>
        sf::st_union() |> 
        biggest_poly()

      # overwrite the geometry in the projected boundary polygon
      sf::st_geometry(boundary_outer_i) = poly_new |> sf::st_transform(crs_utm)
    }
    
    # copy the relevant flow lines
    flow_sub = flow_utm[flow_utm[['COMID']] %in% comid_check, ]

    # filter to COMIDs that appear in flow
    catch_sub = catch[ catch[['FEATUREID']] %in% comid_check, ] |> sf::st_transform(crs_utm)
    
    # spatial filter for lakes
    is_in = sf::st_geometry(lake_utm) |> sf::st_intersection(boundary_outer_i, sparse=FALSE)
    lake_sub = lake_utm[is_in, ]
    
    # copy relevant gages and indicate inlets
    is_outlet = split_result[['gage']][['comid']] %in% comid 
    is_inlet = split_result[['gage']][['downstream']] %in% comid 
    gage_i = rbind(dplyr::mutate(split_result[['gage']][is_inlet,], inlet=TRUE),
                   dplyr::mutate(split_result[['gage']][is_outlet,], inlet=FALSE))

    # transform spatial layers to WGS84
    spatial_out = list(outlet = outlet[i,],
                       inlet = inlet_i,
                       gage = gage_i,
                       boundary = boundary_inner_i,
                       boundary_outer = boundary_outer_i,
                       lake = lake_sub,
                       flow = flow_sub,
                       catchment = catch_sub) |> lapply(\(x) sf::st_transform(x, 4326))
    
    # combine with non-spatial output
    spatial_out |> c(list(comid = comid,
                          edge = edge[ edge[['TOCOMID']] %in% flow_sub[['COMID']], ]))

  })
  
  # # use shorter GNIS names whenever they are unique
  # nm_short = tolower(sapply(result_by_catch, \(x) most_frequent(x[['flow']], 'GNIS_NAME')))
  # nm_short = gsub('[^A-z]+', '_', nm_short)
  # nm_short[ duplicated(nm_short) ] = outlet[['snail_name']][ duplicated(nm_short) ]
  
  # use file-name-friendly title for names
  return(stats::setNames(result_by_catch, outlet[['snail_name']]))
}


#' Save results of `get_split`
#' 
#' This saves the sub-catchments returned by `get_split` to sub-directories of
#' `file.path(data_dir, 'split')` with directory names copied from `names(sub_list)`.
#' The final outputs from `get_dem`, `get_land`, `get_soil`, and `get_nwis` are then
#' filtered to each sub-catchment and written to disk in a loop. A single points data
#' frame is also written to 'split/gage.geojson', mapping gages to sub-directories.
#' 
#' All raster files are cropped to the bounding box of a padded version of the
#' sub-catchment boundary joined to the inlet NHD polygon(s), if any (see `?get_split`).
#' `pad_factor` controls the level of padding in the same way as `get_dem`.
#' 
#' The function returns a list with: `gage`, the path to the mapping file; and `sub`,
#' a vector of paths to the sub-catchment directories. Pipe any of these directories to
#' `save_catch`, `save_dem`, `save_land`, `save_soil`, or `save_nwis` to get a list of the
#' files saved in each category.
#' 
#' Once 'split/gage.geojson' has been written by this function, subsequent calls to
#' `update_nwis(data_dir)` will distribute copies of relevant NWIS files to the
#' sub-catchment directories of "split" so that each one is individually also up to date.
#' 
#' `param_code` and `stat_code` select the stream gage variable to copy from the NWIS
#' dataset for the catchment, and `nwis_nm` is a name for it. These arguments have no effect
#' when listing the (parent) directory paths with `overwrite=FALSE`.
#'
#' @param data_dir character path to the directory to use for output files
#' @param sub_list list returned from `get_split`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param pad_factor numeric >= 0 sets the amount of padding to add to boundary
#' @param nwis_nm character, passed to `nwis_split`
#' @param param_code character, passed to `nwis_split`
#' @param stat_code character, passed to `nwis_split`
#' 
#' @return list of two: the gage file path and a vector of directory paths
#' @export
#'
#' @examples
#' save_split('/example')
#' save_split('/example', extra=TRUE)
save_split = function(data_dir, sub_list=NULL, overwrite=FALSE, pad_factor=1/10,
                      nwis_nm='flow_ft', param_code='00060', stat_code='00003') {
    
  # output paths
  dest_base = file.path(data_dir, 'split')
  dest_dir = list.dirs(dest_base, recursive=FALSE)
  gage_path = file.path(dest_base, 'gage.geojson')
  
  # catch invalid calls
  if( is.null(sub_list) ) {
    
    # switch to file path list mode when no data supplied
    if( overwrite ) {
      
      warning('overwrite=TRUE but sub_list was NULL')
      overwrite = FALSE
    }
    
  } else {
    
    # set destination file paths based on list names
    if( any( is.null( names(sub_list) ) ) ) stop('sub_list must be named')
    dest_dir = file.path(dest_base, names(sub_list))
  }
  
  # save to disk
  if( overwrite ) {
    
    # load lookup table and raster data needed for all iterations
    land = save_land(data_dir)['land'] |> terra::rast()
    dem = save_dem(data_dir)['dem'] |> terra::rast()
    soils = save_soil(data_dir)[['soil']]['soil'] |> terra::rast()

    # loop over sub-catchments (ie sub-directories)
    n_sub = length(sub_list)
    for(i in seq(n_sub)) {
      
      # save a copy of NHD catchment features
      message('copying sub-catchment ', i, '/', n_sub, ' : ', gsub('_', ' ', basename(dest_dir[i])))
      save_catch(dest_dir[i], sub_list[[i]], overwrite=TRUE, extra=TRUE)
      
      # use outer boundary to ensure we have the full catchment up to inlets in the DEM
      bou = sub_list[[i]][['boundary_outer']] |> sf::st_geometry()
      
      # add padding and create bounding box polygon in DEM projection
      bou_utm = bou |> sf::st_transform( sf::st_crs(dem) )
      pad_size = pad_factor * sqrt(sf::st_area(bou_utm))
      bou_pad = bou_utm |> sf::st_buffer(pad_size) |> sf::st_bbox() |> sf::st_as_sfc()

      # mask the rasters and write output to disk
      dem |> clipr(bou_pad, p = save_dem(dest_dir[i])['dem'])
      land |> clipr(bou_pad, p = save_land(dest_dir[i])['land']) 
      soils |> clipr(bou_pad, p = save_soil(dest_dir[i])[['soil']]['soil']) 
    } 
    
    # find mapping from outlet COMID to directory name
    comid = do.call(c, lapply(sub_list, \(x) x[['comid']]))

    # compile outlet gages dataset, append directory names field
    gage = do.call(rbind, lapply(sub_list, \(x) x[['gage']][ !x[['gage']][['inlet']], ] ))
    gage[['dir_name']] = names(comid)[ match(gage[['comid']], comid) ]
    row.names(gage) = NULL
    
    # write to disk
    if( file.exists(gage_path) ) unlink(gage_path)
    gage |> sf::st_sf() |> sf::write_sf(gage_path, quiet=TRUE)
    
    # copy relevant NWIS files to sub-catchments
    split_nwis(data_dir, nwis_nm, param_code=param_code, stat_code=stat_code)
  }
 
  # return all paths 
  return( list(gage=gage_path, sub=dest_dir) )
}


#' Find outlet points for a set of sub-catchments
#'
#' Helper function for `get_split`. This finds the intersection of the catchment
#' boundaries in `catch` with the flow line in `flow` corresponding to the outlet.
#' The function checks both the stream segment associated with the 'comid' field
#' and its immediate downstream neighbour.
#' 
#' The output is always in WGS84 coordinates. If there is no intersection, the
#' function snaps the endpoint of the incomplete stream segment to the boundary
#' and returns that point. If there is more than one intersection, the function
#' returns the one further downstream.
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
    crs_utm = flow[1,] |> sf::st_geometry() |> get_utm() |> suppressMessages()
    
    # include first downstream COMID
    comid_check = comid |> comid_down(edge, first_only=TRUE) |> c(comid)
    
    # build line segments of interest
    boundary = catch |> sf::st_geometry() |> sf::st_transform(crs_utm) |> sf::st_boundary()
    out_line = flow[ flow[['COMID']] %in% comid_check, ] |> 
      sf::st_geometry() |> 
      sf::st_transform(crs_utm)

    # check if the flow line intersects the boundary
    is_complete = out_line |> sf::st_union() |> sf::st_intersects(boundary, sparse=FALSE) |> c()
    out_point =  if(is_complete) { 
      
      # get point(s) of intersection in order downstream, upstream
      sf::st_intersection(out_line, boundary, sparse=FALSE) |> 
        sf::st_cast('POINT') |> head(1)

    } else {
      
      # deal with non-intersecting lines separately
      out_point = sf::st_union(out_line) |> sf::st_nearest_points(boundary) |> 
        sf::st_cast('POINT') |> tail(1)
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
#' * `gage` is an sf points data frame including a mapping to polygons in `boundary`
#' 
#' Sub-catchments in `boundary` are assigned identifiers from their associated `gage`
#' ('station_nm' and 'comid'). If multiple `gage` points lie on the same `catchment`
#' polygon, then they will share a common sub-catchment polygon in `boundary`. If this
#' happens, the function copies the name and COMID from the station with the highest
#' 'count' field (ie the most records). 
#'
#' @param gage sf points data frame, point of interest at which to split the catchment
#' @param edge data frame of flow line directed edges ("PlusFlow" from "NHDPlusAttributes")
#' @param catchment sf data frame of sub-catchment polygons ("Catchment" from "NHDPlusCatchment")
#'
#' @return list with elements 'boundary', 'gage'
#' @export
split_catch = function(gage, edge, catchment) {

  # local UTM projection for computations
  crs_utm = gage[1,] |> sf::st_geometry() |> get_utm() |> suppressMessages()
  
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
  message('resolving stream order for ', nrow(gage), ' COMIDs')
  sub_poly = do.call(c, lapply(gage_result, \(x) x[['boundary']]))
  
  # for each gage point, find nearest downstream neighbour
  downstream_list = seq(n_gage) |> sapply(\(i) {
    
    # find all downstream COMIDs (in order) and return the first nontrivial match
     data.frame(comid = comid_down(gage[['comid']][i], edge)) |>
      dplyr::filter(comid %in% gage[['comid']]) |>
      dplyr::filter(comid != gage[['comid']][i]) |>
      dplyr::pull(comid) |> 
      head(1)
  })
  
  # copy downstream key
  gage[['downstream']] = downstream_list |> sapply(\(x) ifelse(is.null(x), NA, x))

  # flag headwaters sub-catchments
  gage[['headwater']] = !( gage[['comid']] %in% gage[['downstream']] )
  
  # add name in snail case - slightly simpler (but still long) version of station_nm
  gage[['snail_name']] = gsub('[^A-z]+', '_', gage[['station_nm']], perl=TRUE) |> tolower()

  # columns to keep
  poly_nm = c('station_nm', 'snail_name', 'site_no', 'comid', 
              'downstream', 'headwater', 'main_outlet', 'geometry')
  
  # make sf data frame from polygons
  sub_sf = sf::st_sf(gage, geometry=do.call(c, lapply(gage_result, \(x) x[['boundary']]))) |>
    dplyr::arrange( dplyr::desc(count) ) |>
    dplyr::select( dplyr::all_of(poly_nm) )
  
  # omit (any) duplicate sub-catchments and re-order
  sub_sf = sub_sf[!duplicated(sub_sf[['comid']]), ]
  sub_sf = sub_sf[order(sub_sf[['main_outlet']], !sub_sf[['headwater']]), ]
  
  # message about dropped stations
  n_drop = nrow(gage) - nrow(sub_sf)
  if( n_drop > 0 ) message('removed ', n_drop, ' duplicate station site(s) from outlet list')
  
  # clip overlap from sub-catchments to make a partition of the basin
  message('clipping catchment polygons to form ', nrow(sub_sf), ' sub-catchments')
  comid = sub_sf[['comid']][ sub_sf[['main_outlet']] ]
  is_pending = sub_sf[['downstream']] %in% comid
  while( any(is_pending) ) {

    # clip areas upstream of sub-catchment inlets (temporarily use UTM projection)
    is_shrinking = sub_sf[['comid']] %in% comid
    poly_shrink = sub_sf[is_pending,] |> sf::st_transform(crs_utm) |> sf::st_union()
    poly_temp = sub_sf[is_shrinking,] |> 
      sf::st_geometry() |> 
      sf::st_transform(crs_utm) |>
      sf::st_difference(poly_shrink) |>
      biggest_poly()
    
    # overwrite geometry in sub_sf
    sf::st_geometry(sub_sf[is_shrinking,]) = poly_temp

    # update comid and list of pending sub-catchments
    comid = sub_sf[['comid']][is_pending]
    is_pending = sub_sf[['downstream']] %in% comid
  }

  # st_sf sets geometry column properly
  return(result_list = list(boundary = sf::st_sf(sub_sf), gage=sf::st_sf(gage)))
}



