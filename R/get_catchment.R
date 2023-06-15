#' Return a list of geometry objects describing watershed upstream
#'
#' This uses the `nhdR` package to manage downloads of NHD data collections
#' related to the watershed upstream of the point `outlet`.  If `outlet` is
#' a character string instead, it is passed to `nominatim_point` to get the point
#' from OSM.
#' 
#' The function downloads three collections (see `?get_upstream`) from NHD
#' containing highly detailed information on sub-catchments and stream reaches,
#' which is opened and processed to determine the catchment for `outlet`.
#' 
#' Note that `nhdR` uses `rappdirs` to manage persistent storage. This will set
#' up a default data storage folder on your system and use cached files instead
#' of downloading repeatedly whenever possible.
#' 
#' The function returns a list with the following
#' 
#' * `outlet`
#' * `flow`
#' * `catchment`
#' * `lake`
#' * `edge`
#' * `boundary`
#' * `comid`
#'
#' All geo-referenced outputs are converted to the common projection `crs_out`. If this
#' is `NULL` (the default) the function uses the UTM zone overlying `outlet`.
#'
#' @param outlet character to query OSM for location or else a point accepted by `sf::st_geometry`
#' @param crs_out output CRS, anything acceptable to `sf::st_crs` or NULL to use UTM zone of outlet
#'
#' @return list
#' @export
get_catchment = function(outlet, crs_out=NULL) {
  
  # look up locations for character input and/or convert to WGS84 to match NHD data
  if( is.character(outlet) ) outlet = nominatim_point(outlet)
  outlet = outlet |> sf::st_geometry() |> sf::st_transform(4326)
  
  # fetch VPU polygons identifying regions
  vpu_poly = nhdR::vpu_shp |> dplyr::filter(UnitType == 'VPU')
  
  # identify the VPU containing the outlet (use projected coordinates to avoid warning)
  epsg_utm = to_utm(outlet) |> sf::st_crs()
  outlet_utm =  outlet |> sf::st_transform(epsg_utm)
  covers_vpu = vpu_poly |> sf::st_transform(epsg_utm) |> sf::st_intersects(outlet_utm, sparse=FALSE)
  if( sum(covers_vpu) == 0 ) stop('outlet_point lies outside of all known VPUs')
  
  # print long name and copy short name
  vpu_select = vpu_poly[which(covers_vpu)[1], ]
  uid = vpu_select[['UnitID']]
  message('VPU name: ', vpu_select[['UnitName']], ' (', uid, ')\n') 
  
  ## fetch data
  
  # download the NHDPlus data for this VPU
  nhdR::nhd_plus_get(vpu=uid, 'NHDSnapshot', temporary=FALSE)
  nhdR::nhd_plus_get(vpu=uid, 'NHDPlusAttributes', temporary=FALSE)
  nhdR::nhd_plus_get(vpu=uid, 'NHDPlusCatchment', temporary=FALSE)
  
  # load relevant datasets into R
  message('loading NHD stream network geometries')
  flow_line = nhdR::nhd_plus_load(vpu=uid, 'NHDSnapshot', 'NHDFlowline', quiet=TRUE)
  
  message('loading NHD stream network edges')
  flow_df = nhdR::nhd_plus_load(vpu=uid, 'NHDPlusAttributes', 'PlusFlow', quiet=TRUE)
  
  message('loading NHD lake polygons')
  lake_poly = nhdR::nhd_plus_load(vpu=uid, 'NHDSnapshot', 'NHDWaterbody', quiet=TRUE)
  
  message('loading NHD catchment polygons')
  catch_poly = nhdR::nhd_plus_load(vpu=uid, 'NHDPlusCatchment', 'Catchment', quiet=TRUE)
  
  # find upstream components
  message('\n')
  result_list = get_upstream(outlet, flow_line, catchment=catch_poly, lake=lake_poly, edge=flow_df)
  
  # set up output CRS
  message('\ntransforming results to output projection') 
  if( is.null(crs_out) ) crs_out = epsg_utm 
  nm_transform = c('outlet', 'flow', 'catchment', 'lake', 'boundary')
  result_list[nm_transform] = result_list[nm_transform] |> lapply(\(x) sf::st_transform(x, crs_out))
  
  # check for dangling stream reaches
  is_inside = result_list[['flow']] |> sf::st_intersects(result_list[['boundary']], sparse=FALSE) 
  result_list[['flow']] = result_list[['flow']][is_inside,]
  return(result_list)
}


#' Return a list of NHD geometry objects corresponding to the catchment for an outlet
#' 
#' This returns a list of flow lines and sub-catchments within the catchment for the
#' supplied outlet point `outlet`. It first identifies the element of `catchment` containing
#' the outlet and exhaustively traces all upstream paths by following the directed paths in
#' `edge` and collecting the corresponding elements of `catchment` and `flow`.
#' 
#' Arguments `flow`, `catchment`, `lake` and `edge` should all be created using calls to
#' `nhdR::nhd_plus_load` with arguments "dsn" and "component" set appropriately (see code
#' in `get_upstream` for example).
#' 
#' The function returns the relevant subsets of these four datasets, along with a copy
#' of `outlet`, the COMID for outlet, and a new polygon representing the boundary of
#' the entire catchment.
#' 
#' Having encountered duplicate vertex issues in NHD data while using the S2 Geometry
#' Library - terra's default - we now include the `s2` argument to allow switching this
#' off (temporarily) so that the GeographicLib library is used instead for intersection
#' calculations.
#'
#' @param outlet an sfc_POINT object locating the outlet
#' @param flow the "NHDFlowline" dataset from the "NHDSnapshot" component
#' @param catchment the "Catchment" dataset from NHD from the "NHDPlusCatchment" component
#' @param lake the "NHDWaterbody" dataset from NHD from the "NHDSnapshot" component
#' @param edge data frame, the "PlusFlow" dataset from the "NHDPlusAttributes" component
#' @param s2 logical if `FALSE` the function sets `sf::sf_use_s2(FALSE)` for computations
#'
#' @return list containing appropriate subsets of flow, catchment, lake, edge (see details) 
#' @export
get_upstream = function(outlet, flow, catchment, lake, edge, s2=FALSE) {
  
  # follows the directed edges in `edge` to find the components of `flow` and `catchment`
  # that drain into `outlet`. These are returned in a list of geometries and data frames,
  # where each entry represents a complete catchment.

  # turn off spherical approximation
  if(!s2) {
    
    # for less chance of failure with corrupt geometries
    if( sf::sf_use_s2() ) on.exit( sf::sf_use_s2(TRUE) |> suppressMessages() )
    sf::sf_use_s2(FALSE) |> suppressMessages()
  }
  
  # transform outlet coordinates to match catchments polygons
  catch_crs = sf::st_crs(catchment)
  outlet_on_catch = sf::st_transform(outlet, catch_crs)
  
  # find the (sub)catchment COMID for our outlet point
  message('identifying outlet sub-catchment')
  covers_catch = catchment |> 
    sf::st_geometry() |> 
    sf::st_intersects(outlet_on_catch, sparse=FALSE) |> 
    suppressMessages()
  
  # report the COMID for the point
  if( sum(covers_catch) == 0 ) stop('outlet_point lies outside of all known catchments')
  outlet_comid = catchment[['FEATUREID']][covers_catch][1]
  message('outlet COMID: ', outlet_comid) 
  
  # follow all upstream edges in a loop
  id_mat = edge[c('FROMCOMID', 'TOCOMID')] |> as.list()
  comid_caught = outlet_comid
  any_upstream = TRUE
  message('following upstream tributaries')
  while( any_upstream ) {
    
    # look for upstream edges (exclude headwater nodes with code 0)
    is_caught = id_mat[['TOCOMID']] %in% comid_caught[comid_caught != 0]
    
    # update exit condition
    any_upstream = any(is_caught)
    if(any_upstream) {
      
      # add new id(s) to the list, checking if any are new
      id_new = as.numeric(id_mat[['FROMCOMID']][is_caught])
      any_upstream = !all(id_new %in% comid_caught)
      comid_caught = c(comid_caught, id_new)
    }
  }
  
  # vector of all COMID codes found upstream of outlet
  sws_comid = comid_caught[comid_caught != 0]
  sws_poly = catchment[catchment[['FEATUREID']] %in% sws_comid,] |> sf::st_make_valid()
  
  # find subset of relevant edge data and flow-lines
  is_sws_edge = edge[['TOCOMID']] %in% sws_comid
  is_sws_flow = flow[['COMID']] %in% sws_comid
  message('found ', paste(sum(is_sws_flow), 'stream reach(es)'))

  # boundary polygon from union of associated sub-catchments - computations can be slow
  message('merging ', length(sws_comid), ' sub-catchment polygons')
  sws_poly_union = sws_poly |> 
    sf::st_geometry() |>  
    sf::st_union() |>
    sf::st_cast('POLYGON') |> 
    suppressMessages()

  # cleaned up version representing sub-watershed boundary
  boundary = sws_poly_union[[1]][[1]] |> 
    list() |> 
    sf::st_polygon() |> 
    sf::st_sfc(crs=catch_crs) |> 
    suppressMessages()
  
  # find lakes
  is_sws_lake = sf::st_intersects(lake, boundary, sparse=FALSE) |> suppressMessages()
  message('found ', sum(is_sws_lake), ' lake(s)')
  
  # copy relevant subsets to output list
  return(list(outlet = outlet,
              flow = flow[which(is_sws_flow)[!is_dangling],],
              catchment = sws_poly,
              lake = lake[is_sws_lake,],
              edge = edge[is_sws_edge,],
              boundary = boundary,
              comid = outlet_comid))
}
