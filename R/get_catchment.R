#' Return a list of NHD geometry objects modelling the catchment for an outlet
#'
#' This uses the `nhdR` package to manage ETL of NHD data collections related to
#' the catchment of the point `outlet`.  If `outlet` is a character string, it is
#' passed to `nominatim_point` to get the point from OSM.
#' 
#' The function downloads three collections from NHD (see `?get_upstream`) containing
#' geometries and attributes on stream networks, (sub)catchments, and water bodies
#' upstream of `outlet`. It returns the relevant objects in a list, including a
#' boundary polygon representing the full catchment.
#' 
#' Users who are only interested in this boundary polygon can speed things somewhat by
#' setting `fast=TRUE`. This returns the boundary along with the outlet point and its
#' COMID, and omits the more detailed data model for the stream network and sub-catchments. 
#' 
#' Note that `nhdR` uses `rappdirs` to manage persistent storage. This will set
#' up a default data storage folder on your system and, whenever possible, use cached
#' files instead of downloading things repeatedly.
#' 
#' `crs_out` can be set to NA to return everything in its original projection.
#' Otherwise the function transforms all geo-referenced output data to the supplied
#' CRS or EPSG code. If `crs_out=NULL`, the function assigns the UTM projection for the
#' zone overlying the `outlet`, 
#' 
#' The function returns a list with the following
#' 
#' * `comid` integer COMID associated with the outlet 
#' * `outlet` sfc_POINT, the outlet location
#' * `boundary` sfc_POLYGON, the catchment boundary inscribing all areas draining to `outlet`
#' 
#' If `fast=FALSE`, the returned list also includes
#' 
#' * `edge` data frame of edges in the directed graph representing stream flow in the NHD model
#' * `catchment` sf data frame with polygon geometries, the upstream (sub)catchments  
#' * `flow` sf data frame with line geometries, the upstream stream reaches (possibly artificial)
#' * `lake` sf data frame with polygon geometries, the upstream water bodies
#' 
#' All geo-referenced outputs are converted to the common projection `crs_out`. If this
#' is `NULL` (the default) the function uses the UTM zone overlying `outlet`.
#' 
#' See https://cran.r-project.org/package=nhdplusTools and the references pages linked
#' there for more on the NHDPlus data model, and how the COMID ties things together.
#'
#' @param outlet character to query OSM for location or else a point accepted by `sf::st_geometry`
#' @param crs_out output CRS, anything acceptable to `sf::st_crs` or NULL to use UTM zone of outlet
#' @param fast logical, if TRUE the `catchment` object is not returned, speeding things somewhat
#'
#' @return list
#' @export
get_catchment = function(outlet, crs_out=4326, fast=FALSE) {
  
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
  
  # download required NHD data for the VPU
  nhdR::nhd_plus_get(vpu=uid, 'NHDPlusAttributes', temporary=FALSE)
  nhdR::nhd_plus_get(vpu=uid, 'NHDPlusCatchment', temporary=FALSE)
  
  # open directed edge map 
  message('loading NHD stream network edges')
  edge = nhdR::nhd_plus_load(vpu=uid, 'NHDPlusAttributes', 'PlusFlow', quiet=TRUE)
  
  # open catchment polygons (slow)
  message('loading NHD catchment polygons')
  catch_poly = nhdR::nhd_plus_load(vpu=uid, 'NHDPlusCatchment', 'Catchment', quiet=TRUE)
  
  # simpler sub-routine when only the catchment boundary is needed
  if(fast) {
    
    # find upstream components
    message('')
    result_list = get_upstream(outlet, edge, catchment=catch_poly, fast=TRUE)
    
  } else {
  
    # download the NHDSnapshot data to get flow lines and lakes in the VPU
    nhdR::nhd_plus_get(vpu=uid, 'NHDSnapshot', temporary=FALSE)
    
    # open flow lines (slow)
    message('loading NHD stream network geometries')
    flow_line = nhdR::nhd_plus_load(vpu=uid, 'NHDSnapshot', 'NHDFlowline', quiet=TRUE)
    
    # open lake polygons
    message('loading NHD lake polygons')
    lake_poly = nhdR::nhd_plus_load(vpu=uid, 'NHDSnapshot', 'NHDWaterbody', quiet=TRUE)
    
    # find upstream components
    message('')
    result_list = get_upstream(outlet, edge, catchment=catch_poly, flow=flow_line, lake=lake_poly)
  }
  
  # set up output names and projection
  if( is.null(crs_out) ) crs_out = epsg_utm 
  if( !anyNA(crs_out) ) {
    
    nm_transform = c('outlet', 'flow', 'catchment', 'lake', 'boundary')
    nm_transform = nm_transform[ nm_transform %in% names(result_list) ]
    message('\ntransforming results to output projection') 
    result_list[nm_transform] = result_list[nm_transform] |> lapply(\(x) sf::st_transform(x, crs_out))
  }

  return(result_list)
}


#' Save the output of `get_catchment` to disk
#' 
#' When `overwrite=TRUE` the function writes 'outlet.geojson', 'catchment.geojson',
#' 'flow.geojson', 'lake.geojson', and 'boundary.geojson' (by passing the like-named
#' objects to `sf::st_write`), and when `overwrite=FALSE` the function writes nothing
#' but returns the file paths that would be written.
#' 
#' The outlet file contains the COMID as a field. All outputs are in WGS84 coordinates.
#' See `get_catchment` and `get_upstream` for details on input datasets 
#'
#' @param data_dir character path to the directory to use for output files
#' @param catch_list list returned from `get_catchment(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_catchment('/example')
save_catchment = function(data_dir, catch_list=NULL, overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(catch_list) & overwrite ) {
    
    warning('overwrite=TRUE but catch_list was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'nhd')
  
  # output filenames (outlet and edge listed first on purpose)
  dest_fname = c(outlet = 'outlet.geojson',
                 edge = 'edge.csv',
                 catchment = 'catchment.geojson',
                 flow = 'flow.geojson',
                 lake = 'lake.geojson',
                 boundary = 'boundary.geojson')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])
  
  # save COMID and outlet point in the same geoJSON object
  data.frame(comid=catch_list[['comid']]) |> 
    sf::st_sf(geometry=catch_list[['outlet']]) |>
    sf::st_transform(4326) |>
    sf::st_write(dest_path[['outlet']])
  
  # save edge data as CSV
  catch_list[['edge']] |> write.csv(row.names=FALSE, quote=FALSE)
  
  # save everything else in a distinct geoJSON
  names(dest_fname[-(1:2)]) |> lapply(\(x) {
    
    sf::st_sf(geometry=catch_list[[x]]) |>
      sf::st_transform(4326) |>
      sf::st_write(dest_path[[x]])

    })

  # return all paths
  return(dest_path)
}

#' Return a list of NHD geometry objects corresponding to the catchment for an outlet
#' 
#' This returns a list of geometries and other information describing the catchment for the
#' supplied outlet point `outlet`. It first identifies the element of `catchment` containing
#' the outlet and exhaustively traces all upstream paths by following the directed paths in
#' `edge` and collecting the corresponding elements of `catchment` and `flow`.
#' 
#' The function returns the relevant subsets of these four datasets, along with a copy
#' of `outlet`, the COMID for outlet, and a new polygon representing the boundary of
#' the entire catchment. 
#' 
#' Arguments `edge`, `catchment`, `flow`, and `lake` should all be created using calls to
#' `nhdR::nhd_plus_load` with arguments "dsn" and "component" set appropriately (see code
#' in `get_upstream` for example). Arguments `flow` and/or `lake` can be `NULL`, in which
#' case the corresponding output objects are omitted. Set `fast=TRUE` to omit all four,
#' and only return the outlet, COMID, and boundary.
#' 
#' Having encountered duplicate vertex issues in NHD data while using the S2 Geometry
#' Library (terra's default) we now include the `s2` argument to allow switching this
#' off temporarily during the function call. This causes terra to switch to the higher-
#' precision GeographicLib library for the problematic intersection calculations.
#'
#' @param outlet an sfc_POINT object locating the outlet
#' @param flow the "NHDFlowline" dataset from the "NHDSnapshot" component
#' @param catchment the "Catchment" dataset from NHD from the "NHDPlusCatchment" component
#' @param lake the "NHDWaterbody" dataset from NHD from the "NHDSnapshot" component
#' @param edge data frame, the "PlusFlow" dataset from the "NHDPlusAttributes" component
#' @param s2 logical if `FALSE` the function sets `sf::sf_use_s2(FALSE)` for computations
#' @param fast logical if `TRUE` only the outlet, COMID, and boundary computed and returned
#'
#' @return list containing appropriate subsets of flow, catchment, lake, edge (see details) 
#' @export
get_upstream = function(outlet, edge, catchment, flow=NULL, lake=NULL, s2=FALSE, fast=FALSE) {
  
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
  message('locating outlet')
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
  
  # initialize output list
  out_list = list(comid = outlet_comid,
                  outlet = outlet,
                  boundary = boundary)
  
  if(fast) return( out_list )
  
  # copy edges and catchments subsets
  is_sws_edge = edge[['TOCOMID']] %in% sws_comid
  out_list[['edge']] = edge[is_sws_edge,]
  out_list[['catchment']] = sws_poly
  
  # copy flow line subset
  if( !is.null(flow) ) {
    
    # find subset of relevant flow-lines
    is_sws_flow = flow[['COMID']] %in% sws_comid
    message('found ', paste(sum(is_sws_flow), 'stream reach(es)'))
    flow_out = flow[is_sws_flow,]
    
    # check for dangling stream reaches
    is_inside = sf::st_intersects(flow_out, boundary, sparse=FALSE) |> suppressMessages()
    out_list[['flow']] = flow_out[is_inside,]
  }
  
  # copy lakes subset
  if( !is.null(lake) ) {
    
    is_sws_lake = sf::st_intersects(lake, boundary, sparse=FALSE) |> suppressMessages()
    message('found ', sum(is_sws_lake), ' lake(s)')
    out_list[['lake']] = lake[is_sws_lake,]
  }

  return(out_list)
}
