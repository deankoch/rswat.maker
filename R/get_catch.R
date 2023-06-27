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
#' * `comid` integer  associated with the outlet 
#' * `outlet` sf points data frame, the outlet point and some metadata
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
get_catch = function(outlet, crs_out=4326, fast=FALSE) {
  
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
  
  # required NHD data for the VPU
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
    
    # bug fix for variation in COMID field name
    if( 'ComID' %in% names(flow_line) ) names(flow_line)[ names(flow_line) == 'ComID'] = 'COMID'
    
    # open lake polygons
    message('loading NHD lake polygons')
    lake_poly = nhdR::nhd_plus_load(vpu=uid, 'NHDSnapshot', 'NHDWaterbody', quiet=TRUE)

    # find upstream components
    message('')
    result_list = get_upstream(outlet, edge, catchment=catch_poly, flow=flow_line, lake=lake_poly)
  }
  
  # outlet metadata for later use
  outlet_df = data.frame(comid = result_list[['comid']],
                         main_outlet = TRUE,
                         count = 0,
                         station_nm = 'main') 
  
  # copy outlet point along with some metadata for later use
  station_nm = 'main outlet created by rswat'
  result_list[['outlet']] = outlet_df |> 
    sf::st_sf(geometry=result_list[['outlet']])
  
  # set up output names and projection
  if( is.null(crs_out) ) crs_out = epsg_utm 
  if( !anyNA(crs_out) ) {
    
    nm_transform = c('outlet', 'flow', 'catchment', 'lake', 'boundary')
    nm_transform = nm_transform[ nm_transform %in% names(result_list) ]
    message('transforming to output projection') 
    result_list[nm_transform] = result_list[nm_transform] |> lapply(\(x) sf::st_transform(x, crs_out))
  }

  return(result_list)
}


#' Save the output of `get_catch` or `get_split` to disk
#' 
#' When `overwrite=TRUE` the function writes 'outlet.geojson', 'catchment.geojson',
#' 'flow.geojson', 'lake.geojson', and 'boundary.geojson' (by passing the like-named
#' objects to `sf::st_write`), and if `extra=TRUE`, `inlet.geojson`, `gage.geojson`,
#' and `boundary_outer.geojson` (from the output of `get_split`). 
#' 
#' When `overwrite=FALSE` the function writes nothing but returns the file paths that
#' would be written. All outputs are in WGS84 coordinates.
#' 
#' See `get_catch` (and its helper `get_upstream`) and `get_split` (and its helper
#' `split_catch`) for more details on input datasets.
#'
#' @param data_dir character path to the directory to use for output files
#' @param catch_list list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param extra logical, if `TRUE` the function includes additional files used with `get_split`
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_catch('/example')
#' save_catch('/example', extra=TRUE)
save_catch = function(data_dir, catch_list=NULL, overwrite=FALSE, extra=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(catch_list) & overwrite ) {
    
    warning('overwrite=TRUE but catch_list was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'nhd')
  
  # output filenames (edge listed first on purpose)
  dest_fname = c(edge = 'edge.csv',
                 outlet = 'outlet.geojson',
                 catchment = 'catchment.geojson',
                 flow = 'flow.geojson',
                 lake = 'lake.geojson',
                 boundary = 'boundary.geojson')
  
  # extra filenames when saving results of `get_split`
  extra_fname = c(inlet = 'inlet.geojson',
                  gage = 'gage.geojson',
                  boundary_outer = 'boundary_outer.geojson')
                 
  # output paths
  if(extra) dest_fname = c(dest_fname, extra_fname)
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])
  
  # save edge data as CSV
  catch_list[['edge']] |> write.csv(dest_path[['edge']], row.names=FALSE, quote=FALSE)
  
  # save everything else in geoJSON files
  for( nm in names(dest_fname[-1]) ) {
    
    # check for unexpectedly empty list element
    x_out = catch_list[[nm]]
    if( is.null(x_out) ) { warning('nothing to write in element: ', nm) } else {
    
      # write the geometry
      x_out |> sf::st_transform(4326) |> sf::st_write(dest_path[[nm]], quiet=TRUE)
    }
  }

  # return all paths
  return(dest_path)
}


#' Open catchment model geometries saved by `save_catch` or `save_split`
#' 
#' This opens the catchment data in `data_dir`, returning a list of data frames
#' and geometries.  If `sub=TRUE` the function loads the sub-catchments in
#' sub-directory "split" of `data_dir`. If `data_dir` is a vector, the function
#' ignores `sub` and returns a nested list with results from each of the paths.
#' 
#' `extra` refers to the files 'boundary_outer.geojson', 'inlet.geojson', 'gage.geojson'.
#' For now, these are only written by `save_split` and not `save_catch` (since initial
#' catchment delineation is for the entire basin). With default `extra=NULL` the function
#' loads the extra files only if they all exist. The function will otherwise proceed with
#' some/all files missing and simply omit the missing object (with a warning) or, in the
#' case of `edge`, returning a `NULL` in its place
#' 
#' See `?get_catch` and `?get_split` for more on the individual output objects
#'
#' @param data_dir character path to the output files directory
#' @param extra logical, if `TRUE` the function includes additional files used with `get_split
#' @param sub logical, if `TRUE` the function opens sub-catchments in "split/"
#'
#' @return a list with elements 'edge', 'outlet', 'catchment', 'flow', 'lake', 'boundary'
#' @export
open_catch = function(data_dir, extra=NULL, sub=FALSE) {
  
  # vectorized call returns a list (and ignores `sub`)
  if( length(data_dir) > 1 ) return( lapply(data_dir, \(d) open_catch(d, extra=extra)) )
  
  # check for sub-catchments if requested
  if( sub ) {
   
    # if none found, we just continue as if `sub=FALSE`
    sub_dir = save_split(data_dir)[['sub']]
    if( length(sub_dir) > 0 ) return( lapply(\(d) open_catch(d, extra=extra)) )
  }
  
  # set default extra 
  if( is.null(extra) ) extra = save_catch(data_dir, extra=TRUE) |> file.exists() |> all() 
  
  # sanity check
  path_open = save_catch(data_dir, extra=extra)
  path_missing = !file.exists(path_open)
  if( any(path_missing) ) warning('missing file(s): ', paste(path_open[path_missing], collapse='\n'))
  
  # load non-geometry separately
  is_edge = names(path_open) == 'edge'
  edge = if( !any(is_edge) ) NULL else path_open[is_edge] |> read.csv()
  
  # return everything in one list
  is_pending = !is_edge & !path_missing
  if( !any(is_pending) ) return( list(edge=edge) )
  list(edge=edge) |> c(lapply(path_open[is_pending], \(x) sf::st_read(x, quiet=TRUE)))
}

#' Return a list of NHD geometry objects corresponding to the catchment for an outlet
#' 
#' This returns a list of geometries and other information describing the catchment for the
#' supplied `outlet` point. It first identifies the element of `catchment` containing
#' the outlet and exhaustively traces all upstream paths by following the directed paths in
#' `edge` and collecting the corresponding elements of `catchment` and `flow`.
#' 
#' The function returns the relevant subsets of `lake`, `edge`, `catchment` and `flow`, along
#' with a copy of `outlet`, the COMID for `outlet`, and a new polygon representing the boundary
#' of the entire catchment.
#' 
#' Arguments `edge`, `catchment`, `flow`, and `lake` should all be created using calls to
#' `nhdR::nhd_plus_load` with "dsn" and "component" set appropriately. This is
#' done automatically in `get_catch`. Arguments `flow` and/or `lake` can be `NULL`, in which
#' case the corresponding output objects are omitted. Set `fast=TRUE` to omit everything
#' except the outlet, COMID, and boundary.
#' 
#' It is assumed that the basin for `outlet` lies entirely within a single NHD Vector
#' Processing Unit (VPU). These are based on the 4-digit Hydrologic Unit Code (HUC) system,
#' which delineates watersheds on a large scale, effectively limiting the size of the catchment
#' that can be queried with this function. 
#' 
#' There can be many thousands of small catchment polygons within a VPU, so finding
#' the one that intersects with the outlet is slow. To speed things up, we avoid projecting
#' the VPU-level data and do the intersection in geographical coordinates. This can lead to
#' duplicate vertex errors in S2 (terra's default), so we temporarily switch off S2 for this
#' step and use the GeographicLib library instead. All other spatial set operations are done
#' in UTM coordinates, and the result is returned in the coordinate system of the input.
#'
#' @param outlet an sfc_POINT object locating the outlet
#' @param flow the "NHDFlowline" dataset from the "NHDSnapshot" component
#' @param catchment the "Catchment" dataset from NHD from the "NHDPlusCatchment" component
#' @param lake the "NHDWaterbody" dataset from NHD from the "NHDSnapshot" component
#' @param edge data frame, the "PlusFlow" dataset from the "NHDPlusAttributes" component
#' @param fast logical if `TRUE` only the outlet, COMID, and boundary computed and returned
#'
#' @return list containing appropriate subsets of flow, catchment, lake, edge (see details) 
#' @export
get_upstream = function(outlet, edge, catchment, flow=NULL, lake=NULL, fast=FALSE) {

  # project to UTM for computations
  crs_in = sf::st_crs(catchment)
  crs_utm = outlet |> sf::st_geometry() |> to_utm() |> suppressMessages()
  outlet_utm = outlet |> sf::st_transform(crs_utm)
  
  # turn off spherical approximation for intersection with large set of polygons
  s2 = sf::sf_use_s2()
  sf::sf_use_s2(FALSE) |> suppressMessages()

  # find the (sub)catchment COMID for our outlet point
  message('locating outlet')
  covers_catch = catchment |> 
    sf::st_geometry() |> 
    sf::st_intersects(sf::st_transform(outlet, crs_in), sparse=FALSE) |> 
    suppressMessages()
  
  # switch spherical approximation back on
  sf::sf_use_s2(s2) |> suppressMessages()

  # report the COMID for the point
  if( sum(covers_catch) == 0 ) stop('outlet_point lies outside of all known catchments')
  outlet_comid = catchment[['FEATUREID']][covers_catch][1]
  message('outlet COMID: ', outlet_comid) 
  
  # find upstream COMIDs and sub-catchment polygons
  message('following upstream tributaries')
  sws_comid = comid_up(outlet_comid, edge)
  sws_poly = catchment[catchment[['FEATUREID']] %in% sws_comid,] #|> sf::st_make_valid()
  
  # boundary polygon from union of associated sub-catchments - computations can be slow
  message('merging ', nrow(sws_poly), ' sub-catchment polygons')
  sws_poly_union = sws_poly |> 
    sf::st_geometry() |> 
    sf::st_transform(crs_utm) |>
    sf::st_union() |>
    sf::st_cast('POLYGON')
  
  # cleaned up version representing sub-watershed boundary
  boundary_utm = sws_poly_union[[1]][[1]] |> 
    list() |> 
    sf::st_polygon() |> 
    sf::st_sfc(crs=crs_utm)
  
  # initialize output list
  boundary = boundary_utm |> sf::st_transform(crs_in)
  out_list = list(comid = as.character(outlet_comid),
                  outlet = sf::st_transform(outlet, crs_utm),
                  boundary = boundary)
  
  # line break in messages for clarity
  if(fast) {
    
    message('')
    return(out_list)
  }
  
  # copy edges and catchments subsets
  is_sws_edge = edge[['TOCOMID']] %in% sws_comid
  out_list[['edge']] = edge[is_sws_edge,]
  out_list[['catchment']] = sws_poly
  
  # copy flow line subset
  if( !is.null(flow) ) {
    
    # find subset of relevant flow-lines
    is_sws_flow = flow[['COMID']] %in% sws_comid
    message('processing ', paste(sum(is_sws_flow), 'stream reach(es)'))
    out_list[['flow']] = flow[is_sws_flow,] |> sf::st_transform(crs_in)
  }
  
  # copy lakes subset
  if( !is.null(lake) ) {
    
    is_sws_lake = lake |> sf::st_transform(crs_utm) |> sf::st_intersects(boundary_utm, sparse=FALSE)
    message('processing ', sum(is_sws_lake), ' lake(s)')
    out_list[['lake']] = lake[is_sws_lake,] |> sf::st_transform(crs_in)
  }
  
  return(out_list)
}

