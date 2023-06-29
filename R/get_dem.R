#' Wrapper for `FedData::get_ned`
#' 
#' This opens the boundary polygon created by `get_catch` and passes (a
#' buffered version of) it to `FedData::get_ned`, returning the result as 
#' SpatRaster (instead of RasterLayer)
#' 
#' The buffer size default is 10% of the square root of the polygon area. For
#' a square domain this is equal to 1/10 of the side length.
#' 
#' Note that `FedData` downloads tiles to a temporary folder so I don't think
#' we can cache them easily for later use. Save a copy with `save_dem`. Later on 
#' you can open an existing copy by passing the file path `get_dem(data_dir)['dem_src']`
#' to `terra::rast()`
#'
#' @param data_dir character path to the directory to use for output files
#' @param pad_size units object, the width of padding (or NULL to set default)
#'
#' @return SpatRaster of elevation data for the catchment area
#' @export
get_dem = function(data_dir, pad_size=NULL) {
  
  # paths to catchment boundary and outlet point from `get_catch`
  boundary_path = save_catch(data_dir, overwrite=FALSE)[['boundary']]
  outlet_path = save_catch(data_dir, overwrite=FALSE)[['boundary']]
  
  # load outlet to find appropriate UTM zone for computations
  crs_utm = outlet_path |> sf::st_read(quiet=TRUE) |> to_utm() |> suppressMessages()
  
  # sanity check 
  msg_error = paste('missing boundary polygon:', boundary_path)
  msg_suggestion = 'Have you run `get_catch()` yet?'
  if( !file.exists(boundary_path) ) stop(msg_error, '\n', msg_suggestion)
  
  # load boundary in UTM projection and set default padding 
  boundary_utm = boundary_path |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)
  if( is.null(pad_size) ) pad_size = 1e-1 * sqrt(sf::st_area(boundary_utm)) 
  
  # add padding and transform back
  boundary_pad = boundary_utm |> sf::st_buffer(pad_size) |> sf::st_transform(4326)
  
  # fetch NED tiles (default 1 arc-second) and merge into one SpatRaster
  FedData::get_ned(template = boundary_pad, label = basename(data_dir))
  
  # second call to ensure we get the values returned
  FedData::get_ned(template = boundary_pad, label = basename(data_dir)) |> terra::rast()
}

#' Save the output of `get_dem` to disk
#' 
#' When `overwrite=TRUE` the function writes a bounding box polygon, 'bbox.geojson', and three
#' copies of the DEM: 'ned_dem.tif', the unchanged source DEM; 'dem.tif', a version warped
#' to the UTM zone of the main outlet (by bilinear averaging) with resolution `res` X `res`;
#' and 'dem_burn.tif', a copy the UTM raster with stream reaches burned to depth `burn` (meters).
#' 
#' Burn-in refers to artificially reducing the elevation in the DEM under known stream reaches.
#' This is to assist the TauDEM algorithm (used in QSWAT+) in finding the correct routing
#' network based on the DEM alone. The function creates stream reaches by expanding flow lines
#' to form channel polygons of width `buffer` (the pixel width by default). It then uses
#' `terra::rasterizes` to reduce the value of any DEM pixel that overlaps with a channel by
#' the fixed value `burn`.
#' 
#' The function selects an output UTM zone for the two (non-source) rasters based on the location
#' of the main outlet point. The bounding box is always in WGS84 coordinates.
#' 
#' Note that `terra::project` can be slow with large rasters. The default `threads=TRUE` is
#' the faster option. Higher `res` will also speed computation, but results in less detail in
#' (SWAT+) HRUs and stream networks, and less precision in land use and soil type assignment.
#'
#' @param data_dir character path to the directory to use for output files
#' @param catch_list list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param res integer > 0, the resolution in metres (point spacing in both directions)
#' @param buffer numeric > 0, padding distance in metres (for masking to boundary)
#' @param burn numeric >= 0, burn-in depth for flow lines in meters
#' @param threads logical, passed to `terra::project`
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_dem('/example')
save_dem = function(data_dir, dem=NULL, overwrite=FALSE, res=90, buffer=res, burn=50, threads=TRUE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(dem) & overwrite ) {
    
    warning('overwrite=TRUE but dem was NULL')
    overwrite = FALSE
  }
  
  # output directory and file names
  dest_dir = file.path(data_dir, 'ned')
  dest_fname = c(dem_src = 'ned_dem.tif',
                 dem='dem.tif',
                 dem_burn='dem_burn.tif',
                 bbox = 'bbox.geojson')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  
  # make sure the DEM is in RAM
  message('copying raster data to RAM')
  dem = dem + 0
  
  # remove any existing output files then write a fresh copy of the source raster
  if( any(is_over) ) unlink(dest_path[is_over])
  dem |> terra::writeRaster(dest_path[['dem_src']])
  
  # save DEM bounding box of source
  dem_bbox = sf::st_bbox(dem) |> sf::st_as_sfc() |> sf::st_transform(4326) 
  dem_bbox |> sf::st_write(dest_path[['bbox']], quiet=TRUE)
    
  # open files generated by `save_catch`
  flow = save_catch(data_dir)['flow'] |> sf::st_read(quiet=TRUE) 
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) 
  outlet = save_catch(data_dir)['outlet'] |> sf::st_read(quiet=TRUE) 
  
  # two version of boundary for masking, one padded
  message('masking and cropping')
  crs_out = to_utm(outlet) |> suppressMessages()
  boundary_out = sf::st_transform(boundary, crs_out)
  boundary_pad = boundary_out |> 
    sf::st_buffer(units::set_units(buffer, m)) |>
    sf::st_transform(sf::st_crs(dem))
  
  # make a copy of DEM masked and cropped to padded boundary
  dem_mask = dem |> terra::crop(boundary_pad) |> terra::mask(boundary_pad)
  
  # make a template output raster in outlet UTM zone (coerce via sp object)
  rast_out = dem_bbox |> 
    sf::st_transform(crs_out) |>
    as('Spatial') |> 
    as('SpatVector') |> 
    terra::rast(resolution=res)
  
  # warp the DEM to this template then crop/mask to tight boundary 
  message('warping')
  dem_out = dem_mask |> 
    terra::project(rast_out, threads=threads) |> 
    terra::crop(boundary_out) |> 
    terra::mask(boundary_out)

  # save new DEM data
  dem_out |> terra::writeRaster(dest_path[['dem']])
  
  # expand flow lines with buffer value then rasterize to DEM grid
  flow_out = flow |> sf::st_geometry() |> sf::st_transform(crs_out) |> sf::st_buffer(buffer)
  burn_out = data.frame(burn=burn) |> 
    sf::st_sf(geometry=flow_out) |> 
    terra::rasterize(dem_out,
                     field = 'burn',
                     fun = 'min',
                     touches = TRUE)
  
  # make a burn-in version DEM and save to disk
  dem_burn = dem_out
  dem_burn[ !is.na(burn_out) ] = dem_burn[ !is.na(burn_out) ] - burn
  dem_burn |> terra::writeRaster(dest_path[['dem_burn']])
  return(dest_path)
}
