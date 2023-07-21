#' Wrapper for `FedData::get_ned`
#' 
#' This opens the boundary polygon created by `get_catch` and passes (a
#' buffered version of) it to `FedData::get_ned`, returning the result as 
#' SpatRaster (instead of RasterLayer)
#' 
#' To establish a bounding box for the request, the NHD catchment boundary is
#' extended outward by a distance of `pad_factor` times the the square root of
#' the polygon area. For a square domain this is about equal to the side length.
#' The padding is to ensure we can later find all parts of the catchment through
#' a DEM analysis (in QSWAT+) - parts that might otherwise be masked due to
#' to slight inaccuracies in NHD boundaries.
#' 
#' Note that `FedData` downloads tiles to a temporary folder so I don't think
#' we can cache them easily for later use. Save a copy with `save_dem`. Later on, 
#' open an existing copy by passing the file path `get_dem(data_dir)['dem_src']`
#' to `terra::rast()`
#'
#' @param data_dir character path to the directory to use for output files
#' @param pad_factor numeric >= 0 sets the amount of padding to add to boundary
#'
#' @return SpatRaster of elevation data for the catchment area
#' @export
get_dem = function(data_dir, pad_factor=1/10) {
  
  # paths to catchment boundary and outlet point from `get_catch`
  boundary_path = save_catch(data_dir, overwrite=FALSE)[['boundary']]
  outlet_path = save_catch(data_dir, overwrite=FALSE)[['boundary']]
  
  # load outlet to find appropriate UTM zone for computations
  crs_utm = outlet_path |> sf::st_read(quiet=TRUE) |> get_utm() |> suppressMessages()
  
  # sanity check 
  msg_error = paste('missing boundary polygon:', boundary_path)
  msg_suggestion = 'Have you run `get_catch()` yet?'
  if( !file.exists(boundary_path) ) stop(msg_error, '\n', msg_suggestion)
  
  # load boundary in UTM projection and set default padding 
  boundary_utm = boundary_path |> sf::st_read(quiet=TRUE) |> sf::st_transform(crs_utm)
  pad_size = pad_factor * sqrt(sf::st_area(boundary_utm)) 
  
  # add padding and transform back
  boundary_pad = boundary_utm |> sf::st_buffer(pad_size) |> sf::st_transform(4326)
  
  # fetch NED tiles (default 1 arc-second) and merge into one SpatRaster
  FedData::get_ned(template = boundary_pad, label = basename(data_dir))
  
  # second call to ensure we get the values returned
  FedData::get_ned(template = boundary_pad, label = basename(data_dir)) |> terra::rast()
}

#' Save the output of `get_dem` to disk
#' 
#' When `overwrite=TRUE` the function writes a bounding box polygon and two
#' copies of the DEM raster. When `overwrite=FALSE` the function writes nothing
#' but returns the file paths that would be written.
#' 
#' The function writes its output to the 'ned' sub-directory of `data_dir`: 
#' 
#' * 'bbox.geojson', bounding box is always saved in WGS84 coordinates
#' * 'ned_dem.tif', the unchanged source DEM
#' * 'dem.tif', a version warped to UTM (and optionally masked/cropped to the catchment)
#' 
#' The function selects the UTM zone based on the location of the main outlet point. The UTM
#' grid has resolution `res` X `res`, and warping is by nearest neighbour to better preserve
#' extreme values (vs bilinear averaging).
#' 
#' Masking is controlled by `buffer`. Set it to zero to mask and crop to the catchment
#' boundary. Increase it to extend this boundary outwards, or set it to `Inf` not mask
#' or crop (the default). We recommend not masking as it can produce unpredictable behaviour
#' from QSWAT+ delineation (with TauDEM).
#' 
#' See the QSWAT+ manual for a discussion of masking pros/cons.
#' 
#' `terra::project`, which uses GDAL, can be slow with large rasters. The default `threads=TRUE`
#' is the faster option. Higher `res` will also speed computation, but results in less detail in
#' (SWAT+) HRUs and stream networks, less precision in land use and soil type assignment
#' down the line, and a higher risk of failure in the QSWAT+ channel delineation.
#'
#' @param data_dir character path to the directory to use for output files
#' @param catch_list list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param res integer > 0, the resolution in metres (point spacing in both directions)
#' @param buffer numeric > 0, padding distance in metres, for masking
#' @param threads logical, passed to `terra::project`
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_dem('/example')
save_dem = function(data_dir, dem=NULL, overwrite=FALSE, res=90, buffer=Inf, threads=TRUE) {

  # catch invalid calls and switch to file list mode
  if( is.null(dem) & overwrite ) {
    
    warning('overwrite=TRUE but dem was NULL')
    overwrite = FALSE
  }
  
  # output directory and file names
  dest_dir = file.path(data_dir, 'ned')
  dest_fname = c(dem_src = 'ned_dem.tif',
                 dem='dem.tif',
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
  
  # save DEM bounding box of source in WGS84 (used later to set extents for soils, land)
  dem_bbox = sf::st_bbox(dem) |> sf::st_as_sfc()
  dem_bbox |> sf::st_transform(4326) |> sf::st_write(dest_path[['bbox']], quiet=TRUE)
    
  # open files generated by `save_catch`
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) 
  outlet = save_catch(data_dir)['outlet'] |> sf::st_read(quiet=TRUE) 
  
  # make a template output raster in outlet UTM zone (coerced via sp object)
  crs_out = get_utm(outlet) |> suppressMessages() 
  rast_out = dem_bbox |> 
    sf::st_transform(crs_out) |>
    as('Spatial') |> 
    as('SpatVector') |> 
    terra::rast(resolution=res)
  
  # mask and crop DEM to padded boundary to reduce CPU time for warp
  dem_mask = dem |> clip_raster(boundary, buffer=buffer)
  
  # warp the DEM to the template then crop/mask again, in UTM this time
  message('projecting to UTM (GDAL warp)')
  dem_out = dem_mask |> 
    terra::project(rast_out, method='near', threads=threads) |> 
    clip_raster(boundary, buffer=buffer)
  
  # save new DEM data
  dem_out |> terra::writeRaster(dest_path[['dem']])
  return(dest_path)
}
