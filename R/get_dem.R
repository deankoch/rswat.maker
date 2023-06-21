#' Wrapper for `FedData::get_ned`
#' 
#' This opens the boundary polygon created by `get_catch` and passes (a
#' buffered version of) it to `FedData::get_ned`. 
#' 
#' The buffer size default is 10% of the square root of the polygon area. For
#' a square domain this is equal to 1/10 of the side length.
#' 
#' Note that `FedData` downloads tiles to a temporary folder so I don't think
#' we can cache them easily for later use. Save a copy with `save_dem`
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
}

#' Save the output of `get_dem` to disk
#' 
#' When `overwrite=TRUE` the function writes 'outlet.geojson', 'catchment.geojson',
#' 'flow.geojson', 'lake.geojson', and 'boundary.geojson' (by passing the like-named
#' objects to `sf::st_write`), and when `overwrite=FALSE` the function writes nothing
#' but returns the file paths that would be written.
#' 
#' The outlet file contains the COMID as a field. All outputs are in WGS84 coordinates.
#' See `get_catch` and `get_upstream` for details on input datasets 
#'
#' @param data_dir character path to the directory to use for output files
#' @param catch_list list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_dem('/example')
save_dem = function(data_dir, dem=NULL, overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(dem) & overwrite ) {
    
    warning('overwrite=TRUE but dem was NULL')
    overwrite = FALSE
  }
  
  # output directory and file names
  dest_dir = file.path(data_dir, 'ned')
  dest_fname = c(dem = 'ned_dem.tif', bbox = 'ned_bbox.geojson')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])
  
  # save dem bounding box
  sf::st_bbox(dem) |> 
    sf::st_as_sfc() |> 
    sf::st_transform(4326) |> 
    sf::st_write(dest_path[['bbox']])
  
  # save dem data
  dem |> terra::writeRaster(dest_path[['dem']])
  return(dest_path)
}
