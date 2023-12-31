#' Wrapper for `FedData::get_nlcd`
#' 
#' This loads the bounding box for the DEM created by `get_dem` and passes it
#' to `FedData::get_nlcd`, which handles downloading and ETL. The result is returned
#' as a SpatRaster rather than a RasterLayer.
#' 
#' See `?land_use_lookup` for a dictionary of integer codes for land use.
#'
#' @param data_dir character path to the directory to use for output files
#'
#' @return SpatRaster, land use data for the catchment area (direct from `FedData::get_nlcd`)
#' @export
get_land = function(data_dir) {
  
  # open bounding box for source DEM file to get template
  bbox_path = save_dem(data_dir)[['bbox']]

  # sanity check before loading
  msg_error = paste('missing bounding box file:', bbox_path)
  msg_suggestion = 'Have you run `get_dem()` yet?'
  if( !file.exists(bbox_path) ) stop(msg_error, '\n', msg_suggestion)
  dem_bbox = sf::st_read(bbox_path, quiet=TRUE)
  
  # download the data to temporary location and load into RAM
  message('running FedData::get_nlcd')
  FedData::get_nlcd(template=dem_bbox, label=basename(data_dir)) |> terra::rast()
}

#' Save the output of `get_land` to disk
#' 
#' When `overwrite=TRUE` the function writes  two copies of the land use raster. When
#' `overwrite=FALSE` the function writes nothing but returns the file paths that would
#' be written.
#' 
#' The function writes two rasters to the 'land' sub-directory of `data_dir`:
#' 
#' * 'nlcd_land.tif', the unchanged source from `FedData::get_nlcd`
#' * 'land.tif', a version warped to UTM (and optionally masked/cropped to the catchment)
#' 
#' `buffer` and `threads` determine if and how the UTM grid is masked (see `?save_dem`)
#' 
#' @param data_dir character path to the directory to use for output files
#' @param land list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param buffer numeric > 0, padding distance in metres, for masking
#' @param threads logical, passed to `terra::project`
#' 
#' @return the file names to write
#' @export
#'
#' @examples
#' save_land('/example')
save_land = function(data_dir, land=NULL, overwrite=FALSE, buffer=Inf, threads=TRUE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(land) & overwrite ) {
    
    warning('overwrite=TRUE but land was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'land')

  # output filenames
  dest_fname = c(land_src = 'nlcd_land.tif',
                 land = 'land.tif')

  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make sure the DEM is in RAM
  message('copying raster data to RAM')
  land = land + 0
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])

  # remove any existing output files then write a fresh copy of the source raster
  if( any(is_over) ) unlink(dest_path[is_over])
  land |> terra::writeRaster(dest_path[['land_src']])
  
  # open files generated by `save_catch`
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) 
  dem = save_dem(data_dir)['dem'] |> terra::rast()

  # optional mask/crop DEM to reduce CPU time for warp
  land_mask = land |> clip_raster(boundary, buffer=buffer)
  
  # warp the DEM to the template then crop/mask again, in UTM this time
  message('projecting to UTM (GDAL warp)')
  land_out = land_mask |> 
    terra::project(dem, method='near', threads=threads) |> 
    clip_raster(boundary, buffer=buffer)
  
  # write to disk
  land_out |> terra::writeRaster(dest_path[['land']])
  return(dest_path)
}
