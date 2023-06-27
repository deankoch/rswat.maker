#' Wrapper for `FedData::get_nlcd`
#' 
#' This loads the bounding box for the DEM created by `get_dem` and passes it
#' to `FedData::get_nlcd`, which handles downloading and ETL. The result is returned
#' as a SpatRaster rather than a RasterLayer.
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
#' When `overwrite=TRUE` the function writes 'landuse_lookup.csv', a lookup table,
#' and two copies of the land use raster: 'nlcd_land.tif', the unchanged output from
#' `FedData::get_nlcd`; and 'land.tif', a version warped to the UTM zone of the main
#' outlet (by nearest neighbour). When `overwrite=FALSE` the function writes nothing
#' but returns the file paths that would be written.
#' 
#' The lookup table provides the SWAT+ codes mapping to the integer values of the
#' rasters. This CSV file should be compatible with QSWAT+ without additional formatting. 
#' 
#' Warping can be CPU-intensive See `save_dem` for more on `buffer` and `threads`
#' and note that the preceding `save_dem` call establishes the output resolution used
#' by this function.
#' 
#' @param data_dir character path to the directory to use for output files
#' @param land list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param buffer numeric > 0, padding distance in metres (for masking to boundary)
#' @param threads logical, passed to `terra::project`
#' 
#' @return the file names to write
#' @export
#'
#' @examples
#' save_land('/example')
save_land = function(data_dir, land=NULL, overwrite=FALSE, buffer=NULL, threads=TRUE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(land) & overwrite ) {
    
    warning('overwrite=TRUE but land was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'land')

  # output filenames
  dest_fname = c(land_src = 'nlcd_land.tif',
                 land = 'land.tif',
                 lookup = 'landuse_lookup.csv')

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
  
  # open required input files saved by `save_catch`
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) 
  dem = save_dem(data_dir)['dem'] |> terra::rast()
  
  # output projection 
  crs_out = sf::st_crs(dem)
  boundary_out = sf::st_transform(boundary, crs_out)
  
  # set default buffer for masking
  if(is.null(buffer)) buffer = terra::res(dem)[1]
  
  # two version of boundary for masking, one padded
  message('masking and cropping')
  boundary_pad = boundary_out |> 
    sf::st_buffer(units::set_units(buffer, m)) |>
    sf::st_transform(sf::st_crs(land))
  
  # make a copy of land masked and cropped to padded boundary
  land_mask = land |> terra::crop(boundary_pad) |> terra::mask(boundary_pad)
  
  # warp to this template and write to disk
  message('warping to match DEM')
  land_out = land_mask |> terra::project(dem, method='near', threads=threads)
  land_out |> terra::writeRaster(dest_path[['land']])
  
  # unique land codes after trimming
  land_lvl = land_out[] |> unique()
  
  # lookup table of names and descriptions for each ID code in the trimmed raster
  land_df = land_use_lookup |> 
    dplyr::filter(!is.na(id)) |> 
    dplyr::filter(id %in% land_lvl) |> 
    dplyr::select(-'description') |> 
    dplyr::rename('LANDUSE_ID'='id', 'SWAT_CODE'='name')
  
  # save as CSV
  land_df |> write.csv(dest_path[['lookup']], row.names=FALSE, quote=FALSE)
  
  return(dest_path)
}
