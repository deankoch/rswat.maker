#' Wrapper for `FedData::get_nlcd`
#' 
#' This loads the bounding box for the DEM created by `get_dem` and passes it
#' to `FedData::get_nlcd`, which handles downloading and ETL.
#'
#' @param data_dir character path to the directory to use for output files
#'
#' @return SpatRaster, land use data for the catchment area (direct from `FedData::get_nlcd`)
#' @export
get_land = function(data_dir) {
  
  # locate catchment boundary
  dem_path = save_dem(data_dir, overwrite=FALSE)
  bbox_path = dem_path[['bbox']]

  # sanity check
  msg_error = paste('missing bounding box polygon:', bbox_path)
  msg_suggestion = 'Have you run `get_dem()` yet?'
  if( !file.exists(bbox_path) ) stop(msg_error, '\n', msg_suggestion)

  # load boundary and set default padding
  bbox_dem = sf::st_read(bbox_path)
  
  # download the data to temporary location and load into RAM
  FedData::get_nlcd(template=bbox_dem, label=basename(data_dir))
}

#' Save the output of `get_land` to disk
#' 
#'
#' @param data_dir character path to the directory to use for output files
#' @param land list returned from `get_catch(..., fast=FALSE)`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_land('/example')
save_land = function(data_dir, land=NULL, overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(land) & overwrite ) {
    
    warning('overwrite=TRUE but land was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'land')

  # output filenames
  dest_fname = c(rast = 'nlcd_land.tif',
                 lookup = 'nlcd_land_code.csv')

  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])

  # write land use raster to disk
  land |> terra::writeRaster(dest_path[['rast']])

  # save as CSV a lookup table of names and descriptions for each ID code in the raster
  land_lvl = land[] |> unique()
  land_use_lookup |> 
    dplyr::filter(!is.na(id)) |> 
    dplyr::filter(id %in% land_lvl) |> 
    dplyr::select(-description) |> 
    write.csv(dest_path[['lookup']], row.names=FALSE, quote=FALSE)
  
  return(dest_path)
}
