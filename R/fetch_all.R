#' Fetch all required data for a QSWAT+ model build
#'
#' @param data_dir character path to the directory to use for output files
#' @param outlet point object accepted by `sf::st_geometry`
#' @param overwrite logical, should the function create missing files in `data_dir`?
#' @param force_overwrite logical, should the function overwrite existing files in `data_dir`?
#'
#' @return a list of file paths to write
#' @export
fetch_all = function(data_dir, outlet=NULL, overwrite=FALSE, force_overwrite=FALSE) {
  
  # get paths written by the function
  nhd_path = save_catchment(data_dir, overwrite=FALSE)
  dem_path = save_dem(data_dir, overwrite=FALSE)
  soils_path = save_soils(data_dir, overwrite=FALSE)
  
  # concatenate paths in list and return from list mode
  all_path = list(nhd=nhd_path)
  if( !overwrite ) return(all_path)
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  if( force_overwrite | !all(file.exists(nhd_path)) ) {
    
    # NHD data: for UYR initial download is slow, but after that can run in < 1 min
    catch_list = get_catchment(outlet)
    save_catchment(data_dir, catch_list, overwrite=TRUE)
  }
  
  # get DEM from USGS if necessary (using 'FedData')
  if( force_overwrite | !all(file.exists(dem_path)) ) {
    
    # NED data download for UYR completes in < 1 min
    dem = get_dem(data_dir)
    save_dem(data_dir, dem, overwrite=TRUE)
  }
  
  #
}
