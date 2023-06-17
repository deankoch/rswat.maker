#' Fetches all required data for a QSWAT+ model build
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
  nhd_path = save_catch(data_dir, overwrite=FALSE)
  dem_path = save_dem(data_dir, overwrite=FALSE)
  land_path = save_land(data_dir, overwrite=FALSE)
  soils_path = save_soils(data_dir, overwrite=FALSE)
  nwis_path = save_nwis(data_dir, overwrite=FALSE)
  
  # concatenate paths in list and return from list mode
  all_path = list(nhd=nhd_path, ned=dem_path, soils=soils_path)
  if( !overwrite ) return(all_path)
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  if( force_overwrite | !all(file.exists(nhd_path)) ) {
    
    # NHD data: for UYR initial download is slow, but after that can run in < 1 min
    catch_list = get_catch(outlet)
    save_catch(data_dir, catch_list, overwrite=TRUE)
  }

  # get DEM from USGS if necessary (using 'FedData')
  if( force_overwrite | !all(file.exists(dem_path)) ) {
    
    # NED data download for UYR completes in < 1 min
    dem = get_dem(data_dir)
    save_dem(data_dir, dem, overwrite=TRUE)
  }
  
  # get land use data from GAP/LANDFIRE
  if( force_overwrite | !all( file.exists( unlist(land_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min  
    land = get_land(data_dir)
    save_land(data_dir, land, overwrite=TRUE)
  }
  
  # get soils from USDA if necessary (using 'FedData' and FPAC Box URLs)
  if( force_overwrite | !all( file.exists( unlist(soils_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min  
    soils = get_soils(data_dir)
    save_soils(data_dir, soils, overwrite=TRUE)
  }
  
  
  
}
