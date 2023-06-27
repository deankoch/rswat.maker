#' Fetches all required data for a QSWAT+ model build
#'
#' @param data_dir character path to the directory to use for output files
#' @param outlet point object accepted by `sf::st_geometry`
#' @param overwrite logical, should the function create missing files in `data_dir`?
#' @param force_overwrite logical, should the function overwrite existing files in `data_dir`?
#'
#' @return a list of file paths to write
#' @export
fetch_all = function(data_dir, outlet=NULL, overwrite=FALSE, force_overwrite=FALSE,
                     nwis_nm = 'flow_ft', nwis_from = as.Date('2005-01-01')) {
  
  # get paths written by the function
  nhd_path = data_dir |> save_catch()
  dem_path = data_dir |> save_dem()
  land_path = data_dir |> save_land()
  soils_path = data_dir |> save_soils()
  nwis_path = data_dir |> save_nwis(nwis_nm)
  split_path = data_dir |> save_split()
  # TODO: last step - make shapefiles for qswat
  
  # concatenate paths in list and return from list mode
  all_path = list(nhd=nhd_path, ned=dem_path, soils=soils_path)
  if( !overwrite ) return(all_path)
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  if( force_overwrite | !all(file.exists(nhd_path)) ) {
    
    # NHD data: for UYR initial download is slow, but after that can run in < 1 min
    message('')
    message('fetching catchment model...')
    catch_list = get_catch(outlet)
    save_catch(data_dir, catch_list, overwrite=TRUE)
  }
  
  # get DEM from USGS if necessary (using 'FedData')
  if( force_overwrite | !all(file.exists(dem_path)) ) {
    
    # NED data download for UYR completes in < 1 min
    message('')
    message('fetching DEM...')
    dem = get_dem(data_dir)
    save_dem(data_dir, dem, overwrite=TRUE)
  }
  
  # get stream gage records from NWIS (using `dataRetrieval`)
  if( force_overwrite | !all( file.exists( unlist(nwis_path) ) ) ) {
    
    # small batch of downloads, should complete in < 5 min  
    message('')
    message('fetching stream gages...')
    get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
  }
  
  # get land use data from GAP/LANDFIRE
  if( force_overwrite | !all( file.exists( unlist(land_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min
    message('')
    message('fetching land use...')
    land = get_land(data_dir)
    save_land(data_dir, land, overwrite=TRUE)
  }
  
  # get soils from USDA if necessary (using 'FedData' and FPAC Box URLs)
  if( force_overwrite | !all( file.exists( unlist(soils_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min  
    message('')
    message('fetching soils...')
    soil = get_soils(data_dir)
    save_soils(data_dir, soil, overwrite=TRUE)
  }
  
  # split into sub-catchments and copy relevant data subsets
  sub_exists = length(split_path[['sub']]) > 0
  if( force_overwrite | !sub_exists ) {
    
    message('')
    message('splitting at gages...')
    sub_list = get_split(data_dir)
    save_split(data_dir, sub_list, overwrite=TRUE, nwis_nm=nwis_nm)
  }
  
  # TODO: last step - make shapefiles for qswat
}
