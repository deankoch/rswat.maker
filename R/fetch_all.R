#' Fetches all required data for a QSWAT+ model build
#' 
#' By default (`overwrite=FALSE`) this returns a list of the file paths written by the
#' function, in the order they are processed. With `overwrite=TRUE` the function generates
#' each group of files in sequence by downloading data (from USGS and USDA) on various
#' upstream catchment features.
#' 
#' The function produces a set of files in `data_dir` sufficient for building a SWAT+ model
#' for the basin draining to `outlet`. Output files are ready to use with QSWAT+.
#' 
#' Steps can be run individually, and if any fails you should be able to try again by
#' deleting to sub-directory for that step. 
#'
#' @param outlet geo-referenced point object passed to `sf::st_geometry`, the main outlet
#' @param data_dir character path to the directory to use for output files
#' @param overwrite logical, should the function create missing files in `data_dir`?
#' @param force_overwrite logical, should the function overwrite existing files in `data_dir`?
#'
#' @return a list of file paths to write
#' @export
fetch_all = function(outlet, data_dir, overwrite=FALSE, force_overwrite=FALSE,
                     nwis_nm = 'flow_ft', nwis_from = as.Date('2005-01-01')) {
  
  # flag to skip final update if we did it already in the same call
  update_nwis = TRUE
  
  # get paths written by the function
  nhd_path = data_dir |> save_catch()
  dem_path = data_dir |> save_dem()
  land_path = data_dir |> save_land()
  soils_path = data_dir |> save_soil()
  nwis_path = data_dir |> save_nwis(nwis_nm)
  split_path = data_dir |> save_split()
  qswat_path = data_dir |> save_qswat(sub=TRUE)
  
  # concatenate paths in list
  all_path = list(catch = nhd_path,
                  dem = dem_path,
                  land = land_path,
                  soils = soils_path,
                  nwis = nwis_path,
                  qswat = qswat_path)
  
  # return from list mode
  if( !overwrite ) return(all_path)
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  message('')
  message('fetching catchment model...')
  if( force_overwrite | !all(file.exists(nhd_path)) ) {
    
    # NHD data: for UYR initial download is slow, but after that can run in < 1 min
    catch_list = get_catch(outlet)
    save_catch(data_dir, catch_list, overwrite=TRUE)
  }
  
  # get stream gage records from NWIS (using `dataRetrieval`)
  message('')
  message('fetching stream gages...')
  if( force_overwrite | !all( file.exists( unlist(nwis_path) ) ) ) {

    # small batch of downloads, should complete in < 5 min  
    get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
    update_nwis = FALSE
  }
  
  # get DEM from USGS if necessary (using 'FedData')
  message('')
  message('fetching DEM...')
  if( force_overwrite | !all(file.exists(dem_path)) ) {
    
    # NED data download for UYR completes in < 1 min

    dem = get_dem(data_dir)
    save_dem(data_dir, dem, overwrite=TRUE)
  }
  
  # get land use data from GAP/LANDFIRE
  message('')
  message('fetching land use...')
  if( force_overwrite | !all( file.exists( unlist(land_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min
    land = get_land(data_dir)
    save_land(data_dir, land, overwrite=TRUE)
  }
  
  # get soils from USDA if necessary (using 'FedData' and FPAC Box URLs)
  message('')
  message('fetching soils...')
  if( force_overwrite | !all( file.exists( unlist(soils_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min  
    soil = get_soil(data_dir)
    save_soil(data_dir, soil, overwrite=TRUE)
  }
  
  # split into sub-catchments and copy relevant data subsets
  message('')
  message('splitting at gages...')
  sub_exists = length(split_path[['sub']]) > 0
  if( force_overwrite | !sub_exists ) {
    
    sub_list = get_split(data_dir)
    save_split(data_dir, sub_list, overwrite=TRUE, nwis_nm=nwis_nm)
  }
  
  # make shapefiles for qswat
  message('')
  message('making qswat inputs...')
  qswat_exists = length(qswat_path) > 0
  if( force_overwrite | !qswat_exists ) {
    
    save_qswat(data_dir, overwrite=TRUE)
    save_qswat(data_dir, sub=TRUE, overwrite=TRUE)
  }
  
  # optionally update NWIS
  message('')
  message('updating stream gage data...')
  if( update_nwis ) update_nwis(data_dir, nwis_nm)
  
  message('')
  message('all done')
}
