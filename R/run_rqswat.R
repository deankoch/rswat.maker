#' Download input data and run QSWAT+ for the basin defined by `outlet`
#' 
#' By default (`overwrite=FALSE`) this returns a list of the file paths written by the
#' function, in the order they are processed. When `overwrite=TRUE` the function runs
#' the model setup workflow on the catchment upstream of `oulet`, by downloading and
#' processing watershed data and passing it to QSWAT+.
#' 
#' With default settings and `overwrite=TRUE`, the function does the following:
#' 
#' 1. check for existing files on disk
#' 2. downloads public data on the landscape and hydrology
#' 3. partitions the data into sub-catchments that can be modelled individually
#' 4. runs automated QSWAT+ setup on each of the sub-catchments
#' 5. runs SWAT+ Editor to build "TxtInOut" from the QSWAT+ project database
#' 
#' When `force_overwrite=FALSE` (the default), the function skips steps where the output
#' is already found on disk. Set `nwis_check` to run the daily stream gage update for an
#' existing project running any of the other steps.
#'
#' For details on (2) see `?get_catch`, `?get_nwis`, `?get_dem`, `?get_land`, `?get_soil`,
#' `?get_split` (which are called in sequence); For details on (3), see `?get_split`;
#' And for details on SWAT+ setup (4-5), see `?save_qswat` and `?run_qswat`.
#' 
#' Each step in this workflow produces a new sub-directory in `data_dir`, and for every
#' `get_*` function there is a `save_*` function that returns the file paths written within:
#' eg for `get_dem`, call `save_dem(data_dir)` to get the paths. Users can repeat a step
#' manually for debugging by deleting the sub-directory for the step and running the
#' `get_*(...) |> save_*(...,overwrite=TRUE)` pair again.
#' 
#' "TxtInOut" is the directory name for a SWAT+ model, ie for the set of plaintext
#' configuration files that parametrize the simulator. The path to this directory can
#' be passed to `rswat` to manage model fitting and execution.
#' 
#' If you are getting 404 errors on the very first step - downloading/opening the NHDPlus
#' model - but you already have the necessary files cached locally (having called either
#' `run_rqswat` or `save_catch` in `overwrite=TRUE` mode), try setting `no_download=TRUE`.
#' This allows the function to proceed even when the web service is down. 
#'
#' @param outlet geo-referenced point object passed to `sf::st_geometry`, the main outlet
#' @param data_dir character path to the directory to use for writing project files
#' @param overwrite logical, if `FALSE` the function returns file paths and writes nothing
#' @param force_overwrite logical, should the function overwrite existing files in `data_dir`?
#' @param nwis_check logical, if TRUE the function updates the daily NWIS time series files
#' @param nwis_from Date, earlier dates are ignored in NWIS records (NULL to return all)
#' @param no_download logical, if TRUE `nhdR::nhd_plus_get` calls is skipped (for debugging)
#'
#' @return a list of file paths written by the function
#' @export
run_rqswat = function(outlet,
                      data_dir,
                      overwrite = FALSE,
                      force_overwrite = FALSE,
                      nwis_check = FALSE,
                      nwis_from = NULL,
                      no_download = FALSE) {
                  
  # constant because untested with any other choices 
  nwis_nm = 'flow_ft'
  
  # get paths written by the function
  nhd_path = data_dir |> save_catch()
  dem_path = data_dir |> save_dem()
  land_path = data_dir |> save_land()
  soils_path = data_dir |> save_soil()
  nwis_path = data_dir |> save_nwis(nwis_nm)
  split_path = data_dir |> save_split()
  
  # gets sub-catchment directories if they are known yet
  sub_exists = length(split_path[['sub']]) > 0
  
  # QSWAT+ directories, if they are known
  qswat_output_path = qswat_input_path = NULL
  if(sub_exists) {
    
    # `run_qswat` does not have `sub` arg yet so we loop
    qswat_input_path = data_dir |> save_qswat(sub=TRUE)
    qswat_output_path = split_path[['sub']] |> lapply(run_qswat)
    names(qswat_output_path) = basename(split_path[['sub']])
  }

  # concatenate paths in list
  all_path = list(catch = nhd_path,
                  dem = dem_path,
                  land = land_path,
                  soils = soils_path,
                  nwis = nwis_path,
                  split = split_path,
                  qswat_input = qswat_input_path,
                  qswat_output = qswat_output_path)
  
  # return from list mode
  if( !overwrite ) return(all_path)
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  message('')
  message('fetching catchment model...')
  if( force_overwrite | !all(file.exists(nhd_path)) ) {
    
    # NHD data: for UYR initial download is slow, but after that can run in < 1 min
    catch_list = get_catch(outlet, no_download=no_download)
    save_catch(data_dir, catch_list, overwrite=TRUE)
  }
  message('done')
  
  # get stream gage records from NWIS (using `dataRetrieval`)
  message('')
  message('fetching stream gages...')
  if( force_overwrite | !all( file.exists( unlist(nwis_path) ) ) ) {

    # small batch of downloads, should complete in < 5 min  
    get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
    nwis_check = FALSE
  }
  message('done')
  
  # get DEM from USGS if necessary (using 'FedData')
  message('')
  message('fetching DEM...')
  if( force_overwrite | !all(file.exists(dem_path)) ) {
    
    # NED data download for UYR completes in < 1 min
    dem = get_dem(data_dir)
    save_dem(data_dir, dem, overwrite=TRUE)
  }
  message('done')
  
  # get land use data from GAP/LANDFIRE
  message('')
  message('fetching land use...')
  if( force_overwrite | !all( file.exists( unlist(land_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min
    land = get_land(data_dir)
    save_land(data_dir, land, overwrite=TRUE)
  }
  message('done')
  
  # get soils from USDA if necessary (using 'FedData' and FPAC Box URLs)
  message('')
  message('fetching soils...')
  if( force_overwrite | !all( file.exists( unlist(soils_path) ) ) ) {
    
    # several GB to download here, after initial download runs in < 2 min  
    soil = get_soil(data_dir)
    save_soil(data_dir, soil, overwrite=TRUE)
  }
  message('done')
  
  # split into sub-catchments and copy relevant data subsets
  message('')
  message('splitting at gages...')
  if( force_overwrite | !sub_exists ) {
    
    sub_list = get_split(data_dir)
    save_split(data_dir, sub_list, overwrite=TRUE, nwis_nm=nwis_nm)
  }
  message('done')
  
  # make shape-files for QSWAT+
  message('')
  message('making qswat inputs...')
  qswat_input_exists = !is.null(qswat_input_path)
  if(qswat_input_exists) qswat_input_exists = file.exists( unlist(qswat_input_path) )
  if( force_overwrite | !all(qswat_input_exists) ) {

    save_qswat(data_dir, overwrite=TRUE)
    save_qswat(data_dir, sub=TRUE, overwrite=TRUE)
  }
  message('done')
  
  # run QSWAT+
  message('')
  message('model construction...')
  qswat_output_exists = !is.null(qswat_output_path)
  if(qswat_output_exists) qswat_output_exists = file.exists( unlist(qswat_output_path) )
  if( force_overwrite | !all(qswat_output_exists) ) {
    
    # loop over sub-catchments
    subs = split_path[['sub']]
    for(i in seq_along(subs)) {
      
      message('')
      message(paste(i, '/', length(subs)))
      
      # run QSWAT+ then Editor if succesful
      qswat_nudge = run_qswat(subs[i], overwrite=TRUE)
      if( !is.null(qswat_nudge) ) run_editor(subs[i], overwrite=TRUE)
    }
  }
  message('done')

  # optionally update NWIS
  message('')
  message('updating stream gage data...')
  if(nwis_check) update_nwis(data_dir, nwis_nm)
  message('all done')
}
