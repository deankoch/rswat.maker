#' Download input data and run QSWAT+ for the basin defined by `outlet`
#' 
#' This runs a SWAT+ model setup workflow on the catchment upstream of `outlet`,
#' by downloading and processing watershed data and passing it to QSWAT+. When
#' `overwrite=FALSE` (the default) the function returns the output file paths
#' but doesn't write or process anything.
#' 
#' With default settings and `overwrite=TRUE`, the function runs a series of
#' downloading and/or processing steps, each based on outputs from previous
#' steps. This amounts to calling a series of helper functions from this package
#' in the correct order:
#' 
#' 1. 'catch' gets the initial catchment model (see `?get_catch`)
#' 2. 'dem' gets the digital elevation model raster (see `?get_dem`)
#' 3. 'nwis' gets daily stream gauge records (see `?get_nwis`)
#' 4. 'land' gets a land use raster (see `?get_land`)
#' 5. 'soil' gets a soil MUKEY raster (see `?get_soil`)
#' 6. 'split' partitions the catchment into sub-catchments (see `?get_split`)
#' 7. 'input' prepares QSWAT+ input files (see `?save_qswat`)
#' 8. 'qswat' runs QSWAT+ setup (see `?run_qswat`)
#' 9. 'editor' runs SWAT+ Editor setup (see `?run_editor`)
#' 
#' Users can repeat a step manually by calling the associated helper function(s),
#' or by calling `run_maker` with `what` set to the name of one or more steps.
#' For example setting `what=c('catch', 'dem', 'nwis', 'land' , 'soil')`) (or
#' `what='data'` for short) will fetch all the data needed to prepare a QSWAT+
#' project, at which point users could then create the project manually in QGIS3. 
#' 
#' When `force_overwrite=FALSE` (the default), the function skips steps where
#' the output files (in `data_dir`) all exist already. However if `what` is
#' `NULL` (the default) or set to 'nwis', the function will download and save
#' (on disk) the latest daily gauge data for your stations of interest.
#' 
#' Steps (1-5) each download something (unless it is already cached locally) and
#' write processed outputs to like-named new sub-directories of `data_dir`. Step
#' (6) then creates a directory tree with root at sub-directory "split/" and writes
#' a sub-catchment directory inside it for each (unique) gage position discovered
#' in (3). These sub-catchment directories are structured the same as `data_dir`,
#' except without a "split/".
#' 
#' The last three steps (7-9) are done separately in a loop over sub-catchments.
#' Their output is written in the sub-catchments directory ("split/"). If there
#' are no errors in the workflow, this output includes a completed QSWAT+ project,
#' the input files used to create it, and a ready-to-use SWAT+ "TxtInOut" directory.
#' 
#' The SWAT+ models that result from the completed workflow can be fitted
#' and run in isolation using inlet/outlet data from step (3). This means it should
#' be possible to link the "split" sub-catchments together by copying SWAT+ outputs
#' to inlet files, thus parallelizing SWAT+ at the sub-basin level.
#' 
#' If you are getting 404 errors on the very first step - downloading/opening the NHDPlus
#' model - but you already have the necessary files cached locally (having called either
#' `run_maker` or `save_catch` in `overwrite=TRUE` mode), try setting `no_download=TRUE`,
#' which attempts to proceed even when the web service is down. This affects only the
#' 'catch' step (1).
#'
#' @param data_dir character path to the directory to use for writing project files
#' @param outlet geo-referenced point object passed to `sf::st_geometry`, the main outlet
#' @param what character, one of the subroutine names (see DETAILS) or set to `NULL` to run all
#' @param overwrite logical, if `FALSE` the function returns file paths and writes nothing
#' @param force_overwrite logical, should the function overwrite existing files in `data_dir`?
#' @param nwis_from Date, earlier dates are ignored in NWIS records (NULL to return all)
#' @param osgeo_dir character, path to the QGIS installation on your local machine
#' @param no_download logical, if TRUE `nhdR::nhd_plus_get` calls is skipped (for debugging)
#'
#' @return a list of file paths written by the function
#' @export
run_maker = function(data_dir,
                     what = NULL,
                     outlet = NULL,
                     overwrite = FALSE,
                     force_overwrite = FALSE,
                     nwis_from = as.Date('2005-01-01'),
                     osgeo_dir = NULL,
                     no_download = FALSE) {
                  
  # untested with any other choices 
  nwis_nm = 'flow_ft'
  
  # invalid states to catch below
  sum_msg = 'one or more sub-catchments '
  split_msg = sum_msg |> paste('expected but not found in "split/"')
  qswat_msg = sum_msg |> paste('did not have a completed QSWAT+ project')  
  
  # get data paths written by the function
  nhd_path = data_dir |> save_catch()
  dem_path = data_dir |> save_dem()
  land_path = data_dir |> save_land()
  soils_path = data_dir |> save_soil()
  nwis_path = data_dir |> save_nwis(nwis_nm)
  split_path = data_dir |> save_split()
  
  # get sub-catchment directories if they exist (NULL otherwise)
  qswat_output_path = qswat_input_path = editor_output_path = NULL
  split_exists = length(split_path[['sub']]) > 0
  if(split_exists) {
    
    # open all QSWAT+ input JSON files to get input file paths
    qswat_input_path = data_dir |> save_qswat(sub=TRUE)
    
    qswat_exists = unlist(qswat_input_path) |>  file.exists() |> all()
    if(qswat_exists) {
      
      # similar call for output paths, but `run_qswat` does not have `sub` arg so we loop
      qswat_output_path = stats::setNames(nm=split_path[['sub']]) |> lapply(run_qswat)
      
      # and again for SWAT+ Editor
      editor_output_path = stats::setNames(nm=split_path[['sub']]) |> lapply(run_qswat)
    }
  }

  # concatenate paths in list
  all_path = list(catch = nhd_path,
                  dem = dem_path,
                  nwis = nwis_path,
                  land = land_path,
                  soil = soils_path,
                  split = split_path,
                  input = qswat_input_path,
                  qswat = qswat_output_path,
                  editor = editor_output_path)
  
  # execute all of the steps by default
  what_options = names(all_path)
  what_options_str = paste(what_options, collapse=', ')
  if( is.null(what) ) what = what_options
  
  # special keywords to do first and last parts only
  if( (length(what)==1) ) if(what == 'data') what = what_options |> head(6)
  if( length(what)==1 ) if(what == 'swat') what = what_options |> tail(3)
  
  # validity check
  what = what[what %in% what_options]
  if( length(what) == 0 ) stop('argument `what` must be one of: ', what_options_str)
  
  # unlist the length-1 output in single step results
  path_result = all_path[what]
  if( length(path_result) == 1 ) path_result = path_result[[1]]
  if( !overwrite ) return( invisible(path_result) )
  
  # get watershed geometries from NHD if necessary (using `nhdR`)
  catch_exists = file.exists(nhd_path) |> all()
  if( ('catch' %in% what) & ( force_overwrite | !catch_exists ) ) {

    message('')
    message('fetching catchment model...')
    if( is.null(outlet) ) stop('this step requires argument `outlet`')
    
    # NHD data: for UYR initial download is slow, after that it usually runs in < 1 min
    catch_list = get_catch(outlet, no_download=no_download)
    all_path[['catch']] = save_catch(data_dir, catch_list, overwrite=TRUE)
    catch_exists = TRUE
    message('done')
    
  } else { if('catch' %in% what) message('catchment model exists already') }

  
  # get stream gage records from NWIS (using `dataRetrieval`)
  nwis_exists = unlist(nwis_path) |> file.exists() |> all()
  if( ('nwis' %in% what) ) {

    # find relevant stations and download service records
    if( force_overwrite | !nwis_exists ) {
      
      message('')
      message('fetching stream gages...')
      
      # small batch of downloads, should complete in < 5 min  
      all_path[['nwis']] = get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
      nwis_exists = TRUE
      message('done')
  
    } else {
      
      # keep existing stations but update service records
      message('')
      message('updating stream gages...') 
      update_nwis(data_dir, nwis_nm)
    }
  }
  
  # get DEM from USGS (using 'FedData')
  dem_exists = file.exists(dem_path) |> all()
  if( ('dem' %in% what) & ( force_overwrite | !dem_exists ) ) {
    
    message('')
    message('fetching DEM...')
    
    # NED data download for UYR completes in < 1 min
    dem = get_dem(data_dir)
    all_path[['dem']] = save_dem(data_dir, dem, overwrite=TRUE)
    dem_exists = TRUE
    message('done') 
    
  } else { if('dem' %in% what) message('DEM exists already') }
  
  # get land use data from GAP/LANDFIRE
  land_exists = unlist(land_path) |> file.exists() |> all()
  if( ('land' %in% what) & ( force_overwrite | !land_exists ) ) {
    
    message('')
    message('fetching land use...')
    
    # several GB to download here, after initial download runs in < 2 min
    land = get_land(data_dir)
    all_path[['land']] = save_land(data_dir, land, overwrite=TRUE)
    land_exists = TRUE
    message('done')
    
  } else { if('land' %in% what) message('land use data exists already') }
  
  # get soils from USDA (using 'FedData' and FPAC Box URLs)
  soil_exists =  unlist(soils_path) |> file.exists() |> all()
  if( ('soil' %in% what) & ( force_overwrite | !soil_exists ) ) {
    
    message('')
    message('fetching soils...')
    
    # several GB to download here, after initial download runs in < 2 min  
    soil = get_soil(data_dir)
    all_path[['soil']] = save_soil(data_dir, soil, overwrite=TRUE)
    soil_exists = TRUE
    message('done')
    
  } else { if('soil' %in% what) message('soil data exists already') }
  
  # split into sub-catchments and copy relevant data subsets
  if( ('split' %in% what) & ( force_overwrite | !split_exists ) ) {
    
    message('')
    message('splitting at gages...')
    
    # no downloads required, R_based
    sub_list = get_split(data_dir)
    all_path[['split']] = save_split(data_dir, sub_list, overwrite=TRUE, nwis_nm=nwis_nm)
    split_exists = TRUE
    message('done')
    
  } else { if('split' %in% what) message('split exists already') }

  # make shape-files for QSWAT+
  input_exists = !is.null(all_path[['input']])
  if(input_exists) input_exists = unlist(all_path[['input']]) |> file.exists() |> all()
  if( ('input' %in% what) & ( force_overwrite | !input_exists ) ) {
    
    # check the sub-catchments exist first
    if( !split_exists ) stop(split_msg)
    message('')
    message('making QSWAT+ inputs...')

    # vectorized execution over sub-catchments
    all_path[['input']] = save_qswat(data_dir, sub=TRUE, overwrite=TRUE)
    message('done')
    
  } else { if('input' %in% what) message('QSWAT+ inputs exist already') }
  
  # run QSWAT+
  qswat_exists = !is.null(all_path[['qswat']])
  if(qswat_exists) qswat_exists = unlist(all_path[['qswat']]) |> file.exists() |> all()
  if( ('qswat' %in% what) & ( force_overwrite | !qswat_exists ) ) {
    
    # check the sub-catchments exist first
    if( !split_exists ) stop(split_msg)
    message('')
    message('running model construction...')
    
    # loop over sub-catchments
    subs = all_path[['split']][['sub']]
    for(i in seq_along(subs)) {
      
      message('')
      message(paste(i, '/', length(subs)))
      
      # run QSWAT+ using a PyQGIS3 script
      all_path[['qswat']] = run_qswat(subs[i], overwrite=TRUE, osgeo_dir=osgeo_dir)
    }
    
    qswat_exists = TRUE
    message('done')
    
  } else { if('qswat' %in% what) message('QSWAT+ output exists already') }
  
  # run SWAT+ Editor
  editor_exists = !is.null(all_path[['editor']])
  if(editor_exists) editor_exists = unlist(all_path[['editor']]) |> file.exists() |> all()
  if( ('editor' %in% what) & ( force_overwrite | !editor_exists ) ) {
    
    # check the sub-catchments and QSWAT+ projects exist first
    if( !split_exists ) stop(split_msg)
    if(!qswat_exists) stop(qswat_msg)
    message('')
    message('running SWAT+ Editor...')

    # loop over sub-catchments
    subs = all_path[['split']][['sub']]
    for(i in seq_along(subs)) {
      
      message('')
      message(paste(i, '/', length(subs)))
      
      # run SWAT+ Editor from shell using its CLI
      all_path[['editor']] = run_editor(subs[i], overwrite=TRUE)
      editor_exists = TRUE
    }
    message('done')
    
  } else { if('editor' %in% what) message('SWAT+ Editor output exists already') }
  
  message('')
  if(length(what) > 1) message('all done')
  return( invisible(all_path) )
}
