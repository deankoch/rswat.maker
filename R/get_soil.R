#' Soil getter
#'
#' @param data_dir character path to the directory to use for output files
#'
#' @return something
#' @export
get_soil = function(data_dir) {
  
  
}

#' Save the output of `get_soil` to disk
#' 
#'
#' @param data_dir character path to the directory to use for output files
#' @param overwrite logical
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_soil('/example')
save_soil = function(data_dir, statsgo=NULL, ssurgo=NULL, overwrite=FALSE) {
  
}

#' Fetch STATSGO2 data
#' 
#' Downloads and opens STATSGO2 data covering the bounding box of the DEM
#' created by `get_dem`. This uses HTTPS to download from the FPAC Box cloud
#' system at the USDA (see ?statsgo_url).
#' 
#' The function saves its source files to the 'raw' sub-directory. Get the
#' path to this storage directory with `save_statsgo(data_dir, overwrite=FALSE)[['raw_dir']]`
#'
#' @param data_dir character path to the directory to use for output files
#' @param s2 logical, controls whether to use spherical approximation
#'
#' @return sf dataframe object containing a "MUKEY" field and polygon geometries
#' @export
get_statsgo = function(data_dir, s2=FALSE) {
  
  # locate DEM boundary and output paths for source files
  statsgo_path = save_statsgo(data_dir, overwrite=FALSE)
  dem_path = save_dem(data_dir, overwrite=FALSE)
  bbox_path = dem_path[['bbox']]
  
  # sanity check 
  msg_error = paste('missing DEM bounding box polygon:', bbox_path)
  msg_suggestion = 'Have you run `get_dem()` yet?'
  if( !file.exists(bbox_path) ) stop(msg_error, '\n', msg_suggestion)
  
  # load boundary polygon created by make_dem.R
  bbox_dem = sf::st_read(bbox_path)
  
  # find overlapping states and their abbreviation code
  is_over = us_states |> sf::st_intersects(bbox_dem, sparse=FALSE)
  abb_over = us_states[['abbr']][is_over]
  
  #  filter to relevant states
  if( !any(is_over) ) stop('bounding box did not overlap with any polygons in us_states')
  statsgo_fetch = statsgo_url[match(abb_over, statsgo_url[['abb']]), ]
  msg_over = us_states[['abbr']][is_over] |> paste(collapse=', ')
  
  
  # loop over available states and download/load the data
  n_get = nrow(statsgo_fetch)
  poly_list = vector(mode='list', length=n_get)
  message('bounding box overlapped with ', nrow(statsgo_fetch), ' states (', msg_over, ')')
  for(i in seq(n_get) ) {
    
    message('fetching data for ', us_states[['full']][is_over][i])
    
    # download the zip file
    if( !dir.exists(statsgo_path[['raw_dir']]) ) dir.create(statsgo_path[['raw_dir']], recursive=TRUE) 
    path_i = file.path(statsgo_path[['raw_dir']], statsgo_fetch[['file']][i])
    if( !file.exists(path_i) ) download.file(statsgo_fetch[['url']][i],
                                             destfile = path_i,
                                             quiet = TRUE,
                                             mode =' wb')
    
    # unpack to same folder, identify the geometries file
    file_i = path_i |> unzip(exdir=statsgo_path[['raw_dir']])
    idx_dbf = grepl('spatial/.+\\.dbf$', file_i) |> which()
    if( length(idx_dbf) != 1 ) stop('could not locate the geometries file in ', path_i)
    
    # load it and copy to list
    poly_list[[i]] = file_i[idx_dbf] |> sf::st_read()
  }
  
  # turn off spherical approximation for intersection calculation
  if(!s2) {
    
    # for less chance of failure with corrupt geometries
    if( sf::sf_use_s2() ) on.exit( sf::sf_use_s2(TRUE) |> suppressMessages() )
    sf::sf_use_s2(FALSE) |> suppressMessages()
  }
  
  # merge all state data and crop to bounding box of interest
  statsgo_poly = do.call(rbind, poly_list)
  is_in = statsgo_poly |> sf::st_intersects(bbox_dem, sparse=FALSE) |> suppressMessages()
  mukey_poly = statsgo_poly[is_in, ] 
  return(mukey_poly)
}


#' Save the output of `get_statsgo` to disk
#' 
#' When `overwrite=TRUE` the function writes 'statsgo.tif' and 'statsgo.geojson'
#' based on the data in `statsgo`, and when `overwrite=FALSE` the function writes nothing
#' but returns the file paths that would be written.
#' 
#' The function writes a copy of `statsgo` as geoJSON as well as a rasterized version
#' with grid matching the DEM downloaded in `get_dem`
#' 
#' 
#' @param data_dir character path to the directory to use for output files
#' @param statsgo sf dataframe returned by `get_statsgo`
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_soil('/example')
save_statsgo = function(data_dir, statsgo=NULL, overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(statsgo) & overwrite ) {
    
    warning('overwrite=TRUE but statsgo was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, 'statsgo')
  
  # output filenames (outlet listed first on purpose)
  dest_fname = c(rast = 'statsgo.tif',
                 poly = 'statsgo.geojson',
                 raw_dir = 'raw')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])

  # write polygons to disk
  statsgo |> sf::st_write(dest_path[['poly']])
  
  # load the DEM
  dem = save_dem(data_dir, overwrite=FALSE)[['dem']] |> terra::rast()

  # coerce to SpatVector with integer values then rasterize to DEM grid
  statsgo[['MUKEY']] = statsgo[['MUKEY']] |> as.integer()
  statsgo_sv = as(statsgo['MUKEY'], 'SpatVector')
  statsgo_sv |> terra::rasterize(dem,
                                 field = 'MUKEY',
                                 fun = 'min',
                                 touches=TRUE,
                                 filename = dest_path[['rast']])
                                    

  return(dest_path)
}







#' Save the output of `get_soil` to disk
#' 
#'
#' @param data_dir character path to the directory to use for output files
#' @param overwrite logical
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_soil('/example')
save_ssurgo = function(data_dir, ssurgo=NULL, overwrite=FALSE) {
  
  
  
  
}



