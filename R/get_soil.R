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
#' @param data_dir character path to the directory to use for output files
#'
#' @return something
#' @export
get_statsgo = function(data_dir) {
  
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
  
  # find overlapping states and the abbreviation code
  bbox_dem
  
  xx = save_catchment(data_dir, overwrite=FALSE)[['edge']] |> sf::st_read()
  xx
  
  
  
  
  # we are only interested in Montana, Wyoming, Idaho
  statsgo_get = statsgo_url |> filter(abb %in% c('MT', 'WY', 'ID'))
  
  # loop over available states and download/load the data
  n_get = nrow(statsgo_get)
  poly_list = vector(mode='list', length=n_get)
  message('downloading data for ', paste(statsgo_get[['abb']], collapse=', '), '...')
  for(i in seq(n_get) ) {
    
    # download the zip file
    path_i = file.path(path_out[['raw']], statsgo_get[['file']][i])
    if( !file.exists(path_i) ) download.file(statsgo_get[['url']][i],
                                             destfile = path_i,
                                             quiet = TRUE,
                                             mode =' wb')
    
    # unpack to folder in parent directory, identify the geometries file
    file_i = path_i |> unzip(exdir=path_out[['statsgo']])
    idx_dbf = grepl('spatial/.+\\.dbf$', file_i) |> which()
    if( length(idx_dbf) != 1 ) stop('could not locate the geometries file in ', path_i)
    
    # load it and copy to list
    poly_list[[i]] = file_i[idx_dbf] |> sf::st_read()
  }
  
  # merge all state data and crop to AOI
  statsgo_poly = do.call(rbind, poly_list)
  boundary_t = boundary |> sf::st_transform(sf::st_crs(statsgo_poly))
  is_in = statsgo_poly |> st_intersects(boundary_t, sparse=FALSE)
  mukey_poly = statsgo_poly[is_in, ] 
  
  # copy the map unit keys of interest and their polygons
  mukey_poly[['MUKEY']] = mukey_poly[['MUKEY']] |> as.integer()
  mukey = mukey_poly[['MUKEY']] |> unique()
  
  return(mukey_poly)
  

  
  
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
  # statsgo |> sf::st_write(dest_path[['poly']])
  
  # # coerce to SpatVector then rasterize to DEM grid
  # statsgo_sv = as(statsgo['MUKEY'], 'SpatVector')
  # dem = path_in[['dem']] |> terra::rast()
  # statsgo_sv |> terra::rasterize(dem,
  #                                   field = 'MUKEY',
  #                                   fun = 'min',
  #                                   touches=TRUE,
  #                                   filename = path_out[['rast']])

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



