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
save_soil = function(data_dir, soils=NULL, overwrite=FALSE) {
  
}

#' Fetch STATSGO2 or SSURGO data
#' 
#' Downloads and opens STATSGO2 or SSURGO data covering the bounding box of the DEM
#' created by `get_dem`. For STATSGO2 we use HTTPS to download from the FPAC Box cloud
#' system at the USDA (see ?statsgo_url). For SSURGO we use `FedData::get_ssurgo` to
#' download SSA tile archives and handle ETL with `sf` and `terra`.
#' 
#' The function saves its source files to the 'raw' sub-directory. Get the
#' path to this storage directory by calling the following:
#' 
#' `save_statsgo(data_dir, model='ssurgo', overwrite=FALSE)[['raw_dir']]`
#' 
#' SSURGO is very detailed, so when using this model the memory usage may be high,
#' and the intermediate .gpkg file written to the "raw" sub-directory may be large.
#' For example with the UYR the file is around 8 GB.
#'
#' @param data_dir character path to the directory to use for output files
#' @param model character either 'statsgo' or 'ssurgo'
#' @param s2 logical, controls whether to use spherical approximation
#'
#' @return sf dataframe object containing a "MUKEY" field and polygon geometries
#' @export
get_statsgo = function(data_dir, model='statsgo', s2=FALSE) {
  
  # turn off spherical approximation for intersection calculation
  if(!s2) {
    
    # for less chance of failure with corrupt geometries
    if( sf::sf_use_s2() ) on.exit( sf::sf_use_s2(TRUE) |> suppressMessages() )
    sf::sf_use_s2(FALSE) |> suppressMessages()
  }
  
  # locate DEM boundary and output paths for source files
  statsgo_path = save_statsgo(data_dir, model=model, overwrite=FALSE)
  dem_path = save_dem(data_dir, overwrite=FALSE)
  bbox_path = dem_path[['bbox']]
  
  # sanity check 
  msg_error = paste('missing DEM bounding box polygon:', bbox_path)
  msg_suggestion = 'Have you run `get_dem()` yet?'
  if( !file.exists(bbox_path) ) stop(msg_error, '\n', msg_suggestion)
  
  # load boundary polygon created by make_dem.R
  bbox_dem = sf::st_read(bbox_path)
  
  # process STATSGO2 requests
  if( model == 'statsgo' ) {
    
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
    
    # merge all state data
    soils_poly = do.call(rbind, poly_list)
    soils_poly[['MUKEY']] = soils_poly[['MUKEY']] |> as.integer()
    
    # crop to bounding box of interest
    is_in = soils_poly |> sf::st_intersects(bbox_dem, sparse=FALSE) |> suppressMessages()
    soils_poly = soils_poly[is_in, ] 
  }
  
  # process SSURGO requests
  if( model == 'ssurgo' ) {
    
    # delete the SSA directory to download again
    raw_dir = statsgo_path[['raw_dir']]
    if( !dir.exists( raw_dir ) ) {
      
      # download and unzip a .dbf file containing soil survey area keys
      message('fetching soil survey area keys')
      dir.create(raw_dir)
      path_ssa_zip = FedData::download_ssurgo_inventory(raw_dir)
      path_ssa_all = raw_dir |> file.path(basename(path_ssa_zip)) |> unzip(exdir=raw_dir)
    }

    # there should only be the one dbf file in this directory 
    path_ssa_all = list.files(raw_dir)
    dbf_nm = path_ssa_all[grepl('.+.dbf$', path_ssa_all)][1] |> basename()
    path_ssa = raw_dir |> file.path(dbf_nm)
    
    # load SSA polygons, omit those not overlapping with AOI, copy area keys
    message('loading ', basename(path_ssa), ' and checking geometries...')
    ssa_poly = path_ssa |> sf::st_read() |> sf::st_make_valid()
    ssa_in = ssa_poly[sf::st_intersects(ssa_poly, bbox_dem, sparse=FALSE), ]
    ssa = ssa_in[['areasymbol']]
    if( length(ssa) == 0 ) {
     
      warning('no SSURGO polygons in AOI')
      return(NULL)
    }
    
    # download/open all data for these overlapping SSAs
    message('loading SSURGO data from ', length(ssa), ' soil survey area(s)...')
    soil_result = FedData::get_ssurgo(ssa,
                                      label = 'uyr_pad',
                                      raw.dir = raw_dir,
                                      extraction.dir = raw_dir,
                                      force.redo = FALSE)
    
    # split by SSA to reduce memory demands for intersection
    ssa = soil_result[['spatial']][['AREASYMBOL']] |> unique()
    
    # loop over SSAs
    message('looping over ', length(ssa), ' soil survey area(s) to find intersection')
    mukey_in_aoi = length(ssa) |> logical()
    pb = txtProgressBar(0, length(ssa), style=3)
    for(i in seq_along(ssa)) {
      
      # identify the polygons in ith SSA that overlap with AOI, match mukeys to full list
      poly_i = soil_result[['spatial']] |> dplyr::filter(AREASYMBOL == ssa[i])
      is_in_aoi = poly_i |> sf::st_geometry() |> sf::st_intersects(bbox_dem, sparse=FALSE)
      mukey_in_aoi[ soil_result[['spatial']][['MUKEY']] %in% poly_i[['MUKEY']][is_in_aoi] ] = TRUE
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    # extract relevant subset
    soils_poly = soil_result[['spatial']][mukey_in_aoi, ]
    soils_poly[['MUKEY']] = soils_poly[['MUKEY']] |> as.integer()
  }
  
  return(soils_poly)
}


#' Save the output of `get_statsgo` to disk
#' 
#' When `overwrite=TRUE` the function writes the data in `soils` to a pair of files
#' named `model`, with extensions '.tif' and '.geojson'. When `overwrite=FALSE` the
#' function writes nothing but returns the file paths that would be written.
#' 
#' The function writes a copy of `soils` as geoJSON as well as a rasterized version
#' with grid matching the DEM downloaded in `get_dem`
#' 
#' @param data_dir character path to the directory to use for output files
#' @param soils sf dataframe returned by `get_statsgo`
#' @param model character either 'statsgo' or 'ssurgo'
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_statsgo('/example')
save_statsgo = function(data_dir, soils=NULL, model='statsgo', overwrite=FALSE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(soils) & overwrite ) {
    
    warning('overwrite=TRUE but soils was NULL')
    overwrite = FALSE
  }
  
  # output directory
  dest_dir = file.path(data_dir, model)
  
  # output filenames
  dest_fname = c(rast = paste0(model, '.tif'),
                 poly = paste0(model, '.geojson'),
                 raw_dir = 'raw')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])

  # write polygons to disk
  soils |> sf::st_write(dest_path[['poly']])
  
  # load the DEM
  dem = save_dem(data_dir, overwrite=FALSE)[['dem']] |> terra::rast()

  # coerce to SpatVector with integer values then rasterize to DEM grid
  soils_sv = as(soils['MUKEY'], 'SpatVector')
  soils_sv |> terra::rasterize(dem,
                                 field = 'MUKEY',
                                 fun = 'min',
                                 touches=TRUE,
                                 filename = dest_path[['rast']])
                                    

  return(dest_path)
}

