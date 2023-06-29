#' Download/open soil map unit key rasters from STATSGO2 and SSURGO and combine them
#' 
#' This downloads STATSGO2 and SSURGO data to the 'statsgo' and 'ssurgo' sub-directories
#' of `data_dir` then creates a merged copy in the 'soils' sub-directory.
#' 
#' SSURGO tends to be the more precise map but it also tends to have less complete
#' coverage than STATSGO2. In cases where there is no SSURGO map unit key (mukey),
#' this function assigns the STATSGO2 key.
#' 
#' If a SSURGO key is not found in the SWAT+ soils database, the STATSGO2 key is
#' assigned. If that key is not found, then an `NA` is assigned.
#' 
#' `mukey_replace` can be used to replace any number of SSURGO polygons with the
#' underlying STATSGO2 polygons. In some cases (eg with the default 2485736) there
#' can more detail in the STATSGO2 layer. This may change in the future as SSURGO
#' continues to be updated.
#' 
#' If you have called this function previously (in another project), you can save some
#' time and bandwidth by copying the "ssurgo/raw" from the old `data_dir` to the new one.
#' The function will only download files that it doesn't already find in "raw"
#'
#' @param data_dir character path to the directory to use for output files
#' @param force_overwrite logical if TRUE the function overwrites a fresh copy of outputs 
#' @param mukey_replace map unit keys for SSURGO feature to swap for STATSGO feature
#'
#' @return SpatRaster of STATSGO2 and/or SSURGO MUKeys
#' @export
get_soil = function(data_dir, force_overwrite=FALSE, mukey_replace=c(2485736)) {
  
  # set up input/output names
  model_path = save_soil(data_dir)
  model_nm = stats::setNames(nm=names(model_path))
  input_nm = model_nm[1:2]
  
  # delete old files if requested
  if(force_overwrite) model_path[input_nm] |> sapply(\(x) unlink(x[c('soil', 'poly')]))
  
  # fetch data as needed (writes source files to "raw")
  is_done = model_path[input_nm] |> sapply(\(x) all(file.exists(x)) )
  if( any(!is_done) ) model_nm[input_nm][!is_done] |> lapply(\(x) { 
    
    soils = get_statsgo(data_dir, model=x) 
    data_dir |> save_statsgo(soils, model=x, overwrite=TRUE)
    
    })

  # load the two soil key rasters
  message('opening STATSGO2 and SSURGO layers')
  statsgo = model_path[['statsgo']][['soil']] |> terra::rast()
  ssurgo = model_path[['ssurgo']][['soil']] |> terra::rast()
  
  # copy the unique mukeys found in each raster as string
  ssurgo_mukey = ssurgo[] |> unique() |> as.character()
  statsgo_mukey = statsgo[] |> unique()  |> as.character()
  
  # identify those unknown to SWAT+
  ssurgo_ok = ssurgo_mukey %in% swatplus_soils
  statsgo_ok = statsgo_mukey %in% swatplus_soils
  
  # merge STATSGO and SSURGO mukey layers
  message('merging')
  soils = ssurgo
  if( any(!ssurgo_ok) ) {
    
    # unpack both rasters as matrices
    g_ssurgo = ssurgo[]
    g_statsgo = statsgo[]
    
    # set keys unrecognized by SWAT+ to NA
    g_ssurgo[ g_ssurgo %in% ssurgo_mukey[!ssurgo_ok] ] = NA
    g_statsgo[ g_statsgo %in% statsgo_mukey[!statsgo_ok] ] = NA
    
    # set particular SSURGO key(s) to NA to force replacement
    g_ssurgo[ g_ssurgo %in% mukey_replace ] = NA
    
    # replace all unassigned pixels in the SSURGO layer with STATSGO values
    is_replaced = is.na(g_ssurgo)
    g_ssurgo[is_replaced] = g_statsgo[is_replaced]
    
    # export result back to SpatRaster
    soils[] = g_ssurgo 
  }
  return(soils)
}

#' Save the output of `get_soil` to disk
#' 
#' When `overwrite=TRUE` the function writes two copies of the soil MUKEY rasters.
#' When `overwrite=FALSE` the function writes nothing but returns a list containing
#' the file paths that would be written (in element 'soil') along with file paths
#' writting by `get_soil` (in elents 'ssurgo' and 'statsgo').
#' 
#' This writes output to the 'soils' sub-directory of `data_dir`:
#' 
#' * 'soil_src.tif', the rasterized and merged soil MUKEY grid in coordinate system of DEM
#' * 'soil.tif', a version warped to UTM (and optionally masked/cropped to the catchment)
#' 
#' `buffer` and `threads` determine if and how the UTM grid is masked (as in `?save_land`)

#' In a normal workflow you should call `get_soil(...)` to write the two source
#' datasets to disk and make the merged raster, then call `save_soil(overwrite=TRUE)`
#' to save the merged raster in the two files listed above.
#' 
#' See also `?save_land`, where `buffer` and `threads` have the same meaning.
#'
#' @param data_dir character path to the directory to use for output files
#' @param soils SpatRaster of soil MUKey values
#' @param overwrite logical if `FALSE` the function just returns the file that would be written
#' @param buffer numeric > 0, padding distance in metres, for masking
#' @param threads logical, passed to `terra::project`
#' 
#' @return the file names to write
#' @export
#'
#' @examples
#' save_soil('/example')
save_soil = function(data_dir, soil=NULL, overwrite=FALSE, buffer=Inf, threads=TRUE) {
  
  # catch invalid calls and switch to file list mode
  if( is.null(soil) & overwrite ) {
    
    warning('overwrite=TRUE but soils was NULL')
    overwrite = FALSE
  }
  
  # set up input/output names
  dest_dir = file.path(data_dir, 'soils')
  model_nm = stats::setNames(nm=c('statsgo', 'ssurgo'))
  model_path = model_nm |> lapply(\(x) save_statsgo(data_dir, model=x, overwrite=FALSE))
  model_path[['soil']] = c(soil_src = file.path(dest_dir, 'soil_src.tif'),
                           soil = file.path(dest_dir, 'soil.tif'))

  if( !overwrite ) return(model_path)
  
  # this function only writes the one output file
  dest_path = model_path[['soil']]
  dest_dir = dest_path |> head(1) |> dirname()

  # make the directory if necessary and remove any existing output file
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  if( any( file.exists(dest_path) ) ) unlink(dest_path)
  
  # write the source layer
  soil |> terra::writeRaster(dest_path['soil_src'])
  
  # open required input files saved by `save_catch`
  boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) 
  dem = save_dem(data_dir)['dem'] |> terra::rast()
  
  # optional mask/crop DEM to reduce CPU time for warp
  soil_mask = soil |> clipr(boundary, buffer=buffer)
  
  # warp the DEM to the template then crop/mask again, in UTM this time
  message('projecting to UTM (GDAL warp)')
  soil_out = soil_mask |> 
    terra::project(dem, method='near', threads=threads) |> 
    clipr(boundary, buffer=buffer)
  
  # write to disk
  soil_out |> terra::writeRaster(dest_path['soil'])
  return(dest_path)
}

#' Fetch STATSGO2 or SSURGO data
#' 
#' Downloads and opens STATSGO2 or SSURGO data covering the bounding box of the DEM
#' created by `get_dem`. For STATSGO2 we use HTTPS to download from the FPAC Box cloud
#' system at the USDA (see ?statsgo_url). For SSURGO we use `FedData::get_ssurgo` to
#' download SSA tile archives and handle ETL with `sf` and `terra`.
#' 
#' The function saves its source files to the 'raw' sub-directory of the 'statsgo' and
#' 'ssurgo' sub-directories of `data_dir`. Get the path to this storage directory for the
#' SSURGO model by calling the following:
#' 
#' `save_statsgo(data_dir, model='ssurgo', overwrite=FALSE)[['raw_dir']]`
#' 
#' SSURGO is very detailed, so when using this model the memory usage may be high,
#' and the intermediate .gpkg file written to the "raw" sub-directory may be large.
#' For example with the Carter's Bridge example (Upper Yellowstone) the memory usage
#' exceeds 10 GB and about 2 GB are written to disk.
#'
#' @param data_dir character path to the directory to use for output files
#' @param model character either 'statsgo' or 'ssurgo'
#'
#' @return sf dataframe object containing a "MUKEY" field and polygon geometries
#' @export
get_statsgo = function(data_dir, model='statsgo') {
  
  # input and output paths in data_dir
  statsgo_path = save_statsgo(data_dir, model=model, overwrite=FALSE)
  bbox_path = save_dem(data_dir, overwrite=FALSE)[['bbox']]
  outlet_path = save_catch(data_dir, overwrite=FALSE)[['outlet']]
  
  # load outlet to find appropriate UTM zone for computations
  crs_utm = outlet_path |> sf::st_read(quiet=TRUE) |> to_utm() |> suppressMessages()
  
  # sanity check 
  msg_error = paste('missing DEM bounding box polygon:', bbox_path)
  msg_suggestion = 'Have you run `get_dem()` yet?'
  if( !file.exists(bbox_path) ) stop(msg_error, '\n', msg_suggestion)
  
  # load boundary polygon created by make_dem.R
  bbox_utm = sf::st_read(bbox_path, quiet=TRUE) |> sf::st_transform(crs_utm)
  
  # process STATSGO2 requests
  if( model == 'statsgo' ) {
    
    # find overlapping states and their abbreviation code
    is_over = us_states |> sf::st_transform(crs_utm) |> sf::st_intersects(bbox_utm, sparse=FALSE)
    abb_over = us_states[['abbr']][is_over]
    
    #  filter to relevant states
    if( !any(is_over) ) stop('bounding box did not overlap with any polygons in us_states')
    statsgo_fetch = statsgo_url[match(abb_over, statsgo_url[['abb']]), ]
    msg_over = us_states[['abbr']][is_over] |> paste(collapse=', ')
    
    # loop over available states and download/load the data
    n_get = nrow(statsgo_fetch)
    poly_list = vector(mode='list', length=n_get)
    message('bounding box overlapped with ', nrow(statsgo_fetch), ' state(s) (', msg_over, ')')
    for(i in seq(n_get) ) {
      
      message('fetching data for ', us_states[['full']][is_over][i])
      
      # download the zip file if it doesn't exist already
      raw_dir = statsgo_path[['raw_dir']]
      if( !dir.exists(raw_dir) ) dir.create(raw_dir, recursive=TRUE) 
      
      path_i = file.path(raw_dir, statsgo_fetch[['file']][i])
      if( !file.exists(path_i) ) download.file(statsgo_fetch[['url']][i],
                                               destfile = path_i,
                                               quiet = TRUE,
                                               mode =' wb')
      
      # unpack to same folder, identify the geometries file
      file_i = path_i |> unzip(exdir=raw_dir)
      idx_dbf = grepl('spatial/.+\\.dbf$', file_i) |> which()
      if( length(idx_dbf) != 1 ) stop('could not locate the geometries file in ', path_i)
      
      # load it and copy to list
      poly_list[[i]] = file_i[idx_dbf] |> sf::st_read(quiet=TRUE)
    }
    
    # merge all state data
    soils_poly = do.call(rbind, poly_list)
    
    # crop to bounding box of interest
    is_in = soils_poly |> sf::st_transform(crs_utm) |> sf::st_intersects(bbox_utm, sparse=FALSE)
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

    # load all the SSA polygons (slow)
    message('loading ', basename(path_ssa), ' and filtering to area of interest')
    ssa_poly = path_ssa |> sf::st_read(quiet=TRUE)
    
    # omit polygons not overlapping with AOI and copy soil survey area keys
    is_in = ssa_poly |> sf::st_transform(crs_utm) |> sf::st_intersects(bbox_utm, sparse=FALSE)
    ssa_in = ssa_poly[is_in, ]
    ssa = ssa_in[['areasymbol']]
    if( length(ssa) == 0 ) {
     
      warning('no SSURGO polygons in AOI')
      return(NULL)
    }
    
    # download/open all data for these overlapping SSAs (writes gpkg file to "raw")
    message('loading SSURGO data from ', length(ssa), ' soil survey area(s)...')
    soil_result = FedData::get_ssurgo(ssa,
                                      label = 'ssa',
                                      raw.dir = raw_dir,
                                      extraction.dir = raw_dir,
                                      force.redo = FALSE) |> suppressWarnings()
    
    # split by SSA to reduce memory demands for intersection
    ssa = soil_result[['spatial']][['AREASYMBOL']] |> unique()
    
    # loop over SSAs
    message('processing MUKEYs in ', length(ssa), ' soil survey area(s)')
    mukey_in_aoi = length(ssa) |> logical()
    pb = txtProgressBar(0, length(ssa), style=3)
    for(i in seq_along(ssa)) {
      
      # identify overlapping polygons from ith SSA 
      poly_i = soil_result[['spatial']][ soil_result[['spatial']][['AREASYMBOL']] == ssa[i], ]
      is_in_aoi = poly_i |> 
        sf::st_geometry() |> 
        sf::st_transform(crs_utm) |> 
        sf::st_intersects(bbox_utm, sparse=FALSE)

      # flag the matching mukey in full list
      mukey_in_aoi[ soil_result[['spatial']][['MUKEY']] %in% poly_i[['MUKEY']][is_in_aoi] ] = TRUE
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    # extract relevant subset
    soils_poly = soil_result[['spatial']][mukey_in_aoi, ]
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
#' with grid matching the DEM downloaded in `get_dem`. Note that the contents of the
#' "raw" sub-directory are written by `get_statsgo`, instead of this function.
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
  dest_fname = c(soil = paste0(model, '.tif'),
                 poly = paste0(model, '.geojson'),
                 raw_dir = 'raw')
  
  # output paths
  dest_path = file.path(dest_dir, dest_fname) |> stats::setNames(names(dest_fname))
  if( !overwrite ) return(dest_path)
  
  # make the directory if necessary
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  
  # remove any existing output files (but not "raw", which is a directory)
  if( any(is_over) ) unlink(dest_path[is_over], recursive=FALSE)
  
  # write polygons to disk
  soils |> sf::st_write(dest_path[['poly']], quiet=TRUE)
  
  # load the DEM (must be in WGS84 to avoid transforming the very large `soils`)
  dem = save_dem(data_dir, overwrite=FALSE)[['dem_src']] |> terra::rast()

  # coerce to SpatVector (via sp) with integer values then rasterize to DEM grid
  message('rasterizing MUKEYs for ', model)
  soils[['MUKEY']] = soils[['MUKEY']] |> as.integer()
  soils_sv = soils['MUKEY'][!sf::st_is_empty(sf::st_geometry(soils)), ]  |> as('Spatial') |> as('SpatVector')
  soils_sv |> terra::rasterize(dem,
                               field = 'MUKEY',
                               fun = 'min',
                               touches = TRUE,
                               filename = dest_path[['soil']])
                                 
  return(dest_path)
}

