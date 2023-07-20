#' Make a QSWAT+-friendly copy of your (sub-)catchment data files
#' 
#' This function creates a set of files ready to load in QSWAT+ and sufficient to
#' define and construct a basic SWAT+ model. When `overwrite=FALSE` the function
#' writes nothing but returns the file paths that would be written.
#' 
#' When `overwrite=TRUE` and `sub=FALSE` the following are written to the "qswat"
#' sub-directory of your `data_dir`.
#' 
#' * copy of the DEM with streams burned-in (as GeoTIFF)
#' * land use raster
#' * STATSGO2/SSURGO MUKEY raster (as GeoTIFF)
#' * lookup table for land use (as CSV)
#' * inlets/outlets shape file
#' 
#' Users must first run all of the `get_*` functions (`get_catch`, `get_dem`, etc) to
#' download the necessary datasets before calling `save_qswat`. Note that calling this
#' function with `overwrite=TRUE` does not delete existing QSWAT+ project files found in
#' "qswat" (but these should be regenerated with `run_qswat` after making any changes to
#' inputs). 
#' 
#' Burn-in refers to artificially reducing the elevation in the DEM under known stream reaches.
#' This is to assist the TauDEM algorithm (used in QSWAT+) in finding the correct routing
#' network based on the DEM alone. The function creates stream reaches by expanding flow lines
#' to form channel polygons of width `burn`. It then uses `terra::rasterizes` to reduce
#' the value of any DEM pixel that overlaps with a channel by the fixed value `burn`.
#' 
#' The main outlet is always included in the output, along with any inlets listed in the
#' 'inlet.geojson' and 'gage.geojson' files (created by get_split). If the 'site_no' for
#' one of these inlets/outlets matches a row in the gage file, then the coordinates for
#' that point are replaced with the (unsnapped) gage coordinates.
#' 
#' If `sub=TRUE` the function processes the sub-catchments created by `get_split` in a loop
#' (equivalent to passing these sub-directories one at a time to `save_qswat` with `sub=FALSE`).
#' This produces a complete set of QSWAT+ files for each of the sub-directories in "split"
#' 
#' All geo-referenced outputs are written in UTM coordinates, where the zone is
#' determined by the main outlet point location (see `?get_utm`).
#' 
#' Lakes are burned into the land use raster with the integer code signifying 'watr'. Other
#' `NA` values in this raster are assigned the (integer) code `na_land`, or if it is `NULL`,
#' the mode (ie most frequent non-`NA` code, or the first encountered in case of ties).
#' `NA` soil values are handled the same way; The function uses the `na_soil` MUKEY , or
#' if it is `NULL`, the mode.
#' 
#' Note that weather files are the responsibility of the user, and should be added after
#' calling this function (and possibly after running QSWAT+ to find HRU locations).
#'
#' @param data_dir character path to the directory for input/output files
#' @param sub logical, if `TRUE` the function processes sub-catchments in "split/"
#' @param overwrite logical, whether to write the output or just return the file paths
#' @param lake_area numeric (in km2), lakes with area lower than `lake_area` are omitted
#' @param burn numeric >= 0, stream reach burn-in depth and width, in meters
#' @param na_land integer value to replace NaN in land use raster (or NULL to set to mode)
#' @param na_soil integer value to replace NaN in soils raster (or NULL to set to mode)
#' @param na_dem integer value to replace NaN in dem raster (or NULL to set to mode)
#' @param quiet logical, suppresses certain messages (for internal use)
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_qswat('/example')
save_qswat = function(data_dir,
                      sub = FALSE,
                      overwrite = FALSE,
                      quiet = FALSE,
                      lake_area = 0.5,
                      burn = 50, 
                      na_soil = NULL,
                      na_land = NULL,
                      na_dem = -32768) {

  # template for inlet/outlet shapefile fields
  main_outlet = data.frame(ID=0, RES=0, INLET=0, PTSOURCE=0)
  
  # NLCD land use code to write to lake pixels
  watr = land_use_lookup[['id']][ land_use_lookup[['name']] == 'watr' ] |> head(1)
  
  # handle sub-catchments with recursion
  if( sub ) {
    
    # get directories of sub-catchments (or length-0 character)
    split_dir = save_split(data_dir)[['gage']] |> dirname()
    sub_dir = save_split(data_dir)[['sub']]
    msg_error = paste('no sub-catchments directories in ', split_dir)
    msg_empty = '\nHave you run `save_split` yet?'
    if( overwrite & ( length(sub_dir) == 0 ) ) stop(msg_error, msg_empty)
    
    # vectorized case returns a named list
    sub_dir = sub_dir |> stats::setNames(basename(sub_dir))
    if( overwrite & !quiet ) message('writing inputs for ', length(sub_dir), ' sub-catchments')
    return( lapply(sub_dir, \(d) save_qswat(d, 
                                            overwrite=overwrite, 
                                            quiet=TRUE,
                                            lake_area=lake_area,
                                            burn=burn)) )
  }

  # set output directory
  dest_dir = data_dir |> file.path('qswat')
  if( overwrite & !quiet ) message('writing ', basename(data_dir), ' project to ', dest_dir)

  # output filenames
  out_nm = c(outlet='outlet.shp', 
             dem='dem.tif', 
             soil='soil.tif', 
             land='landuse.tif', 
             land_lookup='landuse.csv')
  
  # output paths to (over)write
  dest_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # input geometries
  catch_list = data_dir |> open_catch()
  
  # input rasters that need modification
  dem = save_dem(data_dir)['dem'] |> terra::rast()
  land = save_land(data_dir)['land'] |> terra::rast()
  soil = save_soil(data_dir)[['soil']]['soil'] |> terra::rast()
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])
  
  # helper to find most frequent non-NA value in a categorical raster (or first encountered)
  my_mode = \(r) r[] |> na.omit() |> table() |> sort(decreasing=TRUE) |> names() |> head(1) |> c()
  
  # set default NA values
  if( is.null(na_soil) ) na_soil = my_mode(soil) |> as.integer()
  if( is.null(na_land) ) na_land = my_mode(land) |> as.integer()
  if( is.null(na_dem) ) na_dem = my_mode(dem) 
  
  ## PROCESS SOIL
  
  soil[is.na(soil)] = na_soil
  soil |> terra::writeRaster(dest_path[['soil']])
  
  ## PROCESS DEM
  
  # set up flow lines to burn
  crs_out = sf::st_crs(dem)
  flow_burn = catch_list[['flow']] |> 
    sf::st_geometry() |> 
    sf::st_transform(crs_out)
  
  # rasterize to same grid as DEM
  burn_out = data.frame(x=1) |> 
    sf::st_sf(geometry=flow_burn) |>
    terra::rasterize(dem, field = 'x', fun = 'min', touches = TRUE)
                                                                                      
  # deepen the overlapping pixels by `burn` and write result to disk
  dem[ !is.na(burn_out) ] = dem[ !is.na(burn_out) ] - burn
  
  # QSWAT identifies NAs as minimum of the elevation values
  dem[ is.na(dem) ] = na_dem
  dem |> terra::writeRaster(dest_path[['dem']])
  

  ## PROCESS LAND
  
  # get flow line geometries in the right projection
  flow = catch_list[['flow']] |> sf::st_geometry() |> sf::st_transform(crs_out) |> sf::st_union()
  
  # copy lakes meeting area criterion
  if( nrow(catch_list[['lake']]) == 0 ) { n_lake = 0 } else {
    
    lake = catch_list[['lake']] |> sf::st_geometry() |> sf::st_transform(crs_out)
    n_lake = length(lake)
  }
  
  # add lakes that intersect with a flow line
  if( n_lake > 0 ) {

    # look for intersections in target coordinate system
    is_burned = lake |> sf::st_intersects(flow, sparse=FALSE)
    if( any(is_burned) ) {

      # keep only the first polygon from each lake
      lake_burn = lake[is_burned]
      for( i in seq(sum(is_burned)) ) lake_burn[i] = sf::st_cast(lake_burn[i], 'POLYGON')[1] 
      
      # coerce to SpatVector (via sp) with integer dummy value
      lake_burn_sv = data.frame(lake=1) |> 
        sf::st_sf(geometry=lake_burn) |> 
        as('Spatial') |> 
        as('SpatVector')
      
      # rasterize to same grid as land
      land_lake = lake_burn_sv |> terra::rasterize(land,
                                                   field = 'lake',
                                                   fun = 'min',
                                                   touches = TRUE)
      
      # copy water code to the pixels in land raster
      land[ !is.na(land_lake) ] = watr
    }
  }
  
  # new sequential encoding for land use keys to replace NLCD system
  old_id = land[ !is.na(land) ] |> unique() |> c()
  new_id = cbind(old_id, seq_along(old_id)) 
  land_out = land |> terra::classify(new_id)
  land_out[is.na(land_out)] = na_land
  land_out |> terra::writeRaster(dest_path[['land']])
  
  # make the lookup table
  idx_name = match(old_id, land_use_lookup[['id']])
  if( any( is.na(idx_name) ) ) stop('unrecognized NLCD ID')
  land_df = data.frame(LANDUSE_ID = seq_along(old_id),
                       SWAT_CODE = land_use_lookup[['name']][idx_name]) 

  # save as CSV
  land_df |> write.csv(dest_path[['land_lookup']], row.names=FALSE, quote=FALSE)
  
  # COMID of main outlet for the catchment
  outlet = catch_list[['outlet']] |> sf::st_transform(crs_out)
  comid = catch_list[['outlet']][['comid']]
  site_no = catch_list[['outlet']][['site_no']]
  
  # use outlet gage coordinates (rather than NHD outlet) if they are available
  if( !is.null(catch_list[['gage']]) ) {
  
    # omit NA site numbers (probably a main outlet added by this package)
    gage = catch_list[['gage']] |> sf::st_transform(crs_out)
    gage = gage[ !is.na((gage[['site_no']])), ]
    if( site_no %in% na.omit(gage[['site_no']]) ) outlet = gage[gage[['site_no']] == site_no, ]
  }
  
  # initialize QSWAT+ compatible inlet/outlet data frame with outlet gage location
  io_df = main_outlet |> sf::st_sf(geometry=sf::st_geometry(outlet))
  
  # check for inlets (these may be missing in non-split case)
  if( !is.null(catch_list[['inlet']]) ) {
    
    #  inlet points are a subset of gage 
    inlet_df = catch_list[['inlet']] |> sf::st_transform(crs_out)
    if( nrow(inlet_df) > 0 ) {

      # replace with exact gage location if available
      inlet_pt = inlet_df |> sf::st_geometry()
      is_gaged = inlet_df[['site_no']] %in% gage[['site_no']]
      if( any(is_gaged) )  {
        
        idx_swap = match(inlet_df[['site_no']][is_gaged], gage[['site_no']])
        inlet_pt[is_gaged] = sf::st_geometry(gage[idx_swap,])
      }
      
      # make inlet data frame in QSWAT+ format
      inlet_df = do.call(rbind, rep(list(main_outlet), length(inlet_pt))) |> 
        sf::st_sf(geometry=inlet_pt)
      
      # update fields and join with outlets
      inlet_df[['INLET']] = 1L
      inlet_df[['ID']] = nrow(inlet_df) |> seq()
      io_df = io_df |> rbind(inlet_df)
    }
  }

  # write the shape file (and its babies)
  io_df |> sf::st_write(dest_path[['outlet']], quiet=TRUE)

  # return all paths without printing them
  return( invisible(dest_path) )
}
