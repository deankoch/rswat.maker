#' Make a QSWAT+-friendly copy of your (sub-)catchment data files
#' 
#' This function creates a set of files ready to load in QSWAT+ and sufficient to
#' define and construct a basic SWAT+ model. When `overwrite=FALSE` the function
#' writes nothing but returns the file paths that would be written.
#' 
#' When `overwrite=TRUE` and `sub=FALSE` the following are written to the "qswat"
#' sub-directory of your `data_dir`:
#' 
#' * copy of the DEM with streams burned-in (as GeoTIFF)
#' * land use raster
#' * STATSGO2/SSURGO MUKEY raster (as GeoTIFF)
#' * lookup table for land use (as CSV)
#' * inlets/outlets shape file
#' * QSWAT+ configuration file (JSON)
#' 
#' The configuration file stores the absolute paths of these inputs. This is to
#' simplify system calls to the batch file that `run_qswat` uses to run QSWAT+, and
#' also to create a record of the files used to generate the QSWAT+ project. 
#' 
#' Burn-in refers to artificially reducing the elevation in the DEM under known stream reaches.
#' This is to assist the TauDEM algorithm (used in QSWAT+) in finding the correct routing
#' network based on the DEM alone. The function creates stream reaches by expanding flow lines
#' to form channel polygons of width `burn`. It then uses `terra::rasterizes` to reduce
#' the value of any DEM pixel that overlaps with a channel by the fixed value `burn`.
#' 
#' Outlets and inlets are automatically snapped to the nearest flow-line.
#' 
#' If `sub=TRUE` the function writes its output to the sub-catchments directories
#' created by `get_split`, in a loop. This produces a complete set of QSWAT+ files
#' for each sub-catchment in "split".
#' 
#' All geo-referenced outputs are written in UTM coordinates, where the zone is
#' determined by the main outlet point location (see `?to_utm`). Lakes are burned into
#' the land use raster (code 'watr') and all raster outputs are cropped and masked to
#' the catchment boundary.
#' 
#' Note that weather files are the responsibility of the user, and should be added after
#' calling this function (and possibly after running QSWAT+ to find HRU locations).
#'
#' @param data_dir character path to the directory for input/output files
#' @param sub logical, if `TRUE` the function processes sub-catchments in "split/"
#' @param overwrite logical, if `TRUE` the function writes to output files if they don't exist
#' @param lake_area numeric (in km2), lakes with area lower than `lake_area` are omitted
#' @param burn numeric >= 0, stream reach burn-in depth and width, in meters
#' @param snap_tol numeric >= 0, snapping tolerance distance, in meters
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_qswat('/example')
save_qswat = function(data_dir, sub=FALSE, overwrite=FALSE, quiet=FALSE,
                      lake_area=0.5, burn=50, snap_tol=300) {
  
  # NAs value in soil raster (per QSWAT+ Manual)
  na_soil = -99L
  
  # template for inlet/outlet shapefile fields
  io_temp = data.frame(ID=0, RES=0, INLET=0, PTSOURCE=0)
  
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
                                            burn=burn,
                                            snap_tol=snap_tol)) )
  }
  
  if( overwrite & !quiet ) message('writing ', basename(data_dir))
  
  # set output directory
  dest_dir = data_dir |> file.path('qswat')

  # output filenames
  out_nm = c(outlet='outlet.shp', 
             dem='dem.tif', 
             soil='soil.tif', 
             land='landuse.tif', 
             land_lookup='landuse.csv',
             config='config.json')
  
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
  
  ## PROCESS SOIL
  
  # use a different encoding for NAs
  soil[is.na(soil)] = na_soil
  soil |> terra::writeRaster(dest_path[['soil']])

  ## PROCESS DEM
  
  # make channel polygons by adding width to flow lines
  crs_out = sf::st_crs(dem)
  flow_burn = catch_list[['flow']] |> 
    sf::st_geometry() |> 
    sf::st_transform(crs_out) |> 
    sf::st_buffer(burn)
  
  # rasterize to same grid as DEM
  burn_out = data.frame(x=1) |> 
    sf::st_sf(geometry=flow_burn) |>
    terra::rasterize(dem, field = 'x', fun = 'min', touches = TRUE)
                                                                                      
  # deepen the overlapping pixels by `burn` and write result to disk
  dem[ !is.na(burn_out) ] = dem[ !is.na(burn_out) ] - burn
  
  # QSWAT identifies NAs as minimum of the elevation values (?) This value is from manual
  dem[ is.na(dem) ] = -32768
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
  land_out |> terra::writeRaster(dest_path[['land']])
  
  # make the lookup table
  idx_name = match(old_id, land_use_lookup[['id']])
  if( any( is.na(idx_name) ) ) stop('unrecognized NLCD ID')
  land_df = data.frame(LANDUSE_ID = seq_along(old_id),
                       SWAT_CODE = land_use_lookup[['name']][idx_name]) 

  # save as CSV
  land_df |> write.csv(dest_path[['land_lookup']], row.names=FALSE, quote=FALSE)
  
  # create QSWAT+ compatible inlet/outlet data frame
  outlet = catch_list[['outlet']] |> sf::st_transform(crs_out)
  comid = outlet[['comid']]
  io_df = io_temp |> sf::st_sf(geometry=sf::st_geometry(outlet))
  
  # first check for inlets (may be missing in non-split case)
  if( !is.null(catch_list[['inlet']]) ) {
    
    # if no inlets, this dataframe will be empty
    if( nrow(catch_list[['inlet']]) > 0 ) {
      
      # copy inlet points, omitting duplicates
      gage_df = catch_list[['gage']] |> dplyr::filter(inlet) |> sf::st_transform(crs_out)
      gage_df = gage_df[ gage_df[['site_no']] %in% catch_list[['inlet']][['site_no']], ]
      
      # make inlet data frame in QSWAT+ format
      inlet_df = do.call(rbind, rep(list(io_temp), nrow(gage_df))) |> 
        sf::st_sf(geometry=sf::st_geometry(gage_df))
      
      # update fields and join with outlets
      inlet_df[['INLET']] = 1L
      inlet_df[['ID']] = nrow(inlet_df) |> seq()
      io_df = io_df |> rbind(inlet_df)
    }
  }
  
  # snap outlet points to flow lines
  old_pt = sf::st_geometry(io_df)
  new_pt = do.call(c, lapply(seq_along(old_pt), \(i) {
    
    # make linestring connecting point to nearest flow line, extract intersection point
    sf::st_nearest_points(old_pt[i], flow) |> 
      sf::st_cast('POINT') |> 
      tail(1)
  }))
  
  # write the shapefile with snapped coordinates
  sf::st_geometry(io_df) = new_pt
  io_df |> sf::st_write(dest_path[['outlet']], quiet=TRUE)
  
  # make an rswat configuration file for running QSWAT+
  list(info = paste('configuration file created by rswat on', Sys.Date()),
       name = basename(data_dir),
       dem = dest_path[['dem']],
       outlet = dest_path[['outlet']],
       landuse_lookup = dest_path[['land_lookup']],
       landuse = dest_path[['land']],
       soil = dest_path[['soil']],
       lake_threshold = 50L) |> 
    jsonlite::toJSON(pretty=TRUE) |>
    write(dest_path[['config']])

  # return all paths without printing them
  return( invisible(dest_path) )
}
