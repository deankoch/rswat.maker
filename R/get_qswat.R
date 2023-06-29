#' Make a QSWAT+-friendly copy of your (sub-)catchment data files
#' 
#' This function creates a set of files ready to load in QSWAT+ and sufficient to
#' define and construct a basic SWAT+ model. When `overwrite=FALSE` the function
#' writes nothing but returns the file paths that would be written.
#' 
#' When `overwrite=TRUE` and `sub=FALSE` the following are written to the "qswat"
#' sub-directory of your `data_dir`:
#' 
#' * two copies of the DEM, with and without burn-in (as GeoTIFF)
#' * a land use raster, and a STATSGO2/SSURGO MUKEY raster (as GeoTIFF)
#' * a lookup table for land use (as CSV)
#' * an inlets/outlets shape file
#' * a stream network shape file
#' 
#' 'dem_burn.tif', a copy of the UTM raster with stream reaches burned to depth `burn` (in meters)
#' Burn-in refers to artificially reducing the elevation in the DEM under known stream reaches.
#' This is to assist the TauDEM algorithm (used in QSWAT+) in finding the correct routing
#' network based on the DEM alone. The function creates stream reaches by expanding flow lines
#' to form channel polygons of width `burn`. It then uses `terra::rasterizes` to reduce the value
#' of any DEM pixel that overlaps with a channel by the fixed value `burn`.
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
#' @param burn numeric >= 0, burn-in depth for flow lines in meters
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_qswat('/example')
save_qswat = function(data_dir, sub=FALSE, overwrite=FALSE, lake_area=0.5, burn=50,  quiet=FALSE) {
  
  # template for inlet/outlet shapefile fields
  io_temp = data.frame(ID=0, RES=0, INLET=0, PTSOURCE=0)
  
  # the land use code to write to lake pixels
  watr = 11L
  
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
    
    for(d in sub_dir) save_qswat(d, overwrite=overwrite, quiet=TRUE)
    
    return( lapply(sub_dir, \(d) save_qswat(d, overwrite=overwrite, quiet=TRUE)) )
  }
  
  if( overwrite & !quiet ) message('writing ', basename(data_dir))
  
  # set output directory
  dest_dir = data_dir |> file.path('qswat')

  # output filenames
  out_nm = c(outlet='outlet.shp', 
             stream='stream.shp', 
             dem='dem.tif', 
             dem_burn='dem_burn.tif', 
             soil='soil.tif', 
             land='landuse.tif', 
             land_lookup='landuse.csv')
  
  # output paths to (over)write
  dest_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # input paths for direct copy
  dem_path = save_dem(data_dir)['dem']
  #dem_burn_path = save_dem(data_dir)['dem_burn']
  soil_path = save_soils(data_dir)[['soil']]['soil']
  
  ####
  # # expand flow lines then rasterize to DEM grid
  # message('burning flow lines')
  # flow_out = flow |> sf::st_geometry() |> sf::st_transform(crs_out) |> sf::st_buffer(burn)
  # burn_out = data.frame(dummy=1) |> 
  #   sf::st_sf(geometry=flow_out) |> 
  #   terra::rasterize(dem_out,
  #                    field = 'dummy',
  #                    fun = 'min',
  #                    touches = TRUE)
  # 
  # # make a burn-in version of DEM and save to disk
  # dem_burn = dem_out
  # dem_burn[ !is.na(burn_out) ] = dem_burn[ !is.na(burn_out) ] - burn
  # dem_burn |> terra::writeRaster(dest_path[['dem_burn']])
  # flow = save_catch(data_dir)['flow'] |> sf::st_read(quiet=TRUE) 
  
  
  ####
  
  # load input files
  catch_list = data_dir |> open_catch()
  land = save_land(data_dir)['land'] |> terra::rast()
  dem_burn = save_dem(data_dir)['dem_burn'] |> terra::rast()
  land_df = save_land(data_dir)['lookup'] |> read.csv()
  
  # all geo-referenced outputs in CRS matching DEM (based on outlet)
  crs_out = dem_path |> terra::rast() |> sf::st_crs()
  
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

      # check for existing water code
      is_watr = land_df[['SWAT_CODE']] == 'watr'
      if( any(is_watr) ) {
        
        # check that the code provided is consistent with the existing one
        watr_old = land_df[['LANDUSE_ID']][is_watr]
        if( watr_old != watr ) stop('unexpected value for watr in existing lookup table')
        
      } else {
        
        # add the field to lookup table if it isn't there already
        land_df = data.frame(SWAT_CODE='watr', LANDUSE_ID=watr) |> rbind(land_df)
      }
      
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
  
  # make the directory if necessary and remove any existing output files
  if( !dir.exists(dest_dir) ) dir.create(dest_dir, recursive=TRUE)
  is_over = file.exists(dest_path)
  if( any(is_over) ) unlink(dest_path[is_over])
  
  # write the stream network
  flow |> sf::st_write(dest_path[['stream']], quiet=TRUE)
  
  # write the new land use raster and lookup table
  land |> terra::writeRaster(dest_path[['land']])
  land_df |> write.csv(dest_path[['land_lookup']], row.names=FALSE, quote=FALSE)
  
  # TODO:
  # replace with no-data value mentioned in the QSWAT+ Manual
  # QSWAT identifies NAs as minimum of the values in the raster so the
  # exact number doesn't matter
  #dem_burn[ is.na(dem_burn) ] = -32768
  dem_burn[ is.na(dem_burn) ] = -32768
  dem_burn |> terra::writeRaster(dest_path[['dem_burn']])
  
  # soil and DEM are already good to go by direct copy
  dem_path |> file.copy(dest_path[['dem']])
  #dem_burn_path |> file.copy(dest_path[['dem_burn']])
  soil_path |> file.copy(dest_path[['soil']])
  
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
  
  # write the shapefile
  io_df |> sf::st_write(dest_path[['outlet']], quiet=TRUE)

  # return all paths without printing them
  return( invisible(dest_path) )
}
