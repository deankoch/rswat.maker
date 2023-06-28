#' Make a QSWAT+-friendly copy of the output of `get_catch` or `get_split`
#' 
#' This function creates a set of files ready to load in QSWAT+ and sufficient to
#' define and construct a basic SWAT+ model. This includes: the DEM, land use, and
#' soil MUKEY rasters (as GeoTIFF); a lookup table for land use; an inlets/outlets
#' shape file; and a stream network shape file (to burn in the DEM).
#' 
#' When `overwrite=TRUE` the function writes these files to the "qswat" sub-directory
#' of the catchment specified by `data_dir`. If `sub=TRUE` the function does this in a
#' loop over the sub-catchment found in the 'split' sub-directory of `data_dir`. When
#' `overwrite=FALSE` the function writes nothing but returns the file paths that would
#' be written.
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
#'
#' @return the file names to write
#' @export
#'
#' @examples
#' save_qswat('/example')
save_qswat = function(data_dir, sub=FALSE, overwrite=FALSE, lake_area=0.5, quiet=FALSE) {
  
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
  
  if(!quiet) message('writing ', basename(data_dir))
  
  # set output directory
  dest_dir = data_dir |> file.path('qswat')

  # output filenames
  out_nm = c(outlet='outlet.shp', 
             stream='stream.shp', 
             dem='dem.tif', 
             soil='soil.tif', 
             land='landuse.tif', 
             land_lookup='landuse.csv')
  
  # output paths to (over)write
  dest_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))
  if( !overwrite ) return(dest_path)
  
  # input paths for direct copy
  dem_path = save_dem(data_dir)['dem']
  soil_path = save_soils(data_dir)[['soil']]['soil']
  
  # load input files
  catch_list = data_dir |> open_catch()
  land = save_land(data_dir)['land'] |> terra::rast()
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
  
  # soil and DEM are already good to go by direct copy
  dem_path |> file.copy(dest_path[['dem']])
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
