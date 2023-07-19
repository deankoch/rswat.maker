# # create a main stem LINESTRING for this set of sub-catchments
# comid_stem = split_result[['boundary']][['comid']] |> comid_down(edge)
# main_stem_utm = flow_utm[ flow_utm[['COMID']] %in% comid_stem, ] |> sf::st_union()
# 
# # split at sub-catchment boundaries and transform back to WGS84
# 
# main_stem_split = main_stem_utm |> sf::st_intersection(boundary_utm) |> sf::st_transform(4326)

# It's easy to find new examples like this. Just browse web app at
# https://dashboard.waterdata.usgs.gov/ and look for small headwater
# basins with active gages (paying attention to the total size of the
# catchment)

# TODO: wrap all the FedData calls etc in tryCatch to get more informative
# errors eg "Error in value[[3L]](cond) : Improperly formatted SDA SQL request"
# means any kind of error from the SSURGO requests (and this can happen when
# you already have the data cached and the server is too busy to respond with
# SSA file names)

# TODO: find important paths defined in the `plugin` class and write them to
# disk while QSWAT+ is open

# TODO: extract sub-basin centroids and write dummy weather files
# TODO: run SWAT+ Editor at the end of `run_qswat`

# yellowstone i=6 THIS WORKS after 1 ITERATION
# snake i=4 THIS WORKS AFTER 2 ITERATIONS

# TODO: check main outlet drainage is nonempty
# snake i=5 1 ITERATION HELPS BUT LEFT WITH MAIN OUTLET ISSUE


library(devtools)
load_all()
document()

data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/snake'


#data_dir = 'D:/rswat_data/nooksack' # nice example
#data_dir = 'D:/rswat_data/tuolumne'
#data_dir = 'D:/rswat_data/salmon'
#data_dir = 'D:/rswat_data/ausable'
#data_dir = 'D:/rswat_data/bigthompson'

outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Alpine Junction, WY")
#outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
#outlet = nominatim_point("Clayton, ID")
#outlet = nominatim_point("Cooke Dam Pond, MI")
#outlet = c(-105.56845, 40.34875) |> sf::st_point() |> sf::st_sfc(crs=4326) # Colorado, near

# this command only updates NWIS if you've already built the project
#outlet |> run_maker(data_dir, overwrite=TRUE, no_download=TRUE)
# sub_list = get_split(data_dir)
# save_split(data_dir, sub_list, overwrite=TRUE)
# qswat_dir = save_qswat(data_dir, sub=T, overwrite=TRUE)

# TODO: save all this somewhere and include elev_m.tif (from NED_dem), aoi.geojson (its extent)
if(0) {
  #data_dir |> plot_rast('dem')
  subs = save_split(data_dir)[['sub']]
  
  # collect all sub-basin geometries from sub-catchments
  sub_df = do.call(rbind, lapply(subs, \(p) cbind(load_qswat(p)[['sub']], 
                                                  project = basename(data_dir), 
                                                  split = basename(p)) ))
  
  sub_geo_df = sub_df |> sf::st_transform(4326)
  
  # pass to wxArchive in a geoJSON file
  aoi_path = 'G:/aoi_export.geojson'
  sub_geo_df |> sf::st_write(aoi_path)
}

save_subs


# snake i=6 can't be saved, a good example of finding problems for manual intervention
# eg nooksack i=8, multiple iterations needed
# eg. 
for(i in seq_along(subs)) {

  message('')
  message(paste(i, '/', length(subs)))
  #subs[i] |> plot_rast('dem')
  
  save_qswat(subs[i], sub=F, overwrite=TRUE)
  qswat_path = run_qswat(subs[i], overwrite=TRUE)
}




#('channel', 'sub')



#check_qswat(data_dir=subs[i], make_plot=TRUE)



check_result = check_qswat(data_dir=subs[i], make_plot=TRUE)
data_dir=subs[i]
make_plot=TRUE
nudge_dist=NULL
allow=0
draw


n
overwrite=TRUE
name = basename(data_dir)
osgeo_dir = NULL
lake_threshold = 50L
min_ncell = 16L
channel_threshold = 1e-3
stream_threshold = 1e-2
snap_threshold = 300L
do_test = FALSE
dem_path = NULL
outlet_path = NULL
nudge_clear = TRUE
nudge_dist = NULL
nudge_nmax = 5
nudge_plot=TRUE
nudge_nm = 'outlet_moved'





# TODO: put snapped outlets into subdirectory, automate loop
# with maximum iterations and total snap distance
if( nrow(check_result[['snap']]) > 0 ) {
  
  # load the existing outlets file
  input_json = run_qswat(subs[i])[['input']] |> readLines() |> jsonlite::fromJSON()
  outlet_path = input_json[['outlet']]
  outlet = outlet_path |> sf::st_read(quiet=TRUE)
  
  # replace inlet(s) with snapped version
  id_replace = check_result[['snap']][['ID']]
  nm_outlet = names(outlet)
  outlet[match(id_replace, outlet[['ID']]), nm_outlet] = check_result[['snap']][nm_outlet]

  # write snapped file
  outlet_temp_path = dirname(outlet_path) |> file.path(basename(tempfile('outlet_snap_'))) |> paste0('.shp')
  outlet |> sf::st_write(outlet_temp_path, quiet=TRUE)

  # run QSwAT+ and check again
  qswat_path = run_qswat(subs[i], overwrite=TRUE, outlet_path=outlet_temp_path)
  check_result = check_qswat(data_dir=subs[i], make_plot=TRUE)
}



if(0) {
  
  # clean the polygon to avoid invalid geometry errors
  nudge_poly = sub_problem |> 
    sf::st_make_valid() |>
    sf::st_union() |> 
    biggest_poly() |> 
    sf::st_transform(crs_out)
  
  
  # DEBUGGING: run anyway
  # proceed only if the outlet of interest lies in this polygon
  if( TRUE ) { #sf::st_intersects(outlet, nudge_poly, sparse=FALSE) ) {
    
    # # outlet/inlet sub-basin
    outlet_basin = swat_channels[['BasinNo']][ swat_channels[['LINKNO']] %in% outlet_linkno ]
    outlet_poly = swat_subs[['Subbasin']][swat_subs[['PolygonId']] %in% outlet_basin]
    #   
    # outlet_poly |> sf::st_area()
    # 
    # sub_problem |> sf::st_buffer(1)
    # 
    # 
    # # crop swat_channels to the smaller polygon  (coerce to POINT for better precision)
    # all_points = swat_channels |> sf::st_cast('POINT') |> suppressWarnings()
    # plot(all_points, pch=16, add=T)
    # 
    # linkno_nudge = all_points[['LINKNO']][sf::st_intersects(all_points, outlet_poly, sparse=FALSE)]
    # 
    # outlet_channel = swat_channels[,]
    # 
    # 
    # 
    # outlet_poly |> plot(col='blue')
    # plot(sf::st_geometry(swat_channels), add=T, col='red')
    # sf::st_crosses(outlet_poly, swat_channels, sparse=FALSE) |> any()
    # sf::st_intersects(swat_channels, outlet_poly, sparse=FALSE, model='open') |> any()
    # sf::st_overlaps(swat_channels, outlet_poly, sparse=FALSE) |> any()
    # 
    # 
    # sf::st_intersects(swat_channels, outlet_poly, sparse=FALSE, model='open')
    
    #outlet_channel = swat_channels[swat_channels[['LINKNO']] %in% outlet_linkno]
    #outlet_channel = swat_channels
    # 
    # sf::st_intersects(outlet, all_subs, sparse=FALSE)
    # outlet_poly = sf::st_geometry(all_subs)[]
    # 
    # sf::st_intersects(all_subs, outlet, sparse=FALSE)
    
    outlet_channel = all_channels
    
    
    # use another helper function to snap to boundary
    catch = sf::st_sf(data.frame(comid='1'), nudge_poly)
    flow = sf::st_sf(data.frame(COMID='1'), geometry=sf::st_geometry(outlet_channel))
    edge_dummy = data.frame(FROMCOMID='1', TOCOMID='1')
    outlet_snap = find_outlet(catch, edge_dummy, flow, outlet) |> sf::st_transform(crs_out)
    
    # make a bubble around the boundary point and convert to set of points
    nudge_options = outlet_snap |>
      sf::st_buffer(nudge_dist) |> 
      sf::st_difference(nudge_poly) |>
      sf::st_cast('POINT')
    
    # nudge towards catchment interior (away from invalid area) by picking max distance option
    sf::st_geometry(outlet) = nudge_options[which.max(sf::st_distance(nudge_options, nudge_poly))]
    
    # report distance then replace coordinates in output points object
    message('total distance : ', round(sf::st_distance(outlet_check[idx,], outlet)), ' m')
    outlet_check[idx,] = outlet
  }
  
}


# # TESTING: try masking the DEM and running again
# na_dem=-32768
# if( !is.null(poly_check) ) {
#   
#   input_json_path = run_qswat(subs[i])[['input']]
#   output_json_path = run_qswat(subs[i])[['output']]
#   input_json = jsonlite::fromJSON(readLines(input_json_path))
#   output_json = jsonlite::fromJSON(readLines(output_json_path))
#   
#   # temporary DEM and outlet file paths
#   dem_path = input_json[['dem']]
#   outlet_path = input_json[['outlet']]
#   snap_path = output_json[['outlet']]
#   dem_temp_path = dirname(dem_path) |> file.path(basename(tempfile('dem_'))) |> paste0('.tif')
#   outlet_temp_path = dirname(outlet_path) |> file.path(basename(tempfile('outlet_'))) |> paste0('.shp')
#   
#   # write masked DEM
#   dem = terra::rast(dem_path)
#   dem_mask_inv = terra::rasterize(poly_check, dem)
#   dem[!is.na(dem_mask_inv)[]] = NA
#   dem |> terra::plot()
#   dem |> terra::writeRaster(dem_temp_path, overwrite=TRUE)
#   
#   # write snapped outlets
#   outlet = snap_path |> sf::st_read(quiet=TRUE)
#   outlet |> sf::st_write(outlet_temp_path, quiet=TRUE)
#   
#   # modify the JSON to load the new masked file
#   # input_json[['dem']] = dem_path
#   # input_json |> jsonlite::toJSON() |> writeLines(input_json_path)
#   
#   # overwrite outlet file with QSWAT+ snapped version
#   
#   # run QSwAT+ again
#   qswat_path = run_qswat(subs[i], overwrite=TRUE, dem_path=dem_temp_path, outlet_path=outlet_temp_path)
# 
# }
# poly_check


make_weather(subs[i], overwrite=TRUE)
run_editor(subs[i], overwrite=TRUE)




# Nooksack
# another bug in Nooksack (i=8) South Fork example
#   File "C:\Users\deank\AppData\Roaming\QGIS\QGIS3\profiles\default\python\plugins\QSWATPlus3_9\QSWATPlus\hrus.py", line 3056, in printBasinsDetails
# basinData = self.basins[basin]
# KeyError: 19
# 
# The delineation is screwy (no channels to main outlet!)
# but it is a weird example with a very small catchment polygon and few gages
# appearing close together. Need to look more closely at what is happening.
# In any case this is another good example for error handling

# TODO: detect when this happens and warn user
# an easy way to do this is by setting project name to "test", which triggers internal
# checks based on drainage area totals. This will cause QSWAT to quit early with an
# error when the inlet point bug happens.

# and another bug at i=11!
# this one looks like it should be easy. Maybe another snapping to junction issue?


# 
# 
# # gage points
# qswat_outlet = save_qswat(subs[i])['outlet'] |> sf::st_read()
# 
# # check if a flow line intersects the point already
# is_complete = qswat_outlet |> sf::st_intersects(qswat_channel, sparse=FALSE) |> c()
# if( any(!is_complete) ) for( idx in which(!is_complete) ) {
# 
#   sf::st_geometry(qswat_outlet)[idx] = sf::st_geometry(qswat_outlet)[idx] |> 
#     sf::st_nearest_points(qswat_channel) |>
#     sf::st_cast('POINT') |> tail(1)
# }
# 
# xx = save_qswat(subs[i])['outlet'] |> sf::st_read()
# 
# j = 0
# j = j + 1
# y = xx[j,] |> sf::st_buffer(units::set_units(500, m)) |> sf::st_geometry()
# y |> plot(border=NA)
# plot(qswat_channel, add=TRUE)
# xx[j,] |> sf::st_geometry() |> plot(add=TRUE, pch=16, cex=2, col='red')
# qswat_outlet[j,] |> sf::st_geometry() |> plot(add=TRUE, pch=16, cex=1, col='green')
# 




#   
#   
#   {
#   
#   qswat_outlet[!is_complete]
#     out_point = sf::st_union(out_line) |> sf::st_nearest_points(boundary) |>
#       sf::st_cast('POINT') |> tail(1)
#   
# }

# TODO: snap

# out_point =  if(is_complete) { 
#   
#   # get point(s) of intersection in order downstream, upstream
#   sf::st_intersection(out_line, boundary, sparse=FALSE) |> 
#     sf::st_cast('POINT') |> head(1)
#   
# } else {
#   
#   # deal with non-intersecting lines separately
#   out_point = sf::st_union(out_line) |> sf::st_nearest_points(boundary) |> 
#     sf::st_cast('POINT') |> tail(1)
# }






plot(sf::st_geometry(xx), add=TRUE)






# p = qswat_info[['sub']] |> dirname() |> file.path('subs1.shp')
# xx = p |> sf::st_read()
# 





#' * QSWAT+ configuration file (JSON)
#' 
#' The configuration file stores the absolute paths of these inputs. This is to
#' simplify system calls to the batch file that `run_qswat` uses to run QSWAT+, and
#' also to create a record of the files used to generate the QSWAT+ project. 



plot_rast(subs[i])
open_catch(subs[i]) |> plot_catch(add=TRUE)





# data_dir = subs[i]
sub=F
overwrite=TRUE
quiet=FALSE
lake_area=0.5
burn=50



# # PUT THIS INTO ITS OWN FUNCTION
# 
# qswat_path = save_qswat(subs[i])[['config']]
# 
# batch_name = 'run_qswatplus.bat'
# batch_dir = path.package('rswat.maker') |> file.path('python')
# osgeo_dir = 'C:/Program Files/QGIS 3.32.0'
# 
# # build the shell command 
# call_string = paste0(batch_name, ' "', qswat_path, '" "', osgeo_dir, '"')
# cd_string = paste('pushd', paste0('"', normalizePath(batch_dir), '" &&'))
# 
# # run QSWAT+ setup
# paste(cd_string, call_string) |> shell()






# catch_list = get_catch(outlet, no_download=T)
# save_catch(data_dir, catch_list, overwrite=TRUE)

#
###
####
###

# land = save_land(data_dir)['land_src'] |> terra::rast()
# terra::plot(land)


#save_qswat(data_dir, sub=TRUE)[['yellowstone_river']]



# dem = save_dem(data_dir)['dem_src'] |> terra::rast()
# # 
# save_dem(data_dir, dem, overwrite=TRUE)
# data_dir |> plot_rast('dem')


# sub_list = get_split(data_dir)
# save_split(data_dir, sub_list, overwrite=TRUE)
# i = 5
# x = file.path(subs[i], 'qswat/test/Scenarios/Default/Results/subs.shp')
# sf::st_read(x, quiet=TRUE) |> sf::st_geometry() |> plot(add=T)
# sf::st_read(x, quiet=TRUE) |> sf::st_geometry() |> sf::st_centroid()



#
###
####
###



# load previously saved catchment
catch_list = data_dir |> open_catch(sub=TRUE)
plot_catch(catch_list)

data_dir |> plot_rast('dem', mask=TRUE)

plot_catch(catch_list, add=TRUE)
data_dir |> plot_rast('soil', mask=TRUE, a=0.5)
data_dir |> plot_rast('land', mask=TRUE)

subs = save_split(data_dir)[['sub']]




i = 0

i = i + 1
subs[i] |> plot_rast('dem')
subs[i] |> open_catch() |> plot_catch(add=T)

subs[i] |> plot_rast('soil', mask=T, a=0.3)
subs[i] |> plot_rast('land', mask=T)


# land = save_land(data_dir)['land_src'] |> terra::rast()
# save_land(data_dir, land, overwrite=TRUE)
# 
# 
# dem = save_dem(data_dir)['dem_src'] |> terra::rast()
# save_dem(data_dir, dem, overwrite=TRUE)
# 
# 
# 

# sub_list = data_dir |> get_split()
# data_dir |> save_split(sub_list, overwrite=TRUE)
# 
# 
# 
# sub_list[[11]]
# plot(boundary_inner_i |> sf::st_geometry())
# plot(boundary_outer_i |> sf::st_geometry(), add=T)


# save_split(data_dir, sub_list, overwrite=TRUE)


# open previously saved data
catch_list = data_dir |> open_catch()
plot_catch(catch_list)

# open the previously saved sub-catchments data
sub_list = data_dir |> open_catch(sub=TRUE)
plot_catch(sub_list)


sub_list




i = 0
i = i + 1
plot_catch(sub_list[[i]])



p = 'C:/SWAT/SWATPlus/ExampleDatasets/Robit/MainOutlet/MainOutlet.shp'
x = sf::st_read(p)
names(x)

# 1. shapefile for inlets/outlets with specific fields ("RES" etc)
# 2. shapefile for stream network burn-in with specific fields (?)
# 3. ?? shapefile for lakes (reduced to single shape) - these must have single outlet crossing into lake
# 4. landuse lookup GeoTIFF ?? with WATR field <- check if this actually works
# 5. landuse lookup table with specific fields - filename must contain string "landuse" to show up in QSWAT+ - fields 'LANDUSE_ID', 'SWAT_CODE'
# 6. rasters: DEM, landuse, soils (we use SSURGO/STATSGO)
#
# * transform the shapefiles to match DEM (done already for the other rasters)
# * overwrite DEM with WATR at lakes

save_qswat(data_dir, sub=F)




# load input files
catch_list = sub_dir |> open_catch()
dem = save_dem(sub_dir)['dem'] |> terra::rast()

# all geo-referenced outputs will be in coordinate system of the DEM
crs_out = dem |> sf::st_crs()

# initialize outlet data frame
outlet = catch_list[['outlet']]






# TODO: filter to "main" inlet in case of duplicates (check outlet table for unique site_no)









# # save a result test
# data_dir_test = 'D:/rswat_data/yellowstone/split'
# data_dir_test |> save_catch(catch_list=split_result[[1]], extra=TRUE, overwrite=TRUE)

# example of two sub-catchments in context of larger basin
catch_list |> plot_catch(stream_col=NULL)
split_result |> plot_catch(stream_col=NULL, lake_col=NULL, fill_col=NULL, border_col=NULL, add=TRUE)
split_result[c(3, 4)] |> plot_catch(stem_col='black', add=TRUE)

split_result[c(3, 4)] |> plot_catch(stem_col='black')

# one of the examples, zoomed in
i = 0
i = i + 1
split_result[[i]] |> plot_catch()


sub_list = catch_list
crs_out = NULL
add = FALSE
lwd = 2
border_col = adjustcolor('white', alpha.f=0.8)
fill_col = adjustcolor('black', alpha.f=0.2)
stem_col = adjustcolor('black', alpha.f=0.5)
stream_col = 'grey50'
lake_col = 'grey20'
gage_col = 'grey50'
outlet_col = 'white'
inlet_col = 'white'
add_scale = TRUE
main = NULL

bottom=TRUE
left=NULL
above=bottom
draw=TRUE
size=1/5
y_adj=0
outer_adj=-0.01
lwd=2
cex=1
col='grey30'
box_col=NA
box_border=NA






# overlay some context
split_result |> plot_catch(add=T, fill_col=NULL)

# plot showing overlap (inlet polygon) areas
plot_catch(catch_list, stream_col=NULL)
plot_catch(split_result, border_col=NULL, inlet_col=NULL, add=TRUE)




# save split datasets
data_dir |> save_split(sub_list, overwrite=TRUE)

# later on, we can update weather like this
data_dir |> update_nwis(nwis_nm)

# TODO: prepare QSWAT+ input files:
sub_dir = save_split(data_dir)[['sub']] |> head(1)

save_soil(sub_dir)[['soil']]['soil'] |> terra::rast() |> terra::plot()
save_land(sub_dir)[['land']] |> terra::rast() |> terra::plot()
save_dem(sub_dir)[['dem']] |> terra::rast() |> terra::plot()

