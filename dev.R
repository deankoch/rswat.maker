# # create a main stem LINESTRING for this set of sub-catchments
# comid_stem = split_result[['boundary']][['comid']] |> comid_down(edge)
# main_stem_utm = flow_utm[ flow_utm[['COMID']] %in% comid_stem, ] |> sf::st_union()
# 
# # split at sub-catchment boundaries and transform back to WGS84
# 
# main_stem_split = main_stem_utm |> sf::st_intersection(boundary_utm) |> sf::st_transform(4326)

library(devtools)
load_all()
document()

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


#data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/nooksack' # nice example
#data_dir = 'D:/rswat_data/tuolumne'
data_dir = 'D:/rswat_data/snake'
#data_dir = 'D:/rswat_data/salmon'
#data_dir = 'D:/rswat_data/ausable'
#data_dir = 'D:/rswat_data/bigthompson'

#outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
outlet = nominatim_point("Alpine Junction, WY")
#outlet = nominatim_point("Clayton, ID")
#outlet = nominatim_point("Cooke Dam Pond, MI")
#outlet = c(-105.56845, 40.34875) |> sf::st_point() |> sf::st_sfc(crs=4326) # Colorado, near

# this command only updates NWIS if you've already built the project
outlet |> fetch_all(data_dir, overwrite=TRUE, no_download=TRUE)


# # # 
# sub_list = get_split(data_dir)
# save_split(data_dir, sub_list, overwrite=TRUE)
# qswat_dir = save_qswat(data_dir, sub=T, overwrite=TRUE)




data_dir |> plot_rast('dem')
subs = save_split(data_dir)[['sub']]

i = 0
i = i + 1
paste(i, '/', length(subs)) |> cat()
subs[i] |> plot_rast('dem')

#save_qswat(subs[i], sub=F, overwrite=TRUE)
qswat_path = run_qswat(subs[i], overwrite=TRUE, do_check=TRUE)


# TODO: check percent overlap of old and new boundary and set a 
# conservative threshold to catch inlet issues like like snake i=5,6
# subs_sf = qswat_path['sub'] |> 
#   sf::st_read(quiet=T) |> 
#   dplyr::filter(Subbasin != 0) |> 
#   sf::st_geometry() |>
#   sf::st_make_valid() |> 
#   sf::st_union() |>
#   
#   
# 
# boundary = save_catch(data_dir)['boundary'] |> 
#   sf::st_read(quiet=T) |> 
#   sf::st_transform(sf::st_crs(subs_sf)) |>
#   sf::st_geometry()

make_weather(subs[i], overwrite=TRUE)
run_editor(subs[i], overwrite=TRUE)

# fetch shapefile data from QSWAT+
channel_sf = qswat_path['channel'] |> sf::st_read(quiet=T)
subs_sf = qswat_path['sub'] |> sf::st_read(quiet=T)
hru_sf = qswat_path['hru_full'] |> sf::st_read(quiet=T)
lsu_sf = qswat_path['lsu_full'] |> sf::st_read(quiet=T)

# overlay on DEM plot
subs_sf |> dplyr::filter(Subbasin != 0) |> sf::st_geometry() |> plot(add=TRUE, border=adjustcolor('white', .5))
idx_in = subs_sf |> dplyr::filter(Subbasin != 0) |> dplyr::pull(PolygonId)
channel_sf[channel_sf$BasinNo %in% idx_in,] |> sf::st_geometry() |> plot(add=TRUE, col=adjustcolor('blue', .5))
lsu_sf |> sf::st_geometry() |> plot(add=T, col=adjustcolor('white', .2), border=NA)



data_dir = subs[i]
overwrite = TRUE
lake_threshold=50L
channel_threshold = 1e-3
stream_threshold = 1e-2
osgeo_dir=NULL
snap_threshold = 300L
do_check = TRUE
name = basename(data_dir)
quiet = FALSE




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
# batch_dir = path.package('rswat.uyr') |> file.path('python')
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

