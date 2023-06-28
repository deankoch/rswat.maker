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


#data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/tuolumne'
#data_dir = 'D:/rswat_data/snake'
#data_dir = 'D:/rswat_data/salmon'
#data_dir = 'D:/rswat_data/nooksack' # nice example
#data_dir = 'D:/rswat_data/ausable'
data_dir = 'D:/rswat_data/bigthompson'

#outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
#outlet = nominatim_point("Alpine Junction, WY")
#outlet = nominatim_point("Clayton, ID")
#outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Cooke Dam Pond, MI")
outlet = c(-105.56845, 40.34875) |> sf::st_point() |> sf::st_sfc(crs=4326) # Colorado, near

# this command does nothing if you've already built the project
outlet |> fetch_all(data_dir, overwrite=TRUE)


# 
# land = save_land(data_dir)['land_src'] |> terra::rast()
# save_land(data_dir, land, overwrite=TRUE)
# 
# 
# dem = save_dem(data_dir)['dem_src'] |> terra::rast()
# save_dem(data_dir, dem, overwrite=TRUE)
# 
# 
# 

sub_list = data_dir |> get_split()
data_dir |> save_split(sub_list, overwrite=TRUE)



sub_list[[11]]
plot(boundary_inner_i |> sf::st_geometry())
plot(boundary_outer_i |> sf::st_geometry(), add=T)


# save_split(data_dir, sub_list, overwrite=TRUE)


# open previously saved data
catch_list = data_dir |> open_catch()
plot_catch(catch_list)

# open the previously saved sub-catchments data
sub_list = data_dir |> open_catch(sub=TRUE)
plot_catch(sub_list)

save_qswat(data_dir, sub=T, overwrite=TRUE)


catch_list[['boundary_outer']] |> sf::st_geometry() |> plot()
catch_list[['boundary']] |> sf::st_geometry() |> plot(add=T)
catch_list$outlet |> sf::st_geometry() |> plot(add=TRUE, pch=16, cex=2)




names(sub_list)

sub_list$soda_butte_cr_nr_lamar_ranger_station_ynp$boundary_outer |> sf::st_geometry() |> plot()
sub_list$soda_butte_cr_nr_lamar_ranger_station_ynp$boundary |> sf::st_geometry() |> plot()

y = save_dem('D:/rswat_data/yellowstone/split/soda_butte_cr_nr_lamar_ranger_station_ynp')['dem'] |> terra::rast()
terra::plot(y, reset=FALSE)





x = "D:/rswat_data/yellowstone/split/soda_butte_cr_nr_lamar_ranger_station_ynp/qswat/outlet.shp" |> sf::st_read()
y = "D:/rswat_data/yellowstone/split/soda_butte_cr_nr_lamar_ranger_station_ynp/qswat/dem.tif" |> terra::rast()

terra::plot(y, reset=FALSE)
plot(sf::st_geometry(x) |> sf::st_transform(sf::st_crs(y)), add=T)



data_dir = file.path(data_dir, 'split', names(sub_list)[i])
sub=FALSE
overwrite=TRUE
lake_area=0.5




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

save_soils(sub_dir)[['soil']]['soil'] |> terra::rast() |> terra::plot()
save_land(sub_dir)[['land']] |> terra::rast() |> terra::plot()
save_dem(sub_dir)[['dem']] |> terra::rast() |> terra::plot()

