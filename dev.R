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

data_dir = 'D:/rswat_data/salmon'
#data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/tuolumne'
#data_dir = 'D:/rswat_data/snake'
#data_dir = 'D:/rswat_data/nooksack' # nice example
#data_dir = 'D:/rswat_data/ausable'

outlet = nominatim_point("Clayton, ID")
#outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
#outlet = nominatim_point("Alpine Junction, WY")
#outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Cooke Dam Pond, MI")

outlet |> fetch_all(data_dir, overwrite=TRUE)


# open previously saved data
catch_list = data_dir |> open_catch()
plot_catch(catch_list)

# open the previously saved sub-catchments data
sub_list = save_split(data_dir)[['sub']] |> lapply(open_catch)
plot_catch(sub_list)


i = 0

i = i + 1
plot_catch(sub_list[[i]])










# TODO: save DEM (and other rasters) in UTM (zone from outlet point) then rebuild
# for rebuilding
if(FALSE) {
  
  # catch_list = outlet |> get_catch()
  # data_dir |> save_catch(catch_list, overwrite=TRUE)
  # plot_catch(catch_list)
  
  # dem = get_dem(data_dir)
  # dem = save_dem(data_dir)['dem_src'] |> terra::rast()
  # data_dir |> save_dem(dem, overwrite=TRUE)
  
  # land = get_land(data_dir)
  # data_dir |> save_land(land, overwrite=TRUE)
  
  # soil = get_soils(data_dir)
  # data_dir |> save_soils(soil, overwrite=TRUE)
  
  #land = save_land(data_dir)['land_src'] |> terra::rast()
  
  # get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
  # update_nwis(data_dir, nwis_nm)
}

# save split datasets
data_dir |> save_split(sub_list, overwrite=TRUE)

# later on, we can update weather like this
data_dir |> update_nwis(nwis_nm)

# TODO: prepare QSWAT+ input files:
sub_dir = save_split(data_dir)[['sub']] |> head(1)

save_soils(sub_dir)[['soil']]['soil'] |> terra::rast() |> terra::plot()
save_land(sub_dir)[['land']] |> terra::rast() |> terra::plot()
save_dem(sub_dir)[['dem']] |> terra::rast() |> terra::plot()


# 1. shapefile for inlets/outlets with specific fields ("RES" etc)
# 2. shapefile for stream network burn-in with specific fields (?)
# 3. ?? shapefile for lakes (reduced to single shape) - these must have single outlet crossing into lake
# 4. landuse lookup GeoTIFF ?? with WATR field <- check if this actually works
# 5. landuse lookup table with specific fields - filename must contain string "landuse" to show up in QSWAT+ - fields 'LANDUSE_ID', 'SWAT_CODE'
# 6. rasters: DEM, landuse, soils (we use SSURGO/STATSGO)
#
# * transform the shapefiles to match DEM (done already for the other rasters)
# * overwrite DEM with WATR at lakes

# output filenames (these will be overwritten!)
out_nm = c(outlet='outlet.shp', 
           stream='stream.shp', 
           dem='dem.tif', 
           soil='soil.tif', 
           land='landuse.tif', 
           land_lookup='landuse.csv')

# output directories
dest_dir = sub_dir |> file.path('qswatplus')
out_path = dest_dir |> file.path(out_nm) |> stats::setNames(names(out_nm))

# load input files
catch_list = sub_dir |> open_catch()
dem = save_dem(sub_dir)['dem'] |> terra::rast()

# all geo-referenced outputs will by in coordinate system of the DEM
crs_out = dem |> sf::st_crs()


# initialize outlet data frame
outlet = catch_list[['outlet']]


# select main inlet(s) in cases of duplicates
if( !is.null(catch_list[['inlet']]) ) {
  
  
  
  
}



# TODO: filter to "main" inlet in case of duplicates (check outlet table for unique site_no)





# new function split_nwis




update_nwis()


catch_list$outlet |> to_utm()





sub_list = split_result


plot_catch(split_result, gage_col=NULL)

names(split_result)

#



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



