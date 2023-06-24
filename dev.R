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

data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/tuolumne'
#data_dir = 'D:/rswat_data/snake'
#data_dir = 'D:/rswat_data/nooksack' # nice example
#data_dir = 'D:/rswat_data/ausable'


outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
#outlet = nominatim_point("Alpine Junction, WY")
#outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Cooke Dam Pond, MI")

nwis_from = as.Date('2005-01-01')
nwis_nm = 'flow_ft'

# for rebuilding
if(FALSE) {
  
  catch_list = outlet |> get_catch()
  data_dir |> save_catch(catch_list, overwrite=TRUE)
  plot_catch(catch_list)
  
  get_nwis(data_dir, nwis_nm, from_initial=nwis_from)
  update_nwis(data_dir, nwis_nm)
}

# open previously saved data
catch_list = data_dir |> open_catch()
plot_catch(catch_list)

# split
split_result = data_dir |> get_split()
plot_catch(split_result)

# save split datasets
data_dir |> save_split(split_result, overwrite=TRUE)







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



