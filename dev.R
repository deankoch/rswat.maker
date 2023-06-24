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

#data_dir = 'D:/rswat_data/yellowstone'
#data_dir = 'D:/rswat_data/tuolumne'
#data_dir = 'D:/rswat_data/snake'
data_dir = 'D:/rswat_data/nooksack'
#data_dir = 'D:/rswat_data/provo'

#outlet = nominatim_point("Carter's Bridge, MT")
#outlet = nominatim_point("Tuolumne River, CAL Fire Southern Region")
#outlet = nominatim_point("Alpine Junction, WY")
outlet = nominatim_point("Nugents Corner, WA")
#outlet = nominatim_point("Deer Creek Reservoir, UT")

# for rebuilding
catch_list = outlet |> get_catch()
data_dir |> save_catch(catch_list, overwrite=TRUE)
plot_catch(catch_list)

catch_list = data_dir |> open_catch()
plot_catch(catch_list)

# split
split_result = data_dir |> get_split()
plot_catch(split_result)

# # save a result test
# data_dir_test = 'D:/rswat_data/yellowstone/split'
# data_dir_test |> save_catch(catch_list=split_result[[1]], extra=TRUE, overwrite=TRUE)

# example of two sub-catchments in context of larger basin
catch_list |> plot_catch(stream_col=NULL)
split_result |> plot_catch(stream_col=NULL, lake_col=NULL, fill_col=NULL, border_col=NULL, add=TRUE)
split_result[c(3, 4)] |> plot_catch(stream_col='black', add=TRUE)

# one of the examples, zoomed in
i = 0
i = i + 1
split_result[[i]] |> plot_catch()

sub_list = split_result[[i]]
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





# overlay some context
split_result |> plot_catch(add=T, fill_col=NULL)

# plot showing overlap (inlet polygon) areas
plot_catch(catch_list, stream_col=NULL)
plot_catch(split_result, border_col=NULL, inlet_col=NULL, add=TRUE)


plot_catch(split_result[c(3, 7, 5)])



plot_catch(catch_list)

plot_catch(catch_list)

sub_list = split_result[[1]]
crs_out = NULL
add = FALSE
lwd = 2
border_col = adjustcolor('white', alpha.f=0.5)
fill_col = adjustcolor('black', alpha.f=0.3)
stem_col = adjustcolor('black', alpha.f=0.5)
stream_col = 'grey60'
lake_col = 'grey20'
outlet_col = 'white'
inlet_col = 'white'
add_scale = TRUE
main = NULL


# gage = save_nwis(file.path(data_dir, 'nwis/flow_ft'))['station'] |> sf::st_read(quiet=TRUE)
# 








# split
split_result = data_dir |> get_split()


# plot result
plot_catch(catch_list)
plot_catch(split_result, add=T)

plot_catch(split_result)

plot_catch(split_result[1])

sub_list = split_result[1]
crs_out = NULL
add = FALSE
lwd = 2
border_col = adjustcolor('white', alpha.f=0.5)
fill_col = adjustcolor('black', alpha.f=0.3)
stem_col = adjustcolor('black', alpha.f=0.5)
outlet_col = 'white'
inlet_col = 'white'
add_scale = TRUE
main = NULL

bottom=TRUE
left=FALSE
above=bottom
draw=TRUE
size=1/5
y_adj=0
outer_adj=0
lwd=2
cex=1
col='grey30'
box_col=NA
box_border=NA


xx = obj |> sf::st_geometry() |> sf::st_transform(4326) |> head(1)

sf::st_coordinates(xx)





plot_catch(catch_list)
plot_catch(split_result, add=TRUE)


sub_list = split_result



i = 0

i = i + 1
plot_catch(split_result[[i]])



nwis_from = as.Date('2005-01-01')
nwis_nm = 'nwis/flow_ft'
get_nwis(data_dir, nwis_nm=nwis_nm, from_initial=nwis_from)

# TODO plotter for all this
split_result = data_dir |> get_split(include_inlet=T)

boundary = save_catch(data_dir)['boundary'] |> sf::st_read(quiet=TRUE) |> sf::st_geometry() 
crs_utm = to_utm(boundary) |> suppressMessages()

boundary_utm = boundary |> sf::st_transform(crs_utm)
boundary_sub = do.call(c, lapply(split_result, \(x) sf::st_geometry(x[['boundary_inner']]))) |> sf::st_transform(crs_utm)


plot(boundary_utm, border='blue')
boundary_sub |> plot(add=T, col=adjustcolor('lightblue', alpha.f=0.5), border=adjustcolor('black', alpha.f=0.2))


# single sub-catchment plot
if(0) {

  i = 0
  
  i = i + 1
  crs_utm = split_result[[i]][['outlet']] |> to_utm()
  comid = split_result[[i]][['outlet']][['comid']]
  xx = split_result[[i]][ !( names(split_result[[i]]) %in% c("comid", "site_no", "edge_sub") ) ] |> 
    lapply(\(x) sf::st_transform(x, crs_utm))
  
  main_txt = xx[['flow']]$GNIS_NAME |> table() |> sort() |> tail(1) |> names()
  sub_txt = paste0('(upstream of ', comid, ')')
  xx[['catchment']] |> sf::st_geometry() |> plot(main=paste(main_txt, sub_txt))
  xx[['boundary']] |> sf::st_geometry() |> plot(main=paste(main_txt, sub_txt))
  xx[['flow']] |> sf::st_geometry() |> plot(add=TRUE, col='lightblue')
  xx[['lake']] |> sf::st_geometry() |> plot(add=TRUE, col='darkblue', border=NA)
  xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1)
  xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.9, col='red')
  xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1)
  xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.9, col='green')
  xx[['boundary']] |> draw_scale(left=NULL)

}


# xx[['catchment']] |> sf::st_geometry() |> plot(col='turquoise', border='lightblue')
# xx[['boundary']] |> sf::st_geometry() |> plot(add=T, border='turquoise', lwd=2)
# xx[['flow']] |> sf::st_geometry() |> plot(add=T, col=adjustcolor('darkblue', alpha.f=0.5))
# xx[['lake']] |> sf::st_geometry() |> plot(add=T, col='darkblue')
# xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1)
# xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.9, col='red')
# xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1)
# xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.9, col='green')
# draw_scale(xx[['catchment']])

obj = xx[['catchment']]
obj = xx[['boundary']]

bottom=TRUE
left=NULL
draw=TRUE
above=bottom
size=1/5
y_adj=0
outer_adj=0
col='grey20'
col_bg='white'
lwd=2
cex=1
