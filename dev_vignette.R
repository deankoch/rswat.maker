

library(devtools)
#install()

load_all()
#document()


# open the main project to get an overview map
data_dir = 'D:/rswat_data/yellowstone'

# local path on my machine (you will probably need to change this!)
output_dir = 'D:/rswat_data/lamar'

# Lamar river gage site -->
outlet = c(-110.0495, 45.00009) |> sf::st_point() |> sf::st_sfc(crs=4326)

# update if needed



  
  





subs = run_maker(data_dir, what='split')[['sub']]

#data_dir |> open_catch() |> plot_catch(stream_col=NULL)
main_catch = subs[1] |> open_catch()
main_catch |> plot_catch()
for(i in c(3,4)) subs[i] |> open_catch() |> plot_catch(add=T)

# coordinates of the outlet for this example
open_catch(subs[i])[['outlet']] |> sf::st_coordinates()


outlet = "Carter's Bridge, MT"
quiet=FALSE

# check_outlet = function(outlet, quiet=FALSE) {
#  
#   # this assigns a persistent storage directory to an environmental variable
#   nhdR:::nhd_path(temporary=FALSE)
#   
#   # look up locations for character input and/or convert to WGS84 to match NHD data
#   if( is.character(outlet) ) outlet = nominatim_point(outlet, quiet=quiet)
#   outlet = outlet |> sf::st_geometry() |> sf::st_transform(4326)
#   epsg_utm = get_utm(outlet, quiet=quiet) |> sf::st_crs()
#   
#   # identify VPU polygon for the outlet
#   vpu_id = nhdR::find_vpu(outlet)
#   
#   # copy the VPU polygon
#   vpu_poly = nhdR::vpu_shp[nhdR::vpu_shp[['UnitID']] == vpu_id,]
#   
#   # print long name and copy short name
#   vpu_select = vpu_poly[which(covers_vpu)[1], ]
#   uid = vpu_select[['UnitID']]
#   if(!quiet) message('VPU name: ', vpu_select[['UnitName']], ' (', uid, ')\n')  
#   
#   # return vpu_poly, vpu_id, epsg_utm, 
#   
# }
