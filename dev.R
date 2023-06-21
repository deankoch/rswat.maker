
library(devtools)
load_all()
document()


# current data directory
data_dir = 'D:/rswat_data/uyr'
split_result = get_split(data_dir)


# save function
split_result[[3]] |> str()





i = 0

xx[['lake']]


i = i + 1
xx = split_result[[i]]

xx[['catchment']] |> sf::st_geometry() |> plot(col='turquoise', border='lightblue', bg='black')
xx[['boundary']] |> sf::st_geometry() |> plot(add=T, border='turquoise', lwd=2)
xx[['flow']] |> sf::st_geometry() |> plot(add=T, col=adjustcolor('darkblue', alpha.f=0.5))
xx[['stem']] |> sf::st_geometry() |> plot(add=T, col=adjustcolor('darkblue', alpha.f=0.5), lwd=2)
xx[['lake']] |> sf::st_geometry() |> plot(add=T, col='darkblue')
xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=2)
xx[['outlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1.5, col='red')
xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=2)
xx[['inlet']] |> sf::st_geometry() |> plot(add=T, pch=16, cex=1.5, col='green')

# TODO: add gage data



plot(boundary, add=T)
plot(main_stem, add=T, col='darkblue', lwd=2)

outlet |> sf::st_geometry() |> plot(add=T, pch=16)
outlet |> sf::st_geometry() |> plot(add=T, pch=16, cex=0.5, col='red')

# take subsets of lakes, flow, etc

xx = get_split(data_dir)

result_list = xx







y = xx |> st_geometry() 
y |> plot(col='lightblue', border='white')













# 

# our name and path for the discharge data from NWIS
nwis_nm = 'nwis/flow_ft'

# load NWIS discharge stations
nwis_dir = data_dir |> file.path(nwis_nm)
station_pt = save_nwis(nwis_dir)['station'] |> sf::st_read()

# load static catchment layers
flow = save_catch(data_dir)['flow'] |> sf::st_read()
lake = save_catch(data_dir)['lake'] |> sf::st_read()
boundary = save_catch(data_dir)['boundary'] |> sf::st_read()
catchment = save_catch(data_dir)['catchment'] |> sf::st_read()

# flow line directed graph data
edge = save_catch(data_dir)['edge'] |> read.csv(colClasses='character')

#####









#######

# get current NWIS discharge data, updating files on disk
#get_nwis(data_dir, nwis_nm) 















#######
# CRAP

x = nominatim_point("East Fork, montana")
plot(boundary)
plot(x, add=T, cex=16, pch=16)






# # plot using the palette provided by FedData
# pal = FedData::pal_nlcd() |> rename(id=ID) |> right_join(land_lookup, by=c('id')) |> pull(Color)
# land |> plot(col=pal)






# generate new examples here
comid_example = edge$FROMCOMID
comid_example = comid_example[comid_example != '0']
comid = sample(comid_example, 1)

# A TEST
# an example with a fork downstream that then converges
#comid = '2966866'
xx = comid |> comid_down(edge, first_only=F)
plot(boundary)
flow |> dplyr::filter(COMID %in% xx) |> sf::st_geometry() |> plot(add=TRUE, col='blue')

xxx = comid |> comid_up(edge)
flow |> dplyr::filter(COMID %in% xxx) |> sf::st_geometry() |> plot(add=TRUE, col='darkred')
