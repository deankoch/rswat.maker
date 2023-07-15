library(devtools)
load_all()
document()

data_dir = 'D:/rswat_data/yellowstone'
wx_dir = 'G:'

# full update
# outlet = nominatim_point("Carter's Bridge, MT")
# outlet |> run_rqswat(data_dir, nwis_from=as.Date('2005-01-01'), overwrite=TRUE, no_download=TRUE)

subs = save_split(data_dir)[['sub']]

i = 1 
sub_dir = subs[i]
sub_dir
overwrite=FALSE
quiet=FALSE
wx_list = load_wx(wx_dir, data_dir, var_nm='pcp_mean', sub_dir=subs[i])
load_wx(wx_dir, data_dir)




wx_list |> str()





var_nm='pcp_mean'
sub_dir = subs[i]
subbasin_id = NULL
from = NULL





qswat_sub[['Subbasin']]




sub_dir = subs[i]


# load sub-basins and map to wxArchive weather
qswat_sub = load_qswat(data_dir, 'sub', sub=TRUE) |> sf::st_drop_geometry()
# reference file: sf::st_read('G:/aoi_export.geojson')




# load all qswat outlet records
qswat_outlet = load_qswat(data_dir, 'outlet', sub=TRUE)

# list of all input gage records
gage = save_split(data_dir)[['gage']] |> sf::st_read()
gage[['dir_name']]
qswat_outlet[['split_dir']]

subs = save_split(data_dir)[['sub']]
gage = subs |> sapply(\(x) save_catch(x, extra=TRUE)['gage']) |> stats::setNames( basename(subs) )
gage = subs |> sapply(\(x) save_catch(x, extra=TRUE)['gage']) |> stats::setNames( basename(subs) )
x = gage[1] |> sf::st_read()
x
