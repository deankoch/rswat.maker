
library(devtools)
load_all()
document()

library(rswat)

# update your DESCRIPTION file package name
# update the NEWS.md
# update the name in the README file
# update the .RProj name
# update the folder name
# update your package-.R file
# Do a search (Cmd/Ctr + Shift + F) and look for mentions of your package name


data_dir = 'D:/rswat_data/yellowstone'
wx_dir = 'G:'

subs = rqswat
open_catch(data_dir, sub=TRUE)


# full update
# outlet = nominatim_point("Carter's Bridge, MT")
# outlet |> rqswat(data_dir, nwis_from=as.Date('2005-01-01'), overwrite=TRUE, no_download=TRUE)
# 
rqswat(data_dir, what='editor')



rqswat(data_dir, nwis_from=as.Date('2005-01-01'), overwrite=TRUE, no_download=TRUE)

subs = save_split(data_dir)[['sub']]
i = 2
sub_dir = subs[i]

# partial update
save_qswat(sub_dir, overwrite=TRUE)
run_qswat(sub_dir, overwrite=TRUE)
run_editor(sub_dir, overwrite=TRUE)


wx_list = load_wx(wx_dir, data_dir, var_nm='pcp_mean', sub_dir=subs[i])
write_wx(subs[i], wx_list, overwrite=TRUE)

wx_list = load_wx(wx_dir, data_dir, var_nm='tmp_min', sub_dir=subs[i])
write_wx(subs[i], wx_list, overwrite=TRUE)

wx_list = load_wx(wx_dir, data_dir, var_nm='tmp_max', sub_dir=subs[i])
write_wx(subs[i], wx_list, overwrite=TRUE)

wx_list = load_wx(wx_dir, data_dir, var_nm='hum_mean', sub_dir=subs[i])
write_wx(subs[i], wx_list, overwrite=TRUE)

wx_list = load_wx(wx_dir, data_dir, var_nm='wnd_mean', sub_dir=subs[i])
write_wx(subs[i], wx_list, overwrite=TRUE)


# extract the SWAT+ path
qswat_path = run_qswat(sub_dir)[['output']] |> readLines() |> jsonlite::fromJSON()
swat_dir = qswat_path[['txt']]

# find simulator path and select latest version in SWAT+ Editor "assets" directory
swat_exe = qswat_path[['simulator_dir']] |>
  list.files('^rev.+rel\\.exe$', recursive=TRUE, full.names=TRUE) |>
  sort() |> tail(1)

# load project and assign simulator
swat_dir |> rswat(swat_exe, include='more')

# fix the default print settings to show warm-up
rswat_open('print.prt')[[1]] |>
  dplyr::mutate(nyskip=0L) |>
  rswat_write(overwrite=TRUE)

# simulate this year
dates = as.Date(c('2015-07-15', '2023-07-15'))
dates |> rswat_time(overwrite=TRUE, daily=TRUE)
exec_result = rswat_exec()
# TODO: clean up output here
# TODO: fix summary info message from rswat_write

rswat_files(include='output')

xx = rswat_open('basin_sd_chamorph_day.txt') |> rswat_date_conversion()

xx[c('date', 'flo_out')] |> plot(type='l')


str(y)


rswat_date_conversion(rswat_open('pcp1.pcp')[[2]] )[c('date', 'pcp')] |> tail(600) |> plot(type='l', col='blue')



# grab up-to-date copies of 'time.sim' and 'print.prt'
time.sim = rswat_open('time.sim', quiet=quiet, refresh=TRUE)
print.prt = rswat_open('print.prt', quiet=quiet, refresh=TRUE)

# extract currently assigned start/end dates
date_start = as.Date(paste(time.sim[, c('yrc_start', 'day_start')], collapse='-'), '%Y-%j')
date_end = as.Date(paste(time.sim[, c('yrc_end', 'day_end')], collapse='-'), '%Y-%j')




rswat_open('time.sim')





wx_list = load_wx(wx_dir, data_dir, var_nm='pcp_mean', sub_dir=subs[i])
overwrite=TRUE
quiet=FALSE
bounds = NULL
add = TRUE
# load_wx(wx_dir, data_dir)






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
