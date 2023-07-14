library(devtools)
load_all()
document()

data_dir = 'D:/rswat_data/yellowstone'


# full update
outlet = nominatim_point("Carter's Bridge, MT")
outlet |> run_rqswat(data_dir, nwis_from=as.Date('2005-01-01'), overwrite=TRUE, no_download=TRUE)

# outlet |> run_qswat(data_dir)
