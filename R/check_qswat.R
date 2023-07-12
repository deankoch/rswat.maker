#' Check for QSWAT+ delineation problems and fix them by nudging outlets(s) downstream
#' 
#' This checks for inlet/outlet snapping issues in the stream network shape file of a
#' QSWAT+ project. If any problems are detected the function attempts to fix them by
#' nudging (ie re-positioning) the problematic outlet points in the downstream direction
#' by distance `nudge_dist` (m).
#' 
#' Imprecision in elevation models and/or outlet coordinates can prevent QSWAT+
#' from correctly delineating your stream network. This function checks for two possible
#' issues and attempts to fix them automatically:
#' 
#' 1. Not all channels drain to the main outlet (ie the chain of `LINKNO` keys is broken somewhere)
#' 2. Some/all of the inlet drainage remains attached to the basin (ie QSWAT+ failed to remove it) 
#' 
#' Issue (1) can sometimes be ignored when only a small number of channels have been
#' lost. Argument `allow` sets the maximum number of disconnected channel segments allowed
#' before the main outlet is nudged. 
#' 
#' Both problems can usually be resolved by re-positioning the outlet point
#' to fall on a different DEM cell. Users can do it by hand using the "Review Snapped"
#' feature in QSWAT+. This function automates the process, by nudging problematic outlets
#' by distance `nudge_dist` along a vector pointing downstream. The direction vector for
#' nudging connects the centroid of the outlet's (upstream) flow line with the existing
#' outlet point. The default distance (`nudge_dist=NULL`) is set to the average of the
#' (x and y) side lengths of the DEM grid cells. 
#' 
#' The new positions are returned as sf data frame 'nudge' in a list along with the
#' affected upstream sub-basin polygons (sf data frame 'trim'). If no problems are detected,
#' the function returns empty data frames. If both (1) and (2) occur simultaneously, (2)
#' is ignored and the function returns only the new main outlet position.
#' 
#' When `make_plot=TRUE` the function draws the DEM heatmap and overlays:
#' 
#' * the original NHDPlus model catchment boundary with inlets/outlets (black line, white circles)
#' * the gage positions (if any, as grey circles)
#' * the QSWAT+ sub-basins (transparent white fill and white boundaries)
#' * the QSWAT+ stream reaches (blue lines)
#' * problematic QSWAT+ sub-basins, if any (transparent red fill)
#' * the gage positions after snapping by QSWAT+ (large black circle outlines)
#' * the suggested gage positions (if any, as solid black circles with white outline)
#'
#' @param data_dir character path the the project directory
#' @param make_plot logical, whether to draw a plot showing the QSWAT+ delineation results
#' @param nudge_dist numeric distance (metres) to move inlets/outlets
#' @param allow integer number of un-mapped channels allowed (any more and the main outlet is moved)
#'
#' @return list with elements 'nudge' and 'trim'
#' @export
check_qswat = function(data_dir, make_plot=TRUE, nudge_dist=NULL, allow=0) {
  
  # load QSWAT+ output files
  qswat = load_qswat(data_dir)
  outlet = qswat[['outlet']]
  channel = qswat[['channel']]
  subbasin = qswat[['sub']]
  
  # default based on input DEM
  if( is.null(nudge_dist) ) nudge_dist = qswat[['nudge_default']]

  # find flow line LINKNO associated with main outlet and inlets (if any)
  n_out = nrow(outlet)
  outlet_map = seq(n_out) |> sapply(\(k) which.min(sf::st_distance(outlet[k,], channel)))
  all_outlet_linkno = channel[['LINKNO']][outlet_map]
  outlet_linkno = all_outlet_linkno[ outlet[['INLET']] == 0 ]
  
  # initialize outputs (0 rows means model the model passed checks)
  outlet_check = outlet[0,]
  subbasin_check = subbasin[0,]
  
  # data frame of linkages in stream network (see `?qswat_edge` and `?comid_up`)
  edge = qswat_edge(channel)
  
  # main outlet can have multiple keys associated with it, so we go down then up
  outlet_linkno = outlet_linkno |> comid_down(edge) 
  upstream_linkno = outlet_linkno |> comid_up(edge, hw=-1)
  
  # trace upstream connections to identify unmapped elements
  channel_valid = channel[['LINKNO']] %in% upstream_linkno
  linkno_check = channel[['LINKNO']][!channel_valid] |> sort() |> list()
  main_invalid = sum(!channel_valid) > allow
  msg_problem = 'Channel(s) not draining to the main outlet'
  
  # deal with main outlet issues first
  if( main_invalid ) { outlet_check = outlet[ outlet[['INLET']] == 0, ] } else { 
    
    # check inlets only after main passes check
    if( n_out > 1 ) {
      
      # permit one linkage upstream of inlets
      outlet_linkno = all_outlet_linkno[ outlet[['INLET']] == 1 ] |>
        lapply(\(x) comid_up(x, edge, first_only=TRUE, hw=-1) |> c(x) )
      
      # get all linkages upstream of inlets
      upstream_linkno = outlet_linkno |> lapply(\(x) comid_up(x, edge, hw=-1) )
      
      # check for more than one upstream linkage from any inlet
      linkno_check = Map(\(x, y, z) x[ !(x %in% y) ], x = upstream_linkno, y = outlet_linkno)
      is_inlet_problem = sapply(linkno_check, length) > 0
      if( any(is_inlet_problem) ) {

        linkno_check = linkno_check[is_inlet_problem]
        msg_problem = 'Channel(s) found upstream of inlet'
        outlet_check = outlet[outlet[['INLET']] == 1, ][is_inlet_problem, ]
      }
    }
  }

  # warn of problem outlets and attempt to repair them in a loop
  if( nrow(outlet_check) > 0 ) {
    
    for( idx in seq(nrow(outlet_check)) ) {

      # identify outlet, channels, and basins affected
      outlet_i = outlet_check[idx,]
      linkno_i = linkno_check[[idx]] |> sort()
      channel_i = channel[ channel[['LINKNO']] %in% linkno_i, ]
      basin_i = channel_i[['BasinNo']]

      # extract relevant indices and geometries for problem sub-basins
      sub_problem = subbasin[subbasin[['PolygonId']] %in% basin_i, ]
      subno_check = sub_problem[['Subbasin']] |> unique() |> sort()
  
      # print information about problematic objects
      msg_sub = paste('subbasin(s):', paste(subno_check, collapse=', '))
      msg_link = paste('\nDSLINKNO:', paste(linkno_i, collapse=', '))
      message(paste(msg_problem, 'in', msg_sub, msg_link))
      message('computing new position for ', ifelse(main_invalid, 'main outlet', 'inlet'))
      
      # nudge comes from direction of centroid of the outlet's upstream channels
      nudge_origin = channel[channel[['LINKNO']] %in% linkno_i,] |> 
        sf::st_geometry() |> 
        sf::st_cast('POINT') |> 
        sf::st_coordinates() |>
        apply(2, mean)
        
      # move nudge_dist metres downstream of the original outlet position
      nudge_dest = sf::st_coordinates(outlet_i)
      nudge_vector = nudge_dest - nudge_origin
      nudge_norm = sqrt(sum(nudge_vector^2))
      outlet_snap_xy = nudge_dest + ( nudge_dist / nudge_norm ) * nudge_vector
      outlet_snap = sf::st_point(outlet_snap_xy) |> sf::st_sfc(crs=sf::st_crs(subbasin))
      
      # replace coordinates in output points object
      message('nudge distance : ', round(sf::st_distance(outlet_snap, outlet_i)), ' m')
      sf::st_geometry(outlet_i) = outlet_snap
      outlet_check[idx,] = outlet_i
  
      # copy the problem area to results
      subbasin_check = subbasin_check |> rbind(sub_problem)
    }
  }
  
  # plot problem areas and return diagnostics
  check_result = list(trim=subbasin_check, nudge=outlet_check)
  if(make_plot) plot_qswat(data_dir, check_result, quiet=TRUE)

  return( invisible(check_result) )
}


#' Load shape files from a QSWAT+ project
#' 
#' This loads the channels, sub-basins, and outlets shape-files created
#' during QSWAT+ setup and returns them as sf geometry data frames, in a list
#' along with a suggested nudge distance for moving outlets (see `?check_qswat`).
#' 
#' This is meant for projects created using `run_qswat`; The function expects an
#' input JSON file in the "qswat" directory (parent of the QGIS project directory)
#' which it uses to find the correct input DEM path. The output JSON in this directory
#' (if found) is used to locate the output  QSWAT+ "Shapes" directory. If QSWAT+
#' completes delineation but halts afterwards (eg due to failed checks or database
#' issues), the function  will attempt load the shape files anyway by guessing their
#' path in the `data_dir` directory tree.
#' 
#' The function returns only the subset of sub-basin polygons with positive 'Subbasin'
#' keys, and the channels mapping to them. The returned outlets have coordinates snapped
#' to channels already (by QSWAT+).
#'
#' @param data_dir character path to "qswat" subdirectory 
#'
#' @return list with geometries 'sub', 'channel', 'outlet' and parameter 'nudge_default'
#' @export
load_qswat = function(data_dir, quiet=FALSE) {
  
  # get input parameters for QSWAT+ setup
  err_info = '\nHave you called `run_qswat` yet?'
  input_json = run_qswat(data_dir)[['input']]
  if( !file.exists(input_json) ) stop('file not found: ', input_json, err_info)
  input_path = input_json |> readLines() |> jsonlite::fromJSON()
  
  # default nudge distance is one-half diagonal pixel width of DEM
  nudge_default = terra::res( terra::rast(input_path[['dem']]) ) |> mean()
  
  # get paths to output files from QSWAT+ setup (if it completed)
  output_json = run_qswat(data_dir)[['output']]
  if( file.exists(output_json) ) {
    
    # the output file paths
    qswat_path = output_json |> readLines() |> jsonlite::fromJSON()
    qswat_dir = dirname(qswat_path[['sub']])
    
  } else {
    
    # if setup didn't complete, try guessing the paths
    qswat_dir = dirname(input_json) |> file.path(input_path[['name']], 'Watershed/Shapes')
    qswat_path = list(channel=file.path(qswat_dir, 'demchannel/demchannel.shp'),
                      sub=file.path(qswat_dir, 'demsubbasins.shp'),
                      outlet=file.path(qswat_dir, 'outlet_snap.shp'))
    
    # if setup didn't get that far, there's nothing left to do in this function
    missing_path = paste(c(output_json, qswat_path), collapse=', ')
    err_file = paste0('files not found: ', missing_path,  err_info)
    if( !all(sapply(qswat_path, file.exists)) ) stop(err_file)
  }
  
  if( !quiet ) message('loading QSWAT+ model geometries from: ', qswat_dir)
  
  # load relevant geometries and filter to sub-basins with SWAT+ keys
  all_subs = qswat_path[['sub']] |> sf::st_read(quiet=T)
  swat_outlets = qswat_path[['outlet']] |> sf::st_read(quiet=T) |> dplyr::arrange(INLET)
  all_channels = qswat_path[['channel']] |> sf::st_read(quiet=T)
  
  # filter to channels associated with sub-basins
  swat_subs = all_subs |> dplyr::filter(Subbasin > 0)
  swat_basin_no = swat_subs |> dplyr::pull(PolygonId) |> sort()
  swat_channels = all_channels |> dplyr::filter(BasinNo %in% swat_basin_no)
  return( list(sub = swat_subs,
               channel = swat_channels,
               outlet = swat_outlets,
               nudge_default = nudge_default) )
}

#' Make a data frame of channel network links from QSWAT+ channels shape-file
#' 
#' This creates a data frame of channel linkages based on the three link number fields
#' in the channels shape file returned by QSWAT+ (and/or TauDEM). The output should be
#' compatible with functions from this package having an `edge` argument, like `comid_up`
#' `comid_down` and `find_outlet`.
#' 
#' This helper function allows us to re-use functions designed for NHD data. It constructs
#' the fields 'FROMCOMID' and 'TOCOMID' based on the integer 'LINKNO', 'USLINKNO1', 'USLINKNO2'
#' fields in `swat_channels`. 
#' 
#' Note that 'FROMCOMID' and 'TOCOMID' are so named for compatibility reasons and have
#' nothing to do with the NHD COMID system. They simply link together the rows of `edge`
#' as a directed graph. Relate this back to your QSWAT+ project via the 'LINKNO' key.
#' 
#' Get `swat_channels` by first calling `run_qswat` to run QSWAT+, then find the shape file
#' path in the JSON `run_qswat(data_dir)[['output']]` and pass it to `sf::st_read`.
#'
#' @param swat_channels sf data frame with LINESTRING geometries, created by QSWAT+
#'
#' @return data frame with integer columns 'FROMCOMID', 'TOCOMID'
#' @export
qswat_edge = function(swat_channels) {
  
  # validity check
  channel = swat_channels |> sf::st_drop_geometry()
  if( any(channel[['LINKNO']]) < 0 ) stop('Invalid LINKNO (< 0) in channels shapefile')
  
  # make sure we don't get crazy loops from main outlet to headers
  channel[['DSLINKNO']][ channel[['DSLINKNO']] == -1 ] = -Inf
  
  # edges defined in three different ways in the TauDEM fields
  from = channel[c('LINKNO', 'USLINKNO1', 'USLINKNO2')] |> as.matrix() |> c()
  to = channel[c('DSLINKNO', 'LINKNO', 'LINKNO')] |> as.matrix() |> c()
  edge = data.frame(FROMCOMID=from, TOCOMID=to)
  dplyr::distinct(edge)
}
