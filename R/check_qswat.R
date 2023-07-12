#' Check for QSWAT+ delineation problems and fix them by nudging outlets(s) downstream
#' 
#' This checks for inlet/outlet snapping issues in the stream network shape file of a
#' QSWAT+ project. If any problems are detected the function attempts to fix them by
#' nudging (ie re-positioning) the problematic outlet points in the downstream direction
#' by distance `nudge_dist` (m), or else suggests to increase the `snap_threshold` in
#' the `run_qswat` call.
#' 
#' Imprecision in elevation models and/or outlet coordinates can prevent QSWAT+
#' from correctly delineating your stream network. This function checks for two possible
#' issues and attempts to fix them automatically:
#' 
#' 1. Not all channels drain to the main outlet (ie the chain of `LINKNO` keys is broken somewhere)
#' 2. Some/all of the inlet drainage remains attached to the basin (ie QSWAT+ failed to remove it) 
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
#'
#' @return list with elements 'nudge' and 'trim'
#' @export
check_qswat = function(data_dir, make_plot=TRUE, nudge_dist=NULL) {

  # default nudge distance is one-half diagonal pixel width of DEM
  if( is.null(nudge_dist)) nudge_dist = save_qswat(data_dir)[['dem']] |> 
      terra::rast() |> terra::res() |> mean()
  
  # initialize output
  check_result = list(trim = NULL, 
                      nudge = NULL, 
                      nudge_dist = nudge_dist,
                      snap_problem = FALSE,
                      output_exists = FALSE)
  
  # load QSWAT+ output files (or return some info if required outputs can't be found)
  qswat = load_qswat(data_dir)
  if( any( sapply(qswat[c('outlet', 'log')], is.null) ) ) return( check_result )
  outlet = qswat[['outlet']]
  shell_result = qswat[['log']]
  
  # deal with ERROR (from QgsMessageLog) or Traceback (from R)
  qswat_error = any(grepl('^(Traceback|ERROR)', shell_result))
  if( qswat_error ) {
    
    # report log file where the error message can be found
    message(paste('QSWAT+ reported a problem in', run_qswat(data_dir)[['log']]))
    
    # load the input outlets shape file
    input_json = run_qswat(data_dir)[['input']] |> readLines() |> jsonlite::fromJSON()
    outlet_in = input_json[['outlet']] |> sf::st_read(quiet=TRUE)
    
    # check for unsnapped outlets
    outlet_miss = outlet_in[!(outlet_in[['ID']] %in% outlet[['ID']]),]
    if( nrow(outlet_miss) > 0 ) {
      
      # indicate snapping problem and finish
      check_result[['snap_problem']] = TRUE
      msg_outlet = paste(outlet_miss[['ID']], collapse=', ')
      message('outlet(s) ', msg_outlet, ' not snapped to channel network')
    }
  }
  
  # return some info if required outputs can't be found
  if( any(sapply(qswat[c('channel', 'sub')], is.null)) ) return( check_result )
  check_result[['output_exists']] = TRUE
  channel = qswat[['channel']]
  subbasin = qswat[['sub']]

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
  main_invalid = sum(!channel_valid) > 0
  msg_problem = 'delineation error: channel(s) not draining to the main outlet'
  
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
        msg_problem = 'delineation error: channel(s) found upstream of inlet'
        outlet_check = outlet[outlet[['INLET']] == 1, ][is_inlet_problem, ]
      }
    }
  }

  # warn of problem outlets and attempt to repair them
  if( nrow(outlet_check) > 0 ) {
    
    # loop over problem outlets
    for( idx in seq(nrow(outlet_check)) ) {

      # identify outlet, channels, and basins affected
      outlet_i = outlet_check[idx,]
      linkno_i = linkno_check[[idx]] |> unique() |> sort()
      channel_i = channel[ channel[['LINKNO']] %in% linkno_i, ]
      basin_i = channel_i[['BasinNo']]

      # extract relevant indices and geometries for problem sub-basins
      sub_problem = subbasin[subbasin[['PolygonId']] %in% basin_i, ]
      subno_check = sub_problem[['Subbasin']] |> unique() |> sort()
  
      # print information about problematic objects
      msg_sub = paste('subbasin(s):', paste(subno_check, collapse=', '))
      msg_link = paste('\nthis affects channels with DSLINKNO:', paste(linkno_i, collapse=', '))
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
      outlet_snap_xy = nudge_dest + ( check_result[['nudge_dist']] / nudge_norm ) * nudge_vector
      outlet_snap = sf::st_point(outlet_snap_xy) |> sf::st_sfc(crs=sf::st_crs(subbasin))
      
      # replace coordinates in output points object
      message('nudge distance : ', round(sf::st_distance(outlet_snap, outlet_i)), ' m')
      sf::st_geometry(outlet_i) = outlet_snap
      outlet_check[idx,] = outlet_i
  
      # copy the problem area to results
      subbasin_check = subbasin_check |> rbind(sub_problem)
    }
  }
  
  # copy loop results to output list
  check_result[['nudge']] = outlet_check
  check_result[['trim']] = subbasin_check
  
  # plot problem areas and return diagnostics
  if(make_plot) plot_qswat(data_dir, check_result, quiet=TRUE)
  return( invisible(check_result) )
}


#' Check if any inlet/outlet points were moved this package and/or QSWAT+ and report the distances
#' 
#' This reports the total distances between different versions of outlet points
#' after re-positioning by `check_qswat` and saving the results with a `run_qswat` call.
#' 
#' With default arguments `run_qswat` will run `check_qswat` and then re-run QSWAT+ setup
#' until the project passes the checks, each time changing the path of the outlets file
#' to a new modified version. This function checks if this has happened, and if so it
#' reports the distances 
#'
#' @param data_dir character path the the project directory
#'
#' @return the latest outlet points data frame with new field 'distance' (m)
#' @export
report_moved = function(data_dir, draw=FALSE, line_col='black', snapped=TRUE, quiet=FALSE) {
  
  # load the positions we started with
  input_default = save_qswat(data_dir)
  outlet_first = input_default[['outlet']] |> sf::st_read(quiet=TRUE)
  
  # outlet positions post- or pre-snapping
  if( snapped & file.exists(run_qswat(data_dir)[['output']]) ) {
    
    # paths to latest copy used to run QSWAT+
    output_qswat = run_qswat(data_dir)[['output']] |> readLines() |> jsonlite::fromJSON()
    outlet_path = output_qswat[['outlet']]
    
  } else {
    
    # paths to latest copy used to run QSWAT+
    input_qswat = run_qswat(data_dir)[['input']] |> readLines() |> jsonlite::fromJSON()
    outlet_path = input_qswat[['outlet']]
  }
 
  # load the snapped/nudged version
  outlet_latest = outlet_path |> sf::st_read(quiet=TRUE) 
  for(i in seq( nrow(outlet_latest) )) {
    
    # iterate over individual inlet/outlet points, matching ID fields
    j = match(outlet_latest[['ID']][i], outlet_first[['ID']])
    
    # distance between the two versions
    outlet_latest[['distance']][i] = outlet_latest[i,] |> 
      sf::st_distance(outlet_first[j,]) |> 
      units::drop_units()
    
    # report nonzero distances only
    if( outlet_latest[['distance']][i] > 0 ) {
      
      msg_type = ifelse(outlet_latest[['INLET']][i], 'inlet', 'outlet')
      msg_move = paste(msg_type, outlet_latest[['ID']][i], 'moved ')
      if( !quiet ) message(msg_move, paste(round(outlet_latest[['distance']][i]), 'm in total'))
      
      # draw line segment showing snap
      if(draw) {
  
        p1 = sf::st_coordinates(outlet_first[j,]) |> c()
        p2 = sf::st_coordinates(outlet_latest[i,]) |> c()
        lines(c(p1[1], p2[1]), c(p1[2], p2[2]), col=line_col)
      }
    }
  }

  return(outlet_latest)
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
