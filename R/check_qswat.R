
# TODO: add docs and fix up comid_up to allow different field names, to clean this ode
check_qswat = function(data_dir, make_plot=TRUE) {
  
  # NULL at the end means the model passed all checks
  poly_check = NULL
  outlet_check = NULL
  
  # check that input paths can be found and read them
  output_json = run_qswat(data_dir)[['output']]
  err_info = '\nHave you called `run_qswat` yet?'
  if( !file.exists(output_json) ) stop('file not found: ', output_json, err_info)
  qswat_path = output_json |> readLines() |> jsonlite::fromJSON()
  
  # load relevant geometries and filter to sub-basins with SWAT+ keys
  subs_sf = qswat_path['sub'] |> sf::st_read(quiet=T) |> dplyr::filter(Subbasin > 0)
  outlet_sf = qswat_path['outlet'] |> sf::st_read(quiet=T) 
  channel_sf = qswat_path['channel'] |> sf::st_read(quiet=T)
  
  # filter to channels associated with sub-basins
  idx_in = subs_sf |> dplyr::filter(Subbasin > 0) |> dplyr::pull(PolygonId) |> sort()
  channel_sf = channel_sf |> dplyr::filter(BasinNo %in% idx_in)
  
  # find flow lines associated with main outlet and inlets (if any)
  idx_main = outlet_sf |> dplyr::filter(INLET==0) |> sf::st_distance(channel_sf) |> which.min()
  inlet_sf = outlet_sf |> dplyr::filter(INLET==1) 
  
  # validity check for inlets
  if( nrow(inlet_sf) > 0 ) {
    
    # snap to channels
    idx_inlet = seq( nrow(inlet_sf) ) |> 
      sapply(\(i) which.min(sf::st_distance(inlet_sf[i,], channel_sf)))
    
    # build a data frame of linkages understood by comid_up()
    linkno = channel_sf[['LINKNO']]
    linkno[ linkno == linkno[idx_main] ] = -2
    edge = do.call(rbind, apply(channel_sf, 1, \(x) {
      
      data.frame(FROMCOMID = ( 1 + c(channel_sf[['LINKNO']],
                                     channel_sf[['USLINKNO1']],
                                     channel_sf[['USLINKNO2']]) ) |> as.character(),
                 
                 TOCOMID = ( 1 + c(channel_sf[['DSLINKNO']],
                                   channel_sf[['LINKNO']],
                                   channel_sf[['LINKNO']]) ) |> as.character())
      
    }))
    edge = edge[ edge[['FROMCOMID']] != 0, ] |> dplyr::distinct()
    
    # check linkages upstream of inlets
    linkno_inlet = linkno[idx_inlet]
    comid_inlet = as.character(1 + linkno_inlet)
    comid_up = comid_up(comid_inlet, edge) |> unique()
    
    # link_plot = as.integer(c(comid_inlet, comid_up))-1
    # xx = sf::st_geometry(channel_sf)[channel_sf[['LINKNO']] %in% link_plot]
    # plot(xx, add=T, lwd=3, col='red')
    # plot(sf::st_geometry(inlet_sf), add=T, col='red', cex=5)
    # 
    
    # allow no more than upstream linkage from any inlet
    comid_allow = comid_up(comid_inlet, edge, first_only=TRUE) |> unique() |> c(comid_inlet)
    comid_up = comid_up[ !(comid_up %in% comid_allow) ]
    link_check = as.integer(comid_up) - 1
    
    # warn of any stubs
    if( length(link_check) > 0 ) {
      
      id_check = channel_sf |> dplyr::filter(LINKNO %in% link_check) |> dplyr::pull(BasinNo)
      poly_check = subs_sf |> dplyr::filter(PolygonId %in% id_check)
      sub_check = poly_check[['Subbasin']] |> unique()
      msg_info_1 = paste('subbasin(s):', paste(sub_check, collapse=', '))
      msg_info_2 = paste('\nDSLINKNO:', paste(link_check, collapse=', '))
      msg_problem = paste('Channels found upstream of inlet in', msg_info_1, msg_info_2)
      warning(msg_problem)
      
      # attempt to repair by snapping outlets
      
      # polygon to trim from the catchment
      poly_check_simple = sf::st_union(poly_check) |> 
        biggest_poly() |> 
        sf::st_transform(sf::st_crs(subs_sf))
      
      # find problematic outlet locations and select the first
      idx_problem = inlet_sf |> 
        sf::st_intersects(poly_check_simple, sparse=FALSE) |> 
        which() |>
        head(1)
      
      # find intersection of flow lines with the new boundary after trimming 
      bou = sf::st_boundary(poly_check_simple)
      line = channel_sf[sf::st_intersects(bou, sparse=FALSE),] |> sf::st_geometry()
      inlet_geom = sf::st_intersection(line, bou, sparse=FALSE) |> sf::st_cast('POINT') |> head(1)
      
      # replace the point location in the outlets data frame
      outlet_check = outlet_sf
      idx_mod = which( outlet_check[['ID']] == outlet_check[['ID']][idx_problem] )
      sf::st_geometry(outlet_check[idx_mod,]) = inlet_geom
    }
  }
  
  # plot problem areas
  if(make_plot) {
    
    whiten = \(a=0.3) adjustcolor('white', a)
    redden = \(a=0.3) adjustcolor('red', a)
    
    # base NHD features on DEM plot
    data_dir |> plot_rast('dem')
    
    # overlay SWAT+ sub-basins in white, channels in blue, problem areas in red
    subs_sf |> sf::st_geometry() |> plot(add=TRUE, border=whiten(0.5), col=whiten(0.3))
    channel_sf |> sf::st_geometry() |> plot(add=TRUE, col='blue')
    if( !is.null(poly_check) )  {
      
      sf::st_geometry(poly_check) |> plot(add=TRUE, border=redden(0.5), col=redden(0.5))
    }
  }
  
  check_result = list(trim=poly_check, outlet=outlet_check)
  return( invisible(check_result) )
}
