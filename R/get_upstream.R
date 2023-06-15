# return a list of geometry objects describing watershed upstream
get_upstream = function(outlet_point) {
  
  # identify the VPU containing the outlet
  vpu_poly = nhdR::vpu_shp |> dplyr::filter(UnitType == 'VPU')
  covers_vpu = sf::st_intersects(vpu_poly, outlet_point, sparse=FALSE) 
  if( sum(covers_vpu) == 0 ) stop('outlet_point lies outside of all known VPUs')
  
  # print long name
  vpu_select = vpu_poly[which(covers_vpu)[1], ]
  message('VPU name: ', vpu_select[['UnitName']], ' (', vpu_select[['UnitID']], ')') 
  
  # download the NHDPlus data for this VPU
  nhdR::nhd_plus_get(vpu=vpu_select[['UnitID']], 'NHDSnapshot', temporary=FALSE)
  nhdR::nhd_plus_get(vpu=vpu_select[['UnitID']], 'NHDPlusAttributes', temporary=FALSE)
  nhdR::nhd_plus_get(vpu=vpu_select[['UnitID']], 'NHDPlusCatchment', temporary=FALSE)
  
  # load everything into R
  message('loading NHD geodatabase files...')
  lake_poly = nhdR::nhd_plus_load(vpu=vpu_select[['UnitID']], 'NHDSnapshot', 'NHDWaterbody', quiet=TRUE)
  flow_line = nhdR::nhd_plus_load(vpu=vpu_select[['UnitID']], 'NHDSnapshot', 'NHDFlowline', quiet=TRUE)
  flow_df = nhdR::nhd_plus_load(vpu=vpu_select[['UnitID']], 'NHDPlusAttributes', 'PlusFlow', quiet=TRUE)
  catch_poly = nhdR::nhd_plus_load(vpu=vpu_select[['UnitID']], 'NHDPlusCatchment', 'Catchment', quiet=TRUE)
  
  # temporarily switch off spherical geometry to handle possibly invalid catch_poly
  sf_use_s2(FALSE)
  on.exit( sf_use_s2(TRUE) )
  
  # find the (sub)catchment COMID for our outlet point
  catch_crs = sf::st_crs(catch_poly)
  outlet_point_t = outlet_point |> sf::st_transform(catch_crs)
  covers_catch = catch_poly |> 
    sf::st_geometry() |> 
    sf::st_intersects(outlet_point_t, sparse=FALSE) |> 
    suppressMessages()
  
  if( sum(covers_catch) == 0 ) stop('outlet_point lies outside of all known catchments')
  outlet_comid = catch_poly[['FEATUREID']][covers_catch][1]
  message('outlet COMID: ', outlet_comid) 
  
  # follow all upstream edges in a loop
  id_mat = flow_df[c('FROMCOMID', 'TOCOMID')] |> as.list()
  comid_caught = outlet_comid
  any_upstream = TRUE
  message('following upstream tributaries...')
  while( any_upstream ) {
    
    # look for upstream edges (exclude headwater nodes with code 0)
    is_caught = id_mat[['TOCOMID']] %in% comid_caught[comid_caught != 0]
    
    # update exit condition
    any_upstream = any(is_caught)
    if(any_upstream) {
      
      # add new id(s) to the list, checking if any are new
      id_new = as.numeric(id_mat[['FROMCOMID']][is_caught])
      any_upstream = !all(id_new %in% comid_caught)
      comid_caught = c(comid_caught, id_new)
    }
  }
  
  # vector of all COMID codes found upstream of outlet_point
  sws_comid = comid_caught[comid_caught != 0]
  message('merging ', length(sws_comid), ' sub-catchment polygons...')
  
  # polygons for all associated catchments and their union
  sws_poly = catch_poly[catch_poly[['FEATUREID']] %in% sws_comid,] |> sf::st_make_valid()
  sws_poly_union = sws_poly |> 
    sf::st_geometry() |>  
    sf::st_union() |>
    sf::st_cast('POLYGON') |> 
    suppressMessages()
  
  # cleaned up version representing sub-watershed boundary
  sws_boundary = sws_poly_union[[1]][[1]] |> 
    list() |> 
    sf::st_polygon() |> 
    sf::st_sfc(crs=catch_crs) |> 
    suppressMessages()
  
  # copy subset of relevant edge data, ponds and flow-lines
  is_sws_edge = flow_df[['TOCOMID']] %in% sws_comid
  is_sws_lake = st_intersects(lake_poly, sws_boundary, sparse=FALSE) |> suppressMessages()
  is_sws_flow = st_intersects(flow_line, sws_boundary, sparse=FALSE) |> suppressMessages()
  
  return( list(outlet = outlet_point_t,
               catchment = sws_poly,
               boundary = sws_boundary,
               lake = lake_poly[is_sws_lake,],
               flow = flow_line[is_sws_flow,],
               edge = flow_df[is_sws_edge,]) )
}

# TODO: put this into a helpers file to share with define_watershed
.get_upstream = function(outlet, flow, catchment, edge, s2=TRUE, make_boundary=FALSE) {
  
  # follows the directed edges in `edge` to find the components of `flow` and `catchment`
  # that drain into `outlet`. These are returned in a list of geometries and dataframes,
  # where each entry represents a complete catchment.
  
  # TODO: transform everything to a common projection instead
  # temporarily switch off spherical geometry to handle possibly invalid catchment
  outlet = outlet |> sf::st_geometry()
  if(!s2) {
    sf_use_s2(FALSE)
    on.exit( sf_use_s2(TRUE) )
  }
  
  # find the (sub)catchment COMID for our outlet point
  catch_crs = sf::st_crs(catchment)
  outlet_t = outlet |> sf::st_transform(catch_crs)
  covers_catch = catchment |> 
    sf::st_geometry() |> 
    sf::st_intersects(outlet_t, sparse=FALSE) |> 
    suppressMessages()
  
  if( sum(covers_catch) == 0 ) stop('outlet lies outside of all known catchments')
  outlet_comid = catchment[['FEATUREID']][covers_catch][1]
  message('outlet COMID: ', outlet_comid) 
  
  # follow all upstream edges in a loop
  id_mat = edge[c('FROMCOMID', 'TOCOMID')] |> as.list()
  comid_caught = outlet_comid
  any_upstream = TRUE
  message('following upstream tributaries...')
  while( any_upstream ) {
    
    # look for upstream edges (exclude headwater nodes with code 0)
    is_caught = id_mat[['TOCOMID']] %in% comid_caught[comid_caught != 0]
    
    # update exit condition
    any_upstream = any(is_caught)
    if(any_upstream) {
      
      # add new id(s) to the list, checking if any are new
      id_new = as.numeric(id_mat[['FROMCOMID']][is_caught])
      any_upstream = !all(id_new %in% comid_caught)
      comid_caught = c(comid_caught, id_new)
    }
  }
  
  # vector of all COMID codes found upstream of outlet
  sws_comid = comid_caught[comid_caught != 0]
  sws_poly = catchment[catchment[['FEATUREID']] %in% sws_comid,] |> sf::st_make_valid()
  
  # copy subset of relevant edge data, ponds and flow-lines
  is_sws_edge = edge[['TOCOMID']] %in% sws_comid
  is_sws_flow = flow[['COMID']] %in% sws_comid
  out_list = list(outlet = outlet_t,
                  comid = outlet_comid,
                  catchment = sws_poly,
                  flow = flow[is_sws_flow,],
                  edge = edge[is_sws_edge,])
  
  # report on features collected here
  message('found ', paste(sum(is_sws_flow), 'stream reach(es)'))
  
  # boundary computations can be slow
  if( make_boundary ) {
    
    message('merging ', length(sws_comid), ' sub-catchment polygons...')
    
    # polygon from union of associated sub-catchments
    sws_poly_union = sws_poly |> 
      sf::st_geometry() |>  
      sf::st_union() |>
      sf::st_cast('POLYGON') |> 
      suppressMessages()
    
    # cleaned up version representing sub-watershed boundary
    out_list[['boundary']] = sws_poly_union[[1]][[1]] |> 
      list() |> 
      sf::st_polygon() |> 
      sf::st_sfc(crs=catch_crs) |> 
      suppressMessages()
  }
  return(out_list)
}