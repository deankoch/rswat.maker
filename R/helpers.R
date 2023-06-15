#' Find the appropriate UTM zone for a geometry 
#' 
#' This prints the UTM zone that covers the centroid of the supplied geometry
#' and returns the corresponding EPSG code 
#'
#' @param x any object understood by `sf::st_geometry`
#'
#' @return integer EPSG code
#' @export
#'
#' @examples
#' c(-110, 45) |> sf::st_point() |> to_utm()
to_utm = function(x) {
  
  # find mean of centroids if input is a list
  xy = x |> sf::st_geometry() |> sf::st_centroid() |> sf::st_coordinates()
  if( !is.numeric(xy) ) xy = xy |> sapply(sf::st_centroid) |> apply(1, mean)
  
  # longitude and latitude
  lon = xy[1]
  lat = xy[2]
  
  # UTM zone number from longitude
  utm_num = floor((lon + 180) / 6) + 1
  message(paste('UTM zone', utm_num))

  # EPSG code from zone and signe of latitude (N/S)
  32700 + utm_num - 100*( sign(lat) + 1 )/2 
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