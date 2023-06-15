#' Return a point given a place-name query using Nominatim API for OpenStreetMaps (OSM)
#' 
#' This submits a single query to OSM and returns either a single result, the
#' WGS84 coordinates for the point that (most closely) matches the query, or else
#'`NULL` to indicate no results.
#' 
#' The function also prints the "display_name" field returned by OSM
#' 
#' Loosely based on code by D. Kisler at https://tinyurl.com/2xxcthp5 
#' See also the Nominatim API documentation at
#' https://nominatim.org/release-docs/latest/api
#'
#' @param query character, query to submit to Nominatim API
#'
#' @return sfc_POINT object or NULL for no results 
#' @export
#'
#' @examples
#' nominatim_point('nonexistent placename')
#' nominatim_point('Old Faithful')
nominatim_point = function(query) {
  
  base_url = 'https://nominatim.openstreetmap.org'
  query_extra = '&limit=1&format=json'
  query_norm = gsub('\\s+', '\\%20', query[[1]])
  
  # urls for query and checking server status
  query_url = base_url |> file.path(paste0('search?q=', query_norm, query_extra))
  status_url = base_url |> file.path('status.php?format=json')
  
  # status check
  if( !is.character(query) ) {
    
    status_result = status_url |> jsonlite::fromJSON()
    message('server status: ', status_result[['message']])
    return(invisible())
  }
  
  # search
  query_result = tryCatch({
    
    # send the query and open result as JSON
    nominatim_result = query_url |> jsonlite::fromJSON()
    
    # print the first result name and return coordinates as sfc object
    if( is.null(nominatim_result[['display_name']]) ) { NULL } else {
      
      message('OSM name: ',  nominatim_result[['display_name']] )
      nominatim_result[c('lon', 'lat')] |> 
        unlist() |> 
        as.numeric() |> 
        sf::st_point() |> 
        sf::st_sfc(crs='EPSG:4326')
    }
    
  }, error = \(e) {
    
    message('error: ', as.character(e))
    NULL
  })
  
  # no results case returns NULL invisibly
  if( length(query_result) == 0 ) {
    
    message('no results')
    return( invisible(query_result) )
  }
  
  return(query_result)
}
