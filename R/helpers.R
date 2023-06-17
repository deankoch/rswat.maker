#' Find the appropriate UTM zone for a geometry 
#' 
#' This prints the UTM zone that covers the supplied point
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
  
  # longitude and latitude
  xy = x |> sf::st_geometry() |> sf::st_transform(4326) |> sf::st_coordinates()
  lon = xy[1]
  lat = xy[2]
  
  # UTM zone number from longitude
  utm_num = floor((lon + 180) / 6) + 1
  message(paste('UTM zone:', utm_num))

  # EPSG code from zone and signe of latitude (N/S)
  32700 + utm_num - 100*( sign(lat) + 1 )/2 
}

#' EPSG codes for some common geographical coordinate systems 
#'
#' These are some of the datum names I have encountered in NWIS. If `code`
#' is not 'NAD27' or 'NAD83', the function assumes the WGS84 datum.
#'
#' @param code character, the datum name
#'
#' @return integer, the EPSG code
#' @export
#'
#' @examples
#' datum_to_epsg('WGS84')
#' datum_to_epsg('NAD27')
#' datum_to_epsg('some_unknown_datum')
datum_to_epsg = \(code) {
  
  code |> switch('NAD27' = 4267,
                 'NAD83' = 4269,
                 'WGS84' = 4326, 4326)
}


