#' Plot the catchment for an outlet
#' 
#' This plots the catchment area, flow lines, and lakes/ponds upstream of
#' the `outlet` passed to `get_catch`, along with the outlet itself (as a
#' white circle).
#' 
#' The function adds a scale bar and sets the title automatically (as needed)
#' by copying the most frequently used name in the 'GNIS_NAME' field of the flow
#' lines dataset and appending the COMID for the catchment polygon overlying the
#' outlet.
#' 
#' All input geometries are transformed to coordinates in `crs_in` for plotting.
#' By default this is set to UTM zone of the main outlet. 
#'
#' @param catch_list list returned by `get_catch`
#' @param main character, a title for the plot
#'
#' @return invisibly returns the scale bar line geometry from `draw_scale`
#' @export
plot_catch = function(catch_list, main=NULL, crs_out=NULL) {
  
  # guess the name from a frequency table
  if( is.null(main) ) {
    
    main = catch_list[['flow']][['GNIS_NAME']] |> table() |> sort() |> tail(1) |> names()
    main = main |> paste( paste('upstream of COMID', catch_list[['comid']]) )
  }
  
  # set default projection
  if( is.null(crs_out) ) crs_out = to_utm(catch_list[['outlet']]) |> suppressMessages()
  
  # copy and transform the geo-referenced objects
  nm_geo = c('boundary', 'flow', 'lake', 'outlet', 'inlet')
  is_geo = names(catch_list) %in% nm_geo
  plot_list = catch_list[is_geo] |> lapply(\(x) sf::st_transform(x, crs_out))
  
  # draw the plot objects
  plot_list[['boundary']] |> sf::st_geometry() |> plot(main=main, col='grey90', border=NA)
  plot_list[['flow']] |> sf::st_geometry() |> plot(add=TRUE, col='grey60')
  plot_list[['lake']] |> sf::st_geometry() |> plot(add=TRUE, col='grey20', border=NA)
  plot_list[['outlet']] |> sf::st_geometry() |> plot(add=T, cex=1.5, pch=16, col='white')
  plot_list[['outlet']] |> sf::st_geometry() |> plot(add=T, cex=1.5, lwd=2, col='grey20')
  
  # add scale bar with position automatically set
  scale_sf = plot_list[['boundary']] |> draw_scale(left=NULL)
  return( invisible(scale_sf) )
}


draw_sub_catch = function(sub_list, catch_list, stem=TRUE, crs_out=NULL) {
  
  # set default projection
  if( is.null(crs_out) ) crs_out = to_utm(catch_list[['outlet']]) |> suppressMessages()
  
  # copy outlet/gage list and inner boundaries (partition)
  outlet = do.call(rbind, lapply(sub_list, \(x) x[['outlet']])) |> sf::st_transform(crs_out)
  boundary_sub = do.call(rbind, lapply(split_result, \(x) x[['boundary_inner']])) |> 
    sf::st_geometry() |>
    sf::st_transform(crs_out)

  # find all flow lines downstream of outlet/gage sites
  comid_stem = outlet[['comid']] |> comid_down(catch_list[['edge']]) |> unique()
  line_stem = catch_list[['flow']][catch_list[['flow']][['COMID']] %in% comid_stem, ] |>
    sf::st_geometry() |> sf::st_transform(crs_out)
  
  # highlight subcatchments by shading with transparent colors
  boundary_col = adjustcolor('white', alpha.f=0.5)
  fill_col = adjustcolor('black', alpha.f=0.3)
  boundary_sub |> plot(add = T, col = fill_col, border = boundary_col, lwd = 2)
  
  # draw the plot objects
  line_stem |> plot(add=TRUE, col=adjustcolor('black', 0.5), lwd=2)
  outlet[ !outlet[['main_outlet']], ] |> sf::st_geometry() |> plot(add=T, pch=16, col='white')
  outlet[ !outlet[['main_outlet']], ] |> sf::st_geometry() |> plot(add=T, lwd=2, col='grey20')
  outlet[ outlet[['main_outlet']], ] |> sf::st_geometry() |> plot(add=T, cex=1.5, pch=16, col='white')
  outlet[ outlet[['main_outlet']], ] |> sf::st_geometry() |> plot(add=T, cex=1.5, lwd=2, col='grey20')
}
  
  
  

#' Create a scale bar and add it to an sf plot
#' 
#' This returns a LINESTRING geometry representing a scale bar, and optionally draws
#' it on the plot along with a label giving the distance and units.
#' 
#' The function returns a LINESTRING geometry of known length located in one of the four
#' corners of the bounding box for `obj` (depending on the choice of `bottom` and `left`).
#' The length is selected by finding a `pretty` number close (but less than or equal) to
#' `size` times the horizontal span of the bounding box, and the units are always in 'km'
#' (or 'm' if the distance is less than 1 km).
#' 
#' Move the text label up and down with respect to the scale bar line using `y_adj`,
#' expressed in units of character height. Move the whole scale bar inwards or outwards
#' using `outer_adj`, expressed as a proportion of the square root of the area of the
#' bounding box. `cex`, `col`, `lwd` have their usual meanings (see `?plot`) and `col`
#' is applied to box the line and text. `box_col` causes the function to first draw a
#' rectangle of the requested color behind the scale bar (`box_border` is also passed to
#' `rect` to set the border color).
#' 
#' Calculations are done in the UTM projection but the results are returned in the
#' coordinate system of the input `obj`. This means scale bars for plots in other
#' coordinate systems (especially lon/lat) may look curved, but the reported path length
#' of the line segment drawn will be accurate.
#'
#' @param obj the sf object that was used to create the plot
#' @param bottom logical, whether to position the scale bar at top or bottom
#' @param left logical, whether to position the scale bar at left or right
#' @param above logical, whether to position the text above or below the line
#' @param draw logical, whether to draw the scale bar or just return it as data frame
#' @param size numeric > 0 controls the width of the scale bar (see details)
#' @param y_adj numeric, up/down position adjustment factor for text 
#' @param outer_adj, outer/inner position adjustment factor for scale bar
#' @param col passed to both `sf::plot.sf` and `text` for drawing
#' @param box_col character or NA, a color for painting the box behind the scale bar
#' @param lwd numeric line width, passed to `sf::plot.sf`
#' @param cex numeric text size expansion factor, passed to `text`
#'
#' @return sf LINESTRING data frame describing and positioning the scale bar 
#' @export
draw_scale = function(obj, bottom=TRUE, left=FALSE, above=bottom, draw=TRUE,
                      size=1/5, y_adj=0, outer_adj=0, lwd=2, cex=1,
                      col='grey30', box_col=NA, box_border=NA) {
  
  # take bounding box polygon and project to UTM coordinates
  crs_in = sf::st_crs(obj)
  crs_utm = to_utm(obj) |> suppressMessages()
  bbox_utm = sf::st_geometry(obj) |> sf::st_bbox() |> sf::st_as_sfc() |> sf::st_transform(crs_utm)
  
  # set default left/right choice based on distance
  if( is.null(left) ) {
    
    # project the whole input object (slow)
    obj_utm = obj |> sf::st_geometry() |> sf::st_transform(crs_utm)
    
    # recursive call to get the two location options
    left_option = c(TRUE, FALSE)
    line_option = do.call(c, lapply(left_option, \(x) {
      
      sf::st_geometry( draw_scale(obj_utm,
                                  bottom=bottom, 
                                  left=x, 
                                  draw=FALSE, 
                                  above=above,
                                  size=size, 
                                  y_adj=y_adj, 
                                  outer_adj=outer_adj) )
      
    }) )
    
    # pick the position that maximizes distance to obj
    idx_best = sf::st_distance(line_option, obj_utm) |> apply(1, min) |> which.max()
    left = left_option[idx_best]
  }
  
  # add padding then take bounding box of result
  pad_dist = outer_adj * sqrt( sf::st_area(bbox_utm) )
  bbox_pad = sf::st_buffer(bbox_utm, pad_dist) |> sf::st_bbox() |> sf::st_as_sfc()
  
  # extract line geometry from bounding box
  bbox_line = bbox_pad |> sf::st_cast('LINESTRING') 
  
  # copy the horizontal line segment of the padded bounding box - scale-bar gets drawn here
  idx_draw = if(bottom) 1:2 else 4:3
  if(!left) idx_draw = rev(idx_draw)
  origin_line = bbox_line[[1]][idx_draw,] |> sf::st_linestring() |> sf::st_sfc(crs=crs_utm)
  
  # measure whole side length then scale down to a lower and prettier number (based on `size`)
  origin_len = origin_line |> sf::st_cast('POINT') |> sf::st_distance() |> max()
  output_len = pretty( size * ( origin_len - (2 * pad_dist) ) )[1]
  
  # construct shortened line segment using scaling factor determined above
  size_as_p =  output_len / units::drop_units(origin_len)
  output_xy = origin_line |> sf::st_cast('POINT') |> sf::st_coordinates()
  output_xy[2,'X'] = output_xy[1,'X'] + ifelse(left, 1, -1) * size_as_p * diff(sort(output_xy[,'X'])) 
  output_line = sf::st_linestring(output_xy) |> sf::st_sfc(crs=crs_utm) |> sf::st_transform(crs_in)
  
  # text label and units
  output_m = output_len |> units::set_units('m') |> units::drop_units()
  print_len = ifelse(output_m > 1e3, output_m/1e3, output_m)
  print_unit = ifelse(output_m > 1e3, 'km', 'm')
  print_msg = paste(print_len, print_unit)
  
  # positioning for the label
  y_pad =  ifelse(above, 1, -1) * strheight('0')
  xy_msg = sf::st_coordinates( sf::st_centroid(output_line) ) + c(0, y_adj * y_pad)
  
  # create an sf data frame from this information
  output_sf = data.frame(distance=print_msg, label=print_msg, x=xy_msg[1], y=xy_msg[2]) |> 
    sf::st_sf(geometry=output_line)
  
  # attempt to draw the line and label
  if(draw) {
    
    # draw a rectangle behind it first 
    if( !anyNA(box_col) ) {
      
      xy_line = output_line |> sf::st_cast('POINT') |> sf::st_coordinates()
      rect(xleft = min(xy_line[,'X']) - abs(y_pad),
           xright = max(xy_line[,'X']) + abs(y_pad),
           ybottom = min(xy_line[,'Y']) - abs(y_pad) + ifelse(above, 0, (1.5 + y_adj) * y_pad),
           ytop = max(xy_line[,'Y']) + abs(y_pad) + ifelse(above, (1.5 + y_adj) * y_pad, 0),
           col = box_col,
           border = box_border)
    }
    
    # now draw line with plot.sf and text
    plot(output_line, add=TRUE, xpd=TRUE, col=col, lwd=lwd)
    text(xy_msg[1], xy_msg[2], print_msg, pos=ifelse(above, 3, 1), xpd=TRUE, col=col, cex=cex)
  }
  
  return( invisible(output_sf) )
}