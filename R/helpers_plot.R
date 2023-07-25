#' Plot catchment/sub-catchment features
#' 
#' This plots one or more (sub)catchments listed in `sub_list`, drawing their
#' features in sequence to a new or existing plot. Set a colour argument to
#' `NULL` to not draw the associated feature.
#' 
#' `sub_list` should be the output of `open_catch`, `get_catch` or `get_split`
#' (or one of its sub-catchment elements). Users can also instead pass a character
#' specifying the data directory path to load. If this is the root directory for
#' a project with subdirectory "split", users can additionally set `sub=TRUE` to
#' specify that the sub-catchments in "split" be plotted.
#' 
#' For a single (sub)catchment this draws, in order: the (sub)catchment interior
#' and boundary; minor and main-stem flow lines; lakes; outlets, inlets and gages.
#' The main outlet is plotted as a larger filled circle, and any inlets or gages
#' as smaller filled circles. By default gages are plotted in grey and sub-catchment
#' outlets in white.
#' 
#' When a list of sub-catchments is supplied the function combines them in a single
#' plot by adding layers in the same order, but looping over all sub-catchments at
#' each step. With inlets, this means the function paints the (sub)catchment areas
#' twice - once including the NHD sub-catchment polygon(s) for the inlet(s) and once
#' without. Flow lines downstream of any inlet(s) are also drawn twice, with
#' `stem_col` specifying the color of the second layer.
#' 
#' Set a color argument to NULL to skip adding a feature.
#' 
#' The function adds a scale bar and sets the title automatically (when `main=NULL`)
#' by copying the most frequently used name in the 'GNIS_NAME' field of the flow
#' lines dataset and appending the COMID for the catchment polygon overlying the
#' outlet.
#' 
#' All input geometries are transformed to coordinates in `crs_out` for plotting.
#' By default this is set to UTM zone of the main outlet. 
#' 
#' The default transparent gray-scale color scheme is useful for layering on top
#' of an existing color plot. For example you could plot a whole basin in color
#' then use `plot_sub(..., add=TRUE)` to highlight a specific sub=catchment.
#' Note that `add_scale` and `main` have no effect when adding to an existing plot
#' with `add=TRUE`.
#'
#' @param sub_list character path or list of geometries from `get_split`, `get_catch`, or `open_catch`
#' @param crs_out CRS code accepted by `sf::st_crs` or NULL to use local UTM
#' @param add logical whether to add to an existing plot, or (if `FALSE`) create a new one
#' @param lwd line width for stem lines and boundaries
#' @param border_col character line color for catchment boundaries
#' @param fill_col character fill color for catchment interiors
#' @param stem_col character color for main stem flow lines
#' @param stream_col character color for tributary flow lines
#' @param lake_col character color for lakes
#' @param outlet_col character fill color for outlet points
#' @param inlet_col character fill color for inlet points
#' @param add_scale logical, whether to add a scale bar
#' @param main character, a title for the plot
#' @param sub logical, passed to `open_catch` when `sub_list` is a path 
#' 
#' @return returns nothing but either creates a plot or adds to an existing one
#' @export
plot_catch = function(sub_list, 
                      crs_out = NULL,
                      add = FALSE,
                      lwd = 1,
                      border_col = adjustcolor('white', alpha.f=0.8),
                      fill_col = adjustcolor('black', alpha.f=0.2),
                      stem_col = adjustcolor('black', alpha.f=0.5),
                      stream_col = 'grey50',
                      lake_col = 'grey20',
                      gage_col = 'grey50',
                      outlet_col = 'white',
                      inlet_col = 'white',
                      add_scale = TRUE,
                      main = NULL,
                      sub = FALSE) {
                    
  
  # attempt to load if first argument is character (assume it's a path)
  if( is.character(sub_list) ) sub_list = normalizePath(sub_list) |> open_catch(sub=sub) 
  
  # put first argument in list if needed
  if( !is.list( sub_list[[1]] ) ) sub_list = sub_list |> list()
  if( is.null(sub_list[[1]][['outlet']]) ) sub_list = sub_list |> list()
  n_sub = length(sub_list)
  
  # extract outlets (and possibly some inlets) 
  outlet = do.call(rbind, lapply(unname(sub_list), \(x) x[['outlet']]))
  
  # identify the main outlet (when there is only one it is automatically the main)
  is_out = outlet[['main_outlet']]
  if( !any(is_out) & ( nrow(outlet) == 1 ) ) is_out = TRUE
  idx_main = ifelse(any(is_out), which(is_out), 1)
  
  # set default projection
  if( is.null(crs_out) ) crs_out = outlet[idx_main,] |> get_utm() |> suppressMessages()
  out = outlet |> sf::st_transform(crs_out)
  
  # extract inlet or gage objects, if any (ignore warnings about row-binding empty data frames)
  gage = do.call(rbind, lapply(unname(sub_list), \(x) x[['gage']])) |> suppressWarnings()
  inlet = do.call(rbind, lapply(unname(sub_list), \(x) x[['inlet']])) |> suppressWarnings()
  if( !is.null(inlet) ) inlet = inlet |> sf::st_transform(crs_out) |> sf::st_geometry()
  if( !is.null(gage) ) gage = gage |> sf::st_transform(crs_out) |> sf::st_geometry()
  
  # helper for next loop
  merge_geo = function(nm, is_geo=TRUE) {
   
    # initialize to first list element
    df_out = sub_list[[1]][[nm]]
    if( length(df_out) == 0 ) return(NULL)
    
    # join the like named data frames in multiple elements case (ignore warnings about empty dfs)
    if( n_sub > 1 ) df_out = do.call(rbind, lapply(sub_list, \(x) x[[nm]]))
    if( !is_geo ) return(df_out)
    
    # strip data frame leaving only geometry in output projection
    df_out |> sf::st_geometry() |> sf::st_transform(crs_out)
  }
    
  # copy and/or merge (and don't strip data frame from flow yet)
  plot_list = list(boundary = merge_geo('boundary'),
                   boundary_outer = merge_geo('boundary_outer'),
                   edge = merge_geo('edge', is_geo=FALSE),
                   lake = merge_geo('lake'),
                   flow = merge_geo('flow', is_geo=FALSE))   
  
  # find and copy all COMIDs downstream of outlet/gage sites
  comid_stem = c(outlet[['comid']], inlet[['comid']]) |> comid_down(plot_list[['edge']]) |> unique()
  is_down = plot_list[['flow']][['COMID']] %in% comid_stem
  
  # copy and transform the flow lines
  line_stream = plot_list[['flow']] |> sf::st_geometry() |> sf::st_transform(crs_out)
  
  # initialize the plot if needed
  if( !add ) {
   
    # guess the name from a frequency table
    if( is.null(main) ) {

      main = most_frequent(plot_list[['flow']], 'GNIS_NAME')
      
      # append COMID for main outlet (if there is one)
      if( any(is_out) ) main = main |> paste( paste('upstream of COMID', out[['comid']][is_out]) ) 
    }
    plot_list[['boundary']] |> plot(main=main, border=NA) 
  }
  
  # fill sub-catchment interiors
  if( !is.null(fill_col) ) {
    
    # fill sub-catchment interior first without then with inlet polygons
    plot_list[['boundary']] |> plot(add=TRUE, col=fill_col, border=NA)
    if( !is.null(plot_list[['boundary_outer']]) ) plot_list[['boundary_outer']] |> 
      plot(add=TRUE, col=fill_col, border=NA)
  }
  
  # draw sub-catchment boundaries and flow lines
  if( !is.null(border_col) ) plot_list[['boundary']] |> plot(add=TRUE, border=border_col, lwd=lwd)
  if( !is.null(stream_col) ) line_stream |> plot(add=TRUE, col=stream_col)
  if( !is.null(stem_col) ) line_stream[is_down] |> plot(add=TRUE, col=stem_col, lwd=lwd)
  
  # lakes may be absent
  if( !is.null(lake_col) & !is.null(plot_list[['lake']]) ) plot_list[['lake']] |> 
    plot(add=TRUE, col=lake_col, border=NA)
  
  # points
  if( !is.null(outlet_col) ) out[is_out,] |> draw_outlet(col_in=outlet_col, cex=1.5)
  if( !is.null(inlet_col) ) {
    
    # some inlets may be found in the `outlet` data frame
    if( any(!is_out) ) out[!is_out,] |> draw_outlet(col_in=inlet_col)
    if( !is.null(inlet) ) inlet |> draw_outlet(col_in=inlet_col)
  }
  if( !is.null(gage_col) ) gage |> draw_outlet(col_in=gage_col)

  # add a scale bar
  scale_sf = NULL
  if(!add & add_scale) scale_sf = plot_list[['boundary']] |> draw_scale(left=NULL)
  return(invisible(scale_sf))
}
  

#' Draw a heatmap plot showing the DEM, land use, or soil MUKEYs
#' 
#' Set `what` to one of 'dem', 'land', or 'soil' after running the corresponding
#' workflow (`run_maker`, or its subroutines, `get_dem`, `save_dem`, etc) to create
#' a heat map plot of the requested layer.
#' 
#' The function selects an appropriate theme for the layer and optionally draws
#' the catchment boundary over top (see `?plot_catch` to add more features)
#' Coordinates are projected to the UTM zone of the main outlet before plotting.
#'
#' @param data_dir character path to the data directory
#' @param what character, the layer to plot, either 'dem', 'land', or 'soil'
#' @param main character or NULL (for automatic), the title of the plot
#' @param catch logical, whether to plot the catchment boundary in black
#' @param add_scale logical, whether to add a scale bar by calling `draw_scale`
#' @param mask logical, whether to mask to the catchment boundary
#' @param a numeric in [0,1]. the transparency (alpha) 
#'
#' @return nothing, but creates a plot
#' @export
plot_rast = function(data_dir, what='dem',
                     main = NULL,
                     catch = TRUE,
                     add_scale = TRUE,
                     mask = FALSE,
                     a = 1,
                     ...) {

  nm_valid = c('dem', 'land', 'soil')
  if( !(what %in% nm_valid ) ) stop('valid choices for `what` are: ', paste(nm_valid, collapse=', '))
  
  # open catchment data
  if(catch) {
   
    catch_list = data_dir |> open_catch()
    if( is.null(main) ) {
      
      # set a default title
      comid = catch_list[['outlet']][['comid']]
      basin_nm = basename(data_dir)
      main_lyr = switch(what, 'dem'='elevation', 'land'='land use', 'soil'='soil map units')
      main = paste0(basin_nm, ' (', comid, '): ', main_lyr)
    }
  }
  
  # set a default title
  if( is.null(main) ) main = basename(data_dir)
  
  # select the more conservative outer boundary for masking, if available
  if( mask ) {
   
    bou_path = save_catch(data_dir, extra=TRUE)[c('boundary_outer', 'boundary')]
    bou = bou_path[file.exists(bou_path)] |> head(1) |> sf::st_read(quiet=TRUE) 
  }

  # elevation plot using terrain colors
  if(what=='dem') {
    
    r = save_dem(data_dir)['dem'] |> terra::rast() 
    if( mask ) r = clip_raster(r, bou)
    colour = grDevices::terrain.colors(50)[10:40] |> adjustcolor(a)
    r |>  terra::plot(axes = FALSE,
                      reset = FALSE,
                      main = main,
                      plg = list(title='meters', size=0.8),
                      col = colour,
                      ...)
  }
  
  # land use plot using NLCD colors
  if(what=='land') {
    
    # get a palette from NLCD and filter to IDs found in this extent
    r = save_land(data_dir)['land'] |> terra::rast() 
    if( mask ) r = clip_raster(r, bou)
    pal = FedData::pal_nlcd()[c('ID', 'Color', 'Class')] |> 
      dplyr::filter(ID %in% na.omit(unique(r[])))
    
    # make r a factor raster then plot
    colour = pal[['Color']] |> adjustcolor(a)
    levels(r) = pal[c('ID', 'Class')] |> as.data.frame()
    r |> terra::plot(axes = FALSE,
                     reset = FALSE,
                     main = main,
                     col = colour,
                     plg = list(cex=0.8),
                     ...)
  }
  
  # soils plot using random rainbow color assignment
  if(what=='soil') {

    # categorical data, but the MUKEY doesn't mean much in itself
    r = save_soil(data_dir)[['soil']]['soil'] |> terra::rast()
    if( mask ) r = clip_raster(r, bou)
    mukey = r[] |> unique() |> na.omit() |> c()

    # set the levels and labels in the raster before plotting
    colour = grDevices::rainbow(length(mukey), alpha=a)
    levels(r) = data.frame(id=mukey, level=seq_along(mukey))
    r |> terra::plot(axes = FALSE,
                     reset = FALSE,
                     col = colour,
                     main = main,
                     legend = FALSE,
                     ...)
  }

  # distance scale bar
  if(add_scale) {
    
    # scale bar auto-positioning depends on what is plotted
    if(catch) {
      
      # pass the boundary in correct projection
      catch_list[['boundary']] |> sf::st_transform(crs=sf::st_crs(r)) |> draw_scale(left=NULL)
      
    } else { 
      
      # pass the bounding box, as polygon
      r |> sf::st_bbox() |> sf::st_as_sfc() |> draw_scale(left=FALSE)
    }
  }
  
  # draw catchment boundary
  if(catch) plot_catch(catch_list,
                       add=TRUE,
                       fill_col=NULL,
                       lake_col=NULL,
                       stream_col=NULL,
                       stem_col=NULL,
                       border_col='black') 
}


#' Plot QSWAT+ geometries 
#'
#' @param data_dir character path to the data directory
#' @param check_result list, the result of `check_qswat` (to overlay)
#' @param what character, either a color name (like "black"), or one of 'dem', 'land', or, 'soil'
#'
#' @return nothing but draws a plot
#' @export
plot_qswat = function(data_dir, check_result=NULL, what=NULL, main=NULL, quiet=FALSE, add=FALSE) {
  
  # same as plot_catch
  stream_col = 'grey50'
  
  # default transparency helpers
  white = \(a=0.3) adjustcolor('white', a)
  red = \(a=0.5) adjustcolor('red', a)
  
  # load inputs from QSWAT+ directory
  qswat = load_qswat(data_dir, quiet=quiet)
  
  # optional inputs returned by `qswat_check` but not `qswat_load`
  if( is.null(qswat[['trim']]) ) qswat[['trim']] = data.frame()
  if( is.null(qswat[['snap']]) ) qswat[['trim']] = data.frame()

  # initialize plot
  if( !add ) {
    
    # base layer with flat color or heatmap specified by `what`
    if( is.null(what) ) what = 'grey80'
    if( !(what %in% c('dem', 'land', 'soil')) ) {
      
      # initialize the plot device to the right bounding box, background color, add scale bar
      qswat[['sub']] |> sf::st_bbox() |> sf::st_as_sfc() |> plot(border=NA, bg=what, main=main)
      draw_scale(qswat[['sub']], left=NULL)
      
    } else { data_dir |> plot_rast(what) }
  }
  
  # SWAT+ sub-basins in white, channels in grey, outlets as black circle outlines
  qswat[['sub']] |> sf::st_geometry() |> plot(add=TRUE, border=white(0.5), col=white(0.3))
  qswat[['channel']] |> sf::st_geometry() |> plot(add=TRUE, col=stream_col)
  qswat[['outlet']] |> sf::st_geometry() |> plot(add=TRUE, cex=2)
  
  # draw lines showing snapping translation
  data_dir |> report_moved(draw=TRUE, quiet=TRUE)
  
  # overlay check results when supplied
  if( !is.null(check_result) ) {
    
    nudge = check_result[['nudge']]
    trim = check_result[['trim']]
    
    # problem sub-basins in red 
    if( nrow(trim) > 0 ) sf::st_geometry(trim) |> plot(add=TRUE, border=red(), col=red())
    
    # any suggested new inlet locations in black with white outline
    if( nrow(nudge) > 0 ) draw_outlet(nudge, col_in='black', col_out='white')
  }
}


#' Draw one or more points with filled circles
#' 
#' Wrapper for `sf::plot.sf(p)`. This adds to an existing plot in the same
#' coordinate system as `p`.
#'
#' @param p any POINT object understood by `sf::st_geometry()`
#' @param cex passed to `plot`
#' @param col_in character color of the border
#' @param col_out character fill color
#'
#' @return nothing but draws on existing plot
#' @export
draw_outlet = function(p, cex=1, col_in='white', col_out='grey20') {
  
  if( !is.null(p) ) { 
   
    # solid color for interior, then dark outline
    p |> sf::st_geometry() |> plot(add=TRUE, pch=16, col=col_in, cex=cex)
    p |> sf::st_geometry() |> plot(add=TRUE, col=col_out, cex=cex) 
  }
  
  return(invisible())
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
#' Results are always returned in the coordinate system of the input `obj`, but if `obj`
#' is in geographical coordinates (lon/lat) then computations are done in the UTM projection.
#' This means scale bars in lon/lat plots may look curved, but the reported distance will
#' be accurate for the path length of the line.
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
                      size=1/5, y_adj=0, outer_adj=-0.01, lwd=2, cex=1,
                      col='grey30', box_col=NA, box_border=NA) {
  
  # take bounding box polygon and project to UTM coordinates if needed
  crs_in = sf::st_crs(obj)
  crs_out = if( !sf::st_is_longlat(obj) ) crs_in else get_utm(obj) |> suppressMessages()
  bbox_out = sf::st_geometry(obj) |> sf::st_bbox() |> sf::st_as_sfc() |> sf::st_transform(crs_out)
  
  # set default left/right choice based on distance
  if( is.null(left) ) {
    
    # project the whole input object (slow)
    obj_out = obj |> sf::st_geometry() |> sf::st_transform(crs_out)
    
    # recursive call to get the two location options
    left_option = c(TRUE, FALSE)
    line_option = do.call(c, lapply(left_option, \(x) {
      
      sf::st_geometry( draw_scale(obj_out,
                                  bottom=bottom, 
                                  left=x, 
                                  draw=FALSE, 
                                  above=above,
                                  size=size, 
                                  y_adj=y_adj, 
                                  outer_adj=outer_adj) )
      
    }) )
    
    # pick the position that maximizes distance to obj
    idx_best = sf::st_distance(line_option, obj_out) |> apply(1, min) |> which.max()
    left = left_option[idx_best]
  }
  
  # add padding then take bounding box of result
  pad_dist = outer_adj * sqrt( sf::st_area(bbox_out) )
  bbox_pad = sf::st_buffer(bbox_out, pad_dist) |> sf::st_bbox() |> sf::st_as_sfc()
  
  # extract line geometry from bounding box
  bbox_line = bbox_pad |> sf::st_cast('LINESTRING') 
  
  # copy the horizontal line segment of the padded bounding box - scale-bar gets drawn here
  idx_draw = if(bottom) 1:2 else 4:3
  if(!left) idx_draw = rev(idx_draw)
  origin_line = bbox_line[[1]][idx_draw,] |> sf::st_linestring() |> sf::st_sfc(crs=crs_out)
  
  # measure whole side length then scale down to a lower and prettier number (based on `size`)
  origin_len = origin_line |> sf::st_cast('POINT') |> sf::st_distance() |> max()
  output_len = pretty( size * ( origin_len - (2 * pad_dist) ) )[1]
  
  # construct shortened line segment using scaling factor determined above
  size_as_p =  output_len / units::drop_units(origin_len)
  output_xy = origin_line |> sf::st_cast('POINT') |> sf::st_coordinates()
  output_xy[2,'X'] = output_xy[1,'X'] + ifelse(left, 1, -1) * size_as_p * diff(sort(output_xy[,'X'])) 
  output_line = sf::st_linestring(output_xy) |> sf::st_sfc(crs=crs_out) |> sf::st_transform(crs_in)
  
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
