% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_plot.R
\name{plot_catch}
\alias{plot_catch}
\title{Plot catchment/sub-catchment features}
\usage{
plot_catch(
  sub_list,
  crs_out = NULL,
  add = FALSE,
  lwd = 1,
  border_col = grDevices::adjustcolor("white", alpha.f = 0.8),
  fill_col = grDevices::adjustcolor("black", alpha.f = 0.2),
  stem_col = grDevices::adjustcolor("black", alpha.f = 0.5),
  stream_col = "grey50",
  lake_col = "grey20",
  gage_col = "grey50",
  outlet_col = "white",
  inlet_col = "white",
  add_scale = TRUE,
  main = NULL,
  sub = FALSE
)
}
\arguments{
\item{sub_list}{character path or list of geometries from \code{get_split}, \code{get_catch}, or \code{open_catch}}

\item{crs_out}{CRS code accepted by \code{sf::st_crs} or NULL to use local UTM}

\item{add}{logical whether to add to an existing plot, or (if \code{FALSE}) create a new one}

\item{lwd}{line width for stem lines and boundaries}

\item{border_col}{character line color for catchment boundaries}

\item{fill_col}{character fill color for catchment interiors}

\item{stem_col}{character color for main stem flow lines}

\item{stream_col}{character color for tributary flow lines}

\item{lake_col}{character color for lakes}

\item{outlet_col}{character fill color for outlet points}

\item{inlet_col}{character fill color for inlet points}

\item{add_scale}{logical, whether to add a scale bar}

\item{main}{character, a title for the plot}

\item{sub}{logical, passed to \code{open_catch} when \code{sub_list} is a path}
}
\value{
invisibly returns a boundary polygon for the catchment
}
\description{
This plots one or more (sub)catchments listed in \code{sub_list}, drawing their
features in sequence to a new or existing plot. Set a colour argument to
\code{NULL} to not draw the associated feature.
}
\details{
\code{sub_list} should be the output of \code{open_catch}, \code{get_catch} or \code{get_split}
(or one of its sub-catchment elements). Users can also instead pass a character
specifying the data directory path to load. If this is the root directory for
a project with subdirectory "split", users can additionally set \code{sub=TRUE} to
specify that the sub-catchments in "split" be plotted.

For a single (sub)catchment this draws, in order: the (sub)catchment interior
and boundary; minor and main-stem flow lines; lakes; outlets, inlets and gages.
The main outlet is plotted as a larger filled circle, and any inlets or gages
as smaller filled circles. By default gages are plotted in grey and sub-catchment
outlets in white.

When a list of sub-catchments is supplied the function combines them in a single
plot by adding layers in the same order, but looping over all sub-catchments at
each step. With inlets, this means the function paints the (sub)catchment areas
twice - once including the NHD sub-catchment polygon(s) for the inlet(s) and once
without. Flow lines downstream of any inlet(s) are also drawn twice, with
\code{stem_col} specifying the color of the second layer.

Set a color argument to NULL to skip adding a feature.

The function adds a scale bar and sets the title automatically (when \code{main=NULL})
by copying the most frequently used name in the 'GNIS_NAME' field of the flow
lines dataset and appending the COMID for the catchment polygon overlying the
outlet.

All input geometries are transformed to coordinates in \code{crs_out} for plotting.
By default this is set to UTM zone of the main outlet.

The default transparent gray-scale color scheme is useful for layering on top
of an existing color plot. For example you could plot a whole basin in color
then use \code{plot_sub(..., add=TRUE)} to highlight a specific sub=catchment.
Note that \code{add_scale} and \code{main} have no effect when adding to an existing plot
with \code{add=TRUE}.
}
