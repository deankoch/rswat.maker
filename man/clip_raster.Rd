% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{clip_raster}
\alias{clip_raster}
\title{Crop and mask a raster to a (possibly padded) geometry}
\usage{
clip_raster(r, b, buffer = 0, p = NULL)
}
\arguments{
\item{r}{SpatRaster}

\item{b}{sf polygon geometry within extent of \code{r}}

\item{buffer}{numeric >= 0 (possibly with units), distance by which to expand the boundary of \code{b}}

\item{p}{character path to save as GeoTIFF}
}
\value{
SpatRaster
}
\description{
The function wraps the obvious \code{terra} and \code{sf} functions, handling projections, and
coercing to \code{SpatVector} via \code{sp} to avoid possible bugs.
}
\details{
This returns a copy the sub-grid of raster \code{r} covering geometry \code{b} (or optionally a
buffered version returned by \code{sf::st_buffer}), where all pixels lying outside the geometry
set to NA. If \code{p} is not \code{NULL}, the function deletes anything at that path, then writes
the raster to it with \code{terra::writeRaster} (creating directories as needed)

Set the padding distance in argument \code{buffer}, or 0 for no padding. This value is passed
directly to \code{sf::st_buffer} if it is finite, otherwise the function does no masking or
cropping and ignores \code{b}.
}
\examples{
dem = terra::rast(system.file('ex/elev.tif', package='terra'))
bbox = dem |> terra::ext() |> sf::st_bbox() |> sf::st_as_sfc()
sf::st_crs(bbox) = sf::st_crs(dem)
buffer = units::set_units(10, km)
b = sf::st_centroid(bbox) |> sf::st_buffer(buffer)
# dem |> terra::plot()
# plot(b, add=T)

# clip_raster result
r_result = dem |> clip_raster(b)
# r_result |> terra::plot()
# plot(b, add=T)

# same result using buffer argument
r_result2 = dem |> clip_raster(sf::st_centroid(bbox), buffer)
all.equal(r_result, r_result2)

}
