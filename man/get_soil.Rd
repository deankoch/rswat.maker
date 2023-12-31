% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_soil.R
\name{get_soil}
\alias{get_soil}
\title{Download/open soil map unit key rasters from STATSGO2 and SSURGO and combine them}
\usage{
get_soil(data_dir, force_overwrite = FALSE, mukey_replace = c(2485736))
}
\arguments{
\item{data_dir}{character path to the directory to use for output files}

\item{force_overwrite}{logical if TRUE the function overwrites a fresh copy of outputs}

\item{mukey_replace}{map unit keys for SSURGO feature to swap for STATSGO feature}
}
\value{
SpatRaster of STATSGO2 and/or SSURGO MUKeys
}
\description{
This downloads STATSGO2 and SSURGO data to the 'statsgo' and 'ssurgo' sub-directories
of \code{data_dir} then creates a merged copy in the 'soils' sub-directory.
}
\details{
SSURGO tends to be the more precise map but it also tends to have less complete
coverage than STATSGO2. In cases where there is no SSURGO map unit key (mukey),
this function assigns the STATSGO2 key.

If a SSURGO key is not found in the SWAT+ soils database, the STATSGO2 key is
assigned. If that key is not found, then an \code{NA} is assigned.

\code{mukey_replace} can be used to replace any number of SSURGO polygons with the
underlying STATSGO2 polygons. In some cases (eg with the default 2485736) there
can more detail in the STATSGO2 layer. This may change in the future as SSURGO
continues to be updated.

If you have called this function previously (in another project), you can save some
time and bandwidth by copying the "ssurgo/raw" from the old \code{data_dir} to the new one.
The function will only download files that it doesn't already find in "raw"
}
