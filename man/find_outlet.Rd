% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_split.R
\name{find_outlet}
\alias{find_outlet}
\title{Find outlet points for a set of sub-catchments}
\usage{
find_outlet(catch, edge, flow)
}
\arguments{
\item{catch}{sf data frame of sub-catchment polygons with 'comid' field}

\item{edge}{data frame of flow line directed edges ("PlusFlow" from "NHDPlusAttributes")}

\item{flow}{sf data frame of flow lines ("NHDFlowline" from "NHDSnapshot")}
}
\value{
sf data frame of outlet points with fields copied from \code{catch}
}
\description{
Helper function for \code{get_split}. This finds the intersection of the catchment
boundaries in \code{catch} with the flow line in \code{flow} corresponding to the outlet.
The function checks both the stream segment associated with the 'comid' field
and its immediate downstream neighbour.
}
\details{
The output is always in WGS84 coordinates. If there is no intersection, the
function snaps the endpoint of the incomplete stream segment to the boundary
and returns that point. If there is more than one intersection, the function
returns the one further downstream.
}
