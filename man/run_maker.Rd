% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_maker.R
\name{run_maker}
\alias{run_maker}
\title{Download input data and run QSWAT+ for the basin defined by \code{outlet}}
\usage{
run_maker(
  data_dir,
  what = NULL,
  outlet = NULL,
  overwrite = FALSE,
  force_overwrite = FALSE,
  nwis_from = NULL,
  osgeo_dir = NULL,
  no_download = FALSE
)
}
\arguments{
\item{data_dir}{character path to the directory to use for writing project files}

\item{what}{character, one of the subroutine names (see DETAILS) or set to \code{NULL} to run all}

\item{outlet}{geo-referenced point object passed to \code{sf::st_geometry}, the main outlet}

\item{overwrite}{logical, if \code{FALSE} the function returns file paths and writes nothing}

\item{force_overwrite}{logical, should the function overwrite existing files in \code{data_dir}?}

\item{nwis_from}{Date, earlier dates are ignored in NWIS records (NULL to return all)}

\item{osgeo_dir}{character, path to the QGIS installation on your local machine}

\item{no_download}{logical, if TRUE \code{nhdR::nhd_plus_get} calls is skipped (for debugging)}
}
\value{
a list of file paths written by the function
}
\description{
This runs a SWAT+ model setup workflow on the catchment upstream of \code{outlet},
by downloading and processing watershed data and passing it to QSWAT+. When
\code{overwrite=FALSE} (the default) the function returns the output file paths
but doesn't write or process anything.
}
\details{
With default settings and \code{overwrite=TRUE}, the function runs a series of
downloading and/or processing steps, each based on outputs from previous
steps. This amounts to calling a series of helper functions from this package
in the correct order:
\enumerate{
\item 'catch' gets the initial catchment model (see \code{?get_catch})
\item 'dem' gets the digital elevation model raster (see \code{?get_dem})
\item 'nwis' gets daily stream gage records (see \code{?get_nwis})
\item 'land' gets a land use raster (see \code{?get_land})
\item 'soil' gets a soil MUKEY raster (see \code{?get_soil})
\item 'split' partitions the catchment into sub-catchments (see \code{?get_split})
\item 'input' prepares QSWAT+ input files (see \code{?save_qswat})
\item 'qswat' runs QSWAT+ setup (see \code{?run_qswat})
\item 'editor' runs SWAT+ Editor setup (see \code{?run_editor})
}

Users can repeat a step manually by calling the associated helper function(s),
or by calling \code{run_maker} with \code{what} set to the name of one or more steps.
For example setting \code{what=c('catch', 'dem', 'nwis', 'land' , 'soil')}) (or
\code{what='data'} for short) will fetch all the data needed to prepare a QSWAT+
project, at which point users could then create the project manually in QGIS3.

When \code{force_overwrite=FALSE} (the default), the function skips steps where
the output files (in \code{data_dir}) all exist already. However if \code{what} is
\code{NULL} (the default) or set to 'nwis', the function will download and save
(on disk) the latest daily gage data for your stations of interest.

Steps (1-5) each download something (unless it is already cached locally) and
write processed outputs to like-named new sub-directories of \code{data_dir}. Step
(6) then creates a directory tree with root at sub-directory "split/" and writes
a sub-catchment directory inside it for each (unique) gage position discovered
in (3). These sub-catchment directories are structured the same as \code{data_dir},
except without a "split/".

The last three steps (7-9) are done separately in a loop over sub-catchments.
Their output is written in the sub-catchments directory ("split/"). If there
are no errors in the workflow, this output includes a completed QSWAT+ project,
the input files used to create it, and a ready-to-use SWAT+ "TxtInOut" directory.

The SWAT+ models that result from the completed workflow can be fitted
and run in isolation using inlet/outlet data from step (3). This means it should
be possible to link the "split" sub-catchments together by copying SWAT+ outputs
to inlet files, thus parallelizing SWAT+ at the sub-basin level.

If you are getting 404 errors on the very first step - downloading/opening the NHDPlus
model - but you already have the necessary files cached locally (having called either
\code{run_maker} or \code{save_catch} in \code{overwrite=TRUE} mode), try setting \code{no_download=TRUE},
which attempts to proceed even when the web service is down. This affects only the
'catch' step (1).
}
