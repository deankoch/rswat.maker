#' A lookup table matching NLCD and SWAT+ land use codes
#'
#' This data set matches the National Land Cover Database (NLCD) product
#' from the Multi-Resolution Land Characteristics (MRLC) consortium
#' to SWAT+ land use codes. Matches are found using Table A.3 from
#' the Hydrol. Earth Syst. Sci paper by Havel, Tasdighi, Arabi (2018).
#'
#' SWAT+ land use codes were extracted from the SQLite
#' database that is included in the default SWAT+ installer
#' ("swatplus_datasets.sqlite"). Since the SWAT+ team updates these
#' codes from time to time, this dataset will likely need updating
#' someday. It was last generated in June 2023.
#'
#' If a SWAT+ code cannot be matched using the table, it is assigned
#' the name `NA`. Note that the paper by Havel, Tasdighi and Arabi
#' focused on a mountainous basin similar (and close) to the UYR
#' watershed. Other regions of interest will likely need a different
#' set of mappings reflecting the local vegetation.
#'
#' Learn more about SWAT+ land use codes in the SWAT+ documentation PDF
#' at https://swat.tamu.edu/media/69419/Appendix-A.pdf
#'
#' @format ## `land_use_lookup`
#' A data frame with 272 rows and 3 columns:
#' \describe{
#'   \item{id}{integer NLCD ID}
#'   \item{name}{SWAT+ land use code }
#'   \item{description}{character string with both matched descriptions (NLCD on the right)}
#' }
#' @source <https://doi.org/10.5194/hess-22-2527-2018>
"land_use_lookup"

#' A table of HTTP links to download STATSGO2 data
#'
#' The State Soil Geographic (STATSGO2) dataset is an older source of soils data
#' with a different coverage than the newer (and more detailed) SSURGO product
#' from the USDA. This table provides links to download zipped STATSGO2 soil maps
#' for all 51 US states and three territories.
#'
#' These links are to a publicly accessible FPAC Box that I found by Google search.
#' The filenames suggest they were last modified in 2016 and uploaded in 2017. This
#' suits our purposes, but users may want to check if newer versions are available.
#'
#' Learn more about SSURGO/STATSGO2 on the NRCS metadata links page at
#' https://www.nrcs.usda.gov/resources/data-and-reports/ssurgo/stats2go-metadata
#'
#' @format ## `statsgo_url`
#' A data frame with 20 rows and 5 columns:
#' \describe{
#'   \item{id}{FPAC Box ID}
#'   \item{file}{the zip archive file name}
#'   \item{abb}{state/territory abbreviation code}
#'   \item{state}{state or NA for territories}
#'   \item{url}{URL to download the file with HTTPS}
#' }
#' @source <https://nrcs.app.box.com/v/soils/folder/18247487156>
"statsgo_url"

#' A table of USGS parameter code definitions
#'
#' Parameter codes are used by the US Geological Survey (USGS) National Water
#' Information System (NWIS) to unambiguously identify variables in published
#' datasets. This table provides name/definition pairs for these codes.
#'
#' This data set comprises the name/definition pairs (along with some other fields)
#' found at the following URL:
#'
#' 'https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&group_cd=%'
#'
#' This was last retrieved in June 2023.
#'
#' More information about the parameter codes can be found at the USGS NWIS Help site:
#' https://help.waterdata.usgs.gov/codes-and-parameters/parameters
#'
#' @format ## `usgs_lookup`
#' A data frame with 24953 rows and 13 columns:
#' \describe{
#'   \item{parm_cd}{5-digit parameter code as character}
#'   \item{group}{category of variable}
#'   \item{parm_nm}{definition}
#'   \item{parm_unit}{unit}
#'   ...
#' }
#' @source <https://help.waterdata.usgs.gov/>
"usgs_lookup"
