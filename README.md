
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rswat.uyr

An R package for building QSWAT+ inputs

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of rswat.uyr like so:

``` r
devtools::install_github('deankoch/rswat.uyr')
```

## Workflow

A number of different earth sciences datasets are needed to initialize a
SWAT+ model:

- landscape feature polygons - including AOI and water bodies
- digital elevation model raster (DEM)
- land use raster (and lookup table)
- soils classification raster (and lookup table)

rswat_uyr takes an outlet location of interest, finds the corresponding
watershed area, then downloads and processes the required datasets. It
produces a set of output files ready for QSWAT+, and retains copies of
the source datasets for later use.

## History

rswat_uyr is largely based on code in the URYW_data repository, now much
simplified so that it can be maintained as a self-contained R package.

Our watershed of interest is the Upper Yellowstone, so testing and
examples in rswat.uyr focus on areas upstream of Carter’s Bridge,
Montana. This includes Paradise Valley and most of Yellowstone National
Park.

rswat.uyr is connected to rswat in that it serves as a source of example
data. However rswat is not required. The rswat.uyr workflow prepares
inputs to QSWAT+, whereas rswat manages the SWAT+ model config files
that are created by QSWAT+.

## Motivation

The purpose of rswat.uyr is to automate and document ETL for the
datasets required by QSWAT+ when creating any new SWAT+ model. This is
part of a larger effort to create reproducible workflows for SWAT+
simulations in research

The package helps us manage a freshwater forecasting system for the
Upper Yellowstone River (UYR) watershed based around SWAT+ simulations.

## Devlopment To-Do List

- make a common helpers file
- make sure all geojson files use WGS84 coordinates
- project to appropriate UTM zone before doing s2 operations
- finish watershed splitter
- Blacktail Deer example
