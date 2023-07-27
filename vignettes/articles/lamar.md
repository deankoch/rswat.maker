rswat.maker and the Lamar River
================
Dean Koch
July 24, 2023

- [Introduction](#introduction)
  - [Getting started](#getting-started)
  - [The whole workflow](#the-whole-workflow)
- [Data](#data)
  - [1. NHDPlus catchment model
    (`get_catch`)](#1-nhdplus-catchment-model-get_catch)
  - [2. NWIS gage data (`get_nwis`)](#2-nwis-gage-data-get_nwis)
  - [3. DEM (`get_dem`)](#3-dem-get_dem)
  - [4. NLCD land use (`get_land`)](#4-nlcd-land-use-get_land)
  - [5. STATSGO2/SSURGO soil
    (`get_soil`)](#5-statsgo2ssurgo-soil-get_soil)
- [Processing](#process)
  - [6. Subcatchments (`get_split`)](#6-subcatchments-get_split)
    - [Splitting at gages](#splitting-at-gages)
    - [Snapping and overlap](#snapping-and-overlap)
  - [7. QSWAT+ inputs (`save_qswat`)](#inputs)
  - [8. QSWAT+ setup (`run_qswat`)](#qswat)
    - [Automation with `run_qswat`](#automation-with-run_qswat)
    - [QSWAT+ results](#qswat-results)
  - [9. SWAT+ Editor (`run_editor`)](#editor)
- [Applications](#applications)
  - [Subbasin layout](#subbasin-layout)
  - [Divide and conquer](#divide-and-conquer)
  - [Parallelization](#parallelization)
- [References](#references)

# Introduction

This article explains how to use `rswat.maker` to create a series of
SWAT+ projects, using the [Lamar
River](https://www.nps.gov/articles/000/lamar-river.htm) in Yellowstone
National Park as an example. After introducing the big picture here, we
walk users through each step in detail in the [Data](data) and
[Processing](process) sections.

[SWAT+](https://swat.tamu.edu/software/plus/) is a process-based
watershed simulator (Bieger et al. 2017) familiar to many in the
hydrology and water management communities for its scope, ease of
access, and long history of active development. A graphical user
interface (GUI) called [QSWAT+](https://swat.tamu.edu/software/qswat/)
is provided for model construction. However, like most (distributed)
hydrodynamic models, SWAT+ is very data-hungry. This, combined with its
reliance on complex workflows in GUIs create problems for
reproducibility and collaboration (Chawanda et al. 2020; Eilander et al.
2023).

`rswat.maker` is an R package for constructing SWAT+ models using
GUI-free, automated, programmable steps. It queries federal hydrology
databases for data on your catchment before dividing it into
*subcatchments*, which are then turned into SWAT+ projects. Here is what
that looks like for the Lamar, with the subcatchments in grey and
derived boundaries in white

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/overview-catch-plot-1.png" alt="Three models of the Lamar River: from NHDPlus (left), rswat.maker (middle), and QSWAT+ (right)." width="33%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/overview-catch-plot-2.png" alt="Three models of the Lamar River: from NHDPlus (left), rswat.maker (middle), and QSWAT+ (right)." width="33%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/overview-catch-plot-3.png" alt="Three models of the Lamar River: from NHDPlus (left), rswat.maker (middle), and QSWAT+ (right)." width="33%" />
<p class="caption">
Three models of the Lamar River: from NHDPlus (left), rswat.maker
(middle), and QSWAT+ (right).
</p>

</div>

The final output is a set of three SWAT+ models and a parent QSWAT+
project for each one (plotted in Figure @ref(fig:overview-catch-plot),
right), all linked together at the circled gage sites. By default the
package looks for gages with service records on the [National Water
Information System](https://waterservices.usgs.gov) (NWIS) of the USGS,
and it will maintain a copy of current and historical NWIS flow data on
your local machine.

All of the intermediate steps in an `rswat.maker` project are
individually documented and reproducible, and all source data are cached
prior to transformation. This is similar in spirit to the (currently
inactive) *SWAT+ AW* project by Chawanda et al. (2020), whose
functionality is emulated by the last three steps of `rswat.maker` (in
subsections [7](inputs), [8](qswat), and [9](editor))

## Getting started

Every `rswat.maker` project starts with a main outlet location. In
principle this can be at any position along any major channel in the
continental USA, but we recommend setting it at/near the site of a
stream gage listed on NWIS.

For example, just above the outlet of the Lamar, at its confluence with
the Upper Yellowstone River, there is an active USGS stream gage
reporting daily flow values. This is indicated by the large white circle
in Figure @ref(fig:overview-catch-plot) (left). We set its (WGS84)
coordinates in R like so:

``` r
outlet = c(-110.0018722, 45.00283056) |> sf::st_point() |> sf::st_sfc(crs=4326)
```

The easiest way to browse for site coordinates is using the USGS
[National Water Dashboard](https://dashboard.waterdata.usgs.gov) web app
with the “Hydrology \> Watersheds” layer visible. Look for gages with
daily discharge records in suitably small watersheds. When you have
found your site, go to its metadata section and copy the coordinates.

## The whole workflow

With the outlet positioned we are now ready to generate SWAT+ projects.
For a completely automated workflow, pass the outlet along with a
storage path to the function `run_maker`.

``` r
run_maker(lamar_dir, outlet=outlet, overwrite=TRUE)
```

This path becomes the root of a directory tree where the function writes
all of its output in sequence. The whole process is illustrated in the
flow chart below, where the steps are grouped into four phases for
simplicity:

<div class="figure" style="text-align: center">

<img src="lamar_flowchart/flow_chart_rswat.png" alt="A flow chart for `rswat.maker` splits the whole process into four phases" width="75%" />
<p class="caption">
A flow chart for `rswat.maker` splits the whole process into four phases
</p>

</div>

Run a single step by setting `what` to its name (see `?run_maker` for
the list of step names). Those not interested in SWAT+ can also set the
keyword `what='data'`, which runs only Phase “A” in Figure
@ref(fig:flowchart).

This is a lot to take in all at once. However, `run_maker` is just a
wrapper for a series of helper function calls that run the individual
steps. So instead of showing all of the console output at once, we will
go through it step-by-step, and show how the individual helper functions
work.

# Data

The first five steps in `rswat.maker` are for fetching the data required
to set up QSWAT+.

## 1. NHDPlus catchment model (`get_catch`)

To narrow its search for hydrology data `run_maker` first needs a
spatial extent for the catchment. But all we have at the beginning is a
point - the outlet. The catchment boundary *could* be derived from a DEM
(and QSWAT+ will do so later) but this is tricky without knowing the
approximate extent ahead of time.

Instead, we get our initial model from the [National Hydrography Dataset
Plus](https://doi.org/10.3133/fs20203033) (NHDPlus), which offers a
nation-wide collection of catchment data at moderate resolution (Viger
et al. 2016). The helper function `get_catch` uses the
[`nhdR`](https://cran.r-project.org/package=nhdR) package to query this
database, and returns a subset of the NHDPlus data relevant to your
catchment, along with a catchment boundary.

``` r
catch_list = get_catch(outlet)
#> UTM zone: 12
#> VPU name: Upper Missouri (10U)
#> loading NHD stream network edges
#> loading NHD catchment polygons
#> loading NHD stream network geometries
#> loading NHD lake polygons
#> 
#> locating outlet
#> outlet COMID: 2962790
#> following upstream tributaries
#> merging 19 sub-catchment polygons
#> processing 19 stream reach(es)
#> processing 0 lake(s)
#> transforming to output projection
```

The output `catch_list` is a list of watershed features. Save it with
`save_catch`, reload it with `open_catch`, and visualize it with
`plot_catch`. For example the left panel of Figure
@ref(fig:overview-catch-plot), which shows the catchment, streams, and
lakes, was generated by this simple function call

``` r
plot_catch(lamar_dir, main='NHDPlus')
```

To get a list of file paths written in this step, call `save_catch` with
default argument `overwrite=FALSE`

``` r
save_catch(lamar_dir)
#>                                                     edge 
#>          "D:/rswat_data/lamar_contemporary/nhd/edge.csv" 
#>                                                   outlet 
#>    "D:/rswat_data/lamar_contemporary/nhd/outlet.geojson" 
#>                                                catchment 
#> "D:/rswat_data/lamar_contemporary/nhd/catchment.geojson" 
#>                                                     flow 
#>      "D:/rswat_data/lamar_contemporary/nhd/flow.geojson" 
#>                                                     lake 
#>      "D:/rswat_data/lamar_contemporary/nhd/lake.geojson" 
#>                                                 boundary 
#>  "D:/rswat_data/lamar_contemporary/nhd/boundary.geojson"
```

All of the `get_.` functions in the `rswat_maker` package have a
corresponding `save_.` function that works this way. Similarly, calling
`run_maker` with `overwrite=FALSE` returns a (long) list of all the
destination paths for a project.

## 2. NWIS gage data (`get_nwis`)

`get_nwis` uses the
[`dataRetrieval`](https://cran.r-project.org/package=dataRetrieval)
package (Hirsch and De Cicco 2015) to collect daily service records from
NWIS stream gage sites in your catchment. By default this looks for the
most common stream flow record type, parameter code `00060` (discharge
in ft3/s).

For the Lamar example we set `from_initial` to filter to records
extending into or beyond 2005. This is so that our stream flow record
aligns with a pre-existing weather database from a related R package,
[`wxArchive`](https://github.com/deankoch/wxArchive). Users fetch all
the available time periods by leaving `from_initial` at its default
(`NULL`).

``` r
nwis_points = get_nwis(lamar_dir, from_initial=as.Date('2005-01-01'))
#> requesting service records from NWIS
#>   |=========================================================| 100%
#> aggregating service records
#> found 7 station sites
#> 5 station(s) in catchment reporting flow_ft
#> filtering to 3 of 5 sites with records on/after 2005-01-01
```

The result is a data frame of point geometries with metadata about each
site. The locations of the three stations found here are drawn as small
grey circles in Figure @ref(fig:overview-catch-plot) (middle).

After saving the point data with `save_nwis`, run `update_nwis` to
download up-to-date service records and write them to disk.

``` r
update_nwis(data_dir, overwrite=TRUE)
#> updating site 06187915 (1 of 3)
#> processing 1 record(s) at Soda Butte Cr at Park Bndry at Silver Gate (06187915)
#> NWIS reports 9055 observation(s) of dv-00060
#> 6771 day(s) downloaded
#> 
#> updating site 06187950 (2 of 3)
#> processing 1 record(s) at Soda Butte Cr nr Lamar Ranger Station YNP (06187950)
#> NWIS reports 7265 observation(s) of dv-00060
#> 1360 day(s) downloaded
#> 
#> updating site 06188000 (3 of 3)
#> processing 1 record(s) at Lamar River nr Tower Ranger Station YNP (06188000)
#> NWIS reports 29991 observation(s) of dv-00060
#> 6694 day(s) downloaded
#> 
#> up to date
#> done
```

Subsequent calls to `update_nwis` will only request and append days not
found there already.

For example, our main outlet, “Lamar River nr Tower Ranger Station YNP”,
has site code `"06188000"`. Pass this to `load_nwis` and plot the result
to get a hydrograph for the outlet running up to present day. Notice the
extreme discharge rates during the spring of 2022, when a major flood
event hit Yellowstone National Park.

``` r
flow = load_nwis("06188000", data_dir=lamar_dir, output='values')
plot(value~date, data=flow, type='l', xlab='date', ylab='ft3/s', main='Lamar River discharge')
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/nwis-plot-1.png" alt="Hydrograph of stream flow from the Lamar River basin, measured near Tower Ranger Station"  />
<p class="caption">
Hydrograph of stream flow from the Lamar River basin, measured near
Tower Ranger Station
</p>

</div>

The helper function call in this step, and the ones that follow, has
first argument `data_dir` rather than `outlet` - the opposite of
`get_catch` in the first step. This is because `get_nwis` relies on
derived information that was saved to disk earlier on (the catchment
boundary). In later steps, we will need both the NHDPlus information and
the NWIS station locations. Order matters for most steps in this
workflow.

## 3. DEM (`get_dem`)

Next, `run_maker` prepares an elevation model. Again the area of
interest is determined from the NHDPlus boundary, except with some added
padding around the edges to allow for possible imprecision (cropping to
an imprecise boundary can cause issues in QSWAT+). `get_dem` then uses
the [`FedData`](https://cran.r-project.org/package=FedData) package to
fetch and merge the relevant tiles from the [USGS National Elevation
Dataset](https://nationalmap.gov) (NED) (Gesch et al. 2009).

``` r
dem = get_dem(lamar_dir)
#> Area of interest includes 4 NED tiles.
#> (Down)Loading NED tile for 45N and 110W.
#> (Down)Loading NED tile for 46N and 110W.
#> (Down)Loading NED tile for 45N and 111W.
#> (Down)Loading NED tile for 46N and 111W.
#> Mosaicking NED tiles.
```

Write the result to disk with `save_dem`. This saves both a copy of the
original mosaicked source raster, and another copy cropped to the area
of interest after warping (via bilinear averaging with GDAL) to the
Universal Transverse Mercator (UTM) coordinate system at 90 metre
resolution. View this UTM version of the DEM with `plot_rast`.

``` r
plot_rast(lamar_dir, what='dem')
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 542849.3 ymin: 4929961 xmax: 597659.3 ymax: 5012221
#> Projected CRS: WGS 84 / UTM zone 12N
#> POLYGON ((542849.3 4929961, 597659.3 4929961, 5...
plot_rast(lamar_dir, what='dem', mask=TRUE, main='mask=TRUE')
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 547259.3 ymin: 4934371 xmax: 593159.3 ymax: 5007631
#> Projected CRS: WGS 84 / UTM zone 12N
#> POLYGON ((547259.3 4934371, 593159.3 4934371, 5...
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/dem-plot-1.png" alt="The National Elevation Dataset over the Lamar River basin with and without masking" width="50%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/dem-plot-2.png" alt="The National Elevation Dataset over the Lamar River basin with and without masking" width="50%" />
<p class="caption">
The National Elevation Dataset over the Lamar River basin with and
without masking
</p>

</div>

Setting `mask=TRUE` will clip to the catchment boundary from NHDPlus in
the plot for clarity of presentation, but will not affect the file on
disk (masking is not recommended with QSWAT+).

Both `plot_rast` and `plot_catch` set default titles based on metadata
from the project, as in Figure @ref(fig:dem-plot) (left). The number in
parenthesis is the “COMID” key for the main outlet, which relates it to
other objects in the NHDPlus model.

## 4. NLCD land use (`get_land`)

`get_land` uses the
[`FedData`](https://cran.r-project.org/package=FedData) package to fetch
a raster of land use classifications from the 2019 [National Land Cover
Database](https://www.mrlc.gov) (NLCD) of the USGS (Homer et al. 2012).
These categories either indicate the dominant plant community or else
give a reason why there isn’t one (barren rock, water body, developed
land, *etc*)

``` r
land = get_land(lamar_dir)
#> running FedData::get_nlcd
#> copying raster data to RAM
#> projecting to UTM (GDAL warp)
```

In this step and the next, `rswat.maker` transforms the data to lie on
the same UTM grid as the DEM. This ensures that the outputs from all
three raster steps (3-5) share a common coordinate system and layout. As
before, there is a helper function, `save_land` that writes the source
raster and the derived (UTM) version. Call `plot_rast` to display the
UTM version

``` r
plot_rast(lamar_dir, what='land', mask=TRUE)
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/land-plot-1.png" alt="The National Land Cover Database over the Lamar River basin"  />
<p class="caption">
The National Land Cover Database over the Lamar River basin
</p>

</div>

    #> Geometry set for 1 feature 
    #> Geometry type: POLYGON
    #> Dimension:     XY
    #> Bounding box:  xmin: 547259.3 ymin: 4934371 xmax: 593159.3 ymax: 5007631
    #> Projected CRS: WGS 84 / UTM zone 12N
    #> POLYGON ((547259.3 4934371, 593159.3 4934371, 5...

## 5. STATSGO2/SSURGO soil (`get_soil`)

The process for soil data is similar to the land use step except that we
use two source databases: the older and less detailed STATSGO2, and
SSURGO, its modernized successor. Read more about them both at the
USDA’s [website](https://websoilsurvey.sc.egov.usda.gov).

At the time of writing the coverage of SSURGO is patchy compared to
STATSGO2, but when SSURGO is available it is expected to produce better
a hydrological model (Wang and Melesse 2006). So we fill holes in the
former with data from the latter. `get_soil` does this automatically
after downloading copies of both. The call goes like this

``` r
soil = get_soil(lamar_dir)
#> bounding box overlapped with 2 state(s) (MT, WY)
#> fetching data for Montana
#> fetching data for Wyoming
#> rasterizing MUKEYs for statsgo
#> fetching soil survey area keys
#> loading soilsa_a_nrcs.dbf and filtering to area of interest
#> loading SSURGO data from 5 soil survey area(s)...
#> (Down)Loading SSURGO data for survey area MT623
#> (Down)Loading SSURGO data for survey area MT640
#> (Down)Loading SSURGO data for survey area MT642
#> (Down)Loading SSURGO data for survey area WY656
#> (Down)Loading SSURGO data for survey area WY665
#> processing MUKEYs in 5 soil survey area(s)
#> rasterizing MUKEYs for ssurgo
#> opening STATSGO2 and SSURGO layers
#> merging
#> projecting to UTM (GDAL warp)
```

We edited out some lines from the console output above (output progress
bars and URLs) to keep it short, but the story is the same. `get_soil`
downloads the STATSGO2 polygons by by state from static URLs, then uses
the `FedData` to fetch the relevant pieces of SSURGO’s much larger and
more complex polygon database. Finally, the function rasterizes the
SSURGO polygons to the DEM grid, and replaces any `NA` values (or MUKEYs
that are absent from the SWAT+ database) with the STATSGO2 value at that
location.

Once again we have a helper function, `save_soil`, to write the source
data, along with their rasterized (UTM) versions, and the final merged
product. Call `plot_rast` to display the latter.

``` r
plot_rast(lamar_dir, what='soil', main='STATSGO2/SSURGO soil MUKEY', mask=TRUE)
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/soil-plot-1.png" alt="STATSGO2/SSURGO soil map unit keys (MUKEYS) over the Lamar River basin"  />
<p class="caption">
STATSGO2/SSURGO soil map unit keys (MUKEYS) over the Lamar River basin
</p>

</div>

    #> Geometry set for 1 feature 
    #> Geometry type: POLYGON
    #> Dimension:     XY
    #> Bounding box:  xmin: 547259.3 ymin: 4934371 xmax: 593159.3 ymax: 5007631
    #> Projected CRS: WGS 84 / UTM zone 12N
    #> POLYGON ((547259.3 4934371, 593159.3 4934371, 5...

The colors here serve only to distinguish adjacent polygons, so there is
no legend. The soil attributes themselves can found in a separate
tabular database (indexed by MUKEY), that is distributed by the SWAT+
development team. This is already included in the SWAT+ installer
bundle, so we there is no need to download it separately.

This is the end of the data retrieval phase of the workflow. All the
pieces needed for QSWAT+ have been collected. Now on to processing.

# Processing

QSWAT+ constructs a SWAT+ model by splitting the basin into subbasins,
all linked together by a stream network. At the shared outlet of these
streams is a real-life gage with discharge data. Since all of the
subbasins contribute to discharge at the outlet, this one hydrograph
signal can be used to fit all subbasin parameters at once. For example
with the Lamar River, we could use the data plotted in
@ref(fig:nwis-plot).

However, this approach leads to problems with model-fitting when the
basin is complex. `get_split` helps avoid these problems by preemptively
splitting a basin with multiple gages into subcatchments, so that the
resulting SWAT+ models are individually less complex.

## 6. Subcatchments (`get_split`)

It is up to the modeller to decide how many subbasins to set in SWAT+;
*ie* how detailed to make the jigsaw puzzle that forms the whole basin.
The more we subdivide, the more we achieve realism. But this comes at a
cost. Too many subbasins and the model becomes insensitive to their
individual contributions. Too many parameters, and we risk overfitting,
non-identifiability, and long compute times.

One way to balance this complexity trade-off is by splitting the problem
into smaller, more manageable pieces. The goal with `get_split` is to
use all the hydrographs at our disposal, while also minimizing the
number of unknown parameters fitted to any one of them.

### Splitting at gages

`get_split` partitions an NHDPlus catchment into *subcatchments*, one
per NWIS gage site. This site is always positioned at/near the outlet of
the subcatchment. In cases where one subcatchment drains into another
one, the outlet of the first becomes an inlet of the second. This is
done automatically, similar to the “delineation” step in QSWAT+

We have three gage sites in the Lamar basin - one at the main outlet and
two more on Soda Butte Creek. So, in this example, `get_split` produces
a list of three subcatchment datasets

``` r
split_result = get_split(lamar_dir)
#> loading gage points in D:/rswat_data/lamar_contemporary/nwis/flow_ft/station.geojson
#> computing catchment areas for 4 input gage locations
#>   |=========================================================| 100%      
#> resolving stream order for 4 COMIDs
#> removed 1 duplicate station site(s) from outlet list
#> clipping catchment polygons to form 3 sub-catchments
#> collecting sub-catchment features
#> copying sub-catchment 1/3 : soda butte cr at park bndry at silver gate
#> copying sub-catchment 2/3 : lamar river nr tower ranger station ynp
#> copying sub-catchment 3/3 : soda butte cr nr lamar ranger station ynp
```

Call `save_split` with `overwrite=TRUE` to write the data to disk. This
makes a new subdirectory for each subcatchment, each having (largely)
the same file structure as the root directory. Visualize a subcatchment
by passing one of these subdirectory paths to `plot_catch` or
`plot_rast` in place of `lamar_dir`

``` r
# subcatchment paths
sub_dir = save_split(lamar_dir)[['sub']] 
print(sub_dir)
#> [1] "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp"   
#> [2] "D:/rswat_data/lamar_contemporary/split/soda_butte_cr_at_park_bndry_at_silver_gate"
#> [3] "D:/rswat_data/lamar_contemporary/split/soda_butte_cr_nr_lamar_ranger_station_ynp"

# reordered for presentation
for(d in sub_dir[c(1,3,2)]) plot_catch(d)
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/splitter-subcatchments-plot-1.png" alt="Subcatchments of the Lamar River, linked at NHPlus outlets (white circles)" width="33%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/splitter-subcatchments-plot-2.png" alt="Subcatchments of the Lamar River, linked at NHPlus outlets (white circles)" width="33%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/splitter-subcatchments-plot-3.png" alt="Subcatchments of the Lamar River, linked at NHPlus outlets (white circles)" width="33%" />
<p class="caption">
Subcatchments of the Lamar River, linked at NHPlus outlets (white
circles)
</p>

</div>

To get a basin-level view of how the pieces fit together, pass the
basin-level directory (in our case `lamar_dir`) to `plot_catch` along
with argument `sub=TRUE`. For example, we generated Figure
@ref(fig:overview-catch-plot) (middle) using this simple call

``` r
plot_catch(lamar_dir, sub=TRUE, stream_col=NULL, main='Subcatchments')
```

With default arguments `plot_catch` draws grey circles for the actual
gage sites and white circles for their snapped positions in NHDPlus.

This is about as precise as we can get with NHDPlus. For example the
white and grey points in the Soda Butte Creek headwater (Figure
@ref(fig:splitter-subcatchments-plot), right) lie on the same COMID
polygon, so under that model we cannot distinguish their catchments.
While it may be obvious that the grey point drains a smaller area, it is
not so obvious how to (accurately) revise the catchment boundary.

### Snapping and overlap

In the next step, QSWAT+ will delineate new boundaries (*subbasins*)
where the gage sites more closely coincide with the outlets. However
that involves a DEM covering, at minimum, the entire catchment. To help
avoid accidentally masking part of the catchment, `get_split` snaps
outwards to the nearest NHDPlus model boundaries and uses a buffer
around the result when cropping rasters and geometries.

Subcatchments with inlets usually have some overlap with their upstream
neighbour as a result of snapping. `plot_catch` draws this overlap with
a different tone of fill colour to make it more obvious (as in Figure
@ref(fig:splitter-subcatchments-plot), middle).

Both `get_catch` and `open_catch` return two versions of the boundaries:
one with and one without the overlap (an outer and inner boundary). The
inner boundary forms the partition of the NHDPlus model, whereas the
outer boundary defines the extent for QSWAT+ inputs.

At this stage we have all the information we need to run QSWAT+. What
remains is to translate the data into a format that QSWAT+ can
understand. This is the job of `save_qswat`.

## 7. QSWAT+ inputs (`save_qswat`)

In the following call we set `sub=TRUE` to loop over subcatchments and
make QSWAT+ inputs for each of them. Leave `sub=FALSE` at its default to
make a whole-basin project instead or to run the step for a single
subcatchment.

``` r
qswat_result = save_qswat(lamar_dir, sub=TRUE, overwrite=TRUE)
#> making QSWAT+ inputs...
#> writing inputs for 3 sub-catchments
#> done
```

Call `save_qswat` again, this time dropping the `overwrite=TRUE`
argument, to get the paths written

``` r
qswat_paths = save_qswat(lamar_dir, sub=TRUE)
qswat_paths |> head(1) |> print()
#> $lamar_river_nr_tower_ranger_station_ynp
#>                                                                                             outlet 
#>  "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp/qswat/outlet.shp" 
#>                                                                                                dem 
#>     "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp/qswat/dem.tif" 
#>                                                                                               soil 
#>    "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp/qswat/soil.tif" 
#>                                                                                               land 
#> "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp/qswat/landuse.tif" 
#>                                                                                        land_lookup 
#> "D:/rswat_data/lamar_contemporary/split/lamar_river_nr_tower_ranger_station_ynp/qswat/landuse.csv"
```

These files require no further transformation for QSWAT+. They are
already in the recommended projection (UTM), and `save_qswat` has
formatted their contents using the expected encoding (for `NA` values,
plant codes, MUKEYs, *etc*).

## 8. QSWAT+ setup (`run_qswat`)

At this stage we could leave the R environment and open QGIS to run
QSWAT+ the normal way - with a cursor and a little patience. We
recommend that users do so at least once, and that they familiarize
themselves with QSWAT+ and its documentation before using the automation
in `run_qswat`. If things go awry in `rswat.maker`, the interactive GUI
in QGIS can often reveal the problem (*eg* a snapping issue) at a
glance.

The QSWAT+ plugin is mature, well-documented, and it has a large and
active user-base. However, there are serious drawbacks to using
graphical interfaces (GUIs) for model-building: reproducibility becomes
a challenge, and batch jobs become labour-intensive. `run_qswat` solves
both problems at once.

### Automation with `run_qswat`

QSWAT+ is written in Python and runs in the PyQGIS3 environment (QGIS
Development Team 2023), but users normally interact with it through the
QGIS GUI. `run_qswat` provides an alternative way of using QSWAT+ that
functions more like a command line interface (CLI). This is similar to
the SWAT+ AW program (Chawanda et al. 2020), except based in R.

`run_qswat` manually assigns setup parameters (some defaults, some
user-supplied, and some based on the input data) before calling the
processing modules in the right order. It produces the same output as
QSWAT+, including a working QGIS project file (.qgs) that can be opened
for viewing after setup is complete.

Note that `run_qswat` wraps a batch file that looks for QGIS in
`osgeo_dir`. This means the function is only available to Windows users
with a QGIS installation, and if this is not in the usual location (see
below), `osgeo_dir` must set manually. The example below shows how to
run setup on the subcatchment in Figure
@ref(fig:splitter-subcatchments-plot) (middle).

``` r
# default path to QGIS
osgeo_dir = 'C:/Program Files/QGIS 3.32.0'

# copy subcatchment paths and pick one
sub_dir = save_split(lamar_dir)[['sub']] 
sub_dir_soda = sub_dir[3]

# run setup 
qswat_result = run_qswat(sub_dir_soda, overwrite=TRUE, osgeo_dir=osgeo_dir)
#> running QSWAT+ to create project: soda_butte_cr_nr_lamar_ranger_station_ynp
#> running delineation checks
#> loading QSWAT+ model geometries from /qswat/soda_butte_cr_nr_lamar_ranger_station_ynp/Watershed/Shapes
#> outlet 0 moved 52 m in total
#> inlet 1 moved 14 m in total
#> QSWAT+ completed and passed checks
```

### QSWAT+ results

The console output above is deceptively short. Behind the scenes,
`run_qswat` has:

- recorded a list of inputs
- launched an instance of QGIS (without the GUI) and loaded the plugin
- executed all QSWAT+ steps (delineation, HRUs, *etc*) and saved the
  results
- opened the resulting geometries and run sanity checks (see
  `?check_qswat`)
- written a log file

This process is easy to reproduce. As long as the inputs remain
unchanged, you will get identical output from every call to `run_qswat`.
Get the input and output file paths by calling `run_qswat` with (the
default) `overwrite=FALSE`

``` r
# get paths to outputs from QSWAT+ and rswat.maker
qswat_paths = run_qswat(sub_dir_soda)
qswat_paths |> names() |> print()
#> [1] "qswat"  "input"  "output" "log"
```

“input” is a JSON record of all inputs and parameters supplied to
QSWAT+,

``` r
# list all inputs
qswat_paths[['input']] |> readLines() |> jsonlite::fromJSON() |> names() |> print()
#>  [1] "info"              "name"              "dem"               "outlet"            "landuse_lookup"   
#>  [6] "landuse"           "soil"              "lake_threshold"    "channel_threshold" "stream_threshold" 
#> [11] "snap_threshold"
```

“log” contains (we hope) a string of messages from QSWAT+ about
completed steps, as it does here,

``` r
# read the last few lines of logfile
qswat_paths[['log']] |> readLines() |> tail() |> cat()
#> Writing aquifers and deep aquifers tables ... HRUs done: 359 HRUs formed with 229 channels in 13 subbasins.  writing D:/rswat_data/lamar_contemporary/split/soda_butte_cr_nr_lamar_ranger_station_ynp/qswat/qswat_output.json  >> finished running QSWAT+
```

“output” is another JSON record listing important project-related paths,

``` r
qswat_paths[['output']] |> readLines() |> jsonlite::fromJSON() |> names() |> print()
#>  [1] "channel"       "stream"        "sub"           "outlet"        "lake"          "hru"          
#>  [7] "lsu"           "hru_full"      "lsu_full"      "sql"           "txt"           "editor_exe"   
#> [13] "simulator_dir"
```

and “qswat” points to the root directory of the whole QSWAT+ project.

View the geometries from the “output” list (ESRI shapefiles) by loading
them `load_qswat`, or by plotting them directly with `plot_qswat`. For
example, here are the NHDPlus and QSWAT+ models side by side for
comparison

``` r
sub_dir_soda |> plot_catch()
sub_dir_soda |> plot_qswat(main='QSWAT+ Model')
#> loading QSWAT+ model geometries from /qswat/soda_butte_cr_nr_lamar_ranger_station_ynp/Watershed/Shapes
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/qswat-plot-1.png" alt="Two models of Soda Butte Creek subcatchment geometries: from NHDPlus (left) and QSWAT+ (right)" width="50%" /><img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/qswat-plot-2.png" alt="Two models of Soda Butte Creek subcatchment geometries: from NHDPlus (left) and QSWAT+ (right)" width="50%" />
<p class="caption">
Two models of Soda Butte Creek subcatchment geometries: from NHDPlus
(left) and QSWAT+ (right)
</p>

</div>

The channel geometry from QSWAT+ is far more detailed than NHDPlus in
this example, but this can be adjusted via the threshold arguments in
`run_qswat`. In general users can expect the QSWAT+ delineation output
to slightly differ from (and improve on) the output from NHDPlus.

Outlets and inlets are identified in the QSWAT+ plot with large unfilled
circles. Notice that the NHDPlus outlet is gone, and we have only the
one point for the inlet, correctly positioned at its gage site.

Note also that `run_qswat` currently only supports the “Dominant HRU”
method of delineating HRUs in QSWAT+. This assigns a set of parameters
to each subbasin that is meant to be representative of all the landscape
within it. The larger the subbasin, the more this becomes an abstraction
that summarizing the overall behaviour of several qualitatively
different landscape features.

## 9. SWAT+ Editor (`run_editor`)

QSWAT+ has produced an SQLite database, a set of geometries, and a QGIS
project, but no SWAT+ configuration (config) files. The config files are
what actually configure and initialize the SWAT+ simulator (whereas the
QSWAT+ files provide context). The SWAT+ development team provides a
standalone Python3-based GUI, [SWAT+
Editor](https://bitbucket.org/swatplus/swatplus.editor), for creating
config files.

Users normally open SWAT+ Editor and point it to their project database
(.sqlite) file, or else open the QSWAT+ project (.qgs) file in QGIS and
use its interface. `run_editor` runs config file creation automatically,
by making use of the SWAT+ Editor CLI for batch jobs (via `shell`).

Simply call `run_editor` after completing `run_qswat`

``` r
editor_result = run_editor(sub_dir_soda, overwrite=TRUE)
#> running SWAT+ Editor to populate /qswat/soda_butte_cr_nr_lamar_ranger_station_ynp/Scenarios/Default/TxtInOut
```

This converts the SQLite database from the previous step (QSWAT+) into a
set of several dozen config files in the “TxtInOut” subdirectory. It
also optionally imports weather data from `weather_dir`, or else sets
default weather station locations at the centroids of the subbasins (see
`?run_editor` and `?make_weather`).

As with QSWAT+ setup, this step produces a log file

``` r
# find output file paths and print contents
editor_paths = run_editor(sub_dir_soda)
editor_paths[['log']] |> readLines() |> tail() |> cat()
#>  {"percent": 116, "message": "Importing ru_yr.txt..."}  {"percent": 118, "message": "Importing ru_aa.txt..."}  Done.
editor_paths[['txt']] |> list.files() |> head()
#> [1] "aqu_catunit.ele" "aqu_dr.swf"      "aquifer.aqu"     "aquifer.con"     "aquifer_aa.txt" 
#> [6] "aquifer_yr.txt"
```

The files in “TxtInOut” should now be ready for the SWAT+ simulator. See
our other R package, [`rswat`](https://github.com/deankoch/rswat), which
allows users to explore these files and run simulations (and much more!)
from within the R environment.

# Applications

The idea with `get_split` is to configure your subcatchments (in SWAT+)
to accept inlet data instead of modelling the whole drainage upstream.
This has several advantages, which we talk about below.

The main downside with splitting is that there is no ground water
exchanged between subcatchments. Unlike surface water discharge, which
can be linked to other models via inlets, there is currently no way of
linking aquifers from separate SWAT+ models.

This may not matter, depending on the basin, the layout of gages, and
the modelling goals. But if it does, one possible workaround would be to
use an external ground water simulator that ignores subbasin boundaries,
like *gwflow* (Bailey et al. 2020), or rather an extension of it that
supports multiple concurrent SWAT+ surface water models.

## Subbasin layout

Subcatchments give users a more fine-grained control over the basin-wide
layout of subbasins. For example in Figure @ref(fig:overview-catch-plot)
(right) the number of subbasins per unit subcatchment area is fixed,
resulting in more detail along the well-gaged Soda Butte Creek versus
the rest of the Lamar basin. Here is that plot again with subcatchments
coloured differently.

``` r
# initialize mostly empty plot
plot_catch(lamar_dir,
           sub = TRUE,
           fill_col = 'white',
           stream_col = NULL,
           outlet_col = NULL,
           inlet_col = NULL,
           main = 'QSWAT+ projects for Lamar River basin')

# overlay QSWAT+ geometries
sub_dir = save_split(lamar_dir)[['sub']]
sub_dir[1] |> plot_qswat(fill_col=grDevices::adjustcolor('red', 0.3), quiet=TRUE, add=TRUE)
sub_dir[3] |> plot_qswat(fill_col=grDevices::adjustcolor('green', 0.3), quiet=TRUE, add=TRUE)
sub_dir[2] |> plot_qswat(fill_col=grDevices::adjustcolor('blue', 0.3), quiet=TRUE, add=TRUE)
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/splitter-bigger-plot-1.png" alt="QSWAT+ models for three Lamar River subcatchments with NWIS gages at the circled sites"  />
<p class="caption">
QSWAT+ models for three Lamar River subcatchments with NWIS gages at the
circled sites
</p>

</div>

Using the same ratio of subbasins per hydrograph on the main outlet
catchment (in red), we wind up with less detail because the drainage is
much larger. The main outlet hydrograph is the most difficult to fit
(because of its large drainage), so a simpler model is better suited
there. Upstream of it we leverage additional gage records to model a
smaller area in detail.

The result is a more intentional and balanced model with respect to the
observed data, and this makes model-fitting likelier to succeed.

## Divide and conquer

Splitting allows SWAT+ users to tackle larger basins and more complex
arrangements of gages. For example we can easily create a project
covering the whole Upper Yellowstone River basin, which drains the
Lamar. The output of `get_split` for that project, with no restrictions
on gage era, looks like this

``` r
# print the subcatchment names
yellowstone_split = save_split(yellowstone_dir)
yellowstone_split[['sub']] |> basename() |> print()
#>  [1] "bear_creek_near_jardine_mt"                      "big_creek_near_emigrant_mt"                     
#>  [3] "blacktail_deer_creek_nr_mammoth_ynp"             "east_fork_blacktail_deer_creek_near_mammoth_ynp"
#>  [5] "gardner_river_near_mammoth_ynp"                  "lamar_river_nr_tower_ranger_station_ynp"        
#>  [7] "lupine_creek_near_mammoth_ynp"                   "mill_creek_near_pray_mt"                        
#>  [9] "soda_butte_cr_at_park_bndry_at_silver_gate"      "soda_butte_cr_nr_lamar_ranger_station_ynp"      
#> [11] "soda_butte_creek_at_cooke_city_mt"               "soda_butte_creek_at_silver_gate_mt"             
#> [13] "tower_creek_at_tower_falls_ynp"                  "yellowstone_river_at_corwin_springs_mt"         
#> [15] "yellowstone_river_at_tower_junction_ynp"         "yellowstone_river_at_yellowstone_lk_outlet_ynp" 
#> [17] "yellowstone_river_near_canyon_hotel_ynp"         "yellowstone_river_near_livingston_mt"
```

The code below plots the DEM for the Yellowstone project and overlays
the 18 subcatchment boundaries (in black) and gages (grey circles). This
includes four in the Lamar basin (outlined in red), where `get_nwis` has
picked up an additional (pre-2005) gage site at a more upstream position
in Soda Butte Creek.

``` r
# DEM base layer with subcatchments overlay 
bbox_poly = yellowstone_dir |> plot_rast('dem', mask=TRUE)
boundary_poly = plot_catch(yellowstone_dir,
                           sub = TRUE, 
                           stream_col = 'lightblue',
                           stem_col = 'lightblue',
                           lake_col = 'lightblue',
                           outlet_col= NULL,
                           inlet_col= NULL,
                           border_col = 'black',
                           fill_col = NULL, 
                           add = TRUE)

# highlight Lamar basin
lamar_poly = open_catch(lamar_dir)[['boundary']] |> sf::st_transform(sf::st_crs(bbox_poly))
lamar_poly |> plot(border=grDevices::adjustcolor('red', 0.8), lwd=2, add=TRUE)
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/splitter-yellowstone-plot-1.png" alt="Subcatchments in the Upper Yellowstone River basin, with Lamar River basin highlighted in red"  />
<p class="caption">
Subcatchments in the Upper Yellowstone River basin, with Lamar River
basin highlighted in red
</p>

</div>

## Parallelization

This configuration of inlets and outlets in Figure
@ref(fig:splitter-yellowstone-plot) naturally leads to a
subcatchment-level parallelization scheme: Headwater subcatchments can
be simulated simultaneously as they are hydrologically decoupled.
Descending downstream, and conveying simulated discharge values to
inlets, we reach a second tier of subcatchment(s) where there is again
no interdependence, so these too can be simulated simultaneously.

Iterating in this way until the main outlet, we divide a whole-basin
simulation into a series of staged simulations at different subcatchment
tiers, each running in parallel. This scheme was proposed and
demonstrated in Cibin and Chaubey (2015) as part of their more general
*Multi-Level Spatial Optimization* framework. `get_split` produces a set
of catchment models compatible with that framework.

The scheme is illustrated for the Yellowstone project by the next code
chunk, which colors the 18 subcatchments from Figure
@ref(fig:splitter-yellowstone-plot) according to the stage at which they
would be simulated. To effect a parallelized simulation, users could
replace the `plot_catch` call below with a call to the SWAT+ simulator
followed by an update to any dependent inlet files (*eg* using `rswat`).

``` r

# master gage locations data frame
gage = yellowstone_split[['gage']] |> sf::st_read(quiet=TRUE)

# initialize the plot
col_stage = grDevices::hcl.colors(7, pal='Sunset', rev=TRUE) |> adjustcolor(0.5)
boundary_poly = plot_catch(yellowstone_dir, 
                           fill_col = NULL, 
                           outlet_col = NULL,
                           inlet_col = NULL,
                           stem_col = NULL,
                           stream_col = NULL)

# iterate over subcatchment tiers
for(i in seq_along(col_stage)) {

  # directories to process (initially headwaters)
  head_name = gage |> dplyr::filter(headwater) |> dplyr::pull(dir_name)
  head_dir = file.path(yellowstone_dir, 'split', head_name) |> unique()
  
  # color the subcatchment(s)
  for(d in head_dir) plot_catch(d, add=TRUE,
                                outlet_col = NULL,
                                inlet_col = NULL,
                                stream_col = 'lightblue',
                                stem_col = 'lightblue',
                                lake_col = 'lightblue',
                                fill_col = col_stage[i])
  
  # flag the next tier as "headwater"
  i = i + 1
  gage = gage |> 
    dplyr::filter(!headwater) |> 
    dplyr::mutate(headwater = !(comid %in% downstream) )
}
```

<div class="figure" style="text-align: center">

<img src="D:/rswat.maker/vignettes/articles/lamar_files/figure-gfm/parallel-plot-1.png" alt="Subcatchments colored according to their order in a SWAT+ parallelization scheme"  />
<p class="caption">
Subcatchments colored according to their order in a SWAT+
parallelization scheme
</p>

</div>

In the Yellowstone example simulations would proceed in seven stages,
corresponding to seven tiers in the routing network for the
subcatchments. This is illustrated in Figure @ref(fig:parallel-plot) as
a series of darkening colours, from yellow to purple. In the first three
stages (and particularly in the first), the simulator’s work is
efficiently split into several parallelizable chunks.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bailey2020new" class="csl-entry">

Bailey, Ryan T, Katrin Bieger, Jeffrey G Arnold, and David D Bosch.
2020. “A New Physically-Based Spatially-Distributed Groundwater Flow
Module for SWAT+.” *Hydrology* 7 (4): 75.

</div>

<div id="ref-swatplusbieger2017introduction" class="csl-entry">

Bieger, Katrin, Jeffrey G Arnold, Hendrik Rathjens, Michael J White,
David D Bosch, Peter M Allen, Martin Volk, and Raghavan Srinivasan.
2017. “Introduction to SWAT+, a Completely Restructured Version of the
Soil and Water Assessment Tool.” *JAWRA Journal of the American Water
Resources Association* 53 (1): 115–30.
<https://doi.org/10.1111/1752-1688.12482>.

</div>

<div id="ref-awchawanda2020user" class="csl-entry">

Chawanda, Celray James, Chris George, Wim Thiery, Ann Van Griensven,
Jaclyn Tech, Jeffrey Arnold, and Raghavan Srinivasan. 2020.
“User-Friendly Workflows for Catchment Modelling: Towards Reproducible
SWAT+ Model Studies.” *Environmental Modelling & Software* 134: 104812.
<https://doi.org/10.1016/j.envsoft.2020.104812>.

</div>

<div id="ref-parallelcibin2015computationally" class="csl-entry">

Cibin, Raj, and Indrajeet Chaubey. 2015. “A Computationally Efficient
Approach for Watershed Scale Spatial Optimization.” *Environmental
Modelling & Software* 66: 1–11.
<https://doi.org/10.1016/j.envsoft.2014.12.014>.

</div>

<div id="ref-hydromeilander2023" class="csl-entry">

Eilander, Dirk, Hélène Boisgontier, Joost Buitink, Anaı̈s Couasnon,
Brendan Dalmijn, Mark Hegnauer, Tjalling de Jong, et al. 2023. “HydroMT:
Automated and Reproducible Model Building and Analysis.” *Journal of
Open Source Software* 8 (83): 4897.
<https://doi.org/10.21105/joss.04897>.

</div>

<div id="ref-nedgesch2009national" class="csl-entry">

Gesch, Dean, Gayla Evans, James Mauck, John Hutchinson, William J
Carswell Jr, et al. 2009. “The National Map—Elevation.” *US Geological
Survey Fact Sheet* 3053 (4). <https://doi.org/10.3133/fs20093053>.

</div>

<div id="ref-dataretrievalhirsch2015user" class="csl-entry">

Hirsch, Robert M, and Laura A De Cicco. 2015. “User Guide to Exploration
and Graphics for RivEr Trends (EGRET) and dataRetrieval: R Packages for
Hydrologic Data.” US Geological Survey.
<https://doi.org/10.3133/tm4A10>.

</div>

<div id="ref-nlcdhomer2012national" class="csl-entry">

Homer, Collin H, Joyce A Fry, Christopher A Barnes, et al. 2012. “The
National Land Cover Database.” *US Geological Survey Fact Sheet* 3020
(4): 1–4. <https://doi.org/10.3133/fs20123020>.

</div>

<div id="ref-QGIS_software" class="csl-entry">

QGIS Development Team. 2023. *QGIS Geographic Information System*. QGIS
Association. <https://www.qgis.org>.

</div>

<div id="ref-nhdplusviger2016" class="csl-entry">

Viger, Roland J, Alan Rea, Jeffrey D Simley, and Karen M Hanson. 2016.
“NHDPlusHR: A National Geospatial Framework for Surface-Water
Information.” *JAWRA Journal of the American Water Resources
Association* 52 (4): 901–5. <https://doi.org/10.1111/1752-1688.12429>.

</div>

<div id="ref-soilswang2006effects" class="csl-entry">

Wang, Xixi, and Assefa M Melesse. 2006. “Effects of Statsgo and Ssurgo
as Inputs on Swat Model’s Snowmelt Simulation 1.” *JAWRA Journal of the
American Water Resources Association* 42 (5): 1217–36.
<https://doi.org/10.1111/j.1752-1688.2006.tb05296.x>.

</div>

</div>
