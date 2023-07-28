# Helper script for the lamar.Rmd article
# Dean Koch, July 2023
#
# This creates two rswat.maker projects, skipping workflow steps for any
# existing results. If both projects already exist, the run_maker calls
# should just report that everything is up to date
#
# At the end of the script we render the article. This article's source Rmd
# file has output snippets copied by hand from the log file at `maker_path`,
# so whole the process is a bit circular.
#
# The point is to makes editing the markdown easier, and avoid downloading
# anything more than once. These steps involve (sometimes unreliable) web
# requests and caching with knitr does not seem to work 100% of the time.

library(rswat.maker)

# markdown directory
markdown_file = 'D:/rswat.maker/vignettes/articles/lamar.Rmd'

# local path to outputs folders
data_dir_uyrw = 'D:/rswat_data/yellowstone'
data_dir_smaller = 'D:/rswat_data/lamar'
maker_path = data_dir_smaller |> file.path('run_maker_log.txt')

# This runs the Lamar river QSWAT+ setup example initially to establish files
# and get a complete log. The "lamar.Rmd" article uses snippets from the output
# in this log file (copy/pasted by hand) rather than generating it each time
# the article is rendered (or the cache is replaced).

# Lamar river gage site coordinates (in WGS84)
outlet = c(-110.400953708362735, 44.928980820264847) |> 
  sf::st_point() |> 
  sf::st_sfc(crs=4326)

# two sinks to save console output from the smaller example
if( !dir.exists(data_dir_smaller) ) dir.create(data_dir_smaller)
maker_file = maker_path |> file(open = 'wt')
maker_file |> sink(type='output')
maker_file |> sink(type='message')

# run the entire workflow on smaller example
data_dir_smaller |> run_maker(what = NULL,
                              outlet = outlet,
                              nwis_from = as.Date('2005-01-01'),
                              overwrite = TRUE,
                              no_download = TRUE)

# close the logfile
sink(type='output')
sink(type='message')

# Upper Yellowstone River outlet at Carter's Bridge
outlet_uyrw = c(-110.5665168, 45.5969578) |> 
  sf::st_point() |> 
  sf::st_sfc(crs=4326)

# now run the workflow on Yellowstone, up to splitting, all gage years
data_dir_uyrw |> run_maker(what=c('catch', 'dem', 'nwis', 'land' , 'soil', 'split'),
                           outlet = outlet_uyrw,
                           nwis_from = NULL,
                           overwrite = TRUE,
                           no_download = TRUE)
                               
# when finished, render the article
rmarkdown::render(markdown_file)

