# wallace 0.6.4
- Added explicit package references to writeRaster() calls. Should work now without loading `raster` package first.
- Fixed reference to required field names in component 1 User Occurrences module guidance text. Now reads "names" instead of "species" as first field.
- Added checks for appropriate fields for VertNet and BISON, as not all downloads include all fields. Should avoid erroring due to missing fields now.

# wallace 0.6.3
- Added `repmis` and `rgdal` to package dependencies.
- Turned off evaluation of chunk that loads packages in userReport.Rmd.
- Added error catches to component 5 and 6 if `rJava` cannot load.

# wallace 0.6.2
- Turned off evaluation of code chunks in userReport.Rmd. Now the building of user session code is much, much faster.
- Made code compatible with `leaflet` version 1.0.1. There was a mysterious problem with zooming to points before clearing markers and/or plotting points, which resulted in crashing as soon as points are plotted unless the user had the Github master version of `leaflet` installed. Thus the code was rearranged and cleaned to accomodate the current `leaflet` version on CRAN.
- Fixed some errors in userReport.Rmd relating to the user .csv path and printing of a couple of headings.
