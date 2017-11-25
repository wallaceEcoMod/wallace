# wallace 1.0
- Biggest change: reprogrammed most of the software to integrate shiny modules. Now each module is a separate script in /inst/shiny/modules that contains both UI and server logic. This should make authorship and integration of new modules much easier.
- Second biggest change: integrated leaflet draw toolbar functionality from leaflet.extras package, which replaces the previous code to draw and internally record polygons in comps 2 and 7. The code is much cleaner now and easier to make edits to.
- By popular demand, added user-input environmental variables module. This is a work in progress, so please report bugs to Issues on Github.
- Added buffer by points feature for background extent module (comp4).
- Added cloglog output option for Maxent (updated for Maxent version 3.4x) in map prediction module (comp7).
- Partition occurrences modules (comp5) now plot on the map with a legend showing the partition group per color.
- Maxent models now show the evaluation statistics for all partition groups (ENMevaluate() option bin.output=TRUE) -- the partition group numbers correspond to the comp5 legend.
- Unit tests are now available for all modules in two flavors: 1) control tests run automatically via Travis with each Github push, which test that the UI buttons, forms, etc. are functioning properly; and 2) function tests which run Wallace as a user would by pushing buttons and entering fields, and as these tests take more than 5 minutes to complete, they are not automatically run for now. Unit tests will be a signature part of module contribution.
- Updated guidance text.

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
