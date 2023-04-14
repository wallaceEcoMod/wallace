wallace 2.0.5
=============
- added try() to test_envs_worldclim.R
- fixed broken links in guidance texts refs
- fixed typo in thinOccs documentation

wallace 2.0.4
=============
- Added Spanish translation of vignette
- Updates to v2 vignette
- `skip_on_cran()` added to several tests

wallace 2.0.3
=============
- Fix conditional in test for occs_queryDB

wallace 2.0.2
=============
- Workaround for test when fails to download data from vertnet and ecoClimate

wallace 2.0.1
=============
- Add install.packages from CRAN in the vignette
- Remove BISON as option to download occurrences due to it is no longer available in `spooc` package (i.e. rbison archived in CRAN, and BISON data is available via GBIF).

wallace 2.0.0
=============
What is new in this version?

1. Stop and start your work whenever you want: This version allows you to save a session partway through the workflow and then restart it.  Try this out via the “Save session” tab if you think that is something that matches the way you would run analyses (intermediate-to-advanced users).

2. Greater reproducibility regarding occurrence records: As a step towards greater documentation and reproducibility in biodiversity informatics, this version of Wallace provides an option of querying GBIF and receiving a DOI for the data provided. If you are already a registered GBIF user, check the “Include Data Source Citations” option under the Query Database (Present) module of the OccData component.

3. Cross-time transfers: If you will later be interested in transferring your models to estimates of future (or past) climate, try out the updated Transfer component. This version now has data from WorldClim and EcoClimate (which also has reconstructed data for the past). Make your original model with the same climatic data source that you would like to use for transferring across time periods.

4. Make models for multiple species in the same Wallace session: This advancement supports two important other functionalities: comparisons of species in environmental space (see below).  If you are interested in either of these, try making models for more than one species now. To get the data to do this, in the OccData component either run the query multiple times or do it once with scientific names separated by commas.  Then, in later components use the pull-down menu in the top center of the interface to indicate the species that you want to work with.

5. Environmental space comparisons: It may not be as directly linked to conservation as many of the other advances, but this version now has several modules in the EnvSpace component, which allows users to examine characteristics of environmental space across species, like niche overlap.  If this matches your research interests (for academic purposes, or to study invasive species, for example), try them out.

6. Redesigned R-markdown functionalities: Because of the way this version of Wallace is redesigned, it now makes the documentation files in a new way (Reproduce component).  In addition to making sure that you can download the documentation in ways readable to humans (like pdf), it would be helpful to the development team to have users download the executable RMD code and confirm that it runs successfully in R (intermediate-to-advanced users).

Summary of new features:

* General - Several species in the same session
* General - New download sub-tab at each component
* General - Save and reload session
* Occs - PaleobioDB - Fossil occs (New module!)
* Occs - BIEN (New option!)
* Occs - GBIF OccCite (New option!)
* Occs - Delimitator and Separator (New option!)
* Envs - EcoClimate (New module, including paleo reconstructions!)
* General - Select variables with pickerInput
* General - Batch option (to repeat decisions for other species)
* Process Env - Draw polygon (New module!)
* Environmental Space - NEW COMPONENT with various new modules
* Model - maxnet and maxent.jar - Categorical variables (New option!)
* Model - maxnet and maxent.jar - Parallel (New option!)
* Vis - New bioclim plot
* Vis - New maxent plots
* Vis - mapPred - New threshold option
* Xfer - userXfer - User specified rasters
* Xfer - user background extent (New Option!)
* Xfer - user-specified transferring extent (New option!)
* Xfer - New Time - Ecolimate (New Option!)
* Rep - RMD - New Structure 
* Rep - Metadata - based on rangeModelMetadata package (New module!)
* Rep - RefPackages (New module!)

wallace 1.1.3
=============
- shinytheme() replaced by bslib::bs_theme(). Fixed issue with newer version of RStudio.

wallace 1.1.2
=============
- Adding ecospat in Depends for fixing CBI calculation.

wallace 1.1.1
=============
- Migration to GitHub Actions for Continuous Integration testing

wallace 1.1.0
=============
- Working with new version of ENMeval (v2)

wallace 1.0.6.3
===============
- rgbif updated dependency
- Clamping option back for maxent.jar (Thanks for the new version of Maxent 3.4.4)

wallace 1.0.6.2
===============
- Double distance in point buffer fixed.

wallace 1.0.6.1
===============
- Adding a couple convenience functions for running wallace headless by @isaacovercast

wallace 1.0.6
=============
- Wallace no longer needs rJava to run! Oh happy days! Wallace is now compatible with ENMeval 0.3.0, which now has no rJava dependency and runs Maxent using maxnet by default (CRAN package maxnet; https://onlinelibrary.wiley.com/doi/abs/10.1111/ecog.03049). This means Wallace no longer loads rJava automatically when using the ENMeval partition functions or running Maxent. You can still select the Java implementation of Maxent by choosing "maxent.jar" in the Maxent module, whereupon rJava will load. 
- Wallace now works on computers that error when some non-ASCII characters are used. This problem was discovered during a workshop in Vietnam on some Chinese computers.
- Users can now select bioclimatic variables when using 30 arc second data.
- Added more instructions on how to troubleshoot installing rJava.
- Occurrence points with NA environmental values now disappear from the map.
- We also fixed some other small bugs dealing with the shiny code and Markdown file.
- MESS color gradient
- Small changes in text guidance

wallace 1.0.5
=============
- A brand new vignette was finally added to our website. Please find it here: https://wallaceecomod.github.io/vignettes/wallace_vignette.html
- *Methods in Ecology and Evolution* paper published in April 2018 -- DOI remains the same.
- The lambdas file for each Maxent model can now be viewed in a subtab of Results.
- Projections in the Project component no longer overlap with map predictions from the Visualize component.
- Added more options to Draw toolbar to allow users to erase drawn polygons. Until `leaflet.extras` enables programmatic removal of drawn polygons again, the Reset button will only reset the data, and not affect polygons. 
- Pagination was added back to the model results table.
- Enabled download of the model results table as .csv.
- Removed the pop-up field from the occurrence table for downloads.
- Fixed a problem with downloading rasters as .png.
- Fixed bug that made the app crash if Project is selected after pressing Reset to remove the polygon in the Project component.
- There is now a check to see that `rgdal` is installed before downloads of rasters are allowed. This turns out to be an issue with `raster` package, as the `rgdal` dependency for this function doesn't seem to be functional.
- The slider for regularization multipliers is now restricted to a minimum of 0.5, and is able to be set to increments of 0.5. An RM value of 0 caused errors for Hinge models, and it's not clear whether RM of 0 is recommendable for other FCs, so it was removed.
- In the highly improbable case users select all points in the Select Occs module, the app now does not crash, and instead informs the user to select a subset instead.

wallace 1.0.4
=============
- Fixed the mapped display for the user-drawn polygon in **Module:** ***Select Occurrences On Map*** so that it remains displayed after the Finish button is pressed.
- Changed all `system.file()` calls to files in the `wallace` package to relative paths, which ensures that in those cases where the user downloads from Github and doesn't have the package installed from CRAN, or the package being developed is not the one installed from CRAN, the correct files can be found. To make this work, the folders `inst/Rmd`, `inst/css`, and `inst/js` have been moved to `inst/shiny`. An exception is the `run_wallace()` function, which retains `system.file()` because it is not in the `inst/shiny` folder. 
- Fixed the intro tab text formatting and added a bulleted list.
- Minor changes to simplify code.
- Added link to DOI of Early View manuscript in MEE.

wallace 1.0.3
=============
- Added error checks for when the coordinate reference system (CRS) of an input user raster is NA, and updated guidance text in the User-specified Environmental Data module with a guide on configuring the CRS of rasters in R.
- Fixed a bug that prevented downloads of 30 arcsec Worldclim rasters.
- Updated the intro screen tab module names.

wallace 1.0.2
=============
- There is now an option to download all the response curves at once in the same png.
- Local path to dismo's maxent.jar is now printed inside the log window to avoid failure to word wrap in some browsers.
- Component 2 module "Select Occs" now informs user of the occIDs of points removed, not of the ones retained.

wallace 1.0.1
=============
- Removed button used for development.

wallace 1.0.0
=============
- Biggest change: reprogrammed most of the software to integrate shiny modules. Now each module is a separate script in /inst/shiny/modules that contains both UI and server logic. This should make authorship and integration of new modules much easier.
- Second biggest change: integrated leaflet draw toolbar functionality from leaflet.extras package, which replaces the previous code to draw and internally record polygons in comps 2 and 7. The code is much cleaner now and easier to make edits to.
- By popular demand, added user-input environmental variables module. This is a work in progress, so please report bugs to Issues on Github.
- Added buffer by points feature for background extent module (comp4).
- Added cloglog output option for Maxent (updated for Maxent version 3.4x) in map prediction module (comp7).
- Partition occurrences modules (comp5) now plot on the map with a legend showing the partition group per color.
- Maxent models now show the evaluation statistics for all partition groups (ENMevaluate() option bin.output=TRUE) -- the partition group numbers correspond to the comp5 legend.
- Unit tests are now available for all modules in two flavors: 1) control tests run automatically via Travis with each Github push, which test that the UI buttons, forms, etc. are functioning properly, and 2) function tests which run Wallace as a user would by pushing buttons and entering fields, and as these tests take more than 5 minutes to complete, they are not automatically run for now. Unit tests will be a signature part of module contribution.
- Updated guidance text.

wallace 0.6.4
=============
- Added explicit package references to writeRaster() calls. Should work now without loading `raster` package first.
- Fixed reference to required field names in component 1 User Occurrences module guidance text. Now reads "names" instead of "species" as first field.
- Added checks for appropriate fields for VertNet and BISON, as not all downloads include all fields. Should avoid erroring due to missing fields now.

wallace 0.6.3
=============
- Added `repmis` and `rgdal` to package dependencies.
- Turned off evaluation of chunk that loads packages in userReport.Rmd.
- Added error catches to component 5 and 6 if `rJava` cannot load.

wallace 0.6.2
=============
- Turned off evaluation of code chunks in userReport.Rmd. Now the building of user session code is much, much faster.
- Made code compatible with `leaflet` version 1.0.1. There was a mysterious problem with zooming to points before clearing markers and/or plotting points, which resulted in crashing as soon as points are plotted unless the user had the Github master version of `leaflet` installed. Thus the code was rearranged and cleaned to accomodate the current `leaflet` version on CRAN.
- Fixed some errors in userReport.Rmd relating to the user .csv path and printing of a couple of headings.
