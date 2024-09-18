#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Ratio Overlap
context("inputPoly")

### Set parameters
# Overlap polygon
ovShp_path <- list.files(path = system.file("extdata/wdpa", package = "wallace"), full.names = TRUE)
ovShp_name <- list.files(path = system.file("extdata/wdpa", package = "wallace"), full.names = FALSE)

# Source polygon
overlapRange <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif", package = "wallace"))
overlapRange[overlapRange == 0] <- NA
overlapRange <- terra::as.polygons(overlapRange)
overlapRange <- sf::st_as_sf(overlapRange)

# Run function
suppressWarnings(polyData <- indic_inputPoly(ovShp_path, ovShp_name, overlapRange, logger = NULL, spN = NULL))
# warning msg suppressed: Projection not found for shapefile. It is assumed that shapefile datum is WGS84.

# generate wrong parameters (to test error messages)
## shp file error
ovShp_error <- "Bassaricyon_neblina.prj"

## shapefile that doesn't overlap
overlap_OutofRange <- raster::raster(nrows=108, ncols=108, xmn=-29, xmx=-23)
raster::values(overlap_OutofRange)<- runif(n = (108*108))
overlap_OutofRange <- terra::rast(overlap_OutofRange)
overlap_OutofRange[overlap_OutofRange == 0] <- NA
overlap_OutofRange <- terra::as.polygons(overlap_OutofRange)
overlap_OutofRange <- sf::st_as_sf(overlap_OutofRange)

## shapefile with crs not wgs84
### add a shape not in 4326 to test


################ Tests ####################################

### test if the error messages appear when they are supposed to
 test_that("error checks", {
  # .shp, .shx, or .dbf missing
  expect_error(indic_inputPoly(ovShp_path[1:2],
                               ovShp_name[1:2],
                               overlapRange,
                               logger = NULL,
                               spN = NULL),
               paste0("If entering a shapefile, please select all the following files: .shp, .shx, .dbf."))

  # shapefile not entered
  expect_error(indic_inputPoly(ovShp_path,
                               ovShp_error,
                               overlapArea,
                               logger = NULL,
                               spN = NULL),
               fixed = TRUE,
               "Please enter shapefile (.shp, .shx, .dbf)."
               )

  # Shapefile does not intersect the area to overlap.
  expect_error(suppressWarnings(indic_inputPoly(ovShp_path,
                               ovShp_name,
                               overlap_OutofRange,
                               logger = NULL,
                               spN = NULL)),
               paste0("Shapefile does not intersect the area to overlap. Please specify a new polygon.")
               )
    # warning about shapefile projection suppressed, unless there is a way to expect error AND warning?
 })

### test if the warning messages appear when they are supposed to
 test_that("warnings checks", {
   ## shapefile has no projection
   expect_warning(indic_inputPoly(ovShp_path, ovShp_name, overlapRange, logger = NULL, spN = NULL),
                  paste0("Projection not found for shapefile. It is assumed that shapefile datum is WGS84.")
   )

   ## shapefile projection is not WGS84
#   ###sf::st_crs(polyData)$input != "EPSG:4326")
#   expect_warning(indic_inputPoly(),
#                  paste0("Original coordinate reference system (CRS) is not WGS84 (EPSG:4326). ",
#                         "Shapefile was reprojected to this CRS."))
   #BAJ 09/17/2024: need to add a shape not in 4326 to add this test.
#
 })


### test output features
test_that("output checks", {
  # sf object
  expect_is(polyData, "sf")
  # list of 6
  expect_equal(length(polyData),6)
  # the names are correct
  expect_equal(names(polyData), c("NAME", "ORIG_NAME", "DESIG", "DESIG_ENG", "DESIG_TYPE", "geometry"))
  # overlap values > 0
  expect_equal(sum(lengths(sf::st_intersects(polyData, overlapRange))), 584)
})

### test function steps
