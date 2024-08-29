#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Ratio Overlap
context("inputPoly")

### Set parameters
# overlap polygon
bgShp_path <- list.files(path = system.file("extdata/shp", package = "wallace"),
                         full.names = TRUE)
bgShp_name <- list.files(path = system.file("extdata/shp", package = "wallace"),
                         full.names = FALSE)
#source polygon
r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",
                             package = "wallace"))
r[r == 0] <- NA
r <- terra::as.polygons(r)
overlapArea <- sf::st_as_sf(r)

# generate wrong parameters (to test error messages)
bgShp_error <- "Bassaricyon_neblina.prj"
## shapefile with no crs
## shapefile with crs not wgs84
## shapefile that doesn't overlap

### Run function
spatialPoly <- indic_inputPoly(bgShp_path,
                bgShp_name,
                overlapArea,
                logger = NULL,
                spN = NULL)

################ Tests ####################################

### test if the error messages appear when they are supposed to
# test_that("error checks", {
#   # .shp, .shx, or .dbf missing
#   expect_error(indic_inputPoly(bgShp_path[1:2],
#                                bgShp_name[1:2],
#                                overlapArea,
#                                logger = NULL,
#                                spN = NULL),
#                paste0("If entering a shapefile, please select all the ",
#                       "following files: .shp, .shx, .dbf."))
#
#   # shapefile not entered
#   expect_error(indic_inputPoly(bgShp_path,
#                                bgShp_error,
#                                overlapArea,
#                                logger = NULL,
#                                spN = NULL),
#                paste0("Please enter shapefile (.shp, .shx, .dbf)."))
# })


### test if the warning messages appear when they are supposed to
# test_that("warnings checks", {
#   ## shapefile has no projection
#   ### is.na(sf::st_crs(polyData))
#   expect_warning(indic_inputPoly(),
#                  paste0("Projection not found for shapefile. It is assumed that shapefile datum is WGS84."))
#   #attribute variables are assumed to be spatially constant throughout all geometries
#
#   ## shapefile projection is not WGS84
#   ###sf::st_crs(polyData)$input != "EPSG:4326")
#   expect_warning(indic_inputPoly(),
#                  paste0("Original coordinate reference system (CRS) is not WGS84 (EPSG:4326). ",
#                         "Shapefile was reprojected to this CRS."))
#
#   ##
#   expect_warning(indic_inputPoly(),
#                  paste0("Shapefile does not intersect the area to overlap. Please specify a new polygon."))
#
#   # warnings from sf
#   # although coordinates are longitude/latitude, st_intersects assumes that they are planar
#   # attribute variables are assumed to be spatially constant throughout all geometries
# })


### test output features
test_that("output checks", {
  # sf object
  expect_is(spatialPoly, "sf")
  # list of 2
  expect_equal(length(spatialPoly),2)
  # list is made of 1) an integer 2) a sfc poly
  expect_is(spatialPoly$Bassaricyon_neblina, "integer")
  expect_is(spatialPoly$geometry, "sfc_MULTIPOLYGON")
})

### test function steps
