#### COMPONENT mask: Mask Prediction
#### MODULE: Mask by Shapefile
context("spatialPoly")

### Set parameters
bgShp_path <- list.files(path = system.file("extdata/shp", package = "wallace"),
                         full.names = TRUE)
bgShp_name <- list.files(path = system.file("extdata/shp", package = "wallace"),
                         full.names = FALSE)
sdm <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",
                                  package = "wallace"))

### Run function
suppressWarnings(spatialMask <- mask_spatialPoly(bgShp_path, bgShp_name, sdm,
                                logger = NULL,spN = NULL))

# error messages
bgShp_path_error <- list.files(path = system.file("extdata/wdpa", package = "wallace"),
                               full.names = TRUE)
bgShp_name_error <- list.files(path = system.file("extdata/wdpa", package = "wallace"),
                               full.names = FALSE)
sdm_error <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(sdm_error)<- runif(n = (108*108))

################ Tests ####################################

### test if the error messages appear when they are supposed to
test_that("error checks", {

  # didnt include all files; .shp, .shx, or .dbf missing
  expect_error(mask_spatialPoly(bgShp_path[1:2],
                                bgShp_name[1:2],
                                sdm,
                                logger = NULL,
                                spN = NULL),
               paste0('If entering a shapefile, please select all the ',
                      'following files: .shp, .shx, .dbf.'))

  # file wasn't a shapefile
  expect_error(mask_spatialPoly(bgShp_path[1:1],
                                bgShp_name[1:1],
                                sdm,
                                logger = NULL,
                                spN = NULL),
               paste0("Please enter shapefile: .shp, .shx, & .dbf."))

  # shapefile doesnt intersect sdm
  expect_error(suppressWarnings(mask_spatialPoly(bgShp_path_error,
                                bgShp_name_error,
                                sdm,
                                logger = NULL,
                                spN = NULL)),
               paste0("Shapefile must fall within the SDM extent. Please specify a new polygon. "))

  # the shapefile intersects, but only for NA values
  expect_error(suppressWarnings(mask_spatialPoly(bgShp_path,
                                bgShp_name,
                                sdm_error,
                                logger = NULL,
                                spN = NULL)),
               paste("Shapefile must fall within the SDM extent. Please specify a new polygon."))
})

### test if the warning messages appear when they are supposed to
test_that("warnings checks", {

  # no crs
  expect_warning(mask_spatialPoly(bgShp_path,
                                  bgShp_name,
                                  sdm,
                                  logger = NULL,
                                  spN = NULL),
                 paste0("Projection not found for shapefile. It is assumed that shapefile datum ",
                 "is WGS84. "))

  # includes NA values
  expect_warning(mask_spatialPoly(bgShp_path,
                                  bgShp_name,
                                  sdm,
                                  logger = NULL,
                                  spN = NULL),
                 paste0("Projection not found for shapefile. It is assumed that shapefile datum ",
                        "is WGS84. "),
                 paste0("The polygon selected included some cells with NA values.",
                        "You cannot change the predictions (suitable or unsuitable),",
                        "in these cells. "))

  # warning from sf pkg
  expect_warning(mask_spatialPoly(bgShp_path,
                                  bgShp_name,
                                  sdm,
                                  logger = NULL,
                                  spN = NULL),
                 paste0("Projection not found for shapefile. It is assumed that shapefile datum ",
                        "is WGS84. "),
                 paste0("The polygon selected included some cells with NA values.",
                        "You cannot change the predictions (suitable or unsuitable),",
                        "in these cells. "),
                 paste0("attribute variables are assumed to be spatially constant throughout all geometries ")
                 )

})

### test output features
test_that("output checks", {
  # spatial poly
  expect_is(spatialMask, "SpatialPolygonsDataFrame")
  # crs set to wgs84/to match sdm
  expect_equal(raster::crs(sdm), raster::crs(spatialMask))
})

### test function steps
