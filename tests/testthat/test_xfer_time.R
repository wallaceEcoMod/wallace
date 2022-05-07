#### COMPONENT xfer: Transfer Model
#### MODULE: Transfer to New time
context("xfer_time")

envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = FALSE))
## extent to transfer
# set coordinates
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
               -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
                                                 ID = 1)))
# load model
m <- readRDS(system.file("extdata/model.RDS",
                         package = "wallace"))
occsEnvs <- m@occs
bgEnvs <- m@bg
envsFut <- list.files(path = system.file('extdata/wc/future',
                                         package = "wallace"),
                      full.names = TRUE)
envsFut <- raster::stack(envsFut)
modXfer <- xfer_time(evalOut = m, curModel = 1,
                     envs = envsFut, alg = 'maxent.jar',
                     xfExt = polyExt, clamp = FALSE, outputType = 'cloglog')

# build model and test for both algorithms
test_that("output type checks", {
  # the output is a list
  expect_is(modXfer, "list")
  # the output list has five elements
  expect_equal(length(modXfer), 2)
  # element within the output list are:
  # a rasterBrick
  expect_is(modXfer$xferExt, "RasterBrick")
  # a rasterLayer
  expect_is(modXfer$xferTime, "RasterLayer")
  # there are as many extents of transfer as environmental variables used
  expect_equal(raster::nlayers(envs), raster::nlayers(modXfer$xferExt))
  # there is 1 area of extent
  expect_equal(raster::nlayers(modXfer$xferTime), 1)
})
