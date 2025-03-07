#### COMPONENT xfer: Transfer Model
#### MODULE: Transfer to User provided area
context("xfer_userEnvs")

## extent to transfer
# set coordinates
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
               -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
                                                 ID = 1)))
# build model
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",package = "wallace"))
bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
                           package = "wallace"))
envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = FALSE))
partblock <- part_partitionOccs(occs, bg, method = 'block')
m <- model_maxent(occs, bg, user.grp = partblock,
                  bgMsk = envs, rms = c(1:2), rmsStep = 1, fcs = c('L', 'LQ'),
                  clampSel = TRUE, algMaxent = "maxnet",
                  parallel = FALSE)
envsFut <- list.files(path = system.file('extdata/wc/future',
                                         package = "wallace"),
                      full.names = TRUE)
envsFut <- raster::stack(envsFut)
### run function
modXfer <- xfer_userEnvs(evalOut = m, curModel = 1, envs = envsFut,
                         outputType = "cloglog", alg = "maxnet",
                         clamp = FALSE, xfExt = polyExt)

### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(modXfer, "list")
  # the output list has five elements
  expect_equal(length(modXfer), 2)
  # element within the output list are:
  # a rasterBrick
  expect_is(modXfer$xferExt, "RasterBrick")
  # a rasterLayer
  expect_is(modXfer$xferUser, "RasterLayer")
  # there are as many extent of transfers as environmental variables used
  expect_equal(raster::nlayers(envsFut), raster::nlayers(modXfer$xferExt))
  # there is 1 area of transfer
  expect_equal(raster::nlayers(modXfer$xferUser), 1)
})
