#### COMPONENT proj: Project Model
#### MODULE: Project to User provided area
context("proj_userEnvs")

## extent to project
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
envsFut <- list.files(path = system.file('extdata/wc/future',
                                         package = "wallace"),
                      full.names = TRUE)
envsFut <- raster::stack(envsFut)
### run function
modProj <- proj_userEnvs(evalOut = m, curModel = 1, envs = envsFut,
                         outputType = "cloglog", alg = "maxent.jar",
                         clamp = FALSE, pjExt = polyExt)

### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(modProj, "list")
  # the output list has five elements
  expect_equal(length(modProj), 2)
  # element within the output list are:
  # a rasterBrick
  expect_is(modProj$projExt, "RasterBrick")
  # a rasterLayer
  expect_is(modProj$projUser, "RasterLayer")
  # there are as many projection extents as environmental variables used
  expect_equal(raster::nlayers(envsFut), raster::nlayers(modProj$projExt))
  # there is 1 projection area
  expect_equal(raster::nlayers(modProj$projUser), 1)
})
