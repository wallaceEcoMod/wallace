#### COMPONENT penvs: Process Environmental Data
#### MODULE: Draw polygon
context("drawBgExtent")

### Set parameters
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
## Draw Background Extent
# set coordinates
longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867, -142.19085,
               -45.55045, -28.56050)
latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350, 56.35954,
              54.55045, -7.11861)
# generate matrix
expertDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)

## Small polygon (to test error message)
# set coordinates
longitude2 <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149,
                -66.71319, -71.11931)
latitude2 <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391,
               6.09208, 12.74980)
# generate matrix
expertDrawPoly2 <- matrix(c(longitude2, latitude2), byrow = FALSE, ncol = 2)


### run function and set coordinates reference system
# buffer == 0.5
drawBgBf <- penvs_drawBgExtent(polyExtXY = expertDrawPoly, polyExtID = 1,
                               drawBgBuf = 0.5, occs)
raster::crs(drawBgBf) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
# buffer == 0
drawBg <- penvs_drawBgExtent(polyExtXY = expertDrawPoly, polyExtID = 1,
                             drawBgBuf = 0, occs)
raster::crs(drawBg) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the drawn polygon does not include all localities
  expect_error(
    penvs_drawBgExtent(polyExtXY = expertDrawPoly2, polyExtID = 1,
                       drawBgBuf = 0.5, occs),
    paste0("The drawn polygon did not include all localities. Remove the ",
           "polygon before drawing a new one."),
    fixed = TRUE)
})

### test output features
test_that("output type checks", {
  # the output is a SpatialPolygons
  expect_is(drawBgBf, "SpatialPolygonsDataFrame")
  expect_is(drawBg, "SpatialPolygonsDataFrame")
  # the area of background buffered is different from the one not buffered
  expect_true(raster::area(drawBgBf) > raster::area(drawBg))
  ## check if all the records are within the drawn polygon
  # extract longitude and latitude columns from 'occs' data frame
  points <- occs[, 1:2]
  sp::coordinates(points) <- ~ longitude + latitude
  # create polygon
  Poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(expertDrawPoly)),
                                                ID = 1)))
  # check which points overlap
  overlap <- sp::over(points, Poly)
  expect_false(NA %in% overlap)
})

