##### QUESTIONS
# 1. error with message: 'Too few localities (<2) to create a background polygon.' Expectation 2
# 2. I couldn't test the overlap with the point Buffers background extent. From Andrea: I fixed this, problem was you were selecting a single point buffer

####The errors to expect are:
#'Too few localities (<2) to create a background polygon.'
#'Change buffer distance to positive or negative value.'

#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region
context("bgExtent")

source("test_helper_functions.R")


### Set parameters

## occurrences
occs <-  occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)
# database with less than 2 occurences (to test error message)
foccs <-  occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 1)
foccs <- as.data.frame(foccs[[1]]$cleaned)


## background extent
bBox <- 'bounding box' # bounding Box
bPoint <- 'point buffers' # point Buffers
bPoly <- 'minimum convex polygon' # minimum Convex Polygon

## Study region buffer distance (degree)
bgBuf <- 0.5
#Specify occurrence table
spN<-occs
### run function and set coordinates reference system
## background extent: bounding Box
bgExt1 <- penvs_bgExtent(occs, bgSel = bBox, bgBuf=bgBuf,logger = NULL, spN = occs)
raster::crs(bgExt1) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: point Buffers
bgExt2 <- penvs_bgExtent(occs, bgSel = bPoint, bgBuf=bgBuf,spN=occs,logger = NULL)
raster::crs(bgExt2) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: minimum Convex Polygon
bgExt3 <- penvs_bgExtent(occs, bgSel = bPoly, bgBuf=bgBuf,spN=occs, logger=NULL)
raster::crs(bgExt3) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"


### test if the error messages appear when they are supposed to
test_that("error checks", {

  # <= 2 records with longitude and latitude. Error is the same but des not pass test.
  expect_error(penvs_bgExtent(occs = foccs, bgSel = bBox, bgBuf), 'Too few localities (<2) to create a background polygon.')

  #Expected error is

  # buffer == 0 while using Point Buffers as background extent
  expect_error(penvs_bgExtent(occs , bgSel = bPoint, bgBuf = 0),'Change buffer distance to positive or negative value.')
})

### test output features
test_that("output type checks", {
  # the output is a SpatialPolygonsDataFrame
  expect_is(bgExt1, "SpatialPolygonsDataFrame")
  expect_is(bgExt2, "SpatialPolygonsDataFrame")
  expect_is(bgExt3, "SpatialPolygonsDataFrame")
  # the area of each type of the background extents is different
  expect_false(raster::area(bgExt1) == raster::area(bgExt2))
  expect_false(raster::area(bgExt1) == raster::area(bgExt3))
  expect_false(raster::area(bgExt2) == raster::area(bgExt3))
  # check if all the records are within the study region
  # extract longitude and latitude columns from the 'occs' data frame
  points <- occs[,c(3,4)]
  sp::coordinates(points) <- ~ longitude + latitude
  # bounding Box
  # create polygon
  Poly1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt1@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap
  overlap1 <- sp::over(points, Poly1)
  expect_false(NA %in% overlap1)
  # point Buffers
  # create polygon
  Poly2<-list()
  for (i in 1:9){

   Poly2[[i]]<- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
      bgExt2@polygons[[1]]@Polygons[[i]]@coords)),ID=1)))
     }
  Poly2 <- do.call(raster::bind,Poly2)

  # check which points overlap
  overlap2 <- sp::over(points, Poly2)
  expect_false(NA %in% overlap2)
  # minimum Convex Polygon
  # create polygon
  Poly3 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt3@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap
  overlap3 <- sp::over(points, Poly3)
  expect_false(NA %in% overlap3)
})
