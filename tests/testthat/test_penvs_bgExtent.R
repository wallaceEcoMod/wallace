#### COMPONENT penvs: Process Environmental Data
#### MODULE: Select Study Region
context("bgExtent")

### Set parameters

## occurrences
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
# database with less than 2 occurences (to test error message)
foccs <-  occs[1:2, ]


## background extent
bBox <- 'bounding box' # bounding Box
bPoint <- 'point buffers' # point Buffers
bPoly <- 'minimum convex polygon' # minimum Convex Polygon

## Study region buffer distance (degree)
bgBuf <- 0.5

### run function and set coordinates reference system
## background extent: bounding Box
bgExt1 <- penvs_bgExtent(occs, bgSel = bBox, bgBuf = bgBuf, logger = NULL)
raster::crs(bgExt1) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: point Buffers
bgExt2 <- penvs_bgExtent(occs, bgSel = bPoint, bgBuf = bgBuf, logger = NULL)
raster::crs(bgExt2) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: minimum Convex Polygon
bgExt3 <- penvs_bgExtent(occs, bgSel = bPoly, bgBuf = bgBuf, logger = NULL)
raster::crs(bgExt3) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"


### test if the error messages appear when they are supposed to
test_that("error checks", {

  # <= 2 records with longitude and latitude. Error is the same but des not
  # pass test.
  expect_error(penvs_bgExtent(occs = foccs, bgSel = bBox, bgBuf),
               'Too few localities (<2) to create a background polygon.',
               fixed=TRUE)

  #Expected error is

  # buffer == 0 while using Point Buffers as background extent
  expect_error(penvs_bgExtent(occs , bgSel = bPoint, bgBuf = 0),
               'Change buffer distance to positive or negative value.')
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
  # extract just coordinates
  occs.xy <- occs[c('longitude', 'latitude')]
  # make spatial pts object of original occs and preserve origID
  occs.sp <- sp::SpatialPointsDataFrame(occs.xy, data = occs['occID'])

  # bounding Box
  # create polygon
  Poly1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt1@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap
  overlap1 <- sp::over(occs.sp, Poly1)
  expect_false(NA %in% overlap1)
  # point Buffers
  # create polygon
  Poly2<-list()
  for (i in 1:length(bgExt2@polygons[[1]]@Polygons)){

   Poly2[[i]]<- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
      bgExt2@polygons[[1]]@Polygons[[i]]@coords)),ID=1)))
     }
  Poly2 <- do.call(raster::bind,Poly2)

  # check which points overlap
  overlap2 <- sp::over(occs.sp, Poly2)
  expect_false(NA %in% overlap2)
  # minimum Convex Polygon
  # create polygon
  Poly3 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt3@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap
  overlap3 <- sp::over(occs.sp, Poly3)
  expect_false(NA %in% overlap3)
})
