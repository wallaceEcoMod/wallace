##### QUESTIONS
  # 1. error with message: 'Too few localities (<2) to create a background polygon.' Expectation 2
  # 2. point buffers option doesn't print any message 
  # 3. I couldn't test the overlap with the point Buffers background extent 


#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgExtent")

source("test_helper_functions.R")


### Set parameters

## occurrences
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs$cleaned)
# database with less than 2 occurences (to test error message)
foccs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 1)
foccs <- as.data.frame(foccs$cleaned)

## enviromental variables 
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))

## background extent 
bBox <- 'bb' # bounding Box 
bPoint <- 'ptbuf' # point Buffers 
bPoly <- 'mcp' # minimum Convex Polygon 

## Study region buffer distance (degree)
bgBuf <- 0.5


### run function and set coordinates reference system
## background extent: bounding Box 
bgExt1 <- c4_bgExtent(occs, envs, bgSel = bBox, bgBuf) 
raster::crs(bgExt1) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: point Buffers 
bgExt2 <- c4_bgExtent(occs, envs, bgSel = bPoint, bgBuf)
raster::crs(bgExt2) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## background extent: minimum Convex Polygon 
bgExt3 <- c4_bgExtent(occs, envs, bgSel = bPoly, bgBuf)
raster::crs(bgExt3) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not loaded the environmental data  
  expect_error(c4_bgExtent(occs, envs = NULL , bgSel = bBox, bgBuf),'Before defining the background extent, 
                      obtain environmental data in component 3.')
  # <= 2 records with longitude and latitude   
  expect_error(c4_bgExtent(occs = foccs, envs, bgSel = bBox, bgBuf))
  # buffer == 0 while using Point Buffers as background extent 
  expect_error(c4_bgExtent(occs, envs , bgSel = bPoint, bgBuf = 0),'Change buffer distance to positive
                             or negative value.')
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
  #Poly2 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    #bgExt2@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap 
  #overlap2 <- sp::over(points, Poly2)
  #expect_false(NA %in% overlap2)
  # minimum Convex Polygon 
    # create polygon 
  Poly3 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt3@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap 
  overlap3 <- sp::over(points, Poly3)
  expect_false(NA %in% overlap3)
  })
