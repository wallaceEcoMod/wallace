##### QUESTIONS
  # 1. message: 'Too few localities (<2) to create a background polygon.' Exp.2
  # point buffers option desn't print any message 


#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgExtent - Step 1")

source("test_helper_functions.R")


### get data

## occurrence
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs$cleaned)

foccs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 1)
foccs <- as.data.frame(foccs$cleaned)

## Enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))


### run function and set coordinates reference system
## Bounding Box 
bgExt1 <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 
raster::crs(bgExt1) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## Point Buffers 
bgExt2 <- c4_bgExtent(occs, envs, bgSel = 'ptbuf', bgBuf = 0.5) 
raster::crs(bgExt2) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
## Minimum Convex Polygon 
bgExt3 <- c4_bgExtent(occs, envs, bgSel = 'mcp', bgBuf = 0.5) 
raster::crs(bgExt3) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not loaded the environmental data  
  expect_error(c4_bgExtent(occs, envs = NULL , bgSel = 'bb', bgBuf = 0.5),'Before defining the background extent, 
                      obtain environmental data in component 3.')
  # the user has not loaded or downloaded the ocurence data  
  expect_error(c4_bgExtent(foccs, envs, bgSel = 'bb', bgBuf = 0.5))
  # buffer == 0 while using Point Buffers 
  expect_error(c4_bgExtent(occs, envs , bgSel = 'ptbuf', bgBuf = 0),'Change buffer distance to positive
                             or negative value.')
})

### test output features
test_that("output type checks", {
  # the output is a SpatialPolygonsDataFrame
  expect_is(bgExt1, "SpatialPolygonsDataFrame")
  expect_is(bgExt2, "SpatialPolygonsDataFrame")
  expect_is(bgExt3, "SpatialPolygonsDataFrame")
  # the area of each type of background extent is different
  expect_false(raster::area(bgExt1) == raster::area(bgExt2))
  expect_false(raster::area(bgExt1) == raster::area(bgExt3))
  expect_false(raster::area(bgExt2) == raster::area(bgExt3))
})
