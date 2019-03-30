#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgSample")

source("test_helper_functions.R")


### Set parameters

## occurrences
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs$Panthera_onca$cleaned)

## background mask
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent 
bgExt <- c4_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5) 
# background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)

## Number of background points to sample
bgPtsNum <- 1000


### run function 
bgsample <- c4_bgSample(occs, bgMask, bgPtsNum)


### test output features
test_that("output type checks", {
  # the output is a data frame
  expect_is(bgsample, "data.frame")
  # both latitude and longitude were sampled
  expect_equal(ncol(bgsample), 2)
  # the headers of columns correspond to longitude and latitude
  expect_equal(c('longitude', 'latitude'), names(bgsample))
  # the number of background pints sampled are the same as specified in the function
  expect_equal(nrow(bgsample), bgPtsNum)
  # check if all the points sampled overlap with the study region
    # set longitude and latitude
  sp::coordinates(bgsample) <- ~ longitude + latitude
    # create polygon 
  Poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    bgExt@polygons[[1]]@Polygons[[1]]@coords)),ID=1)))
  # check which points overlap 
  overlap <- sp::over(bgsample, Poly)
  expect_false(NA %in% overlap)
  })
