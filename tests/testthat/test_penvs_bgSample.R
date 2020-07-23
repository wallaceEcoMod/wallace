#### COMPONENT penvs: Process Environmental Data
#### MODULE: Select Study Region
context("bgSample")

source("test_helper_functions.R")


### Set parameters

## occurrences
spN<-"Panthera onca"
occs <-  occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)

## background mask
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)

## Number of background points to sample
bgPtsNum <- 100
bgPtsNum_big<-100000

### run function
bgsample <- penvs_bgSample(occs, bgMask, bgPtsNum,spN=spN)
bgsample_big <- penvs_bgSample(occs, bgMask, bgPtsNum_big,spN=spN)

### test output features
test_that("output type checks", {
  # the output is a data frame
  expect_is(bgsample, "data.frame")
  # both latitude and longitude were sampled
  expect_equal(ncol(bgsample), 2)
  # the headers of columns correspond to longitude and latitude
  expect_equal(c('longitude', 'latitude'), names(bgsample))
  # the number of background points sampled are the same as specified in the function
  expect_equal(nrow(bgsample), bgPtsNum)
  # the number of background points sampled are the the max possible if bgPtsNum is bigger than available
  expect_equal(nrow(bgsample_big),(raster::ncell(bgMask) - raster::freq(bgMask, value = NA)[[1]]))

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
