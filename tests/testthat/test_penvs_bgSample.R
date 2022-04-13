#### COMPONENT penvs: Process Environmental Data
#### MODULE: Select Study Region
context("bgSample")

### Set parameters
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                             package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
# environmental data
envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                                       package = "wallace"),
                                           pattern = ".tif$", full.names = FALSE))
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt)

## Number of background points to sample
bgPtsNum <- 100
bgPtsNum_big <- 19525

### run function
bgsample <- penvs_bgSample(occs, bgMask, bgPtsNum)
bgsample_big <- penvs_bgSample(occs, bgMask, bgPtsNum_big)

### test output features
test_that("output type checks", {
  # the output is a data frame
  expect_is(bgsample, "data.frame")
  # both latitude and longitude were sampled
  expect_equal(ncol(bgsample), 2)
  # the headers of columns correspond to longitude and latitude
  expect_equal(c('longitude', 'latitude'), names(bgsample))
  # the number of background points sampled are the same as specified in the
  #  function
  expect_equal(nrow(bgsample), bgPtsNum)
  # the number of background points sampled are the the max possible if
  # bgPtsNum is bigger than available
  expect_equal(
    nrow(bgsample_big),
    (raster::ncell(bgMask) - raster::freq(bgMask, value = NA)[[1]]))

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
