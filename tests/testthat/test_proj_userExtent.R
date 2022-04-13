#### COMPONENT xfer: Transfer Model
#### MODULE: Transfer to User provided area
context("xfer_userExtent")

pathShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = TRUE)
nameShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = FALSE)
xferUser <- xfer_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                            userBgBuf = 1)
###set up buffer
userBgBuf = 1
userBgBufZero = 0


modXferExt <- xfer_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                              userBgBuf = 1)
modXferExtZero <- xfer_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                                  userBgBuf = 0)
sp::proj4string(modXferExt) <- sp::CRS("+proj=longlat +datum=WGS84")
sp::proj4string(modXferExtZero) <- sp::CRS("+proj=longlat +datum=WGS84")
### test output features
test_that("output type checks", {
  # the output when userBgBuf != 0 is a SpatialPolygonsDataFrame
  expect_is(modXferExt, "SpatialPolygonsDataFrame")
  # the output when userBgBuf = 0 is a SpatialPolygonsDataFrame
  expect_is(modXferExtZero, "SpatialPolygonsDataFrame")
  # the area of background extents buffered is different from the one not
  # buffered
  expect_true(raster::area(modXferExt) > raster::area(modXferExtZero))
})
