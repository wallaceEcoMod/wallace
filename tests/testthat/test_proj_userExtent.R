#### COMPONENT proj: Project Model
#### MODULE: Project to User provided area
context("proj_userExtent")

pathShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = TRUE)
nameShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = FALSE)
projUser <- proj_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                            userBgBuf = 1)
###set up buffer
userBgBuf = 1
userBgBufZero = 0


modProjExt <- proj_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                              userBgBuf = 1)
modProjExtZero <- proj_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                                  userBgBuf = 0)
sp::proj4string(modProjExt) <- sp::CRS("+proj=longlat +datum=WGS84")
sp::proj4string(modProjExtZero) <- sp::CRS("+proj=longlat +datum=WGS84")
### test output features
test_that("output type checks", {
  # the output when userBgBuf != 0 is a SpatialPolygonsDataFrame
  expect_is(modProjExt, "SpatialPolygonsDataFrame")
  # the output when userBgBuf = 0 is a SpatialPolygonsDataFrame
  expect_is(modProjExtZero, "SpatialPolygonsDataFrame")
  # the area of background extents buffered is different from the one not
  # buffered
  expect_true(raster::area(modProjExt) > raster::area(modProjExtZero))
})
