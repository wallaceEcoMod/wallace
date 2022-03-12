#### COMPONENT proj: Project Model
#### MODULE: Project to User provided area
context("proj_userExtent")

spN="Panthera onca"
## extent to project from user provided shapefile
Path <- list.files(path = system.file("extdata/shp", package = "wallace"),
                   pattern = "COL_adm0.", full.names = TRUE)
Name <- list.files(path = system.file("extdata/shp", package = "wallace"),
                   pattern = "COL_adm0.", full.names = FALSE)
userExt<-rgdal::readOGR(Path[2])
###set up buffer
userBgBuf = 1
userBgBufZero = 0


modProjExt <- proj_userExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf,
                            logger = NULL, spN = spN)
modProjExtZero <- proj_userExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf=userBgBufZero,
                              logger = NULL, spN = spN)
sp::proj4string(modProjExt) <- sp::CRS("+proj=longlat +datum=WGS84")
sp::proj4string(modProjExtZero ) <- sp::CRS("+proj=longlat +datum=WGS84")
### test output features
test_that("output type checks", {
  # the output when userBgBuf != 0 is a SpatialPolygonsDataFrame
  expect_is(modProjExt, "SpatialPolygonsDataFrame")
  # the output when userBgBuf = 0 is a SpatialPolygonsDataFrame
  expect_is(modProjExtZero, "SpatialPolygonsDataFrame")
  # the area of background extents buffered is different from the one not buffered
  expect_true(raster::area(modProjExt) > raster::area(modProjExtZero))
})
