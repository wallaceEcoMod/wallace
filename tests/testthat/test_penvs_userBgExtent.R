##### QUESTIONS
# 1. error with message: Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).
## this still needs a test for points outside of polygon part

#### COMPONENT 4: Process Environmental Data
#### MODULE: user-specified
context("userBgExtent")

source("test_helper_functions.R")


### Set parameters
## occurrences
occs <-  occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)
## path to files
Path <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = TRUE)
## files name
Name <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = FALSE)

### generate wrong parameters (to test error messages)
Name.s <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.s", full.names = FALSE)
Name.p <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.p", full.names = FALSE)


### run function
## Buffer == 0.5
userBgbf <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.5,occs=occs)
## Buffer == 0
userBg <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0,occs=occs)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not loaded the environmental data
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.s, userBgBuf = 0.5,occs=occs),'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.',fixed=T)
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.p, userBgBuf = 0.5, occs=occs ),'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).',fixed=T)
})

### test output features
test_that("output type checks", {
  # the output when userBgBuf == 0 is a SpatialPolygons
  expect_is(userBgbf, "SpatialPolygons")
  # the output when userBgBuf != 0 is a SpatialPolygonsDataFrame
  expect_is(userBg, "SpatialPolygonsDataFrame")
  # the area of background extents buffered is different from the one not buffered
  expect_true(raster::area(userBgbf) > raster::area(userBg))
})
