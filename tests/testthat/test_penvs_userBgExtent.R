#### COMPONENT 4: Process Environmental Data
#### MODULE: user-specified
context("userBgExtent")

source("test_helper_functions.R")


### Set parameters
##ocurrences to test function for Colombia using "espeletia argentea" endemic to Colombia (provided shapefile)
occs <-  occs_queryDb(spName = "espeletia argentea", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)

## occurrences to test outside of user polygon (this species is distributed in central/sout america)
occs_out <-  occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs_out <- as.data.frame(occs_out[[1]]$cleaned)
## path to files
Path <- list.files(path='./shapefile', pattern = "COL_adm0.", full.names = TRUE)

## files name
Name <- list.files(path='./shapefile', pattern = "COL_adm0.", full.names = FALSE)

### generate wrong parameters (to test error messages)
Name.s <- list.files(path='./shapefile', pattern = ".s", full.names = FALSE)
Name.p <- list.files(path='./shapefile', pattern = ".prj", full.names = FALSE)


### run function
## Buffer == 0.5
userBgbf <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.5,occs=occs)
## Buffer == 0
userBg <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.01,occs=occs)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not loaded the environmental data
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.s, userBgBuf = 0.5,occs=occs),'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.',fixed=T)
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.p, userBgBuf = 0.5, occs=occs ),'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).',fixed=T)
  #The user background does not include all occurrences
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.5, occs=occs_out ),'The polygon did not include all localities(**). You can remove localities in Process Occs component',fixed=T)
})

### test output features
test_that("output type checks", {
  # the output when userBgBuf != 0 is a SpatialPolygons
  expect_is(userBgbf, "SpatialPolygons")
  # the output when userBgBuf = 0 is a SpatialPolygonsDataFrame
expect_is(userBg, "SpatialPolygons")
  # the area of background extents buffered is different from the one not buffered
 expect_true(raster::area(userBgbf) > raster::area(userBg))
})
