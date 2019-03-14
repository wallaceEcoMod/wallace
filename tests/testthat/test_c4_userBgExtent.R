##### QUESTIONS
  # 1. error with message: Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).


#### COMPONENT 4: Process Environmental Data
#### MODULE: user-specified
context("userBgExtent")

source("test_helper_functions.R")


### Set parameters
## path to files 
Path <- list.files(path='./shapefile/', pattern = "COL_adm0.", full.names = TRUE)
## files name
Name <- list.files(path='./shapefile/', pattern = "COL_adm0.", full.names = FALSE)

### generate wrong parameters (to test error messages)
Name.s <- list.files(path='./shapefile/', pattern = "COL_adm0.s", full.names = FALSE)
Name.p <- list.files(path='./shapefile/', pattern = "COL_adm0.p", full.names = FALSE)


### run function
## Buffer == 0.5
userBgbf <- c4_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.5) 
## Buffer == 0
userBg <- c4_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not loaded the environmental data  
  expect_error(c4_userBgExtent(bgShp_path = Path, bgShp_name = Name.s, userBgBuf = 0.5),'If entering a shapefile, please
                               select all the following files: .shp, .shx, .dbf.')
  expect_error(c4_userBgExtent(bgShp_path = Path, bgShp_name = Name.p, userBgBuf = 0.5 ))
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
