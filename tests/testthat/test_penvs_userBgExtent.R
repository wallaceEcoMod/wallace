##### QUESTIONS

# 1. error with message: Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).
        ###From Andres: this is fixed

##3- There is still a  problem with the function given occs with CRS to fix
## 2-this still needs a test for points outside of polygon part


if (ptRem == 0) {
  if (userBgBuf > 0) {
    logger %>% writeLog('Study extent user-defined polygon buffered by ',
                        userBgBuf, ' degrees.')
  } else {
    logger %>% writeLog("Study extent: user-defined polygon.")
  }
  return(bgExt)
} else if (ptRem > 0) {
  logger %>%
    writeLog(type = 'error',
             paste0("The polygon did not include all localities(**). ",
                    "You can remove localities in Process Occs component"))
  return()
}
#### COMPONENT 4: Process Environmental Data
#### MODULE: user-specified
context("userBgExtent")

source("test_helper_functions.R")


### Set parameters
## occurrences
occs <-  occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)
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
userBg <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0,occs=occs)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not loaded the environmental data
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.s, userBgBuf = 0.5,occs=occs),'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.',fixed=T)
  expect_error(penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name.p, userBgBuf = 0.5, occs=occs ),'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).',fixed=T)
  #The user background does not include all ccurrences
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
