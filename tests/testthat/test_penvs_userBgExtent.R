#### COMPONENT penvs: Process Environmental Data
#### MODULE: user-specified
context("userBgExtent")

### Set parameters
occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
## occurrences to test outside of user polygon (this species is distributed in
## central/sout america)
occs_out <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                             package = "wallace"))[, 2:3]
occs_out$occID <- 1:nrow(occs_out)
## path to files
Path <- list.files(path = system.file("extdata/shp", package = "wallace"),
                   full.names = TRUE)

## files name
Name <- list.files(path = system.file("extdata/shp", package = "wallace"),
                   full.names = FALSE)

### generate wrong parameters (to test error messages)
Name.p <- "Bassaricyon_neblina.prj"


### run function
## Buffer == 0.5
userBgbf <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name,
                               userBgBuf = 0.5, occs = occs)
## Buffer == 0
userBg <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name,
                             userBgBuf = 0, occs = occs)
sp::proj4string(userBgbf) <- sp::CRS("+proj=longlat +datum=WGS84")
sp::proj4string(userBg) <- sp::CRS("+proj=longlat +datum=WGS84")

### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not loaded the environmental data
  expect_error(
    penvs_userBgExtent(bgShp_path = Path[1:2],
                       bgShp_name = Name[1:2],
                       userBgBuf = 0.5,
                       occs = occs),
    'If entering a shapefile, please select all the following files: .shp, .shx, .dbf.',
    fixed = TRUE)
  expect_error(
    penvs_userBgExtent(bgShp_path = Path,
                       bgShp_name = Name.p,
                       userBgBuf = 0.5, occs = occs),
    'Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf).',
    fixed = TRUE)
  #The user background does not include all occurrences
  expect_error(
    penvs_userBgExtent(bgShp_path = Path,
                       bgShp_name = Name,
                       userBgBuf = 0.5,
                       occs = occs_out),
    'The polygon did not include all localities. You can remove localities in Process Occs component',
    fixed = TRUE)
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
