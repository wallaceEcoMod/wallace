#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Area Metrics
context("area")

### Set parameters
# binary raster
r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",package = "wallace"))
# set wkt to wcea
wkt <- getWKT("wcea") #wcea

### Run function
areaRange <- indic_area(r, wkt, logger = NULL)

################ Tests ####################################

### test if the error messages appear when they are supposed to
# No error messages

### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # numeric
  expect_is(areaRange, "numeric")
})

### test function steps
