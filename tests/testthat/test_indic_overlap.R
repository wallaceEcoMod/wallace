#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Ratio Overlap
context("overlap")

### Set parameters
# range map
rangeMap <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif", package = "wallace"))
rangeMap[rangeMap == 0] <- NA
rangeMap <- terra::as.polygons(rangeMap)
rangeMap <- sf::st_as_sf(rangeMap)

# input overlap obj
## raster
inputRaster <- raster::raster(nrows=108, ncols=108, xmn=-80, xmx=-75)
raster::values(inputRaster)<- runif(n = (108*108))

## shapefile
###BAJ testing with shapefile is commented because of a bug in cRR & test failing
ovShp_path <- list.files(path = system.file("extdata/wdpa", package = "wallace"), full.names = TRUE)
ovShp_name <- list.files(path = system.file("extdata/wdpa", package = "wallace"), full.names = FALSE)
suppressWarnings(inputPoly <- indic_inputPoly(ovShp_path, ovShp_name,
                            overlapRange = rangeMap,
                            logger = NULL, spN = NULL))
#field
# NULL if inputOverlap is a raster
fields <- names(inputPoly)
fields <- fields[!(fields %in% "geometry")]
fields <- setNames(as.list(fields), fields)
overlapF <- fields$DESIG_ENG

#category
## NULL if inputOverlap is a raster
category <- unique(inputPoly$DESIG_ENG)
category <- setNames(as.list(category), category)
overlapC <- category$`Regional Natural Parks`


### Run function
# for raster
overlap_r <- indic_overlap(rangeMap,
                         inputOverlap = inputRaster,
                         field = NULL,
                         category = NULL,
                         logger = NULL,
                         spN = NULL)


# for shapefile
suppressWarnings(overlap_shp <- indic_overlap(rangeMap,
                         inputOverlap = inputPoly,
                         field = overlapF,
                         category = overlapC,
                         logger = NULL, spN = NULL))
# warning message suppressed: attribute variables are assumed to be spatially constant throughout all geometries

################ Tests ####################################

### test if the error messages appear when they are supposed to
# No error messages

### test if the warning messages appear when they are supposed to
# No warning messages
#attribute variables are assumed to be spatially constant throughout all geometries

### test output features
test_that("output checks", {
  # list
  expect_is(overlap_r, "list")
  expect_is(overlap_shp, "list")
  # list of 2
  expect_equal(2, length(overlap_r))
  expect_equal(2, length(overlap_shp))
  # list names
  expect_equal(names(overlap_r), c("overlapPolygon", "overlapRatio"))
  expect_equal(names(overlap_shp), c("overlapPolygon", "overlapRatio"))
  # overlapPolygon
  expect_is(overlap_r$overlapPolygon, "sfc_MULTIPOLYGON")
  expect_is(overlap_shp$overlapPolygon, "sfc_MULTIPOLYGON")
  #overlap ratio
  expect_is(overlap_r$overlapRatio, "matrix")
  expect_is(overlap_shp$overlapRatio, "character")
  #length of 4 or 1
  expect_equal(4, length(overlap_r$overlapRatio))
  expect_equal(1, length(overlap_shp$overlapRatio))
})

### test function steps
