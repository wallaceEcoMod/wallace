#### COMPONENT 2: Process Occurrence Data
#### MODULE: Select Occurrences On Map
context("select Occs")

source("test_helper_functions.R")


### get data:

## records
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$cleaned)

## Extention to thin  
expertAddedPoly <- rgdal::readOGR('./shapefile/COL_adm0.shp')
expertAddedPoly <- expertAddedPoly@polygons[[1]]@Polygons[[1]]@coords


### run function 
out.map <- c2_selectOccs(occs = occs, polySelXY = expertAddedPoly,
                         polySelID = 1)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not obtained or loaded the occurrence data
  expect_error(c2_selectOccs(occs = NULL, polySelXY = expertAddedPoly,
                             polySelID = 1),
               'Before processing occurrences, obtain the data in component 1.')
  # the user has not finished the polygon 
  expect_error(out.map <- c2_selectOccs(occs = occs, polySelXY = NULL,
                           polySelID = 1),
               'The polygon has not been finished. Please press "Finish" on the map toolbar then the "Select Occurrences" button.')
})

### test output features
test_that("output type checks", {
   # the output is a data frame
  expect_is(out.map, "data.frame")
})

### test functions stepts
test_that("output data checks", {
   # the original data frame has more records than the thinned one
  expect_true((nrow(occs)) > (nrow(out.map)))
})
