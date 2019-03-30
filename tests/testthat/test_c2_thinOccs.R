#### COMPONENT 2: Process Occurrence Data
#### MODULE: Spatial Thin
context("thinOccs")

source("test_helper_functions.R")


### Set parameters

## occurrences
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$Panthera_onca$cleaned)

## thinning distance (km)
thinDist <- 30


### run function 
out.thin <- c2_thinOccs(occs, thinDist) 


### test if the error messages appear when they are supposed to 
test_that("error checks", {
   # the user has not obtained or loaded the occurrence data
  expect_error(c2_thinOccs(occs = NULL, thinDist),
               'Before processing occurrences, obtain the data in component 1.')
   # the user has inputted a negative value
  expect_error(c2_thinOccs(occs, thinDist = -20),
               'Assign positive distance to thinning parameter.')
  })

### test output features
test_that("output type checks", {
   # the output is a data frame
  expect_is(out.thin, "data.frame")
  })

### test function stepts
test_that("output data checks", {
   # the original data frame has more records than the thinned  one
  expect_true((nrow(occs)) > (nrow(out.thin)))
  })
