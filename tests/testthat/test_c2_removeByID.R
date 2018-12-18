#### COMPONENT 2: Process Occurrence Data
#### MODULE: Remove Occurrences By ID
context("remove ByID")

source("test_helper_functions.R")


### get records
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$cleaned)


### run function 
out.ID <- c2_removeByID(occs = occs, removeID = 81)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
   # the user has not obtained or loaded the occurrence data
  expect_error(c2_removeByID(occs = NULL, removeID = 2),
               'Before processing occurrences, obtain the data in component 1.')
   # the occID to remove is invalid
  expect_error(c2_removeByID(occs = occs, removeID = 110),
               'Entered occID not found.')
  })

### test output features
test_that("output type checks", {
   # the output is a data frame
  expect_is(out.ID, "data.frame")
  })

### test functions stepts 
test_that("output data checks", {
   # the ID specified is not found in the data frame
  expect_false(816 %in% out.ID$occID)
   # the original data frame has one record more than the thinned  one
  expect_equal((nrow(out.ID)), (nrow(occs))-1)
  })


