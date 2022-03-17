#### COMPONENT pocss: Process Occurrence Data
#### MODULE: Remove Occurrences By ID
context("removeByID")

### Set parameters

## occurrences
occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)

## record to remove
removeID <- 11


### run function
out.ID <- poccs_removeByID(occs, removeID)


### test if the error messages appear when they are supposed to
test_that("error checks", {
   # the user has not obtained or loaded the occurrence data
  expect_error(poccs_removeByID(occs = NULL, removeID),
               'Before processing occurrences, obtain the data in component 1.')
   # the occID to remove is invalid
  expect_error(poccs_removeByID(occs, removeID = 110),
               'Entered occID not found.')
  })

### test output features
test_that("output type checks", {
   # the output is a data frame
  expect_is(out.ID, "data.frame")
  })

### test function stepts
test_that("output data checks", {
  # the original data frame has one record more than the thinned  one
  expect_equal((nrow(out.ID)), (nrow(occs))-1)
   # the ID removed corresponds to the one specified by the user (removeID)
  expect_false(11 %in% out.ID$occID)
  })
