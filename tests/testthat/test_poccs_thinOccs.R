#### COMPONENT poccs: Process Occurrence Data
#### MODULE: Spatial Thin
context("thinOccs")


## occurrences
occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                             package = "wallace"))
occs$occID <- 1:nrow(occs)

## thinning distance (km)
thinDist <- 30


### run function
out.thin <- poccs_thinOccs(occs = occs, thinDist)


### test if the error messages appear when they are supposed to
test_that("error checks", {
   # the user has not obtained or loaded the occurrence data
  expect_error(poccs_thinOccs(occs = NULL, thinDist),
               'Before processing occurrences, obtain the data in component 1.')
   # the user has inputted a negative value
  expect_error(poccs_thinOccs(occs, thinDist = -20),
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
  #the thinned data frame has the same coulmns as the orginal dataframe
  expect_equal(names(occs),names(out.thin))
  })
