#### COMPONENT 2: Process Occurrence Data
#### MODULE: Select Occurrences On Map
context("selectOccs")

source("test_helper_functions.R")


### Set parameters

## occurrences
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 1000)
occs <- as.data.frame(out.gbif$Panthera_onca$cleaned)

## extention to thin  
# set coordinates 
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
expertAddedPoly <- matrix(c(longitude, latitude), byrow = F, ncol = 2)


### run function 
out.map <- c2_selectOccs(occs, polySelXY = expertAddedPoly) 


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not obtained or loaded the occurrence data
  expect_error(c2_selectOccs(occs = NULL, polySelXY = expertAddedPoly),
               'Before processing occurrences, obtain the data in component 1.')
  # the user has not finished the polygon 
  expect_error(c2_selectOccs(occs, polySelXY = NULL),
               'The polygon has not been finished. Please press "Finish" on the map toolbar then the "Select Occurrences" button.')
  })

### test output features
test_that("output type checks", {
   # the output is a data frame
  expect_is(out.map, "data.frame")
  })

### test function stepts
test_that("output data checks", {
  ## test if the points out of the extention to thin were removed
  # extract longitude and latitude columns from 'occs' data frame 
  points <- occs[,c(3,4)] 
  sp::coordinates(points) <- ~ longitude + latitude
  # create polygon 
  Poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(expertAddedPoly)), ID=1))) 
  # check which points overlap with the extention to thin 
  overlap <- sp::over(points, Poly)
  # if at least one record was out of the extention to thin 
  if ((NA %in% overlap) == TRUE){
    # the original data frame has more records than the thinned one
    expect_true(nrow(occs) > nrow(out.map))
  } else { # if not, 
    # the original data frame has the same amount of records than the "thinned" one
    expect_equal(nrow(occs), nrow(out.map))
  }
  })
