#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: User-specified
context("userOccs")

source("test_helper_functions.R")


### Set parameters
## path to the file
csvPath <- './Data/Marmosops_sp.csv'
## file name
csvName <- 'Marmosops_sp' 


### run function
user.occs <- c1_userOccs(csvPath, csvName)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
   # user's input headers are in an invalid format
  expect_error(c1_userOccs(csvPath = './Data/Marmosops_wrong.csv', 
                           csvName = 'Marmosops_wrong'),
               'Please input CSV file with columns "scientific_name", "longitude", "latitude".')
  })

### test if the warning messages appear when they are supposed to
test_that("warnings checks", {
   # user's input does not have coordinates
  expect_warning(c1_userOccs(csvPath = './Data/Marmosops_NA.csv', 
                             csvName = 'Marmosops_NA'),
                 paste0('No records with coordinates found in ', "Marmosops_NA", "."))
  })

### test output features 
test_that("output type checks", {
   # the output is a list
  expect_is(user.occs, "list")
   # the list has two elements
  expect_equal(length(user.occs), 2)
   # the elements on the main list are lists too
  expect_is(user.occs[c("Marmosops_chucha","Marmosops_handleyi")], "list")
   # the cleaned table has the minimum number of columns that it is supposed to   
  expect_true(ncol(user.occs$Marmosops_chucha$cleaned) >= 14)
   # the 'occID' column has been created 
  expect_true("occID" %in% names(user.occs$Marmosops_chucha$cleaned))
   # the 'pop' column has been created
  expect_true("pop" %in% names(user.occs$Marmosops_chucha$cleaned))
  })

### test function stepts 
test_that("output data checks", {
  # if the user database has records without coordinates OR duplicates:
  if ((TRUE %in% duplicated(user.occs$Marmosops_chucha$orig[,c('longitude','latitude')]) ==
       TRUE) | (NA %in% user.occs$Marmosops_chucha$orig[,c('longitude','latitude')]) == TRUE){
     # the cleaned table must have fewer records than the original one
    expect_true((nrow(user.occs$Marmosops_chucha$orig)) > 
                  (nrow(user.occs$Marmosops_chucha$cleaned)))
  } else { # if not, 
     # both tables should have the same number of records 
    expect_true((nrow(user.occs$Marmosops_chucha$orig)) ==
                  (nrow(user.occs$Marmosops_chucha$cleaned)))
  }
   # there are not "NA" values in longitude OR latitude columns in the cleaned table
  expect_false(NA %in% user.occs$Marmosops_chucha$cleaned
               [,c('longitude','latitude')])
   # there are not duplicate values in longitude AND latitude columns in the cleaned table
  expect_false(TRUE %in% duplicated(user.occs$Marmosops_chucha$cleaned
                                    [,c('longitude','latitude')]))
  })
