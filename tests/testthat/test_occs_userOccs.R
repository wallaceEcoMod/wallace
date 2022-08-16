#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: User-specified
context("userOccs")

### Set parameters
## path to the file
txtPath <- system.file("extdata/Marmosops_sp.csv", package = "wallace")
txtName <- 'Marmosops_sp'

### run function
user.occs <- occs_userOccs(txtPath, txtName,
                           txtSep = ",", txtDec = ".")

### test if the error messages appear when they are supposed to
test_that("error checks", {
   # user's input headers are in an invalid format
  expect_error(
    occs_userOccs(txtPath = './extdata/Marmosops_wrong.csv',
                  txtName = 'Marmosops_wrong',
                  txtSep = ",", txtDec = "."),
    paste0('Please input a file with columns "scientific_name", ',
           '"longitude", "latitude".'),
    fixed = TRUE)
  expect_error(
    occs_userOccs(txtPath = './extdata/Marmosops_wrongSP.csv',
                  txtName = 'Marmosops_wrongSP',
                  txtSep = ",", txtDec = "."),
    paste0('Please input just genus and species epithet in scientific name ',
           'field in your file (e.g., "Canis lupus").'),
    fixed = TRUE)
  expect_error(
    occs_userOccs(txtPath = './extdata/Marmosops_sp.csv',
                  txtName = 'Marmosops_sp',
                  txtSep = " ", txtDec = "."),
    paste0('There is something wrong in your file. Check file format or ',
           'delimiter and decimal separators.'),
    fixed = TRUE)

  expect_error(
    occs_userOccs(txtPath = './extdata/cerdocyon-thous-2.csv',
                  txtName = 'cerdocyon-thous-2',
                  txtSep = ",", txtDec = "."),
    'Please input txt file. Not all values in longitude or latitude are numeric.',
    fixed = TRUE)
 })

### test if the warning messages appear when they are supposed to
test_that("warnings checks", {
   # user's input does not have coordinates
  expect_warning(
    occs_userOccs(txtPath = './extdata/Marmosops_NA.csv',
                  txtName = 'Marmosops_NA',
                  txtSep = ",", txtDec = "."),
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

