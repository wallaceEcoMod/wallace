#### COMPONENT occs:  Obtain Occurrence Data
#### MODULE: Query Database (Past)
context("paleoDb")

##Set parameters

#species name
spName <- "Didelphis virginiana"
spNameError <- "Panthera onc"
genus <- "panthera"
#maximum number of occurrences to download
occNum <- 100
# For PaleobioDB only Holocene is allowed.
timeInterval <- "Holocene"

# run function if no Windows
if (Sys.info()['sysname'] != "Windows") {
   occsPaleo <- occs_paleoDb(spName, occNum, timeInterval, logger = NULL)
}


###Test that errors check
  ### test if the error messages appear when they are supposed to
test_that("error checks", {
  # paleobioDB failing in Windows
  skip_on_os("windows")
  # the user doesn't input any species name
  expect_error(occs_paleoDb( occNum, occNum, timeInterval),
               'Please input both genus and species names of ONE species.',
               fixed = TRUE)
  # the user inputs just one name (genus or epithet)
  expect_error(occs_paleoDb(spName = genus,occNum, timeInterval),
               'Please input both genus and species names of ONE species.',
               fixed = TRUE)
  # the species' name has spelling errors, or it is not found in the database
  expect_error(occs_paleoDb(spName = spNameError, occNum, timeInterval),
               paste0(hlSpp(spNameError),
                      "No records found, please check the spelling."),
               fixed = TRUE)
})

###Test of warnings:
##Function has one warning for records without coordinates, this is forbidden by
##the database
##thus making the case impossible. If it was possible it would look like this:
# test_that("warnings checks", {
# # the species is found in the database, but it does not have coordinates (Log & lat)
# expect_warning(occs_paleoDb(spName = "impossible species", occNum = 1,
#                             timeInterval),
#                paste0(hlSpp("impossible species"),
#                       "No records with coordinates found in paleobioDB."),
#                fixed = TRUE)
# })

# test output features
test_that("output type checks", {
  # paleobioDB failing in Windows
  skip_on_os("windows")
  # skip on CRAN
  skip_on_cran()
  #output is a list
  expect_is(occsPaleo, "list")
  #List has two elements
  expect_equal(length(occsPaleo), 2)
})
test_that("output data checks", {
  # paleobioDB failing in Windows
  skip_on_os("windows")
  # skip on CRAN
  skip_on_cran()
  # if the original database has records without coordinates OR duplicates:
  if ((TRUE %in% duplicated(occsPaleo$orig[,c('longitude','latitude')]) == TRUE) |
      (NA %in% occsPaleo$orig[,c('longitude','latitude')]) == TRUE){
    # the cleaned table must have fewer records than the original one
    expect_true((nrow(occsPaleo$orig)) > (nrow(occsPaleo$cleaned)))
  } else { # if not,
    # both tables should have the same number of records
    expect_true((nrow(occsPaleo$orig)) == (nrow(occsPaleo$cleaned)))
  }
  # there are not "NA" values in longitude OR latitude columns in the cleaned table
  expect_false(NA %in% occsPaleo$cleaned$latitude) |
    (NA %in% occsPaleo$cleaned$longitude)
  # there are not duplicate values in longitude AND latitude columns in the cleaned table
  expect_false(TRUE %in% duplicated(occsPaleo$cleaned[, c('longitude','latitude')]))
  # downloaded species corresponds to queried species. T
  expect_match(unique(occsPaleo$cleaned$scientific_name),
               spName, ignore.case = TRUE, all = TRUE)
})
##Check headers for both original and cleaned tables
keyPaleoHeaders <- c("occID", "scientific_name", "longitude", "latitude",
                     # "early_interval", "late_interval",
                     "country", "collection_no", "record_type",
                     "early_age", "late_age")

test_that("headers check", {
  # paleobioDB failing in Windows
  skip_on_os("windows")
  #
  expect_false('FALSE' %in%  (keyPaleoHeaders %in% names(occsPaleo$orig)))
  # the headers in the cleaned table are the ones specified in the function
  expect_equal(names(occsPaleo$cleaned),c(keyPaleoHeaders,"pop"))
})


