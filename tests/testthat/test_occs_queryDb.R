#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: Query Database (Present)
context("queryDb")

source("test_helper_functions.R")


### Set parameters
## species names
spNames <- c("panthera onca")
## database
occDb <- "gbif"
## number of occurrence records to download
occNum <- 1000


### run function
out.gbif <- occs_queryDb(spNames, occDb, occNum)

### get the total number of records found in the database
    ##this still needs to be modified for multisp
taxonkey <- rgbif::name_suggest(q='"panthera onca"', rank='species')$key[1]
total_occ <- rgbif::occ_search(taxonkey, limit=0, return='meta')


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user doesn't input any species name
  expect_error(occs_queryDb( occDb, occNum),
               'Please input both genus and species names.')
  # the user inputs just one name (genus or epithet)
  expect_error(occs_queryDb(spNames = "panthera", occDb, occNum),
               'Please input both genus and species names.')
  # the species' name has spelling errors, or it is not found in the database
  expect_error(occs_queryDb(spNames = "Panthera onc", occDb, occNum),
               paste0('No records found for ', em("Panthera onc"),'. Please check the spelling.'))
  })

### test if the warning messages appear when they are supposed to
test_that("warnings checks", {
  # the species is found in the database, but it does not have coordinates (Log & lat)
  expect_warning(occs_queryDb(spName = "Artibeus macleayii", occDb, occNum),
               paste0('No records with coordinates found in ', "gbif", " for ", em("Artibeus macleayii"), "."))
  })

###For multisp this goes in a loop

for (i in 1:length(spNames)){

### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(out.gbif, "list")
  #the list has as many elements as species names provided
  expect_equal(length(out.gbif), length(spNames))
  # each individual species result is a list
  expect_is(out.gbif[[i]], "list")
  #Each individual list has two elements
  expect_equal(length(out.gbif[[i]]), 2)
  # the elements on the main list are lists too
  expect_is(out.gbif[[i]][c("orig","cleaned")], "list")
  # if the database holds more records than the specified by the user (occNum),
      ##careful here because total_occ is being computed for only 1st species of list
  # the number of records downloaded is:
  if (total_occ$count >= occNum){
    # the same as specified in the function (occNum)
    expect_equal(occNum, nrow(out.gbif[[i]]$orig))
  } else { # if not
    # fewer as when the database has fewer records than specified by the user
    expect_true(nrow(out.gbif[[i]]$orig) < occNum)
  }
  # cleaned list has 14 columns
  expect_equal(14, ncol(out.gbif[[i]]$cleaned))
  })

### test function stepts
test_that("output data checks", {
  # if the original database has records without coordinates OR duplicates:
  if ((TRUE %in% duplicated(out.gbif[[i]]$orig[,c('longitude','latitude')]) == TRUE)|
      (NA %in% out.gbif$orig[,c('longitude','latitude')]) == TRUE){
    # the cleaned table must have fewer records than the original one
    expect_true((nrow(out.gbif[[i]]$orig)) > (nrow(out.gbif[[i]]$cleaned)))
  } else { # if not,
    # both tables should have the same number of records
    expect_true((nrow(out.gbif[[i]]$orig)) == (nrow(out.gbif[[i]]$cleaned)))
  }
  # there are not "NA" values in longitude OR latitude columns in the cleaned table
  expect_false(NA %in% out.gbif[[i]]$cleaned$latitude)|(NA %in% out.gbif[[i]]$cleaned$longitude)
  # there are not duplicate values in longitude AND latitude columns in the cleaned table
  expect_false(TRUE %in% duplicated(out.gbif[[i]]$cleaned[,c('longitude','latitude')]))
  })

### check header names

## GBIF
# original GBIF headers
headersGBIF <- c("name", "longitude", "latitude", "country", "stateProvince", "locality",
                 "year", "basisOfRecord", "catalogNumber", "institutionCode", "elevation",
                 "coordinateUncertaintyInMeters")
# check headers
test_that("GBIF headers", {
  # the original headers haven't changed
  expect_false('FALSE' %in%  (headersGBIF %in% names(out.gbif[[i]]$orig)))
  # the headers in the claned table are the ones specified in the function
  expect_equal(names(out.gbif[[i]]$cleaned),
               c("occID", "scientific_name", "longitude", "latitude", "country", "state_province",
                 "locality", "year", "record_type","catalog_number", "institution_code",
                 "elevation", "uncertainty", "pop"))
  })
}
####Starting here I have not checked

## VERTNET
# download data from Vertnet
out.vert <- occs_queryDb(spNames = "panthera onca", occDb = "vertnet", occNum)
# original Vertnet headers
headersVertnet <- c("name", "longitude", "latitude", "country", "stateprovince", "locality",
                    "year", "basisofrecord", "catalognumber", "institutioncode",
                    "maximumelevationinmeters", "coordinateuncertaintyinmeters")
# check headers
test_that("Vertnet headers", {
  # the original headers haven't changed
  expect_false('FALSE' %in%  (headersVertnet %in% names(out.vert$orig)))
  # the headers in the claned table are the ones specified in the function
  expect_equal(names(out.vert$cleaned),
               c("occID", "scientific_name", "longitude", "latitude", "country", "state_province",
                 "locality", "year", "record_type","catalog_number", "institution_code",
                 "elevation", "uncertainty", "pop"))
  })

## BISON
# download data from Bison
out.bison <- occs_queryDb(spName = "panthera onca", occDb = "bison",occNum)
# original Bison headers
headersBison <- c("providedScientificName", "longitude", "latitude", "countryCode",
                  "stateProvince", "verbatimLocality", "year", "basisOfRecord", "catalogNumber",
                  "ownerInstitutionCollectionCode", "verbatimElevation")
# check headers
test_that("Bison headers", {
  # the original headers haven't changed
  expect_false('FALSE' %in%  (headersBison %in% names(out.bison[[1]]$orig)))
  # the headers in the claned table are the ones specified in the function
  expect_equal(names(out.bison[[1]]$cleaned),
               c("occID", "scientific_name", "longitude", "latitude", "country", "state_province",
                 "locality", "year", "record_type","catalog_number", "institution_code",
                 "elevation", "uncertainty", "pop"))
  })
