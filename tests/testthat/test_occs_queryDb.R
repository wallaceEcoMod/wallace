#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: Query Database (Present)
context("queryDb")

### Set parameters
## species names
spNames <- c("Panthera onca","Procyon lotor")
spNamesPlants <- c("Espeletia grandiflora", "Fuchsia boliviana")
## database
occDb <- "gbif"
## number of occurrence records to download
occNum <- 100


### run function
  #gbif
out.gbif <- try(occs_queryDb(spNames, occDb, occNum),
                silent = TRUE)
  #Vertnet: currently not supported
out.vert <- try(occs_queryDb(spNames, occDb = "vertnet", occNum),
                silent = TRUE)
# Bison
# out.bison <- occs_queryDb(spNames, occDb = "bison", occNum)
# BIEN
# out.bien <- occs_queryDb(spNames = spNamesPlants, occDb = "bien", occNum)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user doesn't input any species name
  expect_error(occs_queryDb(occDb, occNum),
               'Please input both genus and species names.')
  # the user inputs just one name (genus or epithet)
  expect_error(occs_queryDb(spNames = "panthera", occDb, occNum),
               'Please input both genus and species names.')
  # the species' name has spelling errors, or it is not found in the database
  expect_error(occs_queryDb(spNames = "Panthera onc", occDb, occNum),
               paste0(hlSpp("Panthera_onc"),
                      'No records found. Please check the spelling.'),
               fixed = TRUE)
  })

### test if the warning messages appear when they are supposed to
# test_that("warnings checks", {

  # the species is found in the database, but it does not have coordinates (Log & lat)
  # GEPB (2022-03-12): It is not working because trigger another error message
  # before: No records found. Please check the spelling.
  # expect_warning(occs_queryDb(spName = "Artibeus macleayii", occDb, occNum),
  #               paste0(hlSpp("Artibeus_macleayii"),
  #                      'No records with coordinates found in ', occDb,". "),
  #               fixed = TRUE)
  #             })

#
##Test for correct outputs in loop to test for multiple species

  ##GBIF

for (i in 1:length(spNames)) {
  # Skip if cannot download
  skip_if(class(out.gbif) == "try-error")
  ### get the total number of records found in the database
  occ <- spocc::occ(spNames[i], 'gbif', limit = 100)
  total_occ <- occ[['gbif']]$meta$found
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
  #downloaded species corresponds to queried species
  expect_match(gsub(" \\(.*\\)","",unique(out.gbif[[i]]$cleaned$scientific_name)),
               spNames[i], ignore.case = TRUE)
  # cleaned list has 14 columns
  expect_equal(14, ncol(out.gbif[[i]]$cleaned))
  # if the database holds more records than the specified by the user (occNum),
  # the number of records downloaded is:
  skip_on_cran()
  if (total_occ >= occNum) {
    # the same as specified in the function (occNum)
    expect_equal(occNum, nrow(out.gbif[[i]]$orig))
  } else { # if not
    # fewer as when the database has fewer records than specified by the user
    expect_true(nrow(out.gbif[[i]]$orig) < occNum)
  }
  })

### test function stepts
test_that("output data checks", {
  # Skip if cannot download
  skip_if(class(out.gbif) == "try-error")
  # if the original database has records without coordinates OR duplicates:
  if ((TRUE %in% duplicated(out.gbif[[i]]$orig[, c('longitude', 'latitude')])) |
      (NA %in% out.gbif[[i]]$orig[, c('longitude', 'latitude')])) {
    # the cleaned table must have fewer records than the original one
    expect_true((nrow(out.gbif[[i]]$orig)) > (nrow(out.gbif[[i]]$cleaned)))
  } else { # if not,
    # both tables should have the same number of records
    expect_true((nrow(out.gbif[[i]]$orig)) == (nrow(out.gbif[[i]]$cleaned)))
  }
  # there are not "NA" values in longitude OR latitude columns in the cleaned table
  expect_false(NA %in% out.gbif[[i]]$cleaned$latitude) |
    (NA %in% out.gbif[[i]]$cleaned$longitude)
  # there are not duplicate values in longitude AND latitude columns in the cleaned table
  expect_false(TRUE %in% duplicated(out.gbif[[i]]$cleaned[,c('longitude','latitude')]))
  })

### check header names

# original GBIF headers (removed elevation)
headersGBIF <- c("name", "longitude", "latitude", "country", "stateProvince",
                 "verbatimLocality", "year", "basisOfRecord", "catalogNumber",
                 "institutionCode", "coordinateUncertaintyInMeters")
# check headers
test_that("GBIF headers", {
  # Skip if cannot download
  skip_if(class(out.gbif) == "try-error")
  # the original headers haven't changed
  expect_false('FALSE' %in% (headersGBIF %in% names(out.gbif[[i]]$orig)))
  # the headers in the cleaned table are the ones specified in the function
  expect_equal(names(out.gbif[[i]]$cleaned),
               c("occID", "scientific_name", "longitude", "latitude", "country",
                 "state_province", "locality", "year", "record_type",
                 "catalog_number", "institution_code", "elevation",
                 "uncertainty", "pop"))
  })
}

## VERTNET: currently not supported by spocc
# original Vertnet headers
headersVertnet <- c("name", "longitude", "latitude", "country", "stateprovince",
                    "year", "basisofrecord", "catalognumber", "institutioncode",
                    "coordinateuncertaintyinmeters")

for (i in 1:length(spNames)) {
  ##Check output
  test_that("output type checks", {
    # Skip if cannot download
    skip_if(class(out.vert) == "try-error")
    # the output is a list
    expect_is(out.vert, "list")
    #the list has as many elements as species names provided
    expect_equal(length(out.vert), length(spNames))
    # each individual species result is a list
    expect_is(out.vert[[i]], "list")
    #Each individual list has two elements
    expect_equal(length(out.vert[[i]]), 2)
    # the elements on the main list are lists too
    expect_is(out.vert[[i]][c("orig","cleaned")], "list")
    #downloaded species corresponds to queried species.
    expect_match(unique(out.vert[[i]]$cleaned$scientific_name),spNames[i],ignore.case=T)

    # cleaned list has 14 columns
    expect_equal(14, ncol(out.vert[[i]]$cleaned))
  })
  ### test function stepts
  test_that("output data checks", {
    # Skip if cannot download
    skip_if(class(out.vert) == "try-error")
    # if the original database has records without coordinates OR duplicates:
    if ((TRUE %in% duplicated(out.vert[[i]]$orig[,c('longitude','latitude')]) == TRUE)|
       (NA %in% out.vert$orig[,c('longitude','latitude')]) == TRUE){
      # the cleaned table must have fewer records than the original one
      expect_true((nrow(out.vert[[i]]$orig)) > (nrow(out.vert[[i]]$cleaned)))
    } else { # if not,
      # both tables should have the same number of records
      expect_true((nrow(out.vert[[i]]$orig)) == (nrow(out.vert[[i]]$cleaned)))
    }
    # there are not "NA" values in longitude OR latitude columns in the cleaned table
    expect_false(NA %in% out.vert[[i]]$cleaned$latitude) |
      (NA %in% out.vert[[i]]$cleaned$longitude)
    # there are not duplicate values in longitude AND latitude columns in the cleaned table
    expect_false(TRUE %in% duplicated(out.vert[[i]]$cleaned[,c('longitude','latitude')]))
 })
  # check headers
  test_that("Vertnet headers", {
    # Skip if cannot download
    skip_if(class(out.vert) == "try-error")
    # the original headers haven't changed
    expect_false('FALSE' %in%  (headersVertnet %in% names(out.vert[[i]]$orig)))
    #the headers in the cleaned table are the ones specified in the function
    expect_equal(names(out.vert[[i]]$cleaned),
                c("occID", "scientific_name", "longitude", "latitude", "country",
                  "state_province", "locality", "year", "record_type",
                  "catalog_number", "institution_code", "elevation",
                  "uncertainty", "pop"))
  })
}
# ## BISON
# # original Bison headers
# headersBison <- c("providedScientificName", "longitude", "latitude", "countryCode",
#                   "stateProvince", "verbatimLocality", "year", "basisOfRecord",
#                   "catalogNumber", "ownerInstitutionCollectionCode")
# for (i in 1:2) {
#   ##Check output
#   test_that("output type checks", {
#     # the output is a list
#     expect_is(out.bison, "list")
#     #the list has as many elements as species names provided
#     expect_equal(length(out.bison), length(spNames))
#     # each individual species result is a list
#     expect_is(out.bison[[i]], "list")
#     #Each individual list has two elements
#     expect_equal(length(out.bison[[i]]), 2)
#     # the elements on the main list are lists too
#     expect_is(out.bison[[i]][c("orig","cleaned")], "list")
#     # downloaded species corresponds to queried species. Taxonomy not standarized
#     # so testing for species name not genus
#     ## works with these 2 species might fail with others if epithet changed
#     expect_match(unique(out.bison[[i]]$cleaned$scientific_name),
#                  strsplit(spNames[i]," ")[[1]][2],
#                  ignore.case = TRUE, all = TRUE)
#
#     # cleaned list has 14 columns
#     expect_equal(14, ncol(out.bison[[i]]$cleaned))
#   })
#   ### test function stepts
#   test_that("output data checks", {
#     # if the original database has records without coordinates OR duplicates:
#     if ((TRUE %in% duplicated(out.bison[[i]]$orig[,c('longitude','latitude')]) == TRUE)|
#         (NA %in% out.bison$orig[,c('longitude','latitude')]) == TRUE){
#       # the cleaned table must have fewer records than the original one
#       expect_true((nrow(out.bison[[i]]$orig)) > (nrow(out.bison[[i]]$cleaned)))
#     } else { # if not,
#       # both tables should have the same number of records
#       expect_true((nrow(out.bison[[i]]$orig)) == (nrow(out.bison[[i]]$cleaned)))
#     }
#     # there are not "NA" values in longitude OR latitude columns in the cleaned table
#     expect_false(NA %in% out.bison[[i]]$cleaned$latitude) |
#       (NA %in% out.bison[[i]]$cleaned$longitude)
#     # there are not duplicate values in longitude AND latitude columns in the cleaned table
#     expect_false(TRUE %in% duplicated(out.bison[[i]]$cleaned[,c('longitude','latitude')]))
#   })
# # check headers
# test_that("Bison headers", {
#   # the original headers haven't changed
#   expect_false('FALSE' %in%  (headersBison %in% names(out.bison[[i]]$orig)))
#   # the headers in the cleaned table are the ones specified in the function
#   expect_equal(names(out.bison[[i]]$cleaned),
#                c("occID", "scientific_name", "longitude", "latitude", "country",
#                  "state_province", "locality", "year", "record_type","catalog_number",
#                  "institution_code", "elevation", "uncertainty", "pop"))
#   })
# }
##BIEN
# original BIEN headers
headersBien <- c("scrubbed_species_binomial", "longitude", "latitude",
                 "collection_code")

# for (i in 1:length(spNamesPlants)){
#   ##Check output
#   test_that("output type checks", {
#     # the output is a list
#     expect_is(out.bien, "list")
#     #the list has as many elements as species names provided
#     expect_equal(length(out.bien), length(spNamesPlants))
#     # each individual species result is a list
#     expect_is(out.bien[[i]], "list")
#     #Each individual list has two elements
#     expect_equal(length(out.bien[[i]]), 2)
#     # the elements on the main list are lists too
#     expect_is(out.bien[[i]][c("orig","cleaned")], "list")
#     # Correct species dowloaded in each case
#     expect_match(unique(out.bien[[i]]$cleaned$scientific_name),spNamesPlants[i],
#                  ignore.case=T)
#
#     # cleaned list has 14 columns
#     expect_equal(14, ncol(out.bien[[i]]$cleaned))
#   })
#   ### test function stepts
#   test_that("output data checks", {
#     # if the original database has records without coordinates OR duplicates:
#     if ((TRUE %in% duplicated(out.bien[[i]]$orig[,c('longitude','latitude')]) == TRUE)|
#         (NA %in% out.bien$orig[,c('longitude','latitude')]) == TRUE){
#       # the cleaned table must have fewer records than the original one
#       expect_true((nrow(out.bien[[i]]$orig)) > (nrow(out.bien[[i]]$cleaned)))
#     } else { # if not,
#       # both tables should have the same number of records
#       expect_true((nrow(out.bien[[i]]$orig)) == (nrow(out.bien[[i]]$cleaned)))
#     }
#     # there are not "NA" values in longitude OR latitude columns in the cleaned table
#     expect_false(NA %in% out.bien[[i]]$cleaned$latitude) |
#       (NA %in% out.bien[[i]]$cleaned$longitude)
#     # there are not duplicate values in longitude AND latitude columns in the cleaned table
#     expect_false(TRUE %in% duplicated(out.bien[[i]]$cleaned[,c('longitude','latitude')]))
#   })
# # check headers
# test_that("Bien headers", {
#   # the original headers haven't changed
#   expect_false('FALSE' %in%  (headersBien %in% names(out.bien[[i]]$orig)))
#   # the headers in the cleaned table are the ones specified in the function
#   expect_equal(names(out.bien[[i]]$cleaned),
#                c("occID", "scientific_name", "longitude", "latitude", "country",
#           "state_province", "locality", "year", "record_type", "catalog_number",
#           "institution_code", "elevation", "uncertainty","pop"))
# })
# }


