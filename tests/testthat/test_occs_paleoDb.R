#### COMPONENT occs:  Obtain Occurrence Data
#### MODULE: Query Database (Past)
# context("paleoDb")
#
# ##Set parameters
#
#   #species name
#     spName<-"Didelphis virginiana"
#     spNameError<-"Panthera onc"
#     genus<-"panthera"
#   #maximum number of occurences to download
#       occNum<-100
#   # For PaleobioDB only Holocene is allowed.
#     timeInterval<-"Holocene"
#   #run function
#     occsPaleo<- occs_paleoDb(spName, occNum, timeInterval, logger = NULL)

###Test that erorrs check
  ### test if the error messages appear when they are supposed to
  # test_that("error checks", {
  #         # the user doesn't input any species name
  #         expect_error(occs_paleoDb( occNum, occNum, timeInterval),
  #                      'Please input both genus and species names of ONE species. ',fixed=T)
  #         # the user inputs just one name (genus or epithet)
  #         expect_error(occs_paleoDb(spName = genus,occNum, timeInterval),
  #                      'Please input both genus and species names of ONE species. ',fixed=T)
  #         # the species' name has spelling errors, or it is not found in the database
  #         expect_error(occs_paleoDb(spName = spNameError, occNum, timeInterval),
  #                     paste0(alfred.hlSpp(spNameError),"No records found, please check the spelling."),fixed=T)
  #       })

 ###Test of warnings:
      ##Function has one warning for records without coordinates, this is forbidden by the database
      ##thus making the case impossible. If it was possible it would look like this:
 # test_that("warnings checks", {
    # the species is found in the database, but it does not have coordinates (Log & lat)
   # expect_warning(occs_paleoDb(spName = "impossible species", occNum=1, timeInterval),
    #               paste0(alfred.hlSpp(impossible species), "No records with coordinates found in paleobioDB."),fixed=T)
#  })

  ### test output features
  # test_that("output type checks", {
  #     #output is a list
  #    expect_is(occsPaleo, "list")
  #   #List has two elements
  #   expect_equal(length(occsPaleo), 2)
  # })
  #
  # test_that("output data checks", {
  #   # if the original database has records without coordinates OR duplicates:
  #   if ((TRUE %in% duplicated(occsPaleo$orig[,c('longitude','latitude')]) == TRUE)|
  #       (NA %in% occsPaleo$orig[,c('longitude','latitude')]) == TRUE){
  #     # the cleaned table must have fewer records than the original one
  #     expect_true((nrow(occsPaleo$orig)) > (nrow(occsPaleo$cleaned)))
  #   } else { # if not,
  #     # both tables should have the same number of records
  #     expect_true((nrow(occsPaleo$orig)) == (nrow(occsPaleo$cleaned)))
  #   }
  #   # there are not "NA" values in longitude OR latitude columns in the cleaned table
  #   expect_false(NA %in% occsPaleo$cleaned$latitude)|(NA %in% occsPaleo$cleaned$longitude)
  #   # there are not duplicate values in longitude AND latitude columns in the cleaned table
  #   expect_false(TRUE %in% duplicated(occsPaleo$cleaned[,c('longitude','latitude')]))
  #   # downloaded species corresponds to queried species. T
  #   expect_match(unique(occsPaleo$cleaned$scientific_name),spName,ignore.case=T,all=T)
  #
  # })
  #
  # ##Check headers for both original and cleaned tables
  #
  # keyPaleoHeaders<- #c("scientific_name", "longitude", "latitude", "early_interval",
  #                    # "late_interval", "country", "collection_no", "record_type",
  #                     #"early_age", "late_age", "occID")
  # c("occID"  ,         "scientific_name", "longitude"    ,   "latitude"      ,  "early_interval" ,
  # "late_interval" ,
  # "country"    ,     "collection_no"  , "record_type"    , "early_age"  ,
  #  "late_age"    )
  #
  # test_that("headers check",{
  #
  #   expect_false('FALSE' %in%  (keyPaleoHeaders %in% names(occsPaleo$orig)))
  #   # the headers in the claned table are the ones specified in the function
  #   expect_equal(names(occsPaleo$cleaned),c(keyPaleoHeaders,"pop"))
  # })


