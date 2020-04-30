#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: Query Database (Past)
context("paleoDb")

source("test_helper_functions.R")

##Set parameters

  #species name
    spName<-"Didelphis virginiana"
  #maximum number of occurences to download
      occNum<-100
  # For PaleobioDB only Holocene is allowed.
    timeInterval<-'Holo'
  #run function
    occsPaleo<- occs_paleoDb(spName, occNum, timeInterval, logger = NULL)

###Test that erorrs check
  ### test if the error messages appear when they are supposed to
  test_that("error checks", {
          # the user doesn't input any species name
          expect_error(occs_paleoDb( occNum, occNum, timeInterval),
                       'Please input both genus and species names of ONE species. (**)',fixed=T)
          # the user inputs just one name (genus or epithet)
          expect_error(occs_paleoDb(spName = "panthera",occNum, timeInterval),
                       'Please input both genus and species names of ONE species. (**)',fixed=T)
          # the species' name has spelling errors, or it is not found in the database
          expect_error(occs_paleoDb(spName = "Panthera onc", occNum, timeInterval),
                      paste0('No records found for ', em("Panthera onc"), ". Please check the spelling."),fixed=T)
        })

