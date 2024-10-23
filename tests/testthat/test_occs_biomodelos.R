# #### COMPONENT occs: Obtain Occurrence Data
# #### MODULE: Query BioModelos Database
# context("biomodelos")
#
# ### Set parameters
# spN <- "Bassaricyon neblina"
# bioKey <- 12345
#
# ### Run function
# occs <- occs_biomodelos(spN, bioKey, logger = NULL)
#
# ### for error tests
# spN_error <- c("Bassaricyon neblina", "Bassaricyon alleni")
# spN_error2 <- "Neblina bassaricyon"
# spN_error3 <- "species"
#
# ################ Tests ####################################
#
# ### test if the error messages appear when they are supposed to
# test_that("error checks", {
#   # More than one species entered
#   expect_error(occs_biomodelos(spN_error, bioKey, logger = NULL),
#                'Please input both genus and species names of ONE species. ')
#   # trycatch error
#     #"Unable to retrieve data from BioModelos. Server may be down. "
#   # Species name not found
#   # this test won't trigger the correct error msg without a working api key
#   # expect_error(occs_biomodelos(spN_error2, bioKey, logger = NULL),
#   #              'Species name not found, please check the spelling. ')
#   # No records on BioModelos
#   # this test won't trigger the correct error msg without a working api key
#   # expect_error(occs_biomodelos(spN_error3, bioKey, logger = NULL),
#   #              'Species without records on BioModelos. ')
#   # API key is not working.
#   expect_error(occs_biomodelos(spN, bioKey, logger = NULL),
#                'API key is not working.')
# })
#
# ### test if the warning messages appear when they are supposed to
# # No warning messages
#
# ### test output features
# #output will not work without functional bioKey (BioModelos API key)
# # test_that("output checks", {
# #   # numeric
# #   expect_is(occs, "list")
# # })
#
# ### test function steps
