library(testthat)
library(wallace)
#some tests need data so they must be run from the right testthat folder
setwd("tests/testthat/")
# To run all tests from tesst folder
test_dir(".")

##this inside an R CMD can't get it to run yet
test_check("wallace")
