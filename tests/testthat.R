library(testthat)
library(wallace)

# To run all tests from thest folder
est_dir("~/tests/testthat/")

##this inside an R CMD can't get it to run yet
test_check("wallace")
