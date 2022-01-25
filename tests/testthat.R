Sys.setenv("R_TEST" = "")
library(testthat)
library(wallace)

test_check("wallace")
