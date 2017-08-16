context("Run the code")

library(testthat)
library(wallace)

# Before running, open run_wallace
test_dir("./test", filter = 'basic', reporter = "Tap") 
