library(testthat)
library(wallace)

context("Run the code")

# Before running, open run_wallace
test_dir("./test", filter = 'basic', reporter = "Tap") 

test_dir("./test", filter = 'mod1-Occ_Data', reporter = "Tap") 

# all tests (a little slow) 
# note that we probably can't have all these slow tests on cran, but we can leave them on the github version
test_dir("./test", reporter = "Tap") 

test_dir("./test", filter = 'c6-Model', reporter = "Tap") 

