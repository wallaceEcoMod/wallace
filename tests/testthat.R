library(testthat)
library(wallace)

# The tests need Wallace to be run separately on a specific port (5556) so that 
# RSelenium can find the app. Thus, automatic tests on CRAN will not work, and
# currently they must be run manually. We're working on ways to get automated tests
# running in the meantime.

# test_check("wallace")
