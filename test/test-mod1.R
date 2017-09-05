context("module_1")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:6268"
