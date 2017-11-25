# Load the package
library(RSelenium)
library(testthat)

# Basic testings (you can build other scripts in the same folder and name different things,
# like: test module 1)
context("basic")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# Here we test if the app opens and matches the title to Wallace
test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "Wallace")  
})

# Here if the app contains the correct tabs and their respective names.
test_that("Tabs are present", {  
  webElems <- remDr$findElements("id", "tabs")
  appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})
  appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  expect_equal(appCtrlLabels[1], "Intro")  
  expect_equal(appCtrlLabels[2], "1 Occ Data")  
  expect_equal(appCtrlLabels[3], "2 Process Occs")  
  expect_equal(appCtrlLabels[4], "3 Env Data")  
  expect_equal(appCtrlLabels[5], "4 Process Envs")  
  expect_equal(appCtrlLabels[6], "5 Partition Occs")  
  expect_equal(appCtrlLabels[7], "6 Model")
  expect_equal(appCtrlLabels[8], "7 Visualize")
  expect_equal(appCtrlLabels[9], "8 Project")
  expect_equal(appCtrlLabels[10], "Session Code")  
})
# Close the connection
remDr$close()
