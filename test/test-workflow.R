context("module_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'mod1', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

# move to Component 1
remDr$navigate(appURL)
# necessary for waiting for db query to load
# remDr$setImplicitWaitTimeout(milliseconds = 10000)
# find all component tabs and labels
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})

# find component 1 tab and click
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
comp1Tab$clickElement()

# find results tabs and labels in results window
resultsTabs <- remDr$findElements("css selector", ".nav a")
resultsTabLabels <- sapply(resultsTabs, function(x){x$getElementText()})
# find different particular results tabs
occsTblTab <- resultsTabs[[which(resultsTabLabels == "Occs Tbl")]]  


test_that("Component 1 Module Query Database: Check DB Query", {
  Sys.sleep(10)
  # select GBIF radio button (for some reason the default selection does not register if you don't click it first)
  field.gbif <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  initState <- field.gbif$isElementSelected()[[1]]
  field.gbif$clickElement()
  # find textInput field and click
  db.search <- comp1Tab$findChildElement(value = "//input[@type='text']")
  db.search$clickElement()
  # write text
  db.search$sendKeysToElement(list("Puma concolor"))
  # find button and click to search for occs
  db.button <- comp1Tab$findChildElement(value = "//button[@id='goDbOccs']")
  db.button$clickElement()
  Sys.sleep(10)
  
  # read current text in log box
  logContent <- remDr$findElement(using = "id", value = 'logContent')
  logText <- logContent$getElementText()
  logTextLines <- strsplit(logText[[1]], split = '\n')[[1]]
  occSearchLine <- strsplit(logTextLines[5], split = ' ')[[1]]
  # make sure Wallace found 100 records in GBIF
  nums <- occSearchLine[grep('[0-9]', occSearchLine)]
  expect_equal(as.numeric(nums[1]), 100)
  
  # click occsTbl tab
  occsTblTab$clickElement()
  # find data table
  # NOTE: finding elements (plural) does not error when none exist
  occsTbl <- occsTblTab$findChildElements(value = '//table')
  # check if the table's there
  expect_true(occsTbl[[1]]$isElementDisplayed()[[1]])
})

test_that("Component 3 Module Worldclim: Test Download", {
  # Move to Component 3
  comp3Tab <- compTabs[[which(compTabLabels == "3 Env Data")]]    
  comp3Tab$clickElement()
  
  # find and click dropdown menu for resolution
  wc.select <- comp3Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
  wc.select$clickElement()
  res10m <- comp3Tab$findChildElement(value = paste0("//div[@data-value='10' and @class='option']"))
  res10m$clickElement()
  
  #skip_on_cran()
  # find and click select button to open variable selection options
  wc.checkbox <- comp3Tab$findChildElement(value = "//input[@id='c3_wcBioclims-bcSelChoice']")
  wc.checkbox$clickElement()
  # select some variables to test the download 
  for (bio in 4:19) {
    check.vars <- wc.checkbox$findChildElement(
      value = paste0("//input[@name='c3_wcBioclims-bcSels' and @value='bio", bio, "']"))
    check.vars$clickElement()
  }
  wc.button <- comp3Tab$findChildElement(value = "//button[@id='goEnvData']")  
  wc.button$clickElement()
  Sys.sleep(10)
  
  # read current text in log box
  logContent <- remDr$findElement(using = "id", value = 'logContent')
  logText <- strsplit(logContent$getElementText()[[1]], "\n")[[1]]
  logText <- logText[length(logText)]
  expect_equal(logText, 
               "> Environmental predictors: WorldClim bioclimatic variables bio1, bio2, bio3 at 10 arcmin resolution.")
})


test_that("Component 4: Test mask and random background points", {
  
  comp4Tab <- compTabs[[which(compTabLabels == "4 Process Envs")]]  
  comp4Tab$clickElement()
  
  select.button <- comp4Tab$findChildElement(value = "//button[@id='goBgExt']")
  select.button$clickElement()
  
  process.button <- comp4Tab$findChildElement(value = "//button[@id='goBgMask']")
  process.button$clickElement()

  # Check the logs to see if everything 
  Sys.sleep(20)
  logContent <- remDr$findElement(using = "id", value = 'logContent')
  logText <- strsplit(logContent$getElementText()[[1]], "\n")[[1]]
  logText1 <- logText[length(logText) - 2]
  logText2 <- logText[length(logText) - 1]
  logText3 <- logText[length(logText)]
  expect_equal(logText1, 
               "> Study extent: bounding box. Study extent buffered by 0.5 degrees.")
  expect_equal(logText2, 
               "> Environmental data masked.")
  expect_equal(logText3, 
               "> Random background points sampled (n = 10000 : 4.71 % of cells with values).")
})

# Close the connection
remDr$close()
