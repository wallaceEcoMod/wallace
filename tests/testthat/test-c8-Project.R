# Load the package
library(RSelenium)
library(testthat)
source('test-functions.R')

context("component_8_Project")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c8', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp8Tab <- compTabs[[which(compTabLabels == "8 Project")]]    
comp8Tab$clickElement()

field.area <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='projArea']")
field.time <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='projTime']")
field.mess <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='mess']")

# select ids that are "not full"
path <- "//select[@class='shinyjs-resettable selectized shiny-bound-input' and @data-shinyjs-resettable-value='']"
selects.notFull <- remDr$findElements(value = path)
selects.notFull.ids <- sapply(selects.notFull, function(x) x$getElementAttribute('id'))
path <- "//div[@class='selectize-input items not-full has-options']"
drop.menus.notFull <- remDr$findElements(value = path)

# select ids that are "full"
path <- "//select[@class='shinyjs-resettable selectized shiny-bound-input' and @data-shinyjs-resettable-value!='']"
selects.full <- remDr$findElements(value = path)
selects.full.ids <- sapply(selects.full, function(x) x$getElementAttribute('id'))
path <- "//div[@class='selectize-input items full has-options has-items']"
drop.menus.full <- remDr$findElements(value = path)


# Here if the app contains the correct tabs and their respective names.
test_that("Component 8: Radio Buttons", {  
  clickButton(field.time)
  clickButton(field.mess)
  clickButton(field.area)
}) 

test_that("Component 8 Module Project to New Extent: Buttons", {  
  button <- remDr$findElement('id', "goProjectArea")
  expect_true(button$isElementDisplayed()[[1]])
  button <- remDr$findElement('id', "goResetProj")
  expect_true(button$isElementDisplayed()[[1]])
  button <- remDr$findElement('id', "dlProj")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 8 Module Project to New Extent: Selects", { 
  # will need to fix this later, but selecting by index bc cannot
  # isolate the right list of ids that matches the dropboxes.
  # the dropbox object does not have any unique signifier besides
  # full / not full.
  select.thr <- drop.menus.full[[7]]
  select.thr$clickElement()
  dataValues <- list("'mtp'", "'p10'")
  selectDropdownItem(comp8Tab, "'noThresh'", dataValues)
  
  select.dl <- drop.menus.full[[9]]
  select.dl$clickElement()
  dataValues <- list("'ascii'","'GTiff'","'png'")
  selectDropdownItem(comp8Tab, "'raster'", dataValues)
})

test_that("Component 8 Module Project to New Time: Buttons", {  
  field.time$clickElement()
  button <- remDr$findElement('id', "goProjectTime")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 8 Module Project to New Time: Selects", {  
  # NOT CLEAR HOW TO SELECT OTHER THRESHOLD SELECTIZE OBJECTS
  # LIKELY BC WE GENERATE MULTIPLE ONES, BUT WHEN USING getElementAttribute(),
  # NOTHING IS UNIQUE BETWEEN THEM, SO IMPOSSIBLE TO SELECT WHICH
  
  path <- "//input[@placeholder='Select period']"
  select.time <- remDr$findElement(value = path)
  select.time$clickElement()
  selectDropdownOption(comp8Tab, "'70'")
  path <- "//div[@data-value='70']"
  select.time <- remDr$findElement(value = path)
  select.time$clickElement()
  selectDropdownOption(comp8Tab, "'50'")
  
  path <- "//input[@placeholder='Select GCM']"
  select.gcm <- remDr$findElement(value = path)
  select.gcm$clickElement()
  gcms <- c("'BC'","'AC'","'CC'","'CE'","'CN'","'GF'","'GD'","'GS'","'HD'","'HG'",
            "'HE'","'IN'","'IP'","'MI'","'MR'","'MC'","'MP'","'MG'","'NO'")
  for (i in gcms) {
    selectDropdownOption(comp8Tab, i)
    path <- paste0("//div[@data-value=", i, "]")
    select.gcm <- remDr$findElement(value = path)
    select.gcm$clickElement()
  }
  select.gcm$clickElement()
  
  path <- "//input[@placeholder='Select RCP']"
  select.rcp <- remDr$findElement(value = path)
  select.rcp$clickElement()
  rcps <- c("'45'", "'26'", "'60'", "'85'")
  for (i in rcps) {
    selectDropdownOption(comp8Tab, i)
    path <- paste0("//div[@data-value=", i, "]")
    select.rcp <- remDr$findElement(value = path)
    select.rcp$clickElement()
  }
  select.rcp$clickElement()
})

test_that("Component 8 Calculate Environmental Similarity: Buttons", {
  field.mess$clickElement()
  button <- remDr$findElement('id', "goEnvSimilarity")
  expect_true(button$isElementDisplayed()[[1]])
})
  
# Close the connection
remDr$close()
