
context("component_3_Env_Data")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

### Making the component 1 ###
# move to Component 1
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 100000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
####################################

# Move to Component 3
comp3Tab <- compTabs[[which(compTabLabels == "3 Env Data")]]    
comp3Tab$clickElement()


test_that("Component 3 Module Worldclim Buttons Click", {  
  #skip_on_cran()
  # Initial state is WorldClim Bioclims
  field.wcbc <- comp3Tab$findChildElement(value = "//input[@type='radio' and @value='wcbc']")
  initState <- field.wcbc$isElementSelected()[[1]]
  field.wcbc$clickElement()
  changeState <- field.wcbc$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_true(initState == changeState)  
  
  # Click User-specified
  field.user <- comp3Tab$findChildElement(value = "//input[@type='radio' and @value='user' and @name='envDataSel']")
  initState <- field.user$isElementSelected()[[1]]
  field.user$clickElement()
  changeState <- field.user$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  # Return
  field.wcbc$clickElement()
  
}) 


test_that("Component 3 Module Worldclim select resolution", {  
  #skip_on_cran()
  drop.menu <- comp3Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
  
  # Test if we can select all resolutions 
  select_resol <- function(res = "'0.5'", delete = TRUE) {
    drop.menu$clickElement()
    if (res == "'0.5'") {
      select.res.05 <- comp3Tab$findChildElement(value = paste0("//div[@data-value=",
                                                                res, "and @class='option active']"))
    } else {
      select.res.05 <- comp3Tab$findChildElement(value = paste0("//div[@data-value=",
                                                                res, "and @class='option']"))
    }
    initState <- select.res.05$isElementSelected()[[1]]
    expect_false(initState)
    select.res.05$clickElement()
    selected.item <- comp3Tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", res,  "]"))
    expect_true(length(selected.item) == 1)
    if (delete) {
      ## Delete the content
      drop.menu$sendKeysToElement(list(key = "backspace"))
      drop.menu$clickElement()
    }
    NULL
  }
  # Select a bioclimatic variable resolution of 30
  select_resol(res = "'0.5'")
  select_resol("'2.5'")
  select_resol("'5'")
  select_resol("'10'", delete = FALSE) # Leave the 10min selected for the next tests
})

test_that("Component 3 Module Worldclim specify variables", {  
  #skip_on_cran()
  # Check the box
  check.spec <- comp3Tab$findChildElement(value = "//input[@id='c3_wcBioclims-bcSelChoice']")
  expect_false(check.spec$isElementSelected()[[1]])
  check.spec$clickElement()
  expect_true(check.spec$isElementSelected()[[1]])
  
  # Select variables
  for (bio in 1:19) {
  check.vars <- check.spec$findChildElement(
    value = paste0("//input[@name='c3_wcBioclims-bcSels' and @value='bio", bio, "']"))
  expect_true(check.vars$isElementSelected()[[1]])
  check.vars$clickElement()
  expect_false(check.vars$isElementSelected()[[1]])
  }
})

# Close the connection
remDr$close()
