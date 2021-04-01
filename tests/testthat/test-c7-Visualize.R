# Load the package
library(RSelenium)
library(testthat)
source('test-functions.R')

context("component_7_Visualize")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c7', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp7Tab <- compTabs[[which(compTabLabels == "7 Visualize")]]    
comp7Tab$clickElement()

field.mxEval <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='mxEval']")
field.resp <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='response']")
field.map <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='map']")
field.bcPlots <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='bcPlots']")

path <- "//select[@class='shinyjs-resettable selectized shiny-bound-input']"
selects <- remDr$findElements(value = path)
selects.ids <- sapply(selects, function(x) x$getElementAttribute('id'))
path <- "//div[@class='selectize-input items full has-options has-items']"
drop.menus <- remDr$findElements(value = path)

# Here if the app contains the correct tabs and their respective names.
test_that("Component 7: Radio Buttons", {  
  clickButton(field.mxEval)
  clickButton(field.resp)
  clickButton(field.map)
  clickButton(field.bcPlots)
})

test_that("Component 7 BIOCLIM Envelope Plots: Inputs", {  
  input.bc1 <- comp7Tab$findChildElement(value = "//input[@id='c7_bcPlots-bc1']")
  input.bc2 <- comp7Tab$findChildElement(value = "//input[@id='c7_bcPlots-bc2']")
  input.bcProb <- comp7Tab$findChildElement(value = "//input[@id='c7_bcPlots-bcProb']")
  
  # Arrow test
  # vals = first is starting value, second and third are tests for arrow, fourth is test for sendKeys
  numInputTest <- function(webElem, vals) {
    expect_equal(webElem$getElementAttribute("value")[[1]], vals[1])
    webElem$sendKeysToElement(list(key ="up_arrow"))
    expect_equal(webElem$getElementAttribute("value")[[1]], vals[2])
    webElem$sendKeysToElement(list(key ="up_arrow"))
    expect_equal(webElem$getElementAttribute("value")[[1]], vals[3])
    webElem$sendKeysToElement(list(key ="down_arrow"))
    expect_equal(webElem$getElementAttribute("value")[[1]], vals[2])
    
    webElem$clearElement()
    webElem$sendKeysToElement(list(vals[4]))
    expect_equal(webElem$getElementAttribute("value")[[1]], vals[4])  
  }
  
  numInputTest(input.bc1, c("1","2","3","4"))
  numInputTest(input.bc2, c("2","3","4","5"))
  numInputTest(input.bcProb, c("0.9","0.95","1","0.8"))
})

test_that("Component 7 BIOCLIM Envelope Plots: Buttons", { 
  button <- remDr$findElement('id', "c7_bcPlots-dlBcPlot")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 7 Maxent Evaluation Plots: Select", { 
  field.mxEval$clickElement()
  # this selectInput can have an item clicked first to list the options
  dataValues <- list("'auc.diff.avg'","'or.mtp.avg'","'or.10p.avg'","'delta.AICc'")
  selectDropdownItem(comp7Tab, "'auc.val.avg'", dataValues)
})

test_that("Component 7 Maxent Evaluation Plots: Buttons", { 
  button <- remDr$findElement('id', "c7_mxEvalPlots-dlMxEvalPlot")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 7 Plot Response Curves: Buttons", { 
  field.resp$clickElement()
  button <- remDr$findElement('id', "c7_respPlots-dlRespPlot")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 7 Map Prediction: Select", { 
  field.map$clickElement()
  # this selectInput needs to be have the container clicked first to list the options
  # for some bizarre reason, the id of this select is "mxEvalSel"
  # this makes little sense -- should look into this
  select.map <- drop.menus[[which(selects.ids == "c7_mxEvalPlots-mxEvalSel")]]
  select.map$clickElement()
  dataValues <- list("'mtp'", "'p10'")
  selectDropdownItem(comp7Tab, "'noThresh'", dataValues)
})

test_that("Component 7 Map Prediction: Buttons", { 
  button <- remDr$findElement('id', "goMapPreds")
  expect_true(button$isElementDisplayed()[[1]])
  button <- remDr$findElement('id', "dlPred")
  expect_true(button$isElementDisplayed()[[1]])
})

# Close the connection
remDr$close()
