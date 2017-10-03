context("module_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)
library(XML)
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

# find all component tabs and click
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]] 
comp2Tab <- compTabs[[which(compTabLabels == "2 Process Occs")]]
comp3Tab <- compTabs[[which(compTabLabels == "3 Env Data")]]
comp4Tab <- compTabs[[which(compTabLabels == "4 Process Envs")]]  
comp5Tab <- compTabs[[which(compTabLabels == "5 Partition Occs")]]
comp6Tab <- compTabs[[which(compTabLabels == "6 Model")]]
comp7Tab <- compTabs[[which(compTabLabels == "7 Visualize")]]
comp8Tab <- compTabs[[which(compTabLabels == "8 Project")]]

# click component 1 tab
comp1Tab$clickElement()

# find results tabs and labels in results window
resultsTabs <- remDr$findElements("css selector", ".nav a")
resultsTabLabels <- sapply(resultsTabs, function(x){x$getElementText()})
# find different particular results tabs
occsTblTab <- resultsTabs[[which(resultsTabLabels == "Occs Tbl")]]  
resultsTblTab <- resultsTabs[[which(resultsTabLabels == "Results")]]  

logTextLines <- function() {
  logContent <- remDr$findElement(using = "id", value = 'logContent')
  logText <- strsplit(logContent$getElementText()[[1]], "\n")[[1]]
  return(logText)
}

test_that("C1 Module Query Database: DB Query Returns Specified Number of Records", {
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
  logText <- logTextLines()
  logText <- logText[length(logText)]
  # make sure Wallace found 100 records in GBIF
  #CM:  occSearchLine wasn't defined so i tried something else
  #nums <- occSearchLine[grep('[0-9]', logText)]
  nums=substr(strsplit(logText,"> Total gbif records for Puma concolor returned [ ",fixed=T)[[1]][2],1,3)
  expect_equal(as.numeric(nums[1]), 100)
})
  
test_that("C1 Module Query Database: DB Query Populates Data Table", {
  occsTblTab$clickElement()
  # find occurrence data table
  occTbl.elem <- remDr$findElement(using = 'id', value = "occTbl")
  # read table
  htmlTxt <- occTbl.elem$getElementAttribute("outerHTML")[[1]]
  occTbl <- XML::readHTMLTable(htmlTxt, header=TRUE, as.data.frame=TRUE)
  # test that species name appears in first row
  expect_equal(occTbl$DataTables_Table_0$name[1], factor('Puma concolor'))
})

test_that("C3 Module Worldclim: Downloads Specified Bioclim Rasters", {
  # Move to Component 3
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
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Environmental predictors: WorldClim bioclimatic variables bio1, bio2, bio3 at 10 arcmin resolution.")
})


test_that("C4 Module Select Study Region: Background Mask Polygon Generated", {
  # switch to comp4 tab
  comp4Tab$clickElement()
  
  select.button <- comp4Tab$findChildElement(value = "//button[@id='goBgExt']")
  select.button$clickElement()
  # read log box
  Sys.sleep(5)
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Study extent: bounding box. Study extent buffered by 0.5 degrees.")
})

test_that("C4 Module Select Study Region: Rasters Clipped and Masked", {
  process.button <- comp4Tab$findChildElement(value = "//button[@id='goBgMask']")
  process.button$clickElement()

  # read log box 
  Sys.sleep(30)
  logText <- logTextLines()
  logText <- logText[length(logText) - 1]
  expect_equal(logText, "> Environmental data masked.")
})

test_that("C4 Module Select Study Region: Background Points Generated", {
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Random background points sampled (n = 10000 : 4.71 % of cells with values).")
})
           
# CM: getting this error consistently which seems to be in the actual kfold code
# Warning: Error in if: missing value where TRUE/FALSE needed
# Stack trace (innermost first):
#   80: singlefold
# 79: kfold
# 78: ENMeval::get.randomkfold
# 77: <reactive> [modules/mod_c5_partitionNonSpat.R#32]
#                 66: partNsp.call
#                 65: observeEventHandler [/Library/Frameworks/R.framework/Versions/3.3/Resources/library/wallace/shiny/server.R#458]
#                                          1: shiny::runApp
                                
test_that("C5 Module Non-spatial partition: Random k-fold Partitions", {
  # switch to comp5 tab
  comp5Tab$clickElement()
  
  nFoldsField <- comp5Tab$findChildElement(value = "//input[@id='c5_partNsp-kfolds']")
  nFoldsField$clickElement()
  nFoldsField$clearElement()
  nFoldsField$sendKeysToElement(list('3'))
  partitionButton <- comp1Tab$findChildElement(value = "//button[@type='button' and @id='goPartNsp']")
  partitionButton$clickElement()
  
  # read log box
  logText <- logTextLines()
  logText <- logText[length(logText)]
  Sys.sleep(10)
  expect_equal(logText, "> Occurrences partitioned by random k-fold (k = 3 ).")
})

test_that("C6 Module BIOCLIM: BIOCLIM Runs", {
  # switch to comp6 tab
  comp6Tab$clickElement()
  
  field.bc <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='BIOCLIM']")
  field.bc$clickElement()
  
  button.goBioclim <- remDr$findElement(using = 'id', value = 'goBioclim')
  button.goBioclim$clickElement()
  
  Sys.sleep(10)
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> BIOCLIM ran successfully and output evaluation results.")
})

test_that("C6 Module BIOCLIM: BIOCLIM Populates Data Table", {
  resTbl.elem <- remDr$findElement(using = 'id', value = "evalTbl")
  htmlTxt <- resTbl.elem$getElementAttribute("outerHTML")[[1]]
  resTbl <- XML::readHTMLTable(htmlTxt, header=TRUE, as.data.frame=TRUE)
  expect_equal(resTbl[[2]][,1], factor(c('test.AUC', 'diff.AUC', 'test.orMTP', 'test.or10pct')))
  # CM gets: Error in resTbl[[2]] : subscript out of bounds
})

test_that("C6 Module Maxent: Maxent Runs", {
  # click Maxent
  field <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='Maxent']")
  field$clickElement()
  
  # select linear features
  linear <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='L']")
  linear$clickElement()
  
  # enter 0.5 for regularization multiplier
  rmsField <- comp6Tab$findChildElement(value = "//input[@id='c6_maxent-rmsStep']")
  rmsField$clickElement()
  rmsField$clearElement()
  rmsField$sendKeysToElement(list('0.5'))
  
  maxentButton <- comp6Tab$findChildElement(value = "//button[@type='button' and @id='goMaxent']")
  maxentButton$clickElement()
  
  Sys.sleep(10)
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent ran successfully and output evaluation results for 3 models.")
})

test_that("C6 Module Maxent: Maxent Populates Data Table", {
  resTbl.elem <- remDr$findElement(using = 'id', value = "evalTbl")
  htmlTxt <- resTbl.elem$getElementAttribute("outerHTML")[[1]]
  resTbl <- XML::readHTMLTable(htmlTxt, header=TRUE, as.data.frame=TRUE)
  expect_equal(resTbl$DataTables_Table_1$settings, factor(c('L_1', 'L_1.5', 'L_2')))
  # CM: for me, this has the names $DataTables_Table_3 instead of $DataTables_Table_1. is that dyanmically generated or any reason it would differ on our machines? maybe just reference it as resTbl[[2]]$settings?
})

test_that("C7 Module Maxent Evaluation Plots: Evaluation Plot Generates", {
  # switch to comp7
  comp7Tab$clickElement()
  
  evalPlots <- remDr$findElement(using = 'id', value = 'mxEvalPlots')
  expect_true(evalPlots$isElementDisplayed()[[1]])
})

test_that("C7 Module Plot Response Curves: Response Curves Generate", {
  field.resp <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='response']")
  field.resp$clickElement()
  respPlots <- remDr$findElement(using = 'id', value = 'respPlots')
  expect_true(respPlots$isElementDisplayed()[[1]])
})

test_that("C7 Module Map Prediction: ", {
  field.map <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='map']")
  field.map$clickElement()
  button.mapPreds <- remDr$findElement(using = 'id', value = 'goMapPreds')
  button.mapPreds$clickElement()
  field.logistic <- comp7Tab$findChildElement(value = "//input[@type='radio' and  @value='logistic']")
  field.logistic$clickElement()
  plot.button=comp7Tab$findChildElement(value = "//button[@id='goMapPreds']")
  plot.button$clickElement()
  # NEED TO READ LOG, BUT CURRENTLY NOTHING PRINTED FOR MAP
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent ran successfully and output evaluation results for 3 models.")
  #CM: well, its working at least, but with no printout, i think we just move on and build a test that relies on this having worked. I left this in there because if some error were to happen, it'd print something else so at least with this we know an error didn't print to the log.
  
})

test_that("C8 Projection: ", {
  # switch to comp8
  comp8Tab$clickElement()
  
  # project to new time
  projectTime <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='projTime']")
  projectTime$clickElement()
  
  ## CM: JAMIE START HERE
  wc.select <- comp8Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
  time.select <- comp8Tab$findChildElement(value = "//div[@id='c8_projectTime-selTime']")
  ## End here

 #  gcm.select <- comp8Tab$findChildElement(value = "//div[@class='selectize-input items not-full has-options']")
 #  gcm.select <- comp8Tab$findChildElement(value = "//input[@placeholder='Select GCM']")
 #  <input autocomplete="off" tabindex="" placeholder="Select GCM" style="width: 77px;" type="text">
 # 
 #  time.select$clickElement()
 #  time2050 <- comp8Tab$findChildElement(value = paste0("//div[@data-value='10' and @class='option']"))
 # # <div class="selectize-input items not-full has-options" style=""><input autocomplete="off" tabindex="" placeholder="Select period" style="width: 88px; opacity: 1; position: relative; left: 0px;" type="text"></div>
 #    
 #  time <- comp8Tab$findChildElement(value = paste0("//div[@data-value='50' and @class='selectize-input items has-options full has-items']"))
 #  time$clickElement()  
 #  
 #  # test mess
 #  mess <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='mess']")
 #  mess$clickElement()
 #  mess.button=comp8Tab$findChildElement(value = "//button[@id='goEnvSimilarity']")
 #  mess.button$clickElement()
  
  
})
# Close the connection
remDr$close()
