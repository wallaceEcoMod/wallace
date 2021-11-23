# Load the package
library(RSelenium)
library(testthat)
library(XML)

context("test-workflow")
skip_on_cran()

# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'workflow', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
browser <- "chrome"
remDr <- remoteDriver(browserName = browser) 
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
Sys.sleep(5)

# find results tabs and labels in results window
resultsTabs <- remDr$findElements("css selector", ".nav a")
resultsTabLabels <- sapply(resultsTabs, function(x){x$getElementText()})
Sys.sleep(1)
# find different particular results tabs
occsTblTab <- resultsTabs[[which(resultsTabLabels == "Occs Tbl")]]  
resultsTblTab <- resultsTabs[[which(resultsTabLabels == "Results")]]  

logTextLines <- function() {
  logContent <- remDr$findElement(using = "id", value = 'logContent')
  logText <- strsplit(logContent$getElementText()[[1]], "\n")[[1]]
  return(logText)
}

test_that("C1 Module Query Database: DB Query Returns Specified Number of Records", {
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
  logText_split <- strsplit(logText, ' ')[[1]]
  nums <- logText_split[grep('[0-9]', logText_split)]
  expect_equal(as.numeric(nums[1]), 100)
})

# CM: some times this fails the first time i run it and then rerunning is fine. not clear if we need a Sys.sleep() but doesn't seem like anything should be slow...
test_that("C1 Module Query Database: DB Query Populates Data Table", {
  occsTblTab$clickElement()
  Sys.sleep(5)
  # find occurrence data table
  occTbl.elem <- remDr$findElement(using = 'id', value = "occTbl")
  # read table
  htmlTxt <- occTbl.elem$getElementAttribute("outerHTML")[[1]]
  occTbl <- XML::readHTMLTable(htmlTxt, header=TRUE, as.data.frame=TRUE)
  # test that species name appears in first row
  expect_equal(occTbl$DataTables_Table_0$name[1], factor('Puma concolor'))
})


test_that("C2 Module Spatial Thin: Spatially thin occurence locations", {
  # Move to Compenent 2
  comp2Tab$clickElement()
  
  # select Spatial Thin radio button 
  field.spthin <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='spthin']")
  initState <- field.spthin$isElementSelected()[[1]]
  field.spthin$clickElement()
  
  # find textInput field for Thinning distance and click
  #db.search <- comp2Tab$findChildElement(value = "//input[@type='numeric']")
  button.thinDist <- comp2Tab$findChildElement(value = "//input[@id='c2_thinOccs-thinDist']")
  button.thinDist$clickElement()
  
  # write a thinning distance
  button.thinDist$clearElement()
  button.thinDist$sendKeysToElement(list("10"))
  
  # find button and click to thin occs
  button.thin <- comp2Tab$findChildElement(value = "//button[@id='goThinOccs']")
  button.thin$clickElement()
  Sys.sleep(10)
  
  # read current text in log box
  logText <- logTextLines()
  logText <- logText[length(logText)]

  # Get the number of records after thinning. This should be less than 100
  logText_split <- strsplit(logText, ' ')[[1]]
  nums <- logText_split[grep('[0-9]', logText_split)]
  expect_lt(as.numeric(nums[1]), 100)
})


test_that("C2 Module Select Occurences on Map: Choose occurence locations by polygon", {
  # select Select Occurences on Map radio button 
  field.selOccs <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='selOccs']")
  initState <- field.selOccs$isElementSelected()[[1]]
  field.selOccs$clickElement()

  # select button to draw polygon
  leaflet.draw <- remDr$findElement(using = 'class', value = 'leaflet-draw-draw-polygon')
  leaflet.draw$clickElement()
  
  # define the map window
  map.window <- remDr$findElement(using = 'id', value = 'map')
  
  if (browser == 'chrome') {
    remDr$mouseMoveToLocation(webElement = map.window)
    remDr$mouseMoveToLocation(-100, -200)
    remDr$click()
    Sys.sleep(2)
    remDr$mouseMoveToLocation(0, 150)
    remDr$click()
    Sys.sleep(2)
    remDr$mouseMoveToLocation(100, 0)
    remDr$click()
    Sys.sleep(2)
    remDr$mouseMoveToLocation(0, -150)
    remDr$click()
    Sys.sleep(2)
    remDr$mouseMoveToLocation(webElement = leaflet.draw)
    remDr$mouseMoveToLocation(20, 0)
    remDr$click()
    Sys.sleep(2)
  }
    
  # select the button to Select Occurrences
  button.selOccs <- remDr$findElement(using = 'id', value = 'goSelectOccs')
  button.selOccs$clickElement()
  Sys.sleep(5)
  
  # read current text in log box
  logText <- logTextLines()
  logText <- logText[length(logText)]
  
  # Get the number of records after thinning. This should be less than 100
  logText_split <- strsplit(logText, ' ')[[1]]
  # Remove entries before "Updated"
  logText_split_shrt <- logText_split[which(logText_split == "Updated"):length(logText_split)]
  nums <- logText_split_shrt[grep('[0-9]', logText_split_shrt)]
  
  # test that the number of points selected are fewer than the original number
  expect_lt(as.numeric(nums[1]), 100)
})


test_that("C3 Module Worldclim: Downloads Specified Bioclim Rasters", {
  # Move to Component 3
  comp3Tab$clickElement()
  
  # find and click dropdown menu for resolution
  wc.select <- comp3Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
  wc.select$clickElement()
  res10m <- comp3Tab$findChildElement(value = paste0("//div[@data-value='10' and @class='option']"))
  #CM: In chrome,this click isn't working. it worked fine in firefox...
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
  Sys.sleep(15)
  
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
  expect_equal(logText, "> Random background points sampled (n = 10000 : 17 % of cells with values).")
})
                                
test_that("C5 Module Non-spatial partition: Random k-fold Partitions", {
  # switch to comp5 tab
  comp5Tab$clickElement()
  
  nFoldsField <- comp5Tab$findChildElement(value = "//input[@id='c5_partNsp-kfolds']")
  nFoldsField$clickElement()
  nFoldsField$clearElement()
  nFoldsField$sendKeysToElement(list('3'))
  partitionButton <- comp1Tab$findChildElement(value = "//button[@type='button' and @id='goPartNsp']")
  partitionButton$clickElement()
  Sys.sleep(5)
  
  # read log box
  logText <- logTextLines()
  logText <- logText[length(logText)]
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

test_that("C7 Module BIOCLIM Envelope Plot: Envelope Plot Displays", {
  # switch to comp7
  comp7Tab$clickElement()
  
  envelPlot <- remDr$findElement(using = 'id', value = 'bcEnvelPlot')
  expect_true(envelPlot$isElementDisplayed()[[1]])
})

test_that("C7 Module Map Prediction: BIOCLIM Map Plots", {
  field.map <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='map']")
  field.map$clickElement()
  button.mapPreds <- remDr$findElement(using = 'id', value = 'goMapPreds')
  button.mapPreds$clickElement()
  Sys.sleep(5)
  
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> BIOCLIM model prediction plotted.")
})

test_that("C6 Module Maxent: Maxent Runs", {
  # switch back to comp6 tab
  comp6Tab$clickElement()
  
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
  # in case Java crashes because "JVM is not running", restart all RStudio sessions and redo, which should fix issue -- not clear why this happens
  maxentButton$clickElement()
  
  Sys.sleep(20)
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent ran successfully and output evaluation results for 3 models.")
})

test_that("C6 Module Maxent: Maxent Populates Data Table", {
  resTbl.elem <- remDr$findElement(using = 'id', value = "evalTbl")
  htmlTxt <- resTbl.elem$getElementAttribute("outerHTML")[[1]]
  resTbl <- XML::readHTMLTable(htmlTxt, header=TRUE, as.data.frame=TRUE)
  expect_equal(resTbl[[2]]$settings, factor(c('L_1', 'L_1.5', 'L_2')))
})

test_that("C7 Module Maxent Evaluation Plots: Evaluation Plot Displays", {
  # switch to comp7
  comp7Tab$clickElement()
  
  evalPlots <- remDr$findElement(using = 'id', value = 'mxEvalPlots')
  expect_true(evalPlots$isElementDisplayed()[[1]])
})

test_that("C7 Module Plot Response Curves: Response Curves Display", {
  field.resp <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='response']")
  field.resp$clickElement()
  respPlots <- remDr$findElement(using = 'id', value = 'respPlots')
  expect_true(respPlots$isElementDisplayed()[[1]])
})

test_that("C7 Module Map Prediction: Maxent Maps Plot", {
  field.map <- comp7Tab$findChildElement(value = "//input[@type='radio' and @value='map']")
  field.map$clickElement()
  button.mapPreds <- remDr$findElement(using = 'id', value = 'goMapPreds')
  button.mapPreds$clickElement()
  Sys.sleep(5)
  
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent raw model prediction plotted.")
  
  field.logistic <- comp7Tab$findChildElement(value = "//input[@type='radio' and  @value='logistic']")
  field.logistic$clickElement()
  plot.button=comp7Tab$findChildElement(value = "//button[@id='goMapPreds']")
  plot.button$clickElement()
  Sys.sleep(5)
  
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent logistic model prediction plotted.")
  
  field.cloglog <- comp7Tab$findChildElement(value = "//input[@type='radio' and  @value='cloglog']")
  field.cloglog$clickElement()
  plot.button=comp7Tab$findChildElement(value = "//button[@id='goMapPreds']")
  plot.button$clickElement()
  Sys.sleep(5)
  
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Maxent cloglog model prediction plotted.")
})

# NOTE: mouse move functions only work in Chrome, and error in Firefox
# thus, these tests can currently only be performed on Chrome
test_that("C8 Module Project to New Extent: Projection Plots", {
  # switch to comp8
  comp8Tab$clickElement()
  
  leaflet.draw <- remDr$findElement(using = 'class', value = 'leaflet-draw-draw-polygon')
  leaflet.draw$clickElement()
  
  map.window <- remDr$findElement(using = 'id', value = 'map')
  
  if (browser == 'chrome') {
    remDr$mouseMoveToLocation(webElement = map.window)
    remDr$mouseMoveToLocation(110, 0)
    remDr$click()
    Sys.sleep(1)
    remDr$mouseMoveToLocation(0, 50)
    remDr$click()
    Sys.sleep(1)
    remDr$mouseMoveToLocation(50, 0)
    remDr$click()
    Sys.sleep(1)
    remDr$mouseMoveToLocation(0, -50)
    remDr$click()
    Sys.sleep(1)
    remDr$mouseMoveToLocation(webElement = leaflet.draw)
    remDr$mouseMoveToLocation(20, 0)
    remDr$click()
    Sys.sleep(1)
    
    button.projArea <- remDr$findElement(using = 'id', value = 'goProjectArea')
    button.projArea$clickElement()
    Sys.sleep(10)
    
    logText <- logTextLines()
    logText <- logText[length(logText)]
    logText <- strsplit(logText, ':')[[1]][1]
    expect_equal(logText, "> New area projection for model L_1 with extent coordinates")  
  } 
})

test_that("C8 Module Project to New Time: Projection Plots", {
  # project to new time
  projectTime <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='projTime']")
  projectTime$clickElement()

  # select ids that are "not full"
  path <- "//input[@placeholder='Select period']"
  select.time <- remDr$findElement(value = path)
  select.time$clickElement()
  yr2070 <- comp8Tab$findChildElement(value = paste0("//div[@data-value='70' and @class='option']"))
  yr2070$clickElement()
  
  path <- "//input[@placeholder='Select GCM']"
  select.gcm <- remDr$findElement(value = path)
  select.gcm$clickElement()
  gcmCCSM <- comp8Tab$findChildElement(value = paste0("//div[@data-value='CC' and @class='option']"))
  gcmCCSM$clickElement()
  
  path <- "//input[@placeholder='Select RCP']"
  select.rcp <- remDr$findElement(value = path)
  select.rcp$clickElement()
  rcp85 <- comp8Tab$findChildElement(value = paste0("//div[@data-value='85' and @class='option']"))
  rcp85$clickElement()
  
  button.projTime <- remDr$findElement(using = 'id', value = 'goProjectTime')
  button.projTime$clickElement()
  Sys.sleep(5)
  
  logText <- logTextLines()
  logText <- logText[length(logText)]
  expect_equal(logText, "> Projected to 2070 for GCM CCSM4 under RCP 8.5 .")
})

test_that("C8 Module Calculate Environmental Similarity", {
   mess <- comp8Tab$findChildElement(value = "//input[@type='radio' and @value='mess']")
   mess$clickElement()
   mess.button=comp8Tab$findChildElement(value = "//button[@id='goEnvSimilarity']")
   mess.button$clickElement()
   Sys.sleep(5)
   
   logText <- logTextLines()
   logText <- logText[length(logText)]
   expect_equal(logText, "> Generated MESS map for 2070 for GCM CCSM4 under RCP 8.5 .")
})
   
# Close the connection
remDr$close()
