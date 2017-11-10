#' c3_ecoClimate
#' 
#' download ecoClimate variables. see www.ecoclimate.org
#' 
#' @param bcRes numeric resolution of the climatic layers
#' @param bcSelChoice boolean TRUE/FALSE TRUE=user selects the variables
#' @param bcSels list of boolean data. selected variables
#' @param rvs monster list 
#' 
#' 
#' 

c3_ecoClimate<- function (bcRes, bcSelChoice, bcSels, rvs){

if (is.null(rvs$occs)) {
  rvs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                   obtain occurrence data in component 1.")
  return()
}
if (input$bcRes == '') {
  rvs %>% writeLog(type = 'error', 'Select a raster resolution.')
  return()
}

# record for RMD
rvs$bcRes <- input$bcRes
rvs$bcSels <- input$bcSels

withProgress(message = "Retrieving WorldClim data...", {
  if (input$bcRes == 0.5) {
    wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes, 
                            lon = mapCntr()[1], lat = mapCntr()[2])
    rvs$bcLon <- mapCntr()[1]
    rvs$bcLat <- mapCntr()[2]
  } else {
    wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes)
    wcbc <- wcbc[[input$bcSels]]
  }
})

if (raster::nlayers(wcbc) == 19) {
  bcSels <- 'bio1-19'
} else {
  bcSels <- paste(names(wcbc), collapse = ", ")
}
logs %>% writeLog("Environmental predictors: WorldClim bioclimatic variables",
                  bcSels, "at", input$bcRes, " arcmin resolution.")

# change names if bio01 is bio1, and so forth
i <- grep('bio[0-9]$', names(wcbc))
editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep='0')
names(wcbc)[i] <- editNames
rvs$bcSels[i] <- editNames

return(wcbc)

}


