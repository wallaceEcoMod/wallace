#' c3_worldclim
#' 
#' download worldclim variables. see www.worldclim.com
#' 
#' @param bcRes numeric resolution of the climatic layers
#' @param bcSelChoice boolean TRUE/FALSE TRUE=user selects the variables
#' @param bcSel list of boolean data. selected variables
#' 
#' 
#' 

c3_worldclim<- function (occs, bcRes, bcSelChoice, bcSel, logs=NULL, shiny=FALSE){
  
  if (is.null(occs)) {
    logs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                   obtain occurrence data in component 1.")
    return()
  }
  
  if (bcRes == '') {
    logs %>% writeLog(type = 'error', 'Select a raster resolution.')
    return()
  }
  
  withProgress(message = "Retrieving WorldClim data...", {
    if (bcRes == 0.5) {
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes, 
                              lon = mapCntr()[1], lat = mapCntr()[2])
      # rvs$bcLon <- mapCntr()[1]
      # rvs$bcLat <- mapCntr()[2]
    } else {
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes)
      wcbc <- wcbc[[bcSel]]
    }
  })
  
  if (raster::nlayers(wcbc) == 19) {
    bcSel <- 'bio1-19'
  } else {
    bcSel <- paste(names(wcbc), collapse = ", ")
  }
  logs %>% writeLog(occs$taxon_name[1], ": WorldClim bioclimatic variables",
                    bcSel, "at", bcRes, " arcmin resolution.")
  
  # change names if bio01 is bio1, and so forth
  i <- grep('bio[0-9]$', names(wcbc))
  editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep='0')
  names(wcbc)[i] <- editNames
  
  return(wcbc)
}