#' c3_ecoClimate
#' 
#' download ecoClimate variables. see www.ecoclimate.org
#' For EcoClimate the resolution is fixed to 0.5 degrees  
#' 
#' @param bcAOGCM name of the Atmospheric and Oceanic Global Circulation Model. 
#' Options are: "CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS", "IPSL","MRI", "MPI"
#' @param bcScenario select the temporal scenario that you want to download. 
#' Options are: "LGM" (Last Glacial Maximum, 21,000 years ago), "Holo" ()
#' @param bcSelChoice boolean TRUE/FALSE TRUE=user selects the variables
#' @param bcSels list of boolean data. selected variables
#' @param rvs monster list 
#' 
#' 


library (repmis)

c3_ecoClimate<- function (bcAOGCM, bcScenario, bcSelChoice=FALSE, bcSels, rvs){

if (is.null(rvs$occs)) {
  rvs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                   obtain occurrence data in component 1.")
  return()
}

# record for RMD
rvs$bcSels <- input$bcSels

withProgress (message = "Retrieving ecoClimate data...", {
if (input$bcAOGCM == "CCSM") {




    wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes, 
                            lon = mapCntr()[1], lat = mapCntr()[2])
    
    rvs$bcLon <- mapCntr()[1]
    rvs$bcLat <- mapCntr()[2]
    
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


