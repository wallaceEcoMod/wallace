#' c3_worldclim
#' 
#' download worldclim variables. see www.worldclim.com
#' 
#' @param rasPath character of directory to rasters
#' @param rasName character vector of raster names
#' @export
#' 
#' 

c3_userEnvs<- function(rasPath, rasName, shinyLogs=NULL){
  
  smartProgress(shinyLogs, message = "Reading in rasters...", {
    userEnvs <- raster::stack(rasPath)
    })
  # assign names
  names(userEnvs) <- fileNameNoExt(rasName)
  
  shinyLogs %>% writeLog("Environmental predictors: User input.")
  
  if(is.na(raster::crs(userEnvs))) {
    shinyLogs %>% writeLog(type = "warning", "Input rasters have undefined coordinate 
                    reference system (CRS). Mapping functionality in components 
                    Visualize Model Results and Project Model will not work. If 
                    you wish to map rasters in these components, please define 
                    their projections and upload again. See guidance text in 
                    this module for more details.")
  }
  return(userEnvs)
}