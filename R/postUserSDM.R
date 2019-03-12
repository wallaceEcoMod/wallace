userSDM <- function(rasPath, shinyLogs = NULL) {
  
  smartProgress(shinyLogs, message = "Reading raster prediction...(**)", {
    rasSDM <- raster::raster(rasPath)
  })
  
  shinyLogs %>% writeLog("Raster: User SDM input.(**)")
  
  if (is.na(raster::crs(rasSDM))) {
    shinyLogs %>% writeLog(type = "warning",'Input raster have undefined coordinate reference system (CRS). Mapping functionality in this component will not work. Please define their projections and upload again. See guidance text in this module for more details.')
  }
  return(rasSDM)
}