c2_selectOccs <- function(occs, polySelXY, polySelID = 1, shinyLogs = NULL) {
  if (is.null(occs)) {
    shinyLogs %>% writeLog(type = 'error', "Before processing occurrences, 
                      obtain the data in component 1.")
    return()
  }
  if (is.null(polySelXY)) {
    shinyLogs %>% writeLog(type = 'error', 'The polygon has not been finished. Please 
                      press "Finish" on the map toolbar, then 
                      the "Select Occurrences" button.')
      return()
    }
    
    occs.xy <- occs[c('longitude', 'latitude')]
    
    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(occs.xy, data=occs['occID'])
    
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polySelXY)), ID=polySelID)))  # create new polygon from coords
    
    intersect <- sp::over(pts, newPoly)
    ptRemIndex <- as.numeric(which(is.na(intersect)))
    
    remIDs <- as.numeric(pts[ptRemIndex,]$occID)
    
    occs.sel <- occs[-ptRemIndex,]
    
    shinyLogs %>% writeLog(em(spName(occs)), ": Removing occurrences with occID = ", remIDs, 
                     ". Updated data has n = ", nrow(occs.sel), " records.")
    return(occs.sel)
}