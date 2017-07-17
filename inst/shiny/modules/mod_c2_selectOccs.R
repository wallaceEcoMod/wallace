selectOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

selectOccs_MOD <- function(input, output, session, rvs, map) {
  
  reactive({
    req(rvs$occs, rvs$selOccsPolyXY)
    
    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(rvs$occs[,2:3], data=rvs$occs['occID'])
    
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$selOccsPolyXY)), ID=1)))  # create new polygon from coords
    
    intersect <- sp::over(pts, newPoly)
    ptSelIndex <- as.numeric(which(!(is.na(intersect))))
    
    selIDs <- as.numeric(pts[ptSelIndex,]$occID)
    
    removedIDs <- rvs$occs$occID[selIDs]
    
    rvs$occs <- rvs$occs[selIDs,]
    
    rvs %>% writeLog("Removed occurrence with ID = ", removedIDs, 
                     ". Updated data has n = ", nrow(rvs$occs), " records.")
  })
}
