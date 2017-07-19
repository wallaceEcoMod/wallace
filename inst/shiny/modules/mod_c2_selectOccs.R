selectOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

selectOccs_MOD <- function(input, output, session, rvs) {
  
  reactive({
    req(rvs$occs, rvs$selOccsPolyXY)
    
    occs.xy <- rvs$occs[c('longitude', 'latitude')]
    
    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(occs.xy, data=rvs$occs['occID'])
    
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$selOccsPolyXY)), ID=rvs$selOccsPolyID)))  # create new polygon from coords
    
    intersect <- sp::over(pts, newPoly)
    ptSelIndex <- as.numeric(which(!(is.na(intersect))))
    
    selIDs <- as.numeric(pts[ptSelIndex,]$occID)
    
    rvs$occs <- rvs$occs[ptSelIndex,]
    
    rvs %>% writeLog("Keeping only occurrences with occID = ", selIDs, 
                     ". Updated data has n = ", nrow(rvs$occs), " records.")
  })
}
