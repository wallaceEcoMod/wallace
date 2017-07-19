projectTime_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selTime"), label = "Select time period",
                choices = list("Select period" = "",
                               # "Last Glacial Maximum (~22,000 years ago)" = 'lgm',
                               # "Mid Holocene (~7000 years ago)" = 'mid',
                               "2050" = 50,
                               "2070" = 70)),
    uiOutput('selGCM'),
    # conditionalPanel("input.selTime == 50 || input.selTime == 70",
    selectInput(ns('selRCP'), label = "Select RCP",
                choices = list("Select RCP" = "",
                               '2.6' = 26,
                               '4.5' = 45,
                               '6.0' = 60,
                               '8.5' = 85))
  )
}

projectTime_MOD <- function(input, output, session, rvs) {
  
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
