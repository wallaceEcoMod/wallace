popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$strong(paste("Longitude:", lon))
  ))
}

drawToolbarGetPolyXY <- function(newFeat) {
  coords <- unlist(newFeat$geometry$coordinates)
  xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
  return(xy)
}

drawToolbarGetPolyID <- function(newFeat) {
  id <- newFeat$properties$`_leaflet_id`
  return(id)
}