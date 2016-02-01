source("functions.R")

comp2_selLocMap_remLocs <- function(remLoc) {
  isolate({
    numTest <- remLoc %in% row.names(values$df)
    rows <- as.numeric(rownames(values$df))  # get row names
    remo <- which(remLoc == rows)  # find which row name corresponds to user selection for removal
    # Remove the offending row
    if (length(remo) > 0) {
      values$removed <- values$df[remo, ]
      values$removedAll <- c(values$removedAll, rownames(values$removed))  # keep vector of all removed pts
      values$df <- values$df[-remo, ]
      values$origOccs <- values$origOccs[-remo, ]
    }
    
    if (numTest) {
      writeLog(paste0("* Removed locality with ID = ", remLoc, "."))
    }
  })
}

comp2_selLocMap_selIntLocs <- function() {
  if (is.null(values$origOccs)) return()
  values$prethinned <- NULL  # resets prethinned to avoid it plotting if sequence is: select pts -> spThin -> select pts -> spThin
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1
  if (is.null(values$polyPts1)) return()
  pts <- SpatialPoints(values$df[,2:3])  # make pts spatial object
  
  newPoly <- SpatialPolygons(list(Polygons(list(Polygon(values$polyPts1)), ID=values$polyID)))  # create new polygon from coords
  if (is.null(values$poly1)) {  # if there are no polygons, draw the new one, otherwise draw the new plus all the old ones
    values$poly1 <- newPoly  # this is passed to mapFuncs to fill the selected polygons
  } else {
    values$poly1 <- spRbind(values$poly1, newPoly)
  }
  
  values$ptSeln <- as.numeric(which(!(is.na(over(pts, values$poly1)))))  # select pts overlapping (intersecting) with polygon(s)
  if (length(values$ptSeln) == 0) {
    values$poly1 <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$polyPts1 <- NULL
    proxy %>% clearShapes()
    return()
  }
  
  # Subset with selected locs
  ptsSel <- values$origOccs[values$ptSeln, ]
  map_plotLocs(ptsSel, fillColor='yellow', fillOpacity=1, clearShapes=FALSE)
  proxy %>% addLegend("topright", colors = c('red','yellow'),
                      title = "GBIF Records", labels = c('original', 'selected'),
                      opacity = 1, layerId = 1)
  values$df <- ptsSel
  
  values$polyPts1 <- NULL
  values$ptsSel <- ptsSel
  isolate(writeLog(paste('* Selected', nrow(values$df), 'localities.')))
}