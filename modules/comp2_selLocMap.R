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
      values$gbifoccs <- values$gbifoccs[-remo, ]
    }
    
    if (numTest) {
      writeLog(paste0("* Removed locality with ID = ", remLoc, "."))
    }
  })
}

comp2_selLocMap_selIntLocs <- function() {
  if (is.null(values$gbifoccs)) return()
  values$prethinned <- NULL  # resets prethinned to avoid it plotting if sequence is: select pts -> spThin -> select pts -> spThin
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1
  if (is.null(values$drawPolyCoordsSelLocs)) return()
  pts <- SpatialPoints(values$df[,2:3])  # make pts spatial object
  
  newPoly <- SpatialPolygons(list(Polygons(list(Polygon(values$drawPolyCoordsSelLocs)), ID=values$polyID)))  # create new polygon from coords
  if (is.null(values$drawPolys)) {  # if there are no polygons, draw the new one, otherwise draw the new plus all the old ones
    values$drawPolys <- newPoly
  } else {
    values$drawPolys <- spRbind(values$drawPolys, newPoly)
  }
  
  values$ptSeln <- as.numeric(which(!(is.na(over(pts, values$drawPolys)))))  # select pts overlapping (intersecting) with polygon(s)
  
  # Subset with selected locs
  ptsSel <- values$gbifoccs[values$ptSeln, ]
  values$df <- ptsSel
  
  values$drawPolyCoordsSelLocs <- NULL
  values$ptsSel <- ptsSel
  isolate(writeLog(paste('* Selected', nrow(values$df), 'localities.')))
}