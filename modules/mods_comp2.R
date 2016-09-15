source("functions.R")

remSelLocs <- function(remLoc) {
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
    zoom2Occs()
    proxy %>% addCircleMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
                               radius = 5, color = 'red', fillColor = 'red',
                               fillOpacity = 0.2, weight = 2, popup = ~pop,
                               group = 'comp2')
  })
}

polySelLocs <- function() {
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
  proxy %>% addCircleMarkers(data = ptsSel, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'yellow',
                             fillOpacity = 1, weight = 2, popup = ~pop,
                             group = 'comp2')
  values$df <- ptsSel

  values$polyPts1 <- NULL
  values$ptsSel <- ptsSel
  isolate(writeLog(paste('* Selected', nrow(values$df), 'localities.')))
}

thinOccs <- function(thinDist) {
  if (is.null(values$df)) {
    writeLog("* WARNING: Obtain species occurrence localities first in Step 1.")
    return()
  }
  lati <- values$df[,3]
  longi <- values$df[,2]
  z <- smartZoom(longi, lati)
  proxy %>% fitBounds(z[1], z[2], z[3], z[4])

  if (input$thinDist <= 0) {
    writeLog("* WARNING: Assign positive distance to thinning parameter.")
    return()
  }
  withProgress(message = "Spatially Thinning Localities...", {  # start progress bar
    output <- thin(values$df, 'latitude', 'longitude', 'species', thin.par = thinDist,
                   reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                   verbose = FALSE)
    values$prethinned <- values$df
    # pull thinned dataset with max records, not just the first in the list
    maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
    maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
    values$df <- values$df[as.numeric(rownames(maxThin)),]
    if (!is.null(values$inFile)) {
      thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
    }
  })
  
  # MAPPING
  proxy %>% addCircleMarkers(data = values$prethinned, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'blue', 
                             fillOpacity = 1, weight = 2, popup = ~pop, 
                             group = 'comp2')
  proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'red', 
                             fillOpacity = 1, weight = 2, popup = ~pop, 
                             group = 'comp2')
  
  proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'red',
                             fillOpacity = 0.2, weight = 2, popup = ~pop, 
                             group = 'df') %>% hideGroup('df')

  values$origOccs <- values$df
  writeLog(paste('* Total records thinned to [', nrow(values$df), '] localities.'))
  # render the thinned records data table
  output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
}
