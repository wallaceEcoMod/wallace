remSelLocs <- function(remLoc) {
  isolate({
    numTest <- remLoc %in% values$df$origID
    remo <- which(remLoc == values$df$origID)  # find which row name corresponds to user selection for removal
    # Remove the offending row
    if (length(remo) > 0) {
      values$removed <- rbind(values$removed, values$df[remo, ])
      values$removedAll <- c(values$removedAll, rownames(values$removed))  # keep vector of all removed pts
      values$df <- values$df[-remo, ]
      values$origOccs <- values$origOccs[-remo, ]
    }

    if (numTest) {
      writeLog(paste0("* Removed locality with ID = ", remLoc, ". Localities data has n = ", nrow(values$df), " records."))
    }

    proxy %>% zoom2Occs(values$origOccs) %>% map_plotLocs(values$origOccs)
  })
}

polySelLocs <- function() {
  if (is.null(values$origOccs)) return()
  values$prethinned <- NULL  # resets prethinned to avoid it plotting if sequence is: select pts -> spThin -> select pts -> spThin
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1
  if (is.null(values$polyPts1)) return()

  # make spatial pts object of original occs and preserve origID
  pts <- sp::SpatialPointsDataFrame(values$origOccs[,2:3], data=values$origOccs['origID'])

  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(values$polyPts1)), ID=values$polyID)))  # create new polygon from coords

  # reset polyPts1
  values$polyPts1 <- NULL

  # if there are no polygons, draw the new one, otherwise draw the new plus all the old ones (bind them together)
  if (is.null(values$poly1)) {
    values$poly1 <- newPoly
  } else {
    values$poly1 <- maptools::spRbind(values$poly1, newPoly)
  }

  # select pts overlapping (intersecting) with polygon(s) and get the indices of selected pts
  intersect <- sp::over(pts, values$poly1)
  ptSelIndex <- as.numeric(which(!(is.na(intersect))))

  # if no pts selected, erase polys and exit function
  if (length(ptSelIndex) == 0) {
    values$poly1 <- NULL
    values$polyErase <- TRUE  # turn on to signal to prevent use existing map click
    values$polyPts1 <- NULL
    proxy %>% clearShapes()
    return()
  }

  # record ID of selected points
  selIDs <- as.numeric(pts[ptSelIndex,]$origID)
  values$ptSelID <- selIDs

  # subset df with selected locs and record the selected points
  values$df <- values$ptsSel <- values$origOccs[selIDs, ]
  values$df <- na.omit(values$df)  # remove phantom NA rows (just problem on Windows?)

  # # plot all
  # proxy %>% map_plotLocs(values$df, fillColor='yellow', fillOpacity=1, clearShapes=FALSE, clearMarkers=FALSE)

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
    output <- spThin::thin(values$df, 'latitude', 'longitude', 'name', thin.par = thinDist,
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

  # MAPPING - blue pts for remove, red pts for keep
  proxy %>% addCircleMarkers(data = values$prethinned, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'blue',
                             fillOpacity = 1, weight = 2, popup = ~pop,
                             group = 'comp2')
  proxy %>% addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'red',
                             fillOpacity = 1, weight = 2, popup = ~pop,
                             group = 'comp2')

  # values$origOccs <- values$df
  writeLog(paste('* Total records thinned to [', nrow(values$df), '] localities.'))
  # render the thinned records data table
  output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
}
