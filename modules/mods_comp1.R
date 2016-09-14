source("functions.R")

gbifBG <- function(){print("The first step is to download occurrence data if the researcher does not 
already have a dataset for the species of interest. We are interested in obtaining data 
that document the presence of a species at particular points in space and time, along with 
other useful metadata data fields when available. Over the past two decades, the worldwide 
biodiversity informatics community has achieved remarkable progress, with many millions of 
occurrence records now available on the internet aggregated via various portals, including 
a substantial subset of records with assigned georeferences (e.g., latitude/longitude 
coordinates). These records include digitized data from natural history museums and herbaria, 
as well as newer data sources, including citizen-science initiatives leveraging mobile technologies.")}

getGbifOccs <- function(spName, occNum) {
  # query GBIF based on user input, remove duplicate records
  writeLog("...Searching GBIF...")
  values$spname <- spName  # record species name
  results <- occ_search(scientificName = spName, limit = occNum, hasCoordinate = TRUE)
  
  # Control species not found
  if (results$meta$count == 0) {
    writeLog(paste('* No records found for ', spName, ". Please check the spelling."))
  }
  
  if (results$meta$count != 0) {
    cols <- c('name','decimalLongitude','decimalLatitude',
              'institutionCode','country', 'stateProvince',
              'locality', 'elevation', 'basisOfRecord')
    results <- fixcols(cols, results)
    locs.in <- results$data[!is.na(results$data[,3]),]
    locs <- remDups(locs.in)
    values$gbifOrig <- locs
    locs <- locs[,cols]  # limit to above columns

    names(locs)[1:3] <- c('species','longitude', 'latitude')    
    locs$origID <- row.names(locs)
    locs$pop <- unlist(apply(locs, 1, popUpContent))
    # add locs to values list and copy
    values$origOccs <- locs
    values$df <- values$origOccs
    
    inName <- isolate(spName)
    nameSplit <- length(unlist(strsplit(inName, " ")))
    
    if (nameSplit == 1 && !is.null(locs)) {
      x <- paste("* Please input both genus and species names. More than one species with this genus was found.")
    } else {if (nameSplit == 1 && is.null(locs)) {
      x <- paste("* Please input both genus and species names.")
    } else {if (nameSplit != 1 && is.null(locs)) {
      x <- paste0('* No records found for ', inName, ". Please check the spelling.")
    } else {if (nameSplit != 1 && !is.null(locs)) {
      x <- paste('* Total GBIF records for', values$origOccs[1,1], 'returned [', nrow(locs.in),
                 '] out of [', results$meta$count, '] total (limit ', occNum, '). 
                   Duplicated records removed [', nrow(locs.in) - nrow(locs), "]: Remaining records [", nrow(locs), "].")
    }}}}
    writeLog(x)
  }
  
  # MAPPING
  
  zoom2Occs()
  proxy %>% addCircleMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fillColor = 'red',
                             fillOpacity = 0.2, weight = 2, popup = ~pop,
                             layerId = 'origOccs', group = 'comp1')

}

getUserOccs <- function(csvPath) {
  inFile <- try(read.csv(csvPath, header = TRUE), silent=TRUE)  # read user csv
  if (class(inFile) == "try-error") {
    isolate(writeLog('* ERROR: The file could not be loaded (check the file requirements)'))
    return()
    }
  if (!all(c('species', 'longitude', 'latitude') %in% names(inFile))) {
    isolate(writeLog('* ERROR: Please input CSV file with columns "species", "longitude", "latitude".'))
    return()
  }
  values$inFile <- inFile  # store original table in values list
  
  #     # IN DEV: make dynamic field selections for ui user-defined kfold groups
  #     output$occgrpSel <- renderUI({
  #       selectInput('occgrp', 'Occurrence Group Field', names(inFile))
  #     })
  #     output$bggrpSel <- renderUI({
  #       selectInput('bggrp', 'Background Group Field', names(inFile))
  #     })
  
  # subset to only occs, not backg, and just fields that match df
  values$spname <- as.character(inFile$species[1])  # get species name
  inFile.occs <- inFile[inFile[,1] == values$spname,]  # limit to records with this name
  for (col in c("institutionCode", "country", "stateProvince", 
                "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
    if (!(col %in% names(inFile.occs))) inFile.occs[,col] <- NA
  }
  
  inFile.occs$origID <- row.names(inFile.occs)  # add col for IDs
  inFile.occs$pop <- unlist(apply(inFile.occs, 1, popUpContent))  # add col for map marker popup text
  
  # add user locs to existing origOccs and df, and remove duplicate records
  values$origOccs <- isolate({
    values$origOccs <- rbind(values$origOccs, inFile.occs)
    values$origOccs <- remDups(values$origOccs)
  })
  values$df <- isolate({
    values$df <- rbind(values$df, inFile.occs)
    values$df <- remDups(values$df)
  })
  isolate(writeLog("* User-specified CSV input."))
  # this makes an infinite loop. not sure why...
  #     x <- paste0("User input ", input$userCSV$name, " with [", nrow(values$df), "[ records.")
  #     values$log <- paste(values$log, x, sep='<br>')
  zoom2Occs()
  map_plotLocs(values$origOccs)
}

zoom2Occs <- function() {
  if (is.null(values$origOccs)) {return()}
  proxy %>% clearShapes()
  lati <- values$origOccs[,3]
  longi <- values$origOccs[,2]
  z <- smartZoom(longi, lati)
  proxy %>% fitBounds(z[1], z[2], z[3], z[4])
  
  # this section makes letter icons for occs based on basisOfRecord
  #     occIcons <- makeOccIcons()
  #     iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3,
  #                      UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6,
  #                      LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
  #     values$origOccs$basisNum <- unlist(iconList[values$origOccs$basisOfRecord])
  #     proxy %>% addMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
  #                          layerId = as.numeric(rownames(values$origOccs)),
  #                          icon = ~icons(occIcons[basisNum]))
}
