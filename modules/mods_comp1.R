source("functions.R")

getDbOccs <- function(spName, occNum) {

    writeLog(paste("... Searching", input$occDb, "..."))
    query <- occ(input$spName, input$occDb, limit=input$occNum, has_coords=TRUE)
    # dbOccsList <- list()
    # for (db in input$occDb) {
    dbOccs <- query[[input$occDb]]$data[[formatSpName(input$spName)]]
    if (nrow(dbOccs) == 0) {
      writeLog(paste("No records found in", input$occDb, "for", input$spName, "."))
      return()
    }
    dbOccs <- as.data.frame(dbOccs)
    names(dbOccs)[which(names(dbOccs) == 'name')] <- 'species'
    if (input$occDb == 'vertnet') {
      names(dbOccs)[which(names(dbOccs) == 'institutioncode')] <- 'institutionCode'
      names(dbOccs)[which(names(dbOccs) == 'stateprovince')] <- 'stateProvince'
      names(dbOccs)[which(names(dbOccs) == 'basisofrecord')] <- 'basisOfRecord'
      names(dbOccs)[which(names(dbOccs) == 'maximumelevationinmeters')] <- 'elevation'
    }
    if (input$occDb == 'bison') {
      names(dbOccs)[which(names(dbOccs) == 'scientificName')] <- 'species'
      names(dbOccs)[which(names(dbOccs) == 'calculatedState')] <- 'stateProvince'
      names(dbOccs)[which(names(dbOccs) == 'verbatimElevation')] <- 'elevation'
    }
    # if species not found, print message to log box
    if (query[[input$occDb]]$meta$found == 0) {
      writeLog(paste('* No records found for ', input$spName, ". Please check the spelling."))
    } else {
      # if species has positive number records remove duplicate records (reactive gbifOrig), and
      # select columns of interest and add columns for click query (reactive gbifSel)
      dbOccs %>% remDups()
      values$dbMod <- input$occDb
      dbOccs$longitude <- as.numeric(dbOccs$longitude)
      dbOccs$latitude <- as.numeric(dbOccs$latitude)
      dbOccs$origID <- row.names(dbOccs)
      dbOccs$pop <- unlist(apply(dbOccs, 1, popUpContent))
      values$origOccs <- dbOccs
      # check to see if columns of interest are in dbOccs, and then subset
      cols <- c("species", "longitude", "latitude","year", "institutionCode", "country", "stateProvince", 
                "locality", "elevation", "basisOfRecord")
      colsInDbOccs <- cols %in% names(dbOccs)
      cols <- cols[colsInDbOccs]
      dbOccs <- dbOccs[c(cols, "origID", "pop")]
      values$df <- dbOccs
    }
    
    # figure out how many separate names (components of scientific name) were entered
    nameSplit <- length(unlist(strsplit(input$spName, " ")))
    # if only one name entered and nothing returned, throw error
    # if only one name entered and records returned, throw warning to enter both names
    if (nameSplit == 1) {
      if (is.null(dbOccs)) {
        addLog <- paste("* Please input both genus and species names.")
      } else {
        addLog <- paste("* Please input both genus and species names. 
                        More than one species with this genus was found.")        
      }
      }
    # If more than one name entered and nothing returned, throw message and warning about spelling.
    # If more than one name entered and records returned, great! Send metadata message.
    if (nameSplit > 1) {
      if (is.null(dbOccs)) {
        addLog <- paste0('* No records found for ', input$spName, ". Please check the spelling.")
      } else {
        origRows <- nrow(query[[input$occDb]]$data[[formatSpName(input$spName)]])
        totRows <- query[[input$occDb]]$meta$found
        remainRows <- nrow(dbOccs)
        dupsRemoved <- origRows - remainRows
        addLog <- paste('* Total', input$occDb, 'records for', input$spName, 'returned [', origRows,
                        '] out of [', totRows, '] total (limit ', input$occNum, ').
                        Duplicated records removed [', dupsRemoved, "]: Remaining records [", remainRows, "].")
      }
      # }
      writeLog(addLog)
      # functionality for concatenating multiple db calls
      # add current dbOccs to the list
      #   dbOccsList[[db]] <- dbOccs
      # }
      # # rbind all the data frames together into one
      # dbOccsConcat <- do.call("rbind", dbOccsList)
      # if (length(input$occDb) > 1) {
      #   concat.orig <- nrow(dbOccsConcat)
      #   # remove records with duplicate coordinates
      #   dbOccsConcat <- dbOccsConcat[!duplicated(dbOccsConcat[,c('longitude', 'latitude')]),]
      #   concat.dupsRem <- nrow(dbOccsConcat)
      #   dupsRemNum <- concat.orig - concat.dupsRem
      #   writeLog(paste("Concatenated", paste(input$occDb, collapse=' and '), "."))
      #   if (dupsRemNum > 0) {
      #     writeLog(paste("Duplicated records removed [", dupsRemNum, "]: Remaining records [", concat.dupsRem, "]."))
      #   }      
    }
    # MAPPING
    proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
}
#   
#   # query database based on user input, remove duplicate records
#   writeLog(paste("... Searching", input$occDb, "..."))
#   values$spname <- spName  # record species name
#   results <- occ_search(scientificName = spName, limit = occNum, hasCoordinate = TRUE)
#   
#   # Control species not found
#   if (results$meta$count == 0) {
#     writeLog(paste('* No records found for ', spName, ". Please check the spelling."))
#   }
#   
#   if (results$meta$count != 0) {
#     cols <- c('name','decimalLongitude','decimalLatitude',
#               'institutionCode','country', 'stateProvince',
#               'locality', 'elevation', 'basisOfRecord')
#     results <- fixcols(cols, results)
#     locs.in <- results$data[!is.na(results$data[,3]),]
#     locs <- as.data.frame(remDups(locs.in))
#     values$gbifOrig <- locs
#     locs <- locs[,cols]  # limit to above columns
# 
#     names(locs)[1:3] <- c('species','longitude', 'latitude')    
#     locs$origID <- row.names(locs)
#     locs$pop <- unlist(apply(locs, 1, popUpContent))
#     # add locs to values list and copy
#     values$origOccs <- locs
#     values$df <- rbind(values$df, values$origOccs)
#     
#     inName <- isolate(spName)
#     nameSplit <- length(unlist(strsplit(inName, " ")))
#     
#     if (nameSplit == 1 && !is.null(locs)) {
#       x <- paste("* Please input both genus and species names. More than one species with this genus was found.")
#     } else {if (nameSplit == 1 && is.null(locs)) {
#       x <- paste("* Please input both genus and species names.")
#     } else {if (nameSplit != 1 && is.null(locs)) {
#       x <- paste0('* No records found for ', inName, ". Please check the spelling.")
#     } else {if (nameSplit != 1 && !is.null(locs)) {
#       x <- paste('* Total GBIF records for', values$origOccs[1,1], 'returned [', nrow(locs.in),
#                  '] out of [', results$meta$count, '] total (limit ', occNum, '). 
#                    Duplicated records removed [', nrow(locs.in) - nrow(locs), "]: Remaining records [", nrow(locs), "].")
#     }}}}
#     writeLog(x)
#   }
#   
#   # MAPPING
#   proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
# }

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
  isolate({
    values$origOccs <- rbind(values$origOccs, inFile.occs)
    values$origOccs <- remDups(values$origOccs)
  })
  isolate({
    values$df <- rbind(values$df, inFile.occs)
    values$df <- remDups(values$df)
  })
  isolate(writeLog("* User-specified CSV input."))
  # this makes an infinite loop. not sure why...
  #     x <- paste0("User input ", input$userCSV$name, " with [", nrow(values$df), "[ records.")
  #     values$log <- paste(values$log, x, sep='<br>')

  # MAPPING
  proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
}
