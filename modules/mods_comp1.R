source("functions.R")

getDbOccs <- function(spName, occNum) {

    writeLog(paste("... Searching", input$occDb, "..."))
    query <- occ(input$spName, input$occDb, limit=input$occNum, has_coords=TRUE)
    # record spName in values
    values$spname <- input$spName
    # create tag to signal db search
    values$mod_db <- TRUE
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

getUserOccs <- function(userCSV) {
  if (is.null(userCSV)) return()
  validate(need(userCSV, message = FALSE))
  csv <- read.csv(userCSV$datapath)
  if (!all(c('species', 'longitude', 'latitude') %in% names(csv))) {
    isolate({writeLog('* ERROR: Please input CSV file with columns "species", "longitude", "latitude".')})
  }
  isolate({writeLog(paste("* User-specified CSV file", userCSV$name, "was uploaded."))})
  # subset to only occs, not backg, and just fields that match df
  spName <- as.character(csv$species[1])  # get species name
  # record species name
  values$spName <- spName
  userOccs <- csv[csv[,1] == spName,]  # limit to records with this name
  # for (col in c("institutionCode", "country", "stateProvince", 
  #               "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
  #   if (!(col %in% names(userOccs))) userOccs[,col] <- NA
  # }
  
  userOccs$origID <- row.names(userOccs)  # add col for IDs
  userOccs$pop <- unlist(apply(userOccs, 1, popUpContent))  # add col for map marker popup text
  values$df <- userOccs
  values$origOccs <- userOccs
  
  # MAPPING
  proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
  
  isolate({writeLog("* User-specified CSV input.")}) 
}
