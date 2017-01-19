getDbOccs <- function(spName, occNum) {

  # trim whitespace (blank spaces) from species name
  spName <- trimws(input$spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Please input both genus and species names.')
    return()
  }

  writeLog(paste("... Searching", input$occDb, "..."))
  # query database
  query <- spocc::occ(spName, input$occDb, limit=input$occNum)

  # if species not found, print message to log box and return
  if (query[[input$occDb]]$meta$found == 0) {
    writeLog(paste('! No records found for ', spName, ". Please check the spelling."))
    values$df <- NULL  # reset df
    shinyjs::disable("dlDbOccs")
    return()
  }

  # record spName and dbMod in values
  values$spName <- spName
  values$dbMod <- input$occDb
  # create tag to signal db search
  values$mod_db <- TRUE
  # extract occurrence tibble
  dbOccs.orig <- query[[input$occDb]]$data[[formatSpName(spName)]]
  # make sure latitude and longitude are numeric (sometimes they aren't)
  dbOccs.orig$latitude <- as.numeric(dbOccs.orig$latitude)
  dbOccs.orig$longitude <- as.numeric(dbOccs.orig$longitude)

  # store dbOccs.orig in values list
  values$df.orig <- dbOccs.orig

  # get number of original rows
  dbOccs.orig.nrows <- nrow(dbOccs.orig)
  # get total number of records found in database
  totRows <- query[[input$occDb]]$meta$found

  # subset to just records with latitude and longitude
  dbOccs <- dbOccs.orig %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  if (nrow(dbOccs) == 0) {
    writeLog(paste('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found in', input$occDb, "for", spName, "."))
    return()
  }

  # standardize column names
  if (input$occDb == 'vertnet') {
    dbOccs <- dbOccs %>%
      dplyr::rename(institutionCode = institutioncode) %>%
      dplyr::rename(stateProvince = stateprovince) %>%
      dplyr::rename(basisOfRecord = basisofrecord) %>%
      dplyr::rename(elevation = maximumelevationinmeters)
  }
  # standardize column names
  if (input$occDb == 'bison') {
    dbOccs <- dbOccs %>%
      dplyr::rename(country = countryCode) %>%
      dplyr::rename(institutionCode = ownerInstitutionCollectionCode) %>%
      dplyr::rename(locality = calculatedCounty) %>%
      dplyr::mutate(elevation = NULL)
  }

  # remove duplicate records
  dbOccsWithDups.nrows <- nrow(dbOccs)
  dbOccs %>% remDups()
  dbOccsNoDups.nrows <- nrow(dbOccs)
  # make sure the class is numeric for coordinates
  dbOccs$longitude <- as.numeric(dbOccs$longitude)
  dbOccs$latitude <- as.numeric(dbOccs$latitude)

  # subset by key columns and make id and popup columns
  cols <- c("name", "longitude", "latitude","year", "institutionCode", "country", "stateProvince",
            "locality", "elevation", "basisOfRecord")
  dbOccs <- dbOccs %>%
    dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(origID = row.names(dbOccs))  # make new column for ID
  dbOccs <- dbOccs %>% dplyr::mutate(pop = unlist(apply(dbOccs, 1, popUpContent)))  # make new column for leaflet marker popup content

  # origOccs is the unmodified occs, to preserve in comp2 when points are modified
  values$df <- values$origOccs <- dbOccs

  noCoordsRemoved <- dbOccs.orig.nrows - dbOccsWithDups.nrows
  dupsRemoved <- dbOccsWithDups.nrows - dbOccsNoDups.nrows
  writeLog(paste('> Total', input$occDb, 'records for', spName, 'returned [', dbOccs.orig.nrows,
                 '] out of [', totRows, '] total (limit ', input$occNum, ').
                  Records without coordinates removed [', noCoordsRemoved, '].
                  Duplicated records removed [', dupsRemoved, ']. Remaining records [', dbOccsNoDups.nrows, '].'))
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
  # MAPPING
  proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
}

getUserOccs <- function(userCSV) {
  if (is.null(userCSV)) return()
  validate(need(userCSV, message = FALSE))
  csv <- read.csv(userCSV$datapath)
  if (!all(c('name', 'longitude', 'latitude') %in% names(csv))) {
    isolate({writeLog('<font color="red"><b>! ERROR</b></font> : Please input CSV file with columns "name", "longitude", "latitude".')})
    return()
  }

  # subset to only occs, not backg, and just fields that match df
  spName <- as.character(csv$name[1])  # get species name
  # trim whitespace (blank spaces) from species name
  spName <- trimws(spName)
  # record species name
  values$spName <- spName
  userOccs <- csv[csv[,1] == spName,]  # limit to records with this name

  # subset to just records with latitude and longitude
  userOccs <- userOccs %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  if (nrow(userOccs) == 0) {
    writeLog(paste('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found in', userCSV$name, "for", spName, "."))
    return()
  }

  isolate({writeLog(paste("> User-specified CSV file", userCSV$name, "with total of", nrow(userOccs),
                          "records with coordinates was uploaded."))})

  # for (col in c("institutionCode", "country", "stateProvince",
  #               "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
  #   if (!(col %in% names(userOccs))) userOccs[,col] <- NA
  # }

  userOccs$origID <- row.names(userOccs)  # add col for IDs
  userOccs$pop <- unlist(apply(userOccs, 1, popUpContent))  # add col for map marker popup text

  # origOccs is the unmodified occs, to preserve in comp2 when points are modified
  values$df <- values$origOccs <- userOccs

  # MAPPING
  proxy %>% zoom2Occs(values$df) %>% map_plotLocs(values$df)
}
