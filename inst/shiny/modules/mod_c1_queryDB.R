
queryDB_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database:",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison')),
    textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species'),
    shinyBS::bsPopover(ns('spName'), title = 'Tip',
                       'Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
                       placement = 'right', options = list(container = "body")),
    sliderInput(ns("occNum"), "Maximum number of occurrences:", min = 1, max = 500, value = 50),
    shinyBS::bsPopover(ns('occNum'), title = 'Tip',
                       'Maximum number of occurrences recovered from databases. 
                       Downloaded records are not sorted randomly: rows are 
                       always consistent between downloads.',
                       placement = 'right', options = list(container = "body"))
  )
}

queryDB <- function(input, output, session, logs) {
  
  spName <- reactive({spName <- trimws(input$spName)})
  
  query <- reactive({
    req(input$spName)
    # figure out how many separate names (components of scientific name) were entered
    nameSplit <- length(unlist(strsplit(spName(), " ")))
    # if two names not entered, throw error and return
    if (nameSplit != 2) {
      logs %>% writeLog('<font color="red"><b>! ERROR</b></font> : Please input both genus and species names.')
      return()
    }
    
    # query database
    q <- spocc::occ(spName(), input$occDb, limit=input$occNum)
    
    # if species not found, print message to log box and return
    if (q[[input$occDb]]$meta$found == 0) {
      logs %>% writeLog('<font color="red"><b>! ERROR</b></font> : No records found for ', 
                     spName(), ". Please check the spelling.")
      shinyjs::disable("dlDbOccs")
      return()
    }
    shinyjs::enable("dlDbOccs")
    
    return(q)
  })
  
  dbOccs <- reactive({
    req(query())
    # extract occurrence tibble
    occs <- query()[[input$occDb]]$data[[formatSpName(spName())]]
    # make sure latitude and longitude are numeric (sometimes they aren't)
    occs$latitude <- as.numeric(occs$latitude)
    occs$longitude <- as.numeric(occs$longitude)
    return(occs)
  })
  
  dbOccs.coords <- reactive({
    req(dbOccs())
    # subset to just records with latitude and longitude
    occs <- dbOccs() %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
    if (nrow(occs) == 0) {
      logs %>% writeLog('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found in', input$occDb, "for", spName(), ".")
      return()
    }
    return(occs)
  })
  
  dbOccs.remDups <- reactive({
    req(dbOccs.coords())
    occs <- dbOccs.coords()
    occs.dups <- duplicated(occs %>% dplyr::select(longitude, latitude))
    occs <- occs[!occs.dups,]
    
    return(occs)
  })
  
  dbOccs.stdCols <- reactive({
    req(dbOccs.remDups())
    occs <- dbOccs.remDups()
    # standardize VertNet column names
    if (input$occDb == 'vertnet') {
      fields <- c('institutioncode', 'stateprovince', 'basisofrecord', 'maximumelevationinmeters')
      for (i in fields) {
        if (!(i %in% names(occs))) occs[i] <- NA
      }
      occs <- occs %>%
        dplyr::rename(institutionCode = institutioncode) %>%
        dplyr::rename(stateProvince = stateprovince) %>%
        dplyr::rename(basisOfRecord = basisofrecord) %>%
        dplyr::rename(elevation = maximumelevationinmeters)
    }
    
    # standardize BISON column names
    if (input$occDb == 'bison') {
      fields <- c('countryCode', 'ownerInstitutionCollectionCode', 'calculatedCounty', 'elevation')
      for (i in fields) {
        if (!(i %in% names(occs))) occs[i] <- NA
      }
      occs <- occs %>%
        dplyr::rename(country = countryCode) %>%
        dplyr::rename(institutionCode = ownerInstitutionCollectionCode) %>%
        dplyr::rename(locality = calculatedCounty)
    }
    
    return(occs)
  })
  
  dbOccs.out <- reactive({
    req(dbOccs.stdCols())
    occs <- dbOccs.stdCols()
    
    # subset by key columns and make id and popup columns
    cols <- c("name", "longitude", "latitude","year", "institutionCode", "country", "stateProvince",
              "locality", "elevation", "basisOfRecord")
    occs <- occs %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(origID = row.names(occs))  # make new column for ID
    
    occs <- occs %>% dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent)))  # make new column for leaflet marker popup content
    
    # get total number of records found in database
    totRows <- query()[[input$occDb]]$meta$found
    
    noCoordsRem <- nrow(dbOccs()) - nrow(dbOccs.coords())
    dupsRem <- nrow(dbOccs.coords()) - nrow(dbOccs.remDups())
    logs %>% writeLog('> Total', input$occDb, 'records for', spName(), 'returned [', nrow(dbOccs()),
                   '] out of [', totRows, '] total (limit ', input$occNum, ').
                   Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
    
    return(occs)
  })
  
  return(dbOccs.out)
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
  # create tag to signal no db search
  values$mod_db <- FALSE
  # limit to records with this name
  userOccs <- csv[csv[,1] == spName,]  

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
  map %>% 
    clearMarkers() %>% 
    map_plotLocs(values$df) %>%
    zoom2Occs(values$df)
}

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
