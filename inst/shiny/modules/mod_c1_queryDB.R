
queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             numericInput(ns("occNum"), "Set maximum number of occurrences", value = 100, min = 1))
  )
}

queryDb_MOD <- function(input, output, session, rvs) {
  spName <- reactive({
    n <- input$spName
    trimws(paste0(toupper(substring(n, 1, 1)), substring(n, 2, nchar(n))))
    })
  
  query <- reactive({
    req(input$spName)
    
    # record for later (RMD, etc.)
    rvs$occDb <- input$occDb
    rvs$occNum <- input$occNum
    rvs$spName <- spName()
    
    # figure out how many separate names (components of scientific name) were entered
    nameSplit <- length(unlist(strsplit(spName(), " ")))
    # if two names not entered, throw error and return
    if (nameSplit != 2) {
      rvs %>% writeLog(type = 'error', 'Please input both genus and species names.')
      return()
    }
    
    # query database
    withProgress(message = paste("Querying", input$occDb, "..."), {
      q <- spocc::occ(spName(), input$occDb, limit=input$occNum)
    })
    
    # if species not found, print message to log box and return
    if (q[[input$occDb]]$meta$found == 0) {
      rvs %>% writeLog(type = 'error', 'No records found for ', 
                     spName(), ". Please check the spelling.")
      shinyjs::disable("dlDbOccs")
      return()
    }
    shinyjs::enable("dlDbOccs")
    
    return(q)
  })
  
  dbOccs.tbl <- reactive({
    req(query())
    # extract occurrence tibble
    recs <- query()[[input$occDb]]$data[[formatSpName(spName())]]
    # make sure latitude and longitude are numeric (sometimes they aren't)
    recs$latitude <- as.numeric(recs$latitude)
    recs$longitude <- as.numeric(recs$longitude)
    
    rvs$occsOrig <- recs
    
    recs <- recs %>% dplyr::mutate(occID = row.names(recs))  # make new column for original ID
    
    return(recs)
  })
  
  dbOccs.coords <- reactive({
    req(dbOccs.tbl())
    # subset to just records with latitude and longitude
    recs <- dbOccs.tbl() %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
    if (nrow(recs) == 0) {
      rvs %>% writeLog(type = 'warning', 'No records with coordinates found in', 
                        input$occDb, "for", spName(), ".")
      return()
    }
    return(recs)
  })
  
  dbOccs.remDups <- reactive({
    req(dbOccs.coords())
    recs <- dbOccs.coords()
    recs.dups <- duplicated(recs %>% dplyr::select(longitude, latitude))
    recs <- recs[!recs.dups,]
    
    return(recs)
  })
  
  dbOccs.stdCols <- reactive({
    req(dbOccs.remDups())
    recs <- dbOccs.remDups()
    # standardize VertNet column names
    if (input$occDb == 'vertnet') {
      fields <- c('institutioncode', 'stateprovince', 'basisofrecord', 'catalognumber', 'maximumelevationinmeters')
      for (i in fields) {
        if (!(i %in% names(recs))) recs[i] <- NA
      }
      recs <- recs %>%
        dplyr::rename(institutionCode = institutioncode) %>%
        dplyr::rename(stateProvince = stateprovince) %>%
        dplyr::rename(basisOfRecord = basisofrecord) %>%
        dplyr::rename(catalogNumber = catalognumber) %>%
        dplyr::rename(elevation = maximumelevationinmeters)
    }
    
    # standardize BISON column names
    if (input$occDb == 'bison') {
      fields <- c('countryCode', 'ownerInstitutionCollectionCode', 'calculatedCounty', 'elevation')
      for (i in fields) {
        if (!(i %in% names(recs))) recs[i] <- NA
      }
      recs <- recs %>%
        dplyr::rename(country = countryCode) %>%
        dplyr::rename(institutionCode = ownerInstitutionCollectionCode) %>%
        dplyr::rename(locality = calculatedCounty)
    }
    
    return(recs)
  })
  
  dbOccs.out <- reactive({
    req(dbOccs.stdCols())
    recs <- dbOccs.stdCols()
    
    for (col in c("year", "institutionCode", "catalogNumber", "basisOfRecord", "country", "stateProvince",
                  "locality", "elevation")) {  # add all cols to match origOccs if not already there
      if (!(col %in% names(recs))) recs[,col] <- NA
    }
    
    # subset by key columns and make id and popup columns
    cols <- c("name", "longitude", "latitude", "year", "institutionCode", "catalogNumber", "basisOfRecord", 
              "country", "stateProvince", "locality", "elevation", "occID")
    recs <- recs %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(pop = unlist(apply(recs, 1, popUpContent)))  # make new column for leaflet marker popup content
    
    # get total number of records found in database
    totRows <- query()[[input$occDb]]$meta$found
    
    noCoordsRem <- nrow(dbOccs.tbl()) - nrow(dbOccs.coords())
    dupsRem <- nrow(dbOccs.coords()) - nrow(dbOccs.remDups())
    rvs %>% writeLog('Total', input$occDb, 'records for', spName(), 'returned [', nrow(dbOccs.tbl()),
                   '] out of [', totRows, '] total (limit ', input$occNum, ').
                   Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(recs), '].')
    
    return(recs)
  })
  
  
  return(dbOccs.out)
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
