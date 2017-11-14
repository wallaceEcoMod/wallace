#' c1_queryPaleoDb
#' 
#' \code{c1_queryPaleoDb} returns species occurrences
#' 
#' query paleobioDB or neotoma databases and returns the complete list of data, 
#' data with coordinates, and data with no duplicates
#' 
#' @param spName character species name. For paleobioDb it returns records associated with the specified taxonomic name, including any synonyms.
#' @param occDb character name of the paleontological database. Either "PaleobioDB" or "neotoma"
#' @param occNum integer maximum number of records
#' @param timeInterval character Either "LGM" (Last Glacial Maximum) or "Holo" (Holocene). For PaleobioDB only Holocene is allowed.
#' @param rvs list of parameters
#' 
#' 

c1_queryPaleoDb <- function(spName, occDb, occNum, timeInterval, rvs) {
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logs %>% writeLog(type = 'error', 'Please input both genus and species names.')
    return()
  }
  
  if (occDb == "PaleobioDB") {
    if (timeInterval == "LGM") {
      logs %>% writeLog(type = 'error', 'PaleobioDB does not have separate LGM records. You can donwload Holocene records only')
      return()
    }
    # query database
    withProgress(message = paste("Querying", occDb, "..."), {
      occsOrig <- try(paleobioDB::pbdb_occurrences(taxon_name=spName, limit=occNum, vocab="pbdb",  
                                                   max_ma= 0.02, show=c("coords", "bin", "loc")), silent =TRUE)
    })
    
  }
  
  if (class(occsOrig) == "try-error") {
    logs %>% writeLog(type = 'error', 'No records found for ', spName, ". Please check the spelling.") 
  } else {
    # get total number of records found in database 
    totRows <- nrow(occsOrig)
    # extract occurrence tibble
    names(occsOrig)[names(occsOrig) == "lng"] <- "longitude"
    names(occsOrig)[names(occsOrig) == "lat"] <- "latitude"
    names(occsOrig)[names(occsOrig) == "early_interval"] <- "time_interval"
    names(occsOrig)[names(occsOrig) == "cc"] <- "country"
    
    # make new column for original ID
    occsOrig$occID <- 1: nrow(occsOrig)
    
    # subset to just records with latitude and longitude
    occsXY <-  occsOrig[!is.na(occsOrig$longitude) & !is.na(occsOrig$latitude),]
    if (nrow(occsXY) == 0) {
      logs %>% writeLog(type = 'warning', 'No records with coordinates found in', occDb, "for", spName, ".")
    }
    
    dups <- duplicated(occsXY[,c('longitude','latitude')])
    occs <- occsXY[!dups,]
    
    # subset by key columns and make id and popup columns
    cols <- c("taxon_name", "longitude", "latitude","time_interval", "collection_no", "country", 
              "collection_no", "record_type", "occID")
    occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent)))  # make new column for leaflet marker popup content
    
  }
  
  if (timeInterval == "Holo") {
    # query database
    withProgress(message = paste("Querying", occDb, "..."), {
      q <- paleobioDB::pbdb_occurrences(taxon_name=spName, limit=occNum, vocab="pbdb",  
                                        max_ma= 0.02)
     
    })
  }
  
  
  # if (occDb=="neotoma") {
  #   if (timeInterval == "LGM") {
  #     query database
  #     withProgress(message = paste("Querying", occDb, "..."), {
  #       q <- neotoma::get_dataset(taxonname= spName,
  #                                 ageold = 25000, ageyoung=15000)
  #       q <- neotoma::get_dataset(datasettype="pollen", ageold = 25000,
  #                                 ageyoung=15000) %>% neotoma::get_download() %>% neotoma::compile_taxa('P25') %>% neotoma::compile_downloads() %>% filter(ageyoung < 25000 & ageold > 15000)
  #       # hacer el objeto de salida! busca las columnas que te molan
  #       str (q[[1]][[1]])
  #       str (q[[1]][[2]])
  #     })
  #   }
  # }
 
  
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  
  dupsRem <- nrow(occsXY) - nrow(occs)
  logs %>% writeLog('Total', occDb, 'records for', spName, 'returned [', nrow(occsOrig),
                    '] out of [', totRows, '] total (limit ', occNum, ').',
                    'Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  return(occs)
}

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$strong(paste("Longitude:", lon))
  ))
}
