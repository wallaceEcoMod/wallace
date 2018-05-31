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
#' 
#' 

c1_queryPaleoDb <- function(spName, occDb, occNum, timeInterval, shinyLogs = NULL) {
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    shinyLogs %>% writeLog(type = 'error', 'Please input both genus and species names.')
    return()
  }
  
  if (occDb == "PaleobioDB") {
   
    if (timeInterval == "LGM") {
      shinyLogs %>% writeLog(type = 'error', 'PaleobioDB does not have separate LGM records. You can only download Holocene records.')
      return()
    } else if (timeInterval == "Holo") {
      
      # query database
      smartProgress(shinyLogs, message = paste0("Querying ", occDb, " ..."), {
        occsOrig <- try(paleobioDB::pbdb_occurrences(taxon_name=spName, limit=occNum, vocab="pbdb",  
                                                     max_ma= 0.02, show=c("coords", "bin", "loc")), silent =TRUE)
      })
    }
  }
  
  if (occDb == "Neotoma") {
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
  }
  
  if (class(occsOrig) == "try-error") {
    shinyLogs %>% writeLog(type = 'error', 'No records found for ', em(spName), ". Please check the spelling.") 
    return()
  }
  

  # get total number of records found in database 
  totRows <- nrow(occsOrig)
  # extract occurrence tibble
  names(occsOrig)[names(occsOrig) == "lng"] <- "longitude"
  names(occsOrig)[names(occsOrig) == "lat"] <- "latitude"
  names(occsOrig)[names(occsOrig) == "early_interval"] <- "time_interval"
  names(occsOrig)[names(occsOrig) == "cc"] <- "country"
  occsOrig$taxon_name <- as.character(occsOrig$taxon_name)
  
  # make new column for original ID
  occsOrig$occID <- 1: nrow(occsOrig)
  
  # subset to just records with latitude and longitude
  occsXY <-  occsOrig[!is.na(occsOrig$longitude) & !is.na(occsOrig$latitude),]
  if (nrow(occsXY) == 0) {
    shinyLogs %>% writeLog(type = 'warning', 'No records with coordinates found in', occDb, "for", em(spName), ".")
  }

  
  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups,]
  
  # subset by key columns and make id and popup columns
  cols <- c("taxon_name", "longitude", "latitude","time_interval", "collection_no", "country", 
            "collection_no", "record_type", "occID")
  occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>% # make new column for leaflet marker popup content
    dplyr::arrange_(cols)
  
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  
  dupsRem <- nrow(occsXY) - nrow(occs)
  shinyLogs %>% writeLog('Total ', occDb, ' records for ', em(spName), ' returned [', nrow(occsOrig),
                    '] out of [', totRows, '] total (limit ', occNum, ').',
                    'Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  return(list(orig=occsOrig, cleaned=occs))
}
