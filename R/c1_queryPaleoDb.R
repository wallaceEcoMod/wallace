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
  
  
  if (occDb=="PaleobioDB"){
    if (timeInterval == "LGM") {
      logs %>% writeLog(type = 'error', 'PaleobioDB does not have separate LGM records. You can donwload Holocene records only')
      return()
    }
    # query database
    withProgress(message = paste("Querying", occDb, "..."), {
      q <- try (paleobioDB::pbdb_occurrences(taxon_name=spName, limit=occNum, vocab="pbdb",  
                                        max_ma= 0.02, show=c("coords", "bin", "loc")), silent =TRUE)
    })
    
  }
  

  if (class (q) == "try-error"){
    logs %>% writeLog(type = 'error', 'No records found for ', spName, ". Please check the spelling.") 
  } else {
   
    # get total number of records found in database 
  totRows <- nrow (q)
  # extract occurrence tibble
  names (q) [names (q)=="lng"]<- "longitude"
  names (q) [names (q)=="lat"]<- "latitude"
  names (q) [names (q)=="early_interval"]<- "time_interval"
  names (q) [names (q)=="cc"]<- "country"
  
  occsOrig <- q
  # make new column for original ID
  occsOrig$occID <- 1: nrow(occsOrig)
  
  
  # subset to just records with latitude and longitude
  occsXY<-  occsOrig [!is.na ( occsOrig$longitude) & !is.na ( occsOrig$latitude), ]
  if (nrow(occsXY) == 0) {
    logs %>% writeLog(type = 'warning', 'No records with coordinates found in', occDb, "for", spName, ".")
  }
  
  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups,]
 
  # subset by key columns and make id and popup columns
  cols <- c("occID", "taxon_name", "longitude", "latitude","time_interval", "collection_no", "country", 
            "collection_no", "record_type")
  occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent)))  # make new column for leaflet marker popup content
  
  }
  
  
if (occDb=="neotoma"){
# query database
withProgress(message = paste("Querying", occDb, "..."), {
q <- paleobioDB::pbdb_occurrences(taxon_name=spName, limit=occNum, vocab="pbdb",  
  max_ma= 0.02)
 })
  
}
  
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  
  dupsRem <- nrow(occsXY) - nrow(occs)
  logs %>% writeLog('Total', occDb, 'records for', spName, 'returned [', nrow(occsOrig),
                    '] out of [', totRows, '] total (limit ', occNum, ').
                   Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  return(list(occsOrig=occsOrig, occsXY=occsXY, occs=occs)) 
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
