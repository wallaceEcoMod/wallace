#' Query online database for species occurrence records.
#'
#' \code{queryDb} returns a formatted tibble of species occurrences with a selection of appropriate fields.
#'
#' This function is called by the module mod_c1_queryDb to query a database for
#' species occurrence records, subset to only those records with coordinates,
#' remove records with duplicate coordinates, and select some columns with fields
#' appropriate to studies in biogeography.
#'
#' @param spName species Latin name, with format "Genus species"
#' @param occDb biodiversity database to query; current choices are "gbif", 
#' "vertnet", and "bison"
#' @param occNum maximum number of occurrence records to return
#' @return formatted tibble of species occurrence records 
#'
#' @examples
#' queryDb(spName = "Tremarctos ornatus", occDb = "gbif", occNum = 100)

c1_queryDb <- function(spName, occDb, occNum, logs) {
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logs %>% writeLog(type = 'error', 'Please input both genus and species names.')
    return()
  }
  
  # query database
  withProgress(message = paste("Querying", occDb, "..."), {
    q <- spocc::occ(spName, occDb, limit=occNum)
  })
  
  # get total number of records found in database
  totRows <- q[[occDb]]$meta$found
  
  # if species not found, print message to log box and return
  if (q[[occDb]]$meta$found == 0) {
    logs %>% writeLog(type = 'error', 'No records found for ', spName, ". Please check the spelling.")
  }
  # extract occurrence tibble
  occsOrig <- q[[occDb]]$data[[formatSpName(spName)]]
  # make sure latitude and longitude are numeric (sometimes they aren't)
  occsOrig$latitude <- as.numeric(occsOrig$latitude)
  occsOrig$longitude <- as.numeric(occsOrig$longitude)
  
  # make new column for original ID
  occsOrig$occID <- row.names(occsOrig)
  
  # subset to just records with latitude and longitude
  occsXY <- occsOrig[!is.na(occsOrig$latitude) & !is.na(occsOrig$longitude),]
  if (nrow(occsXY) == 0) {
    logs %>% writeLog(type = 'warning', 'No records with coordinates found in', occDb, "for", spName, ".")
  }
  
  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups,]
  
  # standardize VertNet column names
  if (occDb == 'vertnet') {
    fields <- c('institutioncode', 'stateprovince', 'basisofrecord', 'maximumelevationinmeters')
    for (i in fields) {
      if (!(i %in% names(occs))) occs[i] <- NA
    }
    occs <- occs %>% dplyr::rename(institutionCode = institutioncode, stateProvince = stateprovince, basisOfRecord = basisofrecord, elevation = maximumelevationinmeters)
  }
  
  # standardize BISON column names
  if (occDb == 'bison') {
    fields <- c('countryCode', 'ownerInstitutionCollectionCode', 'calculatedCounty', 'elevation')
    for (i in fields) {
      if (!(i %in% names(occs))) occs[i] <- NA
    }
    occs <- occs %>% dplyr::rename(country = countryCode, institutionCode = ownerInstitutionCollectionCode, locality = calculatedCounty)
  }
  
  for (col in c("year", "institutionCode", "country", "stateProvince",
                "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
    if (!(col %in% names(occs))) occs[,col] <- NA
  }
  
  # subset by key columns and make id and popup columns
  cols <- c("name", "longitude", "latitude","year", "institutionCode", "country", "stateProvince",
            "locality", "elevation", "basisOfRecord", "occID")
  occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent)))  # make new column for leaflet marker popup content
  
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  dupsRem <- nrow(occsXY) - nrow(occs)
  logs %>% writeLog('Total', occDb, 'records for', spName, 'returned [', nrow(occsOrig),
           '] out of [', totRows, '] total (limit ', occNum, ').
                   Records without coordinates removed [', noCoordsRem, '].
                   Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  
  return(list(occsOrig=occsOrig, occsXY=occsXY, occs=occs))
}

formatSpName <- function(spName) paste(strsplit(spName, split=' ')[[1]], collapse='_')

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
