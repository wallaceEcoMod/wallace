#' Query online database for species occurrence records.
#'
#' \code{c1_queryDb} returns a formatted tibble of species occurrences with a selection of appropriate fields.
#'
#' This function is called by the module mod_c1_queryDb to query a database for
#' species occurrence records, subset to only those records with coordinates,
#' remove records with duplicate coordinates, and select some columns with fields
#' appropriate to studies in biogeography.
#'
#' @param spName character species Latin name, with format "Genus species"
#' @param occDb character biodiversity database to query; current choices are 
#' "gbif", "vertnet", and "bison"
#' @param occNum numeric maximum number of occurrence records to return
#' @param shinyLogs insert the shinyLogs reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param doCitations set TRUE to use `occCite` to get a complete list of original
#'  data sources in a citable format
#' @param gbifUser specify only if using `occCite` with GBIF to get a complete list
#'  of original data sources in a citable format. This, as well as `gbifEmail` 
#'  and `gbifPW` are constraints imposed by GBIF to obtain the complete set of 
#'  metadata associated with occurrence records and is not stored or used by 
#'  `wallace` for any other purposes.
#' @param gbifEmail  specify only if using `occCite` with GBIF to get a 
#' complete list of original data sources in a citable format.
#' @param gbifPW=NULL  specify only if using `occCite` with GBIF to get a complete 
#' list of original data sources in a citable format.
#' @return formatted tibble of species occurrence records 
#'
#' @examples
#' c1_queryDb(spName = "Tremarctos ornatus", occDb = "gbif", occNum = 100)
#' @export

#c1_queryDb <- function(spName, occDb, occNum, shinyLogs=NULL) {
c1_queryDb <- function(spName, occDb, occNum, doCitations = F, gbifUser = NULL, 
                       gbifEmail = NULL, gbifPW = NULL, shinyLogs = NULL) {
  # capitalize genus name if not already, trim whitespace
  spName <- trimws(paste0(toupper(substring(spName, 1, 1)), 
                          substring(spName, 2, nchar(spName))))  
  
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    shinyLogs %>% writeLog(type = 'error', 'Please input both genus and species
                           names.')
    return()
  }

  # query database
  smartProgress(shinyLogs, message = paste0("Querying ", occDb, " for ", 
                                            spName, "..."),{
     if (occDb == 'bison' | occDb == 'vertnet') {
      q <- spocc::occ(spName, occDb, limit = occNum)
      myOccCitations <- NULL
    } else if (occDb == 'gbif') {
      if (doCitations == FALSE) {
        q <- spocc::occ(spName, occDb, limit = occNum)
        myOccCitations <- NULL
      } else if (doCitations == TRUE) {
        if(any(unlist(lapply(list(gbifUser, gbifEmail, gbifPW), is.null)))) {
          shinyLogs %>% writeLog('error', 'Please specify your GBIF username, 
                                 email, and password. This is needed to get
                                 citations for occurrence records. Wallace does
                                 not store your information or use it for
                                 anything else.')
          return()
        }
        myBTO <- occCite::studyTaxonList(x = spName, datasources = "NCBI")
        login <- occCite::GBIFLoginManager(user = gbifUser, email = gbifEmail, 
                                           pwd = gbifPW)
        myBTO <- occCite::occQuery(x = myBTO, GBIFLogin = login, limit = occNum)
        myOccCitations <- occCite::occCitation(myBTO)
        # make something with the same slots as spocc that we use
        q <- list(gbif = list(meta = list(found = NULL),
                              data = list(formatSpName(spName))))
        q[[occDb]]$meta$found <- 
          nrow(myBTO@occResults[[spName]][['GBIF']][['OccurrenceTable']])
        q[[occDb]]$data[[formatSpName(spName)]] <- 
          myBTO@occResults[[spName]][['GBIF']][['OccurrenceTable']]
      }
    } else if (occDb == 'bien') {
      myBTO <- occCite::studyTaxonList(x = spName, datasources = "NCBI")
      myBTO <- occCite::occQuery(x = myBTO, datasources = 'bien', limit = occNum)
      myOccCitations <- NULL
      # make something with the same slots as spocc that we use
      q <- list(bien = list(meta = list(found = NULL),
                            data = list(formatSpName(spName))))
      q[[occDb]]$meta$found <- 
        nrow(myBTO@occResults[[spName]][['BIEN']][['OccurrenceTable']])
      q[[occDb]]$data[[formatSpName(spName)]] <- 
        myBTO@occResults[[spName]][['BIEN']][['OccurrenceTable']]
    }
 })
  
  # get total number of records found in database
  totRows <- q[[occDb]]$meta$found
  
  # if species not found, print message to log box and return
  if (q[[occDb]]$meta$found == 0) {
    shinyLogs %>% writeLog(type = 'error', 'No records found for ', em(spName), 
                           ". Please check the spelling.")
    return()
  }
  # extract occurrence tibble
  occsOrig <- q[[occDb]]$data[[formatSpName(spName)]]
  # make sure latitude and longitude are numeric (sometimes they aren't)
  occsOrig$latitude <- as.numeric(occsOrig$latitude)
  occsOrig$longitude <- as.numeric(occsOrig$longitude)
  
  # make new column for original ID
  occsOrig$occID <- as.numeric(row.names(occsOrig))
  
  # delete colums with list to avoid conflict
  occsOrig["networkKeys"] <- NULL
  
  # subset to just records with latitude and longitude
  occsXY <- occsOrig[!is.na(occsOrig$latitude) & !is.na(occsOrig$longitude),]
  if (nrow(occsXY) == 0) {
    shinyLogs %>% writeLog(type = 'warning', 'No records with coordinates found 
                           in ', occDb, " for ", em(spName), ".")
    return()
  }
  
  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups,]
  
  if (occDb == 'gbif') {
    fields <- c("name", "longitude", "latitude", "country", "stateProvince",
                "locality", "year", "basisOfRecord", "institutionCode", "elevation",
                "coordinateUncertaintyInMeters")
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name, 
                                   state_province = stateProvince, 
                                   record_type = basisOfRecord,
                                   institution_code = institutionCode,
                                   uncertainty = coordinateUncertaintyInMeters) 
} else if (occDb == 'vertnet') { # standardize VertNet column names
    fields <- c("name", "longitude", "latitude", "country", "stateprovince",
                "locality", "year", "basisofrecord", "institutioncode", 
                "maximumelevationinmeters", "coordinateuncertaintyinmeters")
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name,
                                   state_province = stateprovince, 
                                   record_type = basisofrecord, 
                                   institution_code = institutioncode, 
                                   elevation = maximumelevationinmeters,
                                   uncertainty = coordinateuncertaintyinmeters)
  } else if (occDb == 'bison') { # standardize BISON column names
    fields <- c("providedScientificName", "longitude", "latitude", "countryCode",
                "stateProvince", "verbatimLocality", "year", "basisOfRecord",
                "ownerInstitutionCollectionCode", "verbatimElevation", 
                "coordinateUncertaintyInMeters")
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = providedScientificName,
                                   country = countryCode, 
                                   state_province = stateProvince,
                                   locality = verbatimLocality,
                                   record_type = basisOfRecord,
                                   institution_code = 
                                     ownerInstitutionCollectionCode,
                                   elevation = verbatimElevation,
                                   uncertainty = coordinateUncertaintyInMeters)
  } else if (occDb == 'bien') {
    fields <- c("name", "longitude", "latitude", "country",
                "state_province", "locality", "year", "record_type",
                "institution_code", "elevation", 
                "uncertainty")
    # occCite field requirements (no downloaded by occCite) "country", 
    # "state_province", "locality", "year", "record_type", "institution_code",
    # "elevation", "uncertainty"
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name)
  }
  
  # subset by key columns and make id and popup columns
  cols <- c("occID", "taxon_name", "longitude", "latitude", "country", 
            "state_province", "locality", "year", "record_type", "institution_code",
            "elevation", "uncertainty")
  occs <- occs %>% 
    dplyr::select(dplyr::one_of(cols)) %>%
    # make new column for leaflet marker popup content
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%  
    dplyr::arrange_(cols)

  # subset by key columns and make id and popup columns
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  dupsRem <- nrow(occsXY) - nrow(occs)
  
  shinyLogs %>% writeLog('Total ', occDb, ' records for ', em(spName), ' returned 
                         [', nrow(occsOrig), '] out of [', totRows, '] total 
                         (limit ', occNum, '). Records without coordinates 
                         removed [', noCoordsRem, ']. Duplicated records removed
                         [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned = occs))
}

