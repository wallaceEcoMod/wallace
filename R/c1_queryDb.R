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
#' @param occDb character biodiversity database to query; current choices are "gbif", 
#' "vertnet", and "bison"
#' @param occNum numeric maximum number of occurrence records to return
#' @param shinyLogs insert the shinyLogs reactive list here for running in shiny, otherwise leave the default NULL
#' @return formatted tibble of species occurrence records 
#'
#' @examples
#' c1_queryDb(spName = "Tremarctos ornatus", occDb = "gbif", occNum = 100)
#' @export

c1_queryDb <- function(spName, occDb, occNum, shinyLogs=NULL) {
  # capitalize genus name if not already, trim whitespace
  spName <- trimws(paste0(toupper(substring(spName, 1, 1)), substring(spName, 2, nchar(spName))))  
  
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    shinyLogs %>% writeLog(type = 'error', 'Please input both genus and species names.')
    return()
  }

  # query database
  smartProgress(shinyLogs, message = paste0("Querying ", occDb, " for ", spName, "..."), {
    q <- spocc::occ(spName, occDb, limit=occNum)
  })
  
  # get total number of records found in database
  totRows <- q[[occDb]]$meta$found
  
  # if species not found, print message to log box and return
  if (q[[occDb]]$meta$found == 0) {
    shinyLogs %>% writeLog(type = 'error', 'No records found for ', em(spName), ". Please check the spelling.")
    return()
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
    shinyLogs %>% writeLog(type = 'warning', 'No records with coordinates found in', occDb, "for", em(spName), ".")
    return()
  }
  
  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups,]
  
  if (occDb == 'gbif') {
    fields <- c('institutionCode', 'stateProvince', 'basisOfRecord', "country", "locality", "elevation")
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name, 
                                   institution_code = institutionCode, 
                                   state_province = stateProvince, 
                                   record_type = basisOfRecord)
    # standardize VertNet column names
  } else if (occDb == 'vertnet') {
    fields <- c('institutioncode', 'stateprovince', 'basisofrecord', 'maximumelevationinmeters')
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(institution_code = institutioncode, state_province = stateprovince, record_type = basisofrecord, elevation = maximumelevationinmeters)
    # standardize BISON column names
  } else if (occDb == 'bison') {
    fields <- c('countryCode', 'ownerInstitutionCollectionCode', 'calculatedCounty', 'elevation')
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(country = countryCode, institution_code = ownerInstitutionCollectionCode, locality = calculatedCounty)
  }
  
  # subset by key columns and make id and popup columns
  cols <- c("taxon_name", "longitude", "latitude",  "occID", "year", "institution_code", "country", "state_province",
            "locality", "elevation", "record_type")
  occs <- occs %>% 
    dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%  # make new column for leaflet marker popup content
    dplyr::arrange_(cols)
  
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  dupsRem <- nrow(occsXY) - nrow(occs)
  
  shinyLogs %>% writeLog('Total ', occDb, ' records for ', em(spName), ' returned [', nrow(occsOrig),
                    '] out of [', totRows, '] total (limit ', occNum, ').
                    Records without coordinates removed [', noCoordsRem, '].
                    Duplicated records removed [', dupsRem, ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned=occs))
}

