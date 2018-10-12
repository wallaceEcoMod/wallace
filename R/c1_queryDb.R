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
#' @param doCitations set TRUE to use `occCite` to get a complete list of original data sources in a citable format
#' @param gbifUser specify only if using `occCite` with GBIF to get a complete list of original data sources in a citable format. This, as well as `gbifEmail` and `gbifPW` are constraints imposed by GBIF to obtain the complete set of metadata associated with occurrence records and is not stored or used by `wallace` for any other purposes.
#' @param gbifEmail  specify only if using `occCite` with GBIF to get a complete list of original data sources in a citable format.
#' @param gbifPW=NULL  specify only if using `occCite` with GBIF to get a complete list of original data sources in a citable format.
#' @return formatted tibble of species occurrence records 
#'
#' @examples
#' c1_queryDb(spName = "Tremarctos ornatus", occDb = "gbif", occNum = 100)
#' @export

#c1_queryDb <- function(spName, occDb, occNum, shinyLogs=NULL) {
c1_queryDb <- function(spName, 
                       occDb, 
                       occNum, 
                       doCitations=F,
                       gbifUser=NULL, 
                       gbifEmail=NULL,
                       gbifPW=NULL,
                       shinyLogs=NULL) {
  #CM>>
    # for testing
    # case 1: previous versions of wallace
    # spName='Bassaricyon neblina';occDb='gbif';occNum=50; doCitations=F; gbifUser=NULL; gbifEmail=NULL; gbifPW=NULL;shinyLogs=NULL
    # case 2: occCite with gbif
    # spName='Bassaricyon neblina';occDb='gbif';occNum=50; doCitations=T; gbifUser='wallacetester'; gbifEmail='cmerow@yahoo.com'; gbifPW='wallacetester';shinyLogs=NULL; GBIFDownloadDirectory='~/Desktop'
    # case 3: occCite with rbien
    # spName='Turritis glabra';occDb='bien';occNum=50; doCitations=T; gbifUser=NULL; gbifEmail=NULL; gbifPW=NULL;shinyLogs=NULL
  #CM<<
  
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
    #CM+GEPB>>
    if (occDb == 'bison' | occDb == 'vertnet') {
      q <- spocc::occ(spName, occDb, limit = occNum)
      myOccCitations <- NULL
    } else if (occDb == 'gbif') {
      if (doCitations == FALSE) {
        q <- spocc::occ(spName, occDb, limit = occNum)
        myOccCitations <- NULL
      } else if (doCitations == TRUE) {
        if(any(unlist(lapply(list(gbifUser, gbifEmail, gbifPW),is.null)))) {
          shinyLogs %>% writeLog('error', 'Please specify your GBIF username, email, and password. This is needed to get citations for occurrence records. Wallace does not store your information or use it for anything else.')
          return()
        }
        myBTO <- occCite::studyTaxonList(x = spName, datasources = "NCBI")
        login <- occCite::GBIFLoginManager(user = gbifUser, email = gbifEmail, pwd = gbifPW)
        myBTO <- occCite::occQuery(x = myBTO, GBIFLogin = login, limit = occNum)
        myOccCitations <- occCite::occCitation(myBTO)
        # make something with the same slots as spocc that we use
        q=list(gbif=list(meta=list(found=NULL),data=list(formatSpName(spName))))
        q[[occDb]]$meta$found=nrow(myBTO@occResults[[spName]][['GBIF']][['OccurrenceTable']])
        q[[occDb]]$data[[formatSpName(spName)]]=myBTO@occResults[[spName]][['GBIF']][['OccurrenceTable']]
      }
    } else if (occDb == 'bien') {
      myBTO <- occCite::studyTaxonList(x = spName, datasources = "NCBI")
      myBTO <- occCite::occQuery(x = myBTO, datasources = 'bien', limit = occNum)
      myOccCitations <- NULL
      # make something with the same slots as spocc that we use
      q=list(bien=list(meta=list(found=NULL),data=list(formatSpName(spName))))
      q[[occDb]]$meta$found=
        nrow(myBTO@occResults[[spName]][['BIEN']][['OccurrenceTable']])
      q[[occDb]]$data[[formatSpName(spName)]]=
        myBTO@occResults[[spName]][['BIEN']][['OccurrenceTable']]
    }
    #CM+GEPB<<
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
  
  if (occDb == 'gbif') { # standardize BISON column names
    fields <- c('institutionCode', 'stateProvince', 'basisOfRecord', "country", "locality", "elevation")
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name, 
                                   institution_code = institutionCode, 
                                   state_province = stateProvince, 
                                   record_type = basisOfRecord)
  } else if (occDb == 'vertnet') { # standardize VertNet column names
    fields <- c('institutioncode', 'stateprovince', 'basisofrecord', 'maximumelevationinmeters')
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name,
                                   institution_code = institutioncode, 
                                   state_province = stateprovince, 
                                   record_type = basisofrecord, 
                                   elevation = maximumelevationinmeters)
  } else if (occDb == 'bison') { # standardize BISON column names
    fields <- c('countryCode', 'ownerInstitutionCollectionCode', 'calculatedCounty', 'elevation')
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = providedScientificName,
                                   country = countryCode, 
                                   institution_code = ownerInstitutionCollectionCode, 
                                   locality = calculatedCounty,
                                   record_type = basisOfRecord,
                                   state_province = stateProvince)
  } else if (occDb == 'bien') {
    fields <- c('country', 'state_province', 'locality', 'elevation', 'record_type')
    for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    occs <- occs %>% dplyr::rename(taxon_name = name,
                                   institution_code = Dataset)
  }
  # CM >> Do you need to add any formatting here for bien records? GEPB:YES
  
  
  # subset by key columns and make id and popup columns
  cols <- c("taxon_name", "longitude", "latitude",  "occID", 
            "year", "institution_code", "country", "state_province",
            "locality", "elevation", "record_type")
  occs <- occs %>% 
    dplyr::select(dplyr::one_of(cols)) %>%
    # make new column for leaflet marker popup content
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%  
    dplyr::arrange_(cols)

  # subset by key columns and make id and popup columns
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
  dupsRem <- nrow(occsXY) - nrow(occs)
  
  shinyLogs %>% writeLog('Total ', occDb, ' records for ', em(spName), 
                         ' returned [', nrow(occsOrig),'] out of [', totRows,
                         '] total (limit ', occNum, 
                         '). Records without coordinates removed [', noCoordsRem, 
                         ']. Duplicated records removed [', dupsRem, 
                         ']. Remaining records [', nrow(occs), '].')
  
  return(list(orig = occsOrig, cleaned = occs, citations = myOccCitations))
}

