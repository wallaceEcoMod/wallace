#' Query online database for species occurrence records.
#'
#' \code{occs_queryDb} returns a list of lists, one per queried species. Each list of species occurrences with a selection of appropriate fields.
#'
#' This function is called by the module occs_queryDb to query a database for
#' species occurrence records, subset to only those records with coordinates,
#' remove records with duplicate coordinates, and select some columns with fields
#' appropriate to studies in biogeography.
#'
#' @param spNames character species Latin name, with format "Genus species"
#' @param occDb character biodiversity database to query; current choices are
#' "gbif", "vertnet", and "bison"
#' @param occNum numeric maximum number of occurrence records to return
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
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
#' @return List of lists. One list per species with occurence records. Each indiivudal species list with appropriate fields for analysis
#'
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @examples
#' occs_queryDb(spName = "Tremarctos ornatus", occDb = "gbif", occNum = 100)
#' @export

#occs_queryDb <- function(spName, occDb, occNum, logger=NULL) {
occs_queryDb <- function(spNames, occDb, occNum = NULL, doCitations = FALSE,
                         gbifUser = NULL, gbifEmail = NULL, gbifPW = NULL,
                         logger = NULL) {
  # Get all species names for textInput Shiny
  if (length(spNames) == 1) {
    if (grepl(x = spNames, pattern = ",")) {
      spNames <- trimws(strsplit(spNames, ",")[[1]])
    }
  }
  # function for capitalizing genus names
  spCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  }
  # capitalize genus names
  spNames <- sapply(spNames, spCap)
  # figure out how many separate names (components of scientific name) were entered
  namesSplit <- sapply(spNames, function(x) strsplit(x, " "))
  namesSplitCheck <- sapply(namesSplit, function(x) length(x) == 2)
  # if two names not entered, throw error and return
  if (!all(namesSplitCheck)) {
    logger %>%
      writeLog(type = 'error', 'Please input both genus and species names.')
    return()
  }

  occList <- list()

  for (sp in spNames) {
    # query database
    smartProgress(logger,
                  message = paste0("Querying ", occDb, " for ", sp, "..."), {
      if (occDb == 'bison' | occDb == 'vertnet') {
        q <- spocc::occ(sp, occDb, limit = occNum)
        myOccCitations <- NULL
      } else if (occDb == 'gbif') {
        if (doCitations == FALSE) {
          q <- spocc::occ(sp, occDb, limit = occNum)
          myOccCitations <- NULL
        } else if (doCitations == TRUE) {
          if(any(unlist(lapply(list(gbifUser, gbifEmail, gbifPW), is.null)))) {
            logger %>% writeLog(
              type = 'error',
              paste0('Please specify your GBIF username, email, and password. ',
              'This is needed to get citations for occurrence records. Wallace ',
              'does not store your information or use it for anything else.')
              )
            return()
          }
          login <- occCite::GBIFLoginManager(user = gbifUser, email = gbifEmail,
                                             pwd = gbifPW)
          if (is.null(login)) {
            logger %>% writeLog(
              type = 'error',
              "There is an error in your GBIF credentials. Please check them (**)"
            )
            return()
          }
          nameGBIF <- occCite::studyTaxonList(x = sp)
          bestMatch <- as.character(nameGBIF@cleanedTaxonomy$`Best Match`)
          inputMatch <- as.character(nameGBIF@cleanedTaxonomy$`Input Name`)
          if (bestMatch == "No match") {
            logger %>%
              writeLog(
                type = "error",
                hlSpp(em(sp)),
                "There is no match in GBIF database. Please check the spelling. (**)"
              )
            return()
          }
          if (bestMatch != inputMatch) {
            logger %>%
              writeLog(
                type = 'warning',
                hlSpp(em(inputMatch)),
                "There is no a stricly match in the GBIF search. Data ",
                "downloaded corresponds to ", em(bestMatch), ". (**)")
          }

          myBTO <- occCite::occQuery(x = sp,
                                     datasource = "gbif",
                                     GBIFLogin = login,
                                     checkPreviousGBIFDownload = FALSE)
          #myOccCitations <- occCite::occCitation(myBTO)
          # make something with the same slots as spocc that we use
          q <- list(gbif = list(meta = list(found = NULL),
                                data = list(formatSpName(sp))))

          q[[occDb]]$meta$found <-
            nrow(myBTO@occResults[[bestMatch]][['GBIF']][['OccurrenceTable']])
          q[[occDb]]$data[[formatSpName(sp)]] <-
            myBTO@occResults[[bestMatch]][['GBIF']][['OccurrenceTable']]
          doiGBIF <- myBTO@occResults[[bestMatch]][['GBIF']]$Metadata$doi
          dateDOI <- format(as.Date(myBTO@occResults[[bestMatch]][['GBIF']]$Metadata$created),
                            "%d %B %Y")
          citeGBIF <- list(doi = doiGBIF, date = dateDOI)
          logger %>%
            writeLog(
              hlSpp(em(sp)),
              "(**) #CiteTheDOI: Gbif.org (", dateDOI,
              ") GBIF Ocurrence Download https://doi.org/", doiGBIF
            )
        }
      } else if (occDb == 'bien') {
        # myBTO <- occCite::studyTaxonList(x = sp, datasources = "NCBI")
        # myBTO <- occCite::occQuery(x = myBTO, datasources = 'bien', limit = occNum)
        # myOccCitations <- NULL
        qBien <- BIEN::BIEN_occurrence_species(species = sp)
        # make something with the same slots as spocc that we use
        q <- list(bien = list(meta = list(found = NULL),
                              data = list(formatSpName(sp))))
        q[[occDb]]$meta$found <- nrow(qBien)
        q[[occDb]]$data[[formatSpName(sp)]] <- qBien
      }
    })

    # if species not found, print message to log box and return
    if (q[[occDb]]$meta$found == 0) {
      logger %>%
        writeLog(type = 'error',
                 hlSpp(em(sp)),
                 'No records found, please check the spelling. (**)')
      next
    }
    # extract occurrence tibbles

    occsOrig <- q[[occDb]]$data[[formatSpName(sp)]]
    # make sure latitude and longitude are numeric (sometimes they aren't)
    occsOrig$latitude <- as.numeric(occsOrig$latitude)
    occsOrig$longitude <- as.numeric(occsOrig$longitude)
    # make new column for original ID
    occsOrig$occID <- as.numeric(row.names(occsOrig))
    # delete colums with list to avoid conflict
    occsOrig["networkKeys"] <- NULL
    # subset to just records with latitude and longitude
    occsXY <- occsOrig[!is.na(occsOrig$latitude) & !is.na(occsOrig$longitude),]
    # if no records with coordinates, throw warning
    if (nrow(occsXY) == 0) {
      logger %>% writeLog(
        type = 'warning',
        hlSpp(em(sp)),
        'No records with coordinates found in ', occDb, ". (**)")
      return()
    }
    # round longitude and latitude with 5 digits
    occsXY['longitude'] <- round(occsXY['longitude'], 5)
    occsXY['latitude'] <- round(occsXY['latitude'], 5)

    dups <- duplicated(occsXY[,c('longitude','latitude')])
    occs <- occsXY[!dups,]

    if (occDb == 'gbif') {
      fields <- c("name", "longitude", "latitude", "country", "stateProvince",
                  "locality", "year", "basisOfRecord", "catalogNumber",
                  "institutionCode", "elevation", "coordinateUncertaintyInMeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = name,
                      state_province = stateProvince,
                      record_type = basisOfRecord,
                      institution_code = institutionCode,
                      catalog_number = catalogNumber,
                      uncertainty = coordinateUncertaintyInMeters)
    } else if (occDb == 'vertnet') { # standardize VertNet column names
      fields <- c("name", "longitude", "latitude", "country", "stateprovince",
                  "locality", "year", "basisofrecord", "catalognumber",
                  "institutioncode", "maximumelevationinmeters",
                  "coordinateuncertaintyinmeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = name,
                      state_province = stateprovince,
                      record_type = basisofrecord,
                      institution_code = institutioncode,
                      catalog_number = catalognumber,
                      elevation = maximumelevationinmeters,
                      uncertainty = coordinateuncertaintyinmeters)
    } else if (occDb == 'bison') { # standardize BISON column names
      fields <- c("providedScientificName", "longitude", "latitude", "countryCode",
                  "stateProvince", "verbatimLocality", "year", "basisOfRecord",
                  "catalogNumber", "ownerInstitutionCollectionCode",
                  "verbatimElevation", "uncertainty")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>% dplyr::rename(scientific_name = providedScientificName,
                                     country = countryCode,
                                     state_province = stateProvince,
                                     locality = verbatimLocality,
                                     record_type = basisOfRecord,
                                     institution_code =
                                       ownerInstitutionCollectionCode,
                                     catalog_number = catalogNumber,
                                     elevation = verbatimElevation)
    } else if (occDb == 'bien') {
      fields <- c("scrubbed_species_binomial", "longitude", "latitude",
                  "collection_code", "country", "state_province", "locality", "year",
                  "record_type", "catalog_number", "elevation", "uncertainty")
      # BIEN field requirements (no downloaded by BIEN) "country",
      # "state_province", "locality", "year", "record_type", "institution_code",
      # "elevation", "uncertainty"
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>% dplyr::rename(scientific_name = scrubbed_species_binomial,
                                     institution_code = collection_code)
    }

    # subset by key columns and make id and popup columns
    cols <- c("occID", "scientific_name", "longitude", "latitude", "country",
              "state_province", "locality", "year", "record_type", "catalog_number",
              "institution_code", "elevation", "uncertainty")
    occs <- occs %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(year = as.integer(year),
                    uncertainty = as.numeric(uncertainty)) %>%
      # # make new column for leaflet marker popup content
      dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%
      dplyr::arrange_(cols)

    # subset by key columns and make id and popup columns
    noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
    dupsRem <- nrow(occsXY) - nrow(occs)

    # get total number of records found in database
    totRows <- q[[occDb]]$meta$found

    logger %>%
      writeLog(hlSpp(em(sp)), 'Total ', occDb, ' records returned [', nrow(occsOrig),
               '] out of [', totRows, '] total',
               if (!(doCitations | occDb == 'bien')) {paste0(' (limit ', occNum,')')},
               '. Records without coordinates removed [', noCoordsRem,
               ']. Duplicated records removed [', dupsRem,
               ']. Remaining records [', nrow(occs), '].')
    # put into list
    if (doCitations & occDb == "gbif") {
      occList[[formatSpName(sp)]] <- list(orig = occsOrig, cleaned = occs,
                                          citation = citeGBIF)
    } else {
      occList[[formatSpName(sp)]] <- list(orig = occsOrig, cleaned = occs)
    }
  }
  return(occList)
}

