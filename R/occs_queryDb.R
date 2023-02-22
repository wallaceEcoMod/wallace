#' occs_queryDb Query online database for species occurrence records.
#'
#' @description  Queries a given database for occurrence data on the provided species
#'
#' @details
#' This function is called by the module occs_queryDb to query a database for
#'   species occurrence records, subset to only those records with coordinates,
#'   remove records with duplicate coordinates, and select some columns with fields
#'   appropriate to studies in biogeography.
#'
#' @param spNames character. Species Latin name, with format "Genus species".
#' @param occDb character. Biodiversity database to query; current choices are
#'   "gbif", "vertnet", and "BIEN"
#' @param occNum numeric. Maximum number of occurrence records to return
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'   otherwise leave the default NULL
#' @param doCitations logical. Set TRUE to use `occCite` to get a complete list
#'   of original data sources in a citable format
#' @param gbifUser specify only if using `occCite` with GBIF to get a complete list
#'   of original data sources in a citable format. This, as well as `gbifEmail`
#'   and `gbifPW` are constraints imposed by GBIF to obtain the complete set of
#'   metadata associated with occurrence records and is not stored or used by
#'   `wallace` for any other purposes.
#' @param gbifEmail  specify only if using `occCite` with GBIF to get a
#'   complete list of original data sources in a citable format.
#' @param gbifPW specify only if using `occCite` with GBIF to get a complete
#'   list of original data sources in a citable format.
#' @param RmUncertain specify if occurrences without uncertainty information
#'   should be removed (default is FALSE)
#' @return list of lists one list per species with occurrence records. Each
#'   individual species list with appropriate fields for analysis
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Hannah Owens
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @examples
#' \dontrun{
#' occs_queryDb(spName = "Bassaricyon alleni", occDb = "gbif", occNum = 10)
#' }
#' @importFrom rlang .data
#' @export

occs_queryDb <- function(spNames, occDb, occNum = NULL, doCitations = FALSE,
                         gbifUser = NULL, gbifEmail = NULL, gbifPW = NULL,
                         RmUncertain = FALSE, logger = NULL) {
  if (occDb == "bien" & !requireNamespace("BIEN", quietly = TRUE)) {
    logger %>%
      writeLog(
        type = "warning",
        "This option is available if you install the 'BIEN' package ",
        "(which is a suggested package for Wallace, not a required dependency). If you ",
        "want to install it, close Wallace and run the following line in the ",
        "R Console: ", em("install.packages('BIEN')")
      )
    return()
  }

  if (occDb == "gbif" & doCitations == TRUE &
      !requireNamespace("occCite", quietly = TRUE)) {
    logger %>%
      writeLog(
        type = "warning",
        "This option is available if you install the 'occCite' package ",
        "(which is a suggested package for Wallace, not a required dependency). If you ",
        "want to install it, close Wallace and run the following line in the ",
        "R Console: ", em("install.packages('occCite')")
      )
    return()
  }

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
      if (occDb == 'vertnet') {
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
              "There is an error in your GBIF credentials. Please check them"
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
                hlSpp(fmtSpN(sp)),
                "There is no match in GBIF database. Please check the spelling."
              )
            return()
          }
          if (bestMatch != inputMatch) {
            logger %>%
              writeLog(
                type = 'warning',
                hlSpp(inputMatch),
                "There is no a stricly match in the GBIF search. Data ",
                "downloaded corresponds to ", em(bestMatch), ". ")
          }

          myBTO <- occCite::occQuery(x = sp,
                                     datasource = "gbif",
                                     GBIFLogin = login,
                                     checkPreviousGBIFDownload = FALSE)
          # make something with the same slots as spocc that we use
          q <- list(gbif = list(meta = list(found = NULL),
                                data = list(fmtSpN(sp))))
          gbif_raw <- utils::read.table(unz(
            as.character(myBTO@occResults[[bestMatch]][['GBIF']][['RawOccurrences']]),
            "occurrence.txt"), sep = "\t", header = TRUE, quote = "",
            encoding = "UTF-8")
          gbif_occCite_df <- gbif_raw %>% dplyr::as_tibble() %>%
            dplyr::select(.data$scientificName, .data$decimalLongitude,
                          .data$decimalLatitude, .data$countryCode,
                          .data$stateProvince, .data$locality, .data$year,
                          .data$basisOfRecord, .data$catalogNumber,
                          .data$institutionCode, .data$elevation,
                          .data$coordinateUncertaintyInMeters) %>%
            dplyr::rename(name = .data$scientificName,
                          longitude = .data$decimalLongitude,
                          latitude = .data$decimalLatitude,
                          country = .data$countryCode)
          q[[occDb]]$meta$found <-
            nrow(myBTO@occResults[[bestMatch]][['GBIF']][['OccurrenceTable']])
          q[[occDb]]$data[[fmtSpN(sp)]] <- gbif_occCite_df
          doiGBIF <- myBTO@occResults[[bestMatch]][['GBIF']]$Metadata$doi
          dateDOI <- format(as.Date(myBTO@occResults[[bestMatch]][['GBIF']]$Metadata$created),
                            "%d %B %Y")
          citeGBIF <- list(doi = doiGBIF, date = dateDOI)
          logger %>%
            writeLog(
              hlSpp(fmtSpN(sp)),
              " #CiteTheDOI: Gbif.org (", dateDOI,
              ") GBIF Ocurrence Download https://doi.org/", doiGBIF
            )
        }
      } else if (occDb == 'bien') {
        qBien <- BIEN::BIEN_occurrence_species(species = sp)
        # make something with the same slots as spocc that we use
        q <- list(bien = list(meta = list(found = NULL),
                              data = list(fmtSpN(sp))))
        q[[occDb]]$meta$found <- nrow(qBien)
        q[[occDb]]$data[[fmtSpN(sp)]] <- qBien
      }
    })

    # if species not found, print message to log box and return
    if (q[[occDb]]$meta$found == 0) {
      logger %>%
        writeLog(type = 'error',
                 hlSpp(fmtSpN(sp)),
                 'No records found. Please check the spelling.')
      return()
    }
    # extract occurrence tibbles

    occsOrig <- q[[occDb]]$data[[fmtSpN(sp)]]
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
        hlSpp(fmtSpN(sp)),
        'No records with coordinates found in ', occDb, ". ")
      return()
    }
    noCoordsRem <- nrow(occsOrig) - nrow(occsXY)


    # round longitude and latitude with 5 digits
    occsXY['longitude'] <- round(occsXY['longitude'], 5)
    occsXY['latitude'] <- round(occsXY['latitude'], 5)

    occs<-occsXY

    if (occDb == 'gbif') {

      fields <- c("name", "longitude", "latitude", "country", "stateProvince",
                  "locality", "year", "basisOfRecord", "catalogNumber",
                  "institutionCode", "elevation", "coordinateUncertaintyInMeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = .data$name,
                      state_province = .data$stateProvince,
                      record_type = .data$basisOfRecord,
                      institution_code = .data$institutionCode,
                      catalog_number = .data$catalogNumber,
                      uncertainty = .data$coordinateUncertaintyInMeters)

    } else if (occDb == 'vertnet') { # standardize VertNet column names
      fields <- c("name", "longitude", "latitude", "country", "stateprovince",
                  "locality", "year", "basisofrecord", "catalognumber",
                  "institutioncode", "maximumelevationinmeters",
                  "coordinateuncertaintyinmeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = .data$name,
                      state_province = .data$stateprovince,
                      record_type = .data$basisofrecord,
                      institution_code = .data$institutioncode,
                      catalog_number = .data$catalognumber,
                      elevation = .data$maximumelevationinmeters,
                      uncertainty = .data$coordinateuncertaintyinmeters)
    # } else if (occDb == 'bison') { # standardize BISON column names
    #   fields <- c("providedScientificName", "longitude", "latitude", "countryCode",
    #               "stateProvince", "verbatimLocality", "year", "basisOfRecord",
    #               "catalogNumber", "ownerInstitutionCollectionCode",
    #               "elevation", "uncertainty")
    #   # BISON field requirements (no downloaded by spocc) "elevation"
    #   for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
    #   occs <- occs %>% dplyr::rename(scientific_name = .data$providedScientificName,
    #                                  country = .data$countryCode,
    #                                  state_province = .data$stateProvince,
    #                                  locality = .data$verbatimLocality,
    #                                  record_type = .data$basisOfRecord,
    #                                  institution_code =
    #                                    .data$ownerInstitutionCollectionCode,
    #                                  catalog_number = .data$catalogNumber)
    } else if (occDb == 'bien') {
      fields <- c("scrubbed_species_binomial", "longitude", "latitude",
                  "collection_code", "country", "state_province", "locality", "year",
                  "record_type", "catalog_number", "elevation", "uncertainty")
      # BIEN field requirements (no downloaded by BIEN) "country",
      # "state_province", "locality", "year", "record_type", "institution_code",
      # "elevation", "uncertainty"
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>% dplyr::as_tibble() %>%
        dplyr::rename(scientific_name = .data$scrubbed_species_binomial,
                      institution_code = .data$collection_code)
    }
    noUncertainRem <- 0
    if (RmUncertain == TRUE) {

      occs <- occs[!is.na(occs$uncertainty), ]
      noUncertainRem<- nrow(occsOrig) - (nrow(occs)+noCoordsRem)
      if(nrow(occs)==0){
        logger %>% writeLog(
          type = 'warning',
          hlSpp(fmtSpN(sp)),
          'No records with coordinate uncertainty information found in ', occDb, ".")
        return()
    }
}
dups <- duplicated(occs[,c('longitude','latitude')])
occs <- occs[!dups,]
    # subset by key columns and make id and popup columns
    cols <- c("occID", "scientific_name", "longitude", "latitude", "country",
              "state_province", "locality", "year", "record_type", "catalog_number",
              "institution_code", "elevation", "uncertainty")
    occs <- occs %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(year = as.integer(.data$year),
                    uncertainty = as.numeric(.data$uncertainty)) %>%
      # # make new column for leaflet marker popup content
      dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%
      dplyr::arrange(dplyr::across(cols))

    # subset by key columns and make id and popup columns
    noCoordsRem <- nrow(occsOrig) - nrow(occsXY)
    dupsRem <- nrow(occsXY) - nrow(occs)

    # get total number of records found in database
    totRows <- q[[occDb]]$meta$found

   if (RmUncertain == TRUE) {
     logger %>%
       writeLog(hlSpp(fmtSpN(sp)), 'Total ', occDb, ' records returned [',
                nrow(occsOrig), '] out of [', totRows, '] total',
                if (!(doCitations | occDb == 'bien')) {paste0(' (limit ', occNum,')')},
                '. Records without coordinates removed [', noCoordsRem,
                ']. Records without uncertainty information removed [',
                noUncertainRem, ']. Duplicated records removed [', dupsRem,
                ']. Remaining records [', nrow(occs), '].')
   }
    else {logger %>%
      writeLog(hlSpp(fmtSpN(sp)), 'Total ', occDb, ' records returned [',
               nrow(occsOrig), '] out of [', totRows, '] total',
               if (!(doCitations | occDb == 'bien')) {paste0(' (limit ', occNum,')')},
               '. Records without coordinates removed [', noCoordsRem,
               ']. Duplicated records removed [', dupsRem,
               ']. Remaining records [', nrow(occs), '].')
    }


     # put into list
    if (doCitations & occDb == "gbif") {
      occList[[fmtSpN(sp)]] <- list(orig = occsOrig,
                                           cleaned = as.data.frame(occs),
                                           citation = citeGBIF)
    } else {
      occList[[fmtSpN(sp)]] <- list(orig = occsOrig,
                                           cleaned = as.data.frame(occs))
    }
  }
  return(occList)
}

