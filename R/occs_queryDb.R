# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# occs_queryDb.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
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
#'   "gbif", "vertnet", "bien", "obis" (Ocean Biodiversity Information System,
#'   marine records) and "gbif_obis" (both GBIF and OBIS, deduplicated with
#'   \code{\link{occs_mergeGbifObis}}; the per-record source is kept in the
#'   \code{source} column of the \code{orig} table)
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
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
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

  if (occDb %in% c("obis", "gbif_obis") &
      !requireNamespace("jsonlite", quietly = TRUE)) {
    logger %>%
      writeLog(
        type = "warning",
        "This option is available if you install the 'jsonlite' package ",
        "(which is a suggested package for Wallace, not a required dependency). If you ",
        "want to install it, close Wallace and run the following line in the ",
        "R Console: ", em("install.packages('jsonlite')")
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
      } else if (occDb == 'obis') {
        qObis <- occs_queryObis(sp, occNum)
        # make something with the same slots as spocc that we use
        q <- list(obis = list(meta = list(found = qObis$found),
                              data = list(fmtSpN(sp))))
        q[[occDb]]$data[[fmtSpN(sp)]] <- qObis$occs
      } else if (occDb == 'gbif_obis') {
        qGbif <- spocc::occ(sp, 'gbif', limit = occNum)
        qObis <- occs_queryObis(sp, occNum)
        gbifRaw <- as.data.frame(qGbif$gbif$data[[fmtSpN(sp)]])
        gbifRaw["networkKeys"] <- NULL
        merged <- occs_mergeGbifObis(gbifRaw, qObis$occs)
        nObisDup <- nrow(qObis$occs) - sum(merged$source == "obis")
        logger %>%
          writeLog(hlSpp(fmtSpN(sp)), 'GBIF records returned [', nrow(gbifRaw),
                   '] out of [', qGbif$gbif$meta$found, '] total; OBIS records ',
                   'returned [', nrow(qObis$occs), '] out of [', qObis$found,
                   '] total. OBIS records duplicating a GBIF record (same ',
                   'occurrenceID, or same coordinates and date) removed [',
                   nObisDup, '].')
        q <- list(gbif_obis = list(
          meta = list(found = qGbif$gbif$meta$found + qObis$found),
          data = list(fmtSpN(sp))))
        q[[occDb]]$data[[fmtSpN(sp)]] <- merged
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

    if (occDb %in% c('gbif', 'obis', 'gbif_obis')) {
      # OBIS records are standardized to GBIF/spocc field names in
      # occs_queryObis(), so the three sources share this block
      fields <- c("name", "longitude", "latitude", "country", "stateProvince",
                  "locality", "year", "basisOfRecord", "catalogNumber",
                  "institutionCode", "elevation", "coordinateUncertaintyInMeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = "name",
                      state_province = "stateProvince",
                      record_type = "basisOfRecord",
                      institution_code = "institutionCode",
                      catalog_number = "catalogNumber",
                      uncertainty = "coordinateUncertaintyInMeters")

    } else if (occDb == 'vertnet') { # standardize VertNet column names
      fields <- c("name", "longitude", "latitude", "country", "stateprovince",
                  "locality", "year", "basisofrecord", "catalognumber",
                  "institutioncode", "maximumelevationinmeters",
                  "coordinateuncertaintyinmeters")
      for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
      occs <- occs %>%
        dplyr::rename(scientific_name = "name",
                      state_province = "stateprovince",
                      record_type = "basisofrecord",
                      institution_code = "institutioncode",
                      catalog_number = "catalognumber",
                      elevation = "maximumelevationinmeters",
                      uncertainty = "coordinateuncertaintyinmeters")
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


#' @title occs_queryObis Query OBIS for species occurrence records
#' @description Query the Ocean Biodiversity Information System (OBIS) API v3
#'   (https://api.obis.org) for occurrence records of one species and return
#'   them with GBIF/spocc-style field names so that \code{\link{occs_queryDb}}
#'   can clean them with the same code as GBIF records.
#'
#' @details Records are requested in pages (OBIS caps a page at 10,000) using
#'   the API's \code{after} cursor until \code{occNum} records are collected
#'   or the records are exhausted. Only the fields needed by Wallace are
#'   requested. Fields are renamed to the names \code{spocc::occ()} uses for
#'   GBIF: \code{name}, \code{longitude}, \code{latitude}, \code{year} (from
#'   OBIS \code{date_year}); \code{elevation} is \code{NA} (OBIS stores
#'   \code{depth} instead, which is kept). A \code{source} column with value
#'   "obis" is added.
#'
#' @param spName character. Species Latin name, with format "Genus species".
#' @param occNum numeric. Maximum number of occurrence records to return.
#'   \code{NULL} or 0 returns all records (can be very slow for common species).
#' @return list with \code{occs} (data frame of records) and \code{found}
#'   (total number of records OBIS holds for the name).
#' @author Ben Best <ben@@ecoquants.com>
#' @examples
#' \dontrun{
#' occs_queryObis("Mola mola", occNum = 50)
#' }
#' @export
occs_queryObis <- function(spName, occNum = NULL) {
  fields <- c("id", "scientificName", "decimalLongitude", "decimalLatitude",
              "country", "stateProvince", "locality", "date_year", "eventDate",
              "basisOfRecord", "catalogNumber", "institutionCode",
              "coordinateUncertaintyInMeters", "occurrenceID", "depth",
              "dataset_id")
  if (is.null(occNum) || is.na(occNum) || occNum < 1) occNum <- Inf
  base_url <- paste0("https://api.obis.org/v3/occurrence?scientificname=",
                     utils::URLencode(spName, reserved = TRUE),
                     "&fields=", paste(fields, collapse = ","))
  pages <- list()
  n <- 0
  after <- NULL
  found <- 0
  repeat {
    size <- min(10000, occNum - n)
    url <- paste0(base_url, "&size=", size,
                  if (!is.null(after)) paste0("&after=", after))
    res <- jsonlite::fromJSON(url, flatten = TRUE)
    found <- res$total
    page <- res$results
    if (is.null(page) || length(page) == 0 || nrow(page) == 0) break
    pages[[length(pages) + 1]] <- page
    n <- n + nrow(page)
    after <- page$id[nrow(page)]
    if (n >= occNum || n >= found) break
  }
  if (length(pages) == 0) {
    occs <- data.frame()
  } else {
    occs <- dplyr::bind_rows(pages)
  }
  for (i in fields) if (!(i %in% names(occs))) occs[[i]] <- NA
  occs <- occs %>%
    dplyr::rename(name = "scientificName",
                  longitude = "decimalLongitude",
                  latitude = "decimalLatitude",
                  year = "date_year") %>%
    dplyr::mutate(elevation = NA_real_, source = "obis")
  occs <- as.data.frame(occs)
  rownames(occs) <- NULL
  list(occs = occs, found = found)
}

#' @title occs_mergeGbifObis Merge GBIF and OBIS records, removing duplicates
#' @description Combine a GBIF table (as returned by \code{spocc::occ()}) with
#'   an OBIS table (as returned by \code{\link{occs_queryObis}}) into one table
#'   with a per-record \code{source} column, dropping OBIS records that
#'   duplicate a GBIF record.
#'
#' @details Deduplication rule (GBIF records are always kept; an OBIS record is
#'   dropped when either test matches a GBIF record):
#' \enumerate{
#'   \item identical \code{occurrenceID} (compared only when both values are
#'     non-missing and non-empty);
#'   \item identical rounded coordinates and date: longitude and latitude
#'     rounded to \code{digits} decimals (default 4, about 11 m) together with
#'     the event date, taken as the first 10 characters of \code{eventDate}
#'     (YYYY-MM-DD) when present, otherwise \code{year}. Records lacking
#'     coordinates are never matched by this test.
#' }
#'   Records that are duplicates within one source are not removed here;
#'   \code{\link{occs_queryDb}} later removes records with identical
#'   coordinates as it does for every database.
#'
#' @param gbif data frame of GBIF records with at least \code{longitude},
#'   \code{latitude} columns; \code{occurrenceID}, \code{eventDate} and
#'   \code{year} are used when present.
#' @param obis data frame of OBIS records with the same column names (see
#'   \code{\link{occs_queryObis}}).
#' @param digits integer. Decimals to round coordinates to before matching.
#' @return data frame with the GBIF rows first (\code{source = "gbif"}) followed
#'   by the retained OBIS rows (\code{source = "obis"}); columns are the union
#'   of both inputs, with \code{NA} where a source lacks a column.
#' @author Ben Best <ben@@ecoquants.com>
#' @examples
#' gbif <- data.frame(name = "Mola mola", longitude = c(-70.1, -70.2),
#'                    latitude = c(41.1, 41.2), eventDate = c("2020-06-01", NA),
#'                    year = c(2020L, 2019L), occurrenceID = c("a", "b"))
#' obis <- data.frame(name = "Mola mola", longitude = c(-70.1, -70.3),
#'                    latitude = c(41.1, 41.3), eventDate = c("2020-06-01", NA),
#'                    year = c(2020L, 2018L), occurrenceID = c("x", "y"))
#' occs_mergeGbifObis(gbif, obis) # 3 rows: 2 gbif + 1 obis
#' @export
occs_mergeGbifObis <- function(gbif, obis, digits = 4) {
  gbif <- as.data.frame(gbif)
  obis <- as.data.frame(obis)
  gbif$source <- "gbif"
  obis$source <- "obis"
  for (nm in c("occurrenceID", "eventDate", "year")) {
    if (!nm %in% names(gbif)) gbif[[nm]] <- NA
    if (!nm %in% names(obis)) obis[[nm]] <- NA
  }
  # key 1: occurrenceID
  id_ok <- function(x) !is.na(x) & nzchar(as.character(x))
  gbif_ids <- as.character(gbif$occurrenceID[id_ok(gbif$occurrenceID)])
  dup_id <- id_ok(obis$occurrenceID) & as.character(obis$occurrenceID) %in% gbif_ids
  # key 2: rounded coordinates + date (eventDate day, else year)
  xy_date_key <- function(d) {
    lon <- round(as.numeric(d$longitude), digits)
    lat <- round(as.numeric(d$latitude), digits)
    date <- substr(as.character(d$eventDate), 1, 10)
    date <- ifelse(is.na(date) | !nzchar(date), as.character(d$year), date)
    key <- paste(lon, lat, date, sep = "|")
    key[is.na(lon) | is.na(lat)] <- NA
    key
  }
  gbif_keys <- stats::na.omit(xy_date_key(gbif))
  obis_keys <- xy_date_key(obis)
  dup_xy <- !is.na(obis_keys) & obis_keys %in% gbif_keys
  obis_keep <- obis[!(dup_id | dup_xy), , drop = FALSE]
  # harmonize column types shared by both sources (e.g. GBIF eventDate is a
  # Date, OBIS eventDate is character) so bind_rows() does not fail
  for (nm in intersect(names(gbif), names(obis_keep))) {
    if (!identical(class(gbif[[nm]]), class(obis_keep[[nm]]))) {
      gbif[[nm]] <- as.character(gbif[[nm]])
      obis_keep[[nm]] <- as.character(obis_keep[[nm]])
    }
  }
  merged <- dplyr::bind_rows(gbif, obis_keep)
  merged <- as.data.frame(merged)
  rownames(merged) <- NULL
  merged
}
