#' @title `occs_paleoDb` query paleobioDB database
#' @description
#' query paleobioDB database and returns the complete list of data, data with
#'   coordinates, and data with no duplicates
#'
#' @details
#' This function is called by the module occs_queryDb to query the paleobioDB
#'   database for species occurrence records in the Holocene. It removes
#'   records with duplicate coordinates, and selects some columns with fields
#'   appropriate to studies in biogeography.
#'
#' @param spName character. Species name. For paleobioDb it returns records
#'   associated with the specified taxonomic name, including any synonyms.
#' @param occNum integer maximum number of records.
#' @param timeInterval character currently a single timeInterval is allowed:
#'   "Holocene" (Holocene).
#' @param logger Stores all notification messages to be displayed in the
#'   Log Window of Wallace GUI. Insert the logger reactive list here for
#'   running in shiny, otherwise leave the default NULL
#' @return A list of 2 dataframes. First dataframe is the original downloaded
#'   dataset, second dataframe without duplicates and with appropriate fields
#'   for analyses.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Sara Varela <sara_varela@@yahoo.com>
#' @examples
#' \dontrun{
#' spName <- "Didelphis virginiana"
#' occNum <- 100
#' timeInterval <- "Holocene"
#' occsPaleo <- occs_paleoDb(spName, occNum, timeInterval)
#' }
#' @export

occs_paleoDb <- function(spName, occNum, timeInterval, logger = NULL) {
  if (!requireNamespace("paleobioDB", quietly = TRUE)) {
    logger %>%
      writeLog(
        type = "warning",
        "This module is available if you install the 'paleobioDB' package ",
        "(which is a suggested package for Wallace, not a required dependency). If you ",
        "want to install it, close Wallace and run the following line in the ",
        "R Console: ", em("install.packages('paleobioDB')")
      )
    return()
  }
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logger %>% writeLog(type = 'error',
      'Please input both genus and species names of ONE species.')
    return()
  }
  spName <- paste0(toupper(substring(spName, 1, 1)),
                   substring(spName, 2, nchar(spName)))
  smartProgress(logger, message = paste0("Querying paleobioDB ..."), {
    occsOrig <- try(paleobioDB::pbdb_occurrences(taxon_name = spName,
                                                 limit = occNum,
                                                 interval = timeInterval,
                                                 vocab = 'pbdb',
                                                 show = c("coords", "bin", "loc")),
                    silent = TRUE)
  })

  if (inherits(occsOrig, "try-error")) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(hlSpp(fmtSpN(spName))),
      "No records found, please check the spelling.")
    return()
  }

  occsOrig <- dplyr::as_tibble(occsOrig)
  occsOrig$lng <- as.numeric(occsOrig$lng)
  occsOrig$lat <- as.numeric(occsOrig$lat)
  # get total number of records found in database
  totRows <- nrow(occsOrig)
  # extract occurrence tibble
  names(occsOrig)[names(occsOrig) == "lng"] <- "longitude"
  names(occsOrig)[names(occsOrig) == "lat"] <- "latitude"
  names(occsOrig)[names(occsOrig) == "cc"] <- "country"
  occsOrig$taxon_name <- as.character(occsOrig$taxon_name)
  names(occsOrig)[names(occsOrig) == "taxon_name"] <- "scientific_name"

  # make new column for original ID
  occsOrig$occID <- 1:nrow(occsOrig)

  # subset to just records with latitude and longitude
  # all plaeobioDB recors have coords, so this warning is commented until future database
  # occsXY <-  occsOrig[!is.na(occsOrig$longitude) & !is.na(occsOrig$latitude),]
  # if (nrow(occsXY) == 0) {
  #   logger %>% writeLog(
  #     type = 'warning',
  #     hlSpp(spName), "No records with coordinates found in paleobioDB.")
  # }
  occsXY <- occsOrig

  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups, ]

  # subset by key columns and make id and popup columns
  cols <- c("occID", "scientific_name", "longitude", "latitude",
            # "early_interval", "late_interval",
            "country", "collection_no", "record_type",
            "early_age", "late_age")
  occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
    # make new column for leaflet marker popup content
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%
    dplyr::arrange(dplyr::across(cols))
  occs$early_age <- as.numeric(occs$early_age)
  occs$late_age <- as.numeric(occs$late_age)
  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)

  dupsRem <- nrow(occsXY) - nrow(occs)
  logger %>% writeLog(
    hlSpp(fmtSpN(spName)),
    'Total paleobioDb records returned [', nrow(occsOrig), '] (limit ', occNum,
    '). Records without coordinates removed [',
    noCoordsRem, ']. Duplicated records removed [', dupsRem,
    ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned = as.data.frame(occs)))
}
