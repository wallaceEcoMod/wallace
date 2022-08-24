#' @title occs_userOccs Loads user provided occurrence records
#' @description
#' Load user database with species occurrence records. Returns a list of lists,
#'   one per species provided in database in each species list with a set of
#'   appropriate fields
#' @details
#' This function is called by the module occs_queryDb to load a user provided
#' database for species occurrence records, subset to only those records with
#' coordinates, remove records with duplicate coordinates, and select some
#' columns with fields appropriate to studies in biogeography.
#'
#' @param txtPath path to database including database name and extension
#' @param txtName name of database without the extension. Database must have
#'   at least three columns named 'scientific_name', 'longitude', 'latitude'
#' @param txtSep  field separator used in database (as in read.delim)
#' @param txtDec  decimal separator used for coordinates in database
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL
#' @examples
#' txtPath <- system.file("extdata/Bassaricyon_alleni.csv", package = "wallace")
#' txtName <- 'Bassaricyon_alleni'
#' user.occs <- occs_userOccs(txtPath, txtName)
#'
#'
#' @return List of lists. One list per species with occurrence records. Each
#'   individual species list with appropriate fields for analysis
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @importFrom rlang .data
#' @export

occs_userOccs <- function(txtPath, txtName, txtSep = ",", txtDec = ".",
                          logger = NULL) {

  # read in txt
  txt <- tryCatch(expr = utils::read.delim(file = txtPath, header = TRUE,
                                           sep = txtSep, dec = txtDec),
                  error = function(e) "error")
  if (inherits(txt, "character")) {
    logger %>% writeLog(
      type = "error",
      paste0("There is something wrong in your file. Check file format or ",
             "delimiter and decimal separators."))
    return()
  }

  # check to make sure all column names are correct
  if (sum(c('scientific_name', 'longitude', 'latitude') %in% names(txt)) != 3) {
    logger %>% writeLog(
      type = "error",
      paste0('Please input a file with columns "scientific_name", ',
             '"longitude", "latitude".'))
    return()
  }


  # subset to just records with non-NA latitude and longitude
  txt.xy <- txt %>% dplyr::filter(!is.na(.data$latitude) & !is.na(.data$longitude))
  txt.xy$scientific_name <- trimws(txt.xy$scientific_name)
  # get all species names
  occs <- txt.xy %>% dplyr::filter(!grepl("bg_", .data$scientific_name))
  spNames <- trimws(as.character(occs$scientific_name))

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
      writeLog(type = 'error',
               paste0('Please input just genus and species epithet in scientific',
                      ' name field in your file (e.g., "Canis lupus").'))
    return()
  }


  if (nrow(occs) == 0) {
    logger %>% writeLog(type = 'warning',
      'No records with coordinates found in ', txtName, ".")
    return()
  }

  # Check that longitude and latitude are numeric
  else if (!is.numeric(txt$longitude) | !is.numeric(txt$latitude)) {
    logger %>% writeLog(
      type = "error",
      'Please input txt file. Not all values in longitude or latitude are numeric.')
    return()
  }
  # Transform scientific_name field
  txt.xy$scientific_name <- spNames

  # put species into a list in the same form as spp
  occsList <- list()
  for (i in unique(spNames)) {
    sp.occs <- txt.xy %>% dplyr::filter(.data$scientific_name == i)
    # add occID field if it doesn't exist
    if(!("occID" %in% names(sp.occs))) sp.occs$occID <- row.names(sp.occs)
    # add all cols to match dbOccs if not already there
    for (col in c("country", "state_province", "locality", "year", "record_type",
                  "catalog_number", "institution_code", "elevation",
                  "uncertainty")) {
      if (!(col %in% names(sp.occs))) sp.occs[,col] <- NA
    }
    # add popup field and arrange
    cols <- c("occID", "scientific_name", "longitude", "latitude", "country",
              "state_province", "locality", "year", "record_type", "catalog_number",
              "institution_code", "elevation", "uncertainty")
    sp.occs <- sp.occs %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(year = as.integer(.data$year),
                    uncertainty = as.numeric(.data$uncertainty)) %>%
      # # make new column for leaflet marker popup content
      dplyr::mutate(pop = unlist(apply(sp.occs, 1, popUpContent))) %>%
      dplyr::arrange(dplyr::across(cols))

    n <- fmtSpN(i)

    # subset to just records with latitude and longitude
    occsXY <- sp.occs[!is.na(sp.occs$latitude) & !is.na(sp.occs$longitude),]

    # round longitude and latitude with 5 digits
    occsXY['longitude'] <- round(occsXY['longitude'], 5)
    occsXY['latitude'] <- round(occsXY['latitude'], 5)

    dups <- duplicated(occsXY[,c('longitude','latitude')])
    occs <- occsXY[!dups,]

    occsList[[n]] <- list(orig = sp.occs,
                          cleaned = as.data.frame(occs))

    # subset by key columns and make id and popup columns
    dupsRem <- nrow(sp.occs) - nrow(occs)

    logger %>% writeLog(
      hlSpp(n), "Data uploaded from <i>'", txtName,
      "'</i>: Duplicated records removed [",
      dupsRem, "]. Remaining records [", nrow(occs), "].")

    # look for background records
    sp.bg <- txt.xy %>% dplyr::filter(.data$scientific_name == paste0("bg_", i))
    # if they exist, load them into occsList for the current species
    if(nrow(sp.bg) > 0) {
      occsList[[n]]$bg <- sp.bg
      logger %>% writeLog(
        hlSpp(n), "Data for uploaded from <i>'", txtName, "'</i>: ",
        nrow(sp.bg), " background records.")
    }
  }
  return(occsList)
}
