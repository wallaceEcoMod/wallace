#' @title `occs_biomodelos` query biomodelos database
#' @description
#' query biomodelos database and returns the complete list of data, data with coordinates, and data with no duplicates
#'
#' @details
#'This function is called by the module occs_biomodelos to query the biomodelos database for
#' species occurrence records. It removes records with duplicate coordinates, and select some columns with fields
#' appropriate to studies in biogeography.
#'
#' @param spName character species name. For biomodelos it returns records associated with the specified taxonomic name, including any synonyms.
#' @param bioKey character. Key to access biomodelos API
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
#' @return A list of 2 dataframes. First dataframe is the original dowloaded dataset, second data frame without duplicates and with appropriate fields for analyses

#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Peter Ersts?
#' @export

occs_biomodelos <- function(spName, bioKey, logger = NULL) {
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logger %>% writeLog(type = 'error',
      'Please input both genus and species names of ONE species. (**)')
    return()
  }
  spName <- paste0(toupper(substring(spName, 1, 1)),
                   substring(spName, 2, nchar(spName)))
  bioName <- gsub(pattern = " ", replacement = "%20", x = spName)
  urlSearch <- paste0('https://api-biomodelos.humboldt.org.co/v2/species/search/',
                      bioName)
  respSearch <- httr::GET(urlSearch,
                          httr::add_headers(host = 'api-biomodelos.humboldt.org.co',
                                      authorization = paste0('apiKey ', bioKey)))
  jsonSearch <-  httr::content(respSearch, 'parsed')
  if (jsonSearch == "Unauthorized") {
    logger %>% writeLog(
      type = 'error', "API key is not working.")
    return()
  }
  if (length(jsonSearch) == 0) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spName), "Species name not found, please check the spelling")
    return()
  }
  urlOccs <- paste('https://api-biomodelos.humboldt.org.co/v2/species/records/',
                   jsonSearch[[1]]$taxID, sep = '')

  smartProgress(logger, message = paste0("Querying biomodelos ..."), {
    respOccs <- httr::GET(urlOccs,
                          httr::add_headers(host = 'api-biomodelos.humboldt.org.co',
                                            authorization = paste0('apiKey ', bioKey)))
    jsonOccs <-  httr::content(respOccs, 'text')
    geo <-  geojsonsf::geojson_sf(jsonOccs)
  })

  if (nrow(geo) == 0) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spName), "Species without records on Biomodelos")
    return()
  }

  occsOrig <- dplyr::as_tibble(geo) %>%
    dplyr::mutate_all(function(x){iconv(x, from= "UTF-8", to = "UTF-8")}) %>%
    dplyr::mutate(longitude = sf::st_coordinates(geo$geometry)[, 1],
                  latitude = sf::st_coordinates(geo$geometry)[, 2])

  # make new column for original ID
  occsOrig$occID <- 1:nrow(occsOrig)

  occsXY <- occsOrig

  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups, ]

  fields <- c("acceptedNameUsage", "longitude", "latitude", "country",
              "suggestedStateProvince", "verbatimLocality", "year", "basisOfRecord",
              "catalogNumber", "institutionCode", "verbatimElevation", "uncertainty")
  for (i in fields) if (!(i %in% names(occs))) occs[i] <- NA
  occs <- occs %>% dplyr::rename(scientific_name = acceptedNameUsage,
                                 state_province = suggestedStateProvince,
                                 locality = verbatimLocality,
                                 record_type = basisOfRecord,
                                 institution_code = institutionCode,
                                 catalog_number = catalogNumber,
                                 elevation = verbatimElevation)

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

  logger %>% writeLog(
    hlSpp(spName), 'Total biomodelos records returned [', nrow(occsOrig),
    ']. Records without coordinates removed [',
    noCoordsRem, ']. Duplicated records removed [', dupsRem,
    ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned = occs))
}
