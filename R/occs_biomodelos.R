# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# occs_biomodelos.R
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
#' @title `occs_biomodelos` query biomodelos database
#' @description
#' query BioModelos database and returns the complete list of data, data with coordinates, and data with no duplicates
#'
#' @details
#'This function is called by the module occs_biomodelos to query the BioModelos database for
#' species occurrence records. It removes records with duplicate coordinates, and select some columns with fields
#' appropriate to studies in biogeography.
#'
#' @param spN character species name. For BioModelos it returns records associated with the specified taxonomic name, including any synonyms.
#' @param bioKey character. Key to access BioModelos API
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
#' @return A list of 2 dataframes. First dataframe is the original dowloaded dataset, second data frame without duplicates and with appropriate fields for analyses

#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#' @author Peter Ersts
#' @examples
#' \dontrun{
#' ### Set parameters
#' spN <- "Bassaricyon neblina"
#' bioKey <- 12345
#' ### Run function
#' occs <- occs_biomodelos(spN, bioKey, logger = NULL)
#' }
#' @export

occs_biomodelos <- function(spN, bioKey, logger = NULL) {
  spN <- trimws(spN)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spN, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logger %>% writeLog(type = 'error',
      'Please input both genus and species names of ONE species. ')
    return()
  }
  spN <- paste0(toupper(substring(spN, 1, 1)),
                   substring(spN, 2, nchar(spN)))
  bioName <- gsub(pattern = " ", replacement = "%20", x = spN)
  spN <- gsub(pattern = " ", replacement = "_", x = spN)
  urlSearch <- paste0('https://api-biomodelos.humboldt.org.co/v2/species/search/',
                      bioName)
  respSearch <- tryCatch(expr = httr::GET(urlSearch,
                          httr::add_headers(host = 'api-biomodelos.humboldt.org.co',
                                      authorization = paste0('apiKey ', bioKey))),
                         error= function(e) NULL)
                #trycatch error
                if (is.null(respSearch)) {
                  logger %>% writeLog(
                  type = "error",
                  paste0("Unable to retrieve data from BioModelos.
                           Server may be down. "))
                  return()
                } else {
    jsonSearch <-  httr::content(respSearch, 'parsed')
    }

  if (length(jsonSearch) == 0) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spN), "Species name not found, please check the spelling. ")
    return()
  }

  if (jsonSearch == "Unauthorized") {
    logger %>% writeLog(
      type = 'error', "API key is not working.")
    return()
  }

  urlOccs <- paste('https://api-biomodelos.humboldt.org.co/v2/species/records/',
                   jsonSearch[[1]]$taxID, sep = '')

  smartProgress(logger, message = paste0("Querying BioModelos ..."), {
    respOccs <- httr::GET(urlOccs,
                          httr::add_headers(host = 'api-biomodelos.humboldt.org.co',
                                            authorization = paste0('apiKey ', bioKey)))
    jsonOccs <-  httr::content(respOccs, 'text')
    geo <-  geojsonsf::geojson_sf(jsonOccs)
  })

  if (nrow(geo) == 0) {
    logger %>% writeLog(
      type = 'error',
      hlSpp(spN), "Species without records on BioModelos. ")
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
  occs <- occs %>% dplyr::rename(scientific_name = .data$acceptedNameUsage,
                                 state_province = .data$suggestedStateProvince,
                                 locality = .data$verbatimLocality,
                                 record_type = .data$basisOfRecord,
                                 institution_code = .data$institutionCode,
                                 catalog_number = .data$catalogNumber,
                                 elevation = .data$verbatimElevation)

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

  logger %>% writeLog(
    hlSpp(spN), 'Total BioModelos records returned [', nrow(occsOrig),
    ']. Records without coordinates removed [',
    noCoordsRem, ']. Duplicated records removed [', dupsRem,
    ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned = as.data.frame(occs),
              taxID = jsonSearch[[1]]$taxID))
}
