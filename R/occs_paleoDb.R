#' @title `occs_paleoDb` returns species occurrences
#' @description #' query paleobioDB or neotoma databases and returns the complete list of data, data with coordinates, and data with no duplicates
#'
#' @details
#' See Examples.
#'
#' @param spName character species name. For paleobioDb it returns records associated with the specified taxonomic name, including any synonyms.
#' @param occNum integer maximum number of records
#' @param timeInterval character Either "LGM" (Last Glacial Maximum) or "Holo" (Holocene). For PaleobioDB only Holocene is allowed.
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

occs_paleoDb <- function(spName, occNum, timeInterval, logger = NULL) {
  spName <- trimws(spName)
  # figure out how many separate names (components of scientific name) were entered
  nameSplit <- length(unlist(strsplit(spName, " ")))
  # if two names not entered, throw error and return
  if (nameSplit != 2) {
    logger %>% writeLog(type = 'error',
      'Please input both genus and species names of ONE species. (**)')
    return()
  }
  smartProgress(logger, message = paste0("Querying paleobioDB ..."), {
    occsOrig <- try(paleobioDB::pbdb_occurrences(taxon_name = spName,
                                                 limit = occNum,
                                                 interval = timeInterval,
                                                 vocab = 'pbdb',
                                                 show = c("coords", "bin", "loc")),
                    silent = TRUE)
  })

  # if (occDb == "PaleobioDB") {
  #
  #   if (timeInterval == "LGM") {
  #     logger %>% writeLog(type = 'error',
  #       'PaleobioDB does not have separate LGM records. You can only download Holocene records.')
  #     return()
  #   } else if (timeInterval == "Holo") {
  #
  #     # query database
  #     smartProgress(logger, message = paste0("Querying ", occDb, " ..."), {
  #       occsOrig <- try(paleobioDB::pbdb_occurrences(scientific_name=spName, limit=occNum, vocab="pbdb",
  #                                                    max_ma= 0.02, show=c("coords", "bin", "loc")), silent =TRUE)
  #     })
  #   }
  # }

  # if (occDb == "Neotoma") {
    #   if (timeInterval == "LGM") {
    #     query database
    #     withProgress(message = paste("Querying", occDb, "..."), {
    #       q <- neotoma::get_dataset(taxonname= spName,
    #                                 ageold = 25000, ageyoung=15000)
    #       q <- neotoma::get_dataset(datasettype="pollen", ageold = 25000,
    #                                 ageyoung=15000) %>% neotoma::get_download() %>% neotoma::compile_taxa('P25') %>% neotoma::compile_downloads() %>% filter(ageyoung < 25000 & ageold > 15000)
    #       # hacer el objeto de salida! busca las columnas que te molan
    #       str (q[[1]][[1]])
    #       str (q[[1]][[2]])
    #     })
    #   }
    # }
  # }

  if (class(occsOrig) == "try-error") {
    logger %>% writeLog(type = 'error',
      'No records found for ', em(spName), ". Please check the spelling.")
    return()
  }


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
  occsXY <-  occsOrig[!is.na(occsOrig$longitude) & !is.na(occsOrig$latitude),]
  if (nrow(occsXY) == 0) {
    logger %>% writeLog(type = 'warning',
      'No records with coordinates found in ', occDb, " for ", em(spName), ".")
  }


  dups <- duplicated(occsXY[,c('longitude','latitude')])
  occs <- occsXY[!dups, ]

  # subset by key columns and make id and popup columns
  cols <- c("scientific_name", "longitude", "latitude", "early_interval",
            "late_interval", "country", "collection_no", "record_type",
            "early_age", "late_age", "occID")
  occs <- occs %>% dplyr::select(dplyr::one_of(cols)) %>%
    # make new column for leaflet marker popup content
    dplyr::mutate(pop = unlist(apply(occs, 1, popUpContent))) %>%
    dplyr::arrange_(cols)

  noCoordsRem <- nrow(occsOrig) - nrow(occsXY)

  dupsRem <- nrow(occsXY) - nrow(occs)
  logger %>% writeLog(
    hlSpp(spName), 'Total paleobioDb records returned [', nrow(occsOrig),
    '] (limit ', occNum, '). Records without coordinates removed [',
    noCoordsRem, ']. Duplicated records removed [', dupsRem,
    ']. Remaining records [', nrow(occs), '].')
  return(list(orig = occsOrig, cleaned = occs))
}
