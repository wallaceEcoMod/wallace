#' @title occs_userOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param txtPath x
#' @param txtName x
#' @param txtSep x
#' @param txtDec x
#' @param logger x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked
#  in the documentation.

#' @export

occs_userOccs <- function(txtPath, txtName, txtSep, txtDec,
                          logger = NULL) {

  # read in txt
  txt <- tryCatch(expr = read.table(file = txtPath, header = TRUE, sep = txtSep,
                                    dec = txtDec),
                  error = function(e) "error")

  if (class(txt) == "character") {
    logger %>% writeLog(
      type = "error",
      paste0("There is something wrong in your file. Check file format or ",
             "delimiter and decimal separators.(**)"))
    return()
  }

  # check to make sure all column names are correct
  if (!all(c('scientific_name', 'longitude', 'latitude') %in% names(txt))) {
    logger %>% writeLog(
      type = "error",
      paste0('Please input txt file with columns "scientific_name", ',
             '"longitude", "latitude" or check delimeter and decimal ',
             'separators. (**)'))
    return()
  }

  # subset to just records with non-NA latitude and longitude
  txt.xy <- txt %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  txt.xy$scientific_name <- trimws(txt.xy$scientific_name)
  # get all species names
  occs <- txt.xy %>% dplyr::filter(!grepl("bg_", scientific_name))
  spNames <- trimws(as.character(unique(occs$scientific_name)))

  if (nrow(occs) == 0) {
    logger %>% writeLog(type = 'warning',
      'No records with coordinates found in ', txtName, ".")
    return()
  }

  # put species into a list in the same form as spp
  occsList <- list()
  for (i in spNames) {
    sp.occs <- txt.xy %>% dplyr::filter(scientific_name == i)
    # add occID field if it doesn't exist
    if(!("occID" %in% names(sp.occs))) sp.occs$occID <- row.names(sp.occs)
    # add all cols to match dbOccs if not already there
    for (col in c("country", "state_province", "locality", "year", "record_type",
                  "catalog_number", "institution_code", "elevation",
                  "uncertainty")) {
      if (!(col %in% names(sp.occs))) sp.occs[,col] <- NA
    }
    # add popup field
    sp.occs$pop <- unlist(apply(sp.occs, 1, popUpContent))
    n <- formatSpName(i)

    # subset to just records with latitude and longitude
    occsXY <- sp.occs[!is.na(sp.occs$latitude) & !is.na(sp.occs$longitude),]

    # round longitude and latitude with 5 digits
    occsXY['longitude'] <- round(occsXY['longitude'], 5)
    occsXY['latitude'] <- round(occsXY['latitude'], 5)

    dups <- duplicated(occsXY[,c('longitude','latitude')])
    occs <- occsXY[!dups,]

    occsList[[n]] <- list(orig = sp.occs, cleaned = occs)

    # subset by key columns and make id and popup columns
    dupsRem <- nrow(sp.occs) - nrow(occs)

    logger %>% writeLog(
      "Data for ", em(i), " uploaded from ", txtName, ": Duplicated records removed [", dupsRem, "]. Remaining records [", nrow(occs), "].")

    # look for background records
    sp.bg <- txt.xy %>% dplyr::filter(scientific_name == paste0("bg_", i))
    # if they exist, load them into occsList for the current species
    if(nrow(sp.bg) > 0) {
      occsList[[n]]$bg <- sp.bg
      logger %>% writeLog(
        "Data for ", em(i), " uploaded from ", txtName, ": ", nrow(sp.bg), " background records.")
    }
  }

  return(occsList)
}
