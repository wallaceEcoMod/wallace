# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# poccs_removeByID.R
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

#' @title poccs_removeByID Remove occurrence by ID
#' @description This function removes user selected occurrences by ID.
#'
#' @details
#' This function is called by the remove occurrences by ID module. It allows for
#'   removal of a single occurrence flagged by the user on the map. The function
#'   will return a data frame of occurrences with all relevant columns for
#'   further analyses and without the occurrence selected by the user.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data
#' @param removeID the ID of the occurrence to be removed from the occurrences
#'   dataframe.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data. Used to obtain species name for logger messages.
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
#'                  package = "wallace"))[, 2:3]
#' occs$occID <- 1:nrow(occs)
#' out.ID <- poccs_removeByID(occs, 11)
#'
#' @return A new occurence dataframe without the user selected occurrence
#'   mantaining all columns from original dataframe for further analyses.

#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
# @note

# @seealso
# @references

#' @export

poccs_removeByID <- function(occs, removeID, logger = NULL, spN = NULL) {
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
      "Before processing occurrences, obtain the data in component 1.")
    return()
  }

  if (!(removeID %in% occs$occID)) {
    logger %>% writeLog(type = 'error','Entered occID not found.')
    return()
  }

  # find which occID corresponds to row for removal
  i <- which(removeID == occs$occID)
  # remove the row
  occs.remID <- occs[-i,]

  logger %>% writeLog(
    hlSpp(spN), "Removed occurrence with occID = ", removeID,
    ". Updated data has n = ", nrow(occs.remID), " records.")

  if (nrow(occs.remID) < 4) {
    logger %>% writeLog(type = 'error',
      hlSpp(spN), "After removing occurrences, there are three or less points. ",
      "You need more occurrences to continue the analysis."
    )
    return()
  }
  return(occs.remID)
}
