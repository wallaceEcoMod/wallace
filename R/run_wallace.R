# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# run_wallace.R
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
#' @title Run \emph{Wallace} Application
#' @description This function runs the \emph{Wallace} application in the user's
#' default web browser.
#' @param launch.browser Whether or not to launch a new browser window.
#' @param port The port for the shiny server to listen on. Defaults to a
#' random available port.
#' @note Please see the official website (\url{https://wallaceecomod.github.io/})
#' for more details. If you have questions about the application,
#' please participate in the \href{https://groups.google.com/forum/#!forum/wallaceecomod}{Google Group},
#' or email the team directly: \email{wallaceEcoMod@@gmail.com}.
#'
#' @examples
#' if(interactive()) {
#' run_wallace()
#' }
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @export
run_wallace <- function(launch.browser = TRUE, port = getOption("shiny.port")) {
  app_path <- system.file("shiny", package = "wallace")
  knitcitations::cleanbib()
  options("citation_format" = "pandoc")
  preexisting_objects <- ls(envir = .GlobalEnv)
  on.exit(rm(list = setdiff(ls(envir = .GlobalEnv), preexisting_objects), envir = .GlobalEnv))
  return(shiny::runApp(app_path, launch.browser = launch.browser, port = port))
}
