# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# wallace-package.R
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
#' @name wallace-package
#' @aliases wallace
#' @aliases wallace-package
#' @docType package
#' @title \emph{Wallace}: A modular platform for reproducible ecological modeling
#' @description \emph{Wallace} is a \code{shiny} app that guides users through a complete
#'   species niche/distributional modeling analysis, from the acquisition of
#'   species occurrence and environmental data to visualizing model predictions
#'   on an interactive map (\code{rleaflet}), thus bundling complex workflows
#'   into a single, streamlined GUI interface. New functionality, in the form of
#'    modules, can be added to \emph{Wallace} via contributions from the user
#'    community. In addition, executable session code (R Markdown format) can
#'    be downloaded to share with others or use as supplementary information
#'    for scientific papers and reports. The application is run via the
#'    function \code{\link{run_wallace}}.
#'
#' @details Please see the official website (\url{https://wallaceecomod.github.io/}) for
#'   more details. If you have questions about the application, please participate
#'   in the \href{https://groups.google.com/forum/#!forum/wallaceecomod}{Google Group},
#'   or email the team directly: \email{wallaceEcoMod@@gmail.com}.
#'
#' @import leaflet shiny
#' @importFrom magrittr "%>%"
NULL
