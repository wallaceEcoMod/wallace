#'The letsR package is being developed to help researchers in the handling, processing,
#'and analysis of macroecological data. Its purpose is to integrate these methodological processes
#'into a single software platform for macroecological analyses.
#'The package's main functions allow users to build presence-absence matrices, the basic analytical tool in macroecology,
#'from species' geographical distributions and merge them with species' traits, conservation information
#'(downloadable using functions from this package) and spatial environmental layers.
#'In addition, other package's functions enable users to summarize and visualize information from presence-absence matrices.
#'
#'All functions in this package use a prefix and a suffix separated by a dot.
#'The prefix refers to the package's name and the suffix to the actual function.
#'This is done to avoid confusion with potentially similarly-named functions from other R packages.
#'For instance, the letsR function used to create presence-absence matrices is called \code{\link{lets.presab}} (but see also
#'\code{\link{lets.presab.birds}} and \code{\link{lets.presab.points}}) whereas the one used to add variables to a
#'presence-absence matrix is called \code{\link{lets.addvar}}.
#'The package's basic functions create and work on a particular S3 object class called \code{PresenceAbsence}.
#'Such \code{\link{PresenceAbsence}} object class allows storing information beyond presence-absence data
#'(e.g. user-defined grid-cell system) and using the generic \code{plot}, \code{summary} and \code{print} functions of R.
#'Also, some package's functions allow the user to input customary R objects (e.g. \code{vector}, \code{matrix},
#'\code{data.frame}).
#'
#'Another set of functions in this package allow the user to download species' information related to
#'their description and conservation status as provided by the IUCN's REdList database (\code{\link{lets.iucn}},
#'\code{\link{lets.iucn.ha}}, \code{\link{lets.iucn.his}}).
#'For this, such functions use the IUCN's RedList API to retrieve information from its webpage.
#'
#'If you are looking for the most recent version of the package, you can get the development version
#'of letsR on github (\url{http://www.github.com/macroecology/letsR}).
#'
#' @name wallace-package
#' @aliases wallace
#' @aliases wallace-package
#' @docType package
#' @title Tools for Data Handling and Analysis in  Macroecology.
#' @author \strong{Bruno Vilela}\cr
#' (email: \email{bvilela@@wustl.edu};
#' Website: \url{http://bvilela.weebly.com/})
#' @author \strong{Fabricio Villalobos}\cr
#' (email: \email{fabricio.villalobos@@gmail.com};
#' Website: \url{https://sites.google.com/site/fabriciovillalobos/})
#'
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab lestR\cr
#' Type: \tab Package\cr
#' Version: \tab 2.6\cr
#' Date: \tab 2015-06-01\cr
#' License: \tab GPL-2\cr
#' }
#'
#' @references Vilela, B., & Villalobos, F. (2015). wallace: a new R package for data handling and analysis in macroecology. Methods in Ecology and Evolution.
#' @import shiny leaflet shinyjs shinyBS DT RColorBrewer rmarkdown dplyr spThin dismo ENMeval rgeos maptools rgdal raster shinythemes repmis
#' @importFrom magrittr "%>%"
#' @importFrom spocc "occ"
NULL
