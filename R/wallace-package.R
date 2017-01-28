#'\emph{Wallace} is a \code{shiny} app that guides users through a complete species niche/distributional modeling analysis, from the acquisition of species occurrence and environmental data to visualizing model predictions on an interactive map (\code{rleaflet}), thus bundling complex workflows into a single, streamlined GUI interface. New functionality, in the form of modules, can be added to \emph{Wallace} via contributions from the user community. In addition, executable session code (R Markdown format) can be downloaded to share with others or use as supplementary information for scientific papers and reports. The application is run via the function \code{\link{run_wallace}}.
#'
#'Please see the official website (\url{https://wallaceecomod.github.io/}) for more details. If you have questions about the application, please participate in the \href{https://groups.google.com/forum/#!forum/wallaceecomod}{Google Group}, or email the team directly: \url{wallaceEcoMod@@gmail.com}.
#'
#' @name wallace-package
#' @aliases wallace
#' @aliases wallace-package
#' @docType package
#' @title \emph{Wallace}: A modular platform for reproducible ecological modeling
#' @author \strong{Jamie M. Kass}\cr
#' (email: \email{jkass@@gradcenter.cuny.edu};
#' Website: \url{https://ndimhypervol.github.io/})
#' @author \strong{Bruno Vilela}\cr
#' (email: \email{bvilela@@wustl.edu};
#' Website: \url{https://bvilela.weebly.com/})
#' @author \strong{Matthew E. Aeillo-Lammens}\cr
#' (email: \email{matt.lammens@@gmail.com};
#' Website: \url{https://mlammens.github.io/})
#' @author \strong{Robert Muscarella}\cr
#' (email: \email{bob.muscarella@@gmail.com};
#' Website: \url{https://bobmuscarella.weebly.com/})
#' @author \strong{Robert P. Anderson}\cr
#' (email: \email{randerson@@ccny.cuny.edu};
#' Website: \url{https://www.andersonlab.ccny.cuny.edu/})
#'
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab wallace\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6.1\cr
#' Date: \tab 2017-01-14\cr
#' License: \tab GNU 3.0\cr
#' }
#'
#' @references Kass J.M., Vilela B., Aeillo-Lammens M.E., Muscarella R., and Anderson R.P. (2017) \emph{Wallace}: A modular platform for reproducible ecological modeling. Version 0.6.1.
#' @import shiny leaflet
#' @importFrom magrittr "%>%"
NULL