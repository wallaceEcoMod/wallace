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
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
run_wallace <- function(launch.browser = TRUE, port = getOption("shiny.port")) {
  app_path <- system.file("shiny", package = "wallace")
  knitcitations::cleanbib()
  options("citation_format" = "pandoc")
  preexisting_objects <- ls(envir = .GlobalEnv)
  on.exit(rm(list = setdiff(ls(envir = .GlobalEnv), preexisting_objects), envir = .GlobalEnv))
  return(shiny::runApp(app_path, launch.browser = launch.browser, port = port))
}
