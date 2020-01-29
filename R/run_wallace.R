#'Run \emph{Wallace} Application
#'
#' @author Jamie M. Kass
#'
#' @description This function runs the \emph{Wallace} application in the user's 
#' default web browser.
#'
#' @note Please see the official website (\url{https://wallaceecomod.github.io/})
#'  for more details. If you have questions about the application, please 
#'  participate in the \href{https://groups.google.com/forum/#!forum/wallaceecomod}{Google Group}, 
#'  or email the team directly: \email{wallaceecomod@@gmail.com}.
#' 
#' @param launch.browser Run Wallace in browser
#' @param port If launch.browser is FALSE, specify port to run Wallace
#'
#' @examples if(interactive()){
#' run_wallace()
#' }
#'
#' @export
run_wallace <- function(launch.browser = TRUE, port = getOption("shiny.port")){
  app_path <- system.file("shiny", package = "wallace")
  return(shiny::runApp(app_path, launch.browser = launch.browser, port = port))
}
