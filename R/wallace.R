#' @export
wallace <- function(){
  app_path <- system.file("shiny", package = "wallace")
  return(shiny::runApp(app_path))
}
