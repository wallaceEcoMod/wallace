#' @export
wallace <- function(){
  app_path <- system.file("shiny", package = "wallace")
  devtools::load_all()
  return(shiny::runApp(app_path))
}
