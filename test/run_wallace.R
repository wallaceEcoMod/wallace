# Run Wallace
# Open a different R session and run this code before testing
require(wallace)
app_path <- system.file("shiny", package = "wallace")
return(shiny::runApp(app_path, launch.browser = TRUE, port = 5556, test.mode = TRUE))
