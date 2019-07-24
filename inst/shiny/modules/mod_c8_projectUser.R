projectUser_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("projUserNames"),
    fileInput(ns("userEnvs"),
              label = paste0('Input rasters in single-file format (i.e. .tif, ',
                             '.asc). All rasters must have the same extent and ',
                             'resolution (cell size). (**)'),
              accept = c(".asc", ".tif"), multiple = TRUE)
  )
}

projectUser_MOD <- function(input, output, session) {
  print("Ready to go!")
}

projectUser_INFO <-
  infoGenerator(modName = "Project to User-files (**)",
                modAuts = paste0("Gonzalo E. Pinilla-Buitrago, Jamie M. Kass, ",
                                 "Robert P. Anderson"),
                pkgName = "dismo")
