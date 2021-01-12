
maxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Select algorithm"), br(),
    tags$div(title = 'text',
             radioButtons(ns("algMaxent"), label='',
                          choices = list("maxnet", "maxent.jar"), inline = TRUE)),
    strong("Select feature classes "), strong(em("(flexibility of modeled response)")), br(),
    "key: ", strong("L"), "inear, ", strong("Q"), "uadratic, ", strong("H"), "inge, ", 
    strong("P"), "roduct",
    tags$div(title='Feature combinations to be explored. Features are constructed using different relationships within and among the environmental predictors, and are used to constrain the computed probability distribution. In short, more features = more potential model complexity.',
             checkboxGroupInput(ns("fcs"), label='',
                       choices = list("L", "LQ", "H", "LQH", "LQHP"), inline = TRUE)),
    strong("Select regularization multipliers "), strong(em("(penalty against complexity)")),
    tags$div(title='Range of regularization multipliers to explore. Greater values of the regularization multiplier lead to increased penalty against overly complex and/or overfit models. A value of 0 results in no regularization.',
             sliderInput(ns("rms"), label = "",
                min = 0.5, max = 10, step=0.5, value = c(1, 2))),
    tags$div(title='Value used to step through regularization multiplier range (e.g. range of 1-3 with step 0.5 results in [1, 1.5, 2, 2.5, 3]).',
             numericInput(ns("rmsStep"), label = "Multiplier step value", value = 1)),
    strong("Clamping?"), tags$div(title = 'Clamp model predictions?',
                                  checkboxInput(ns("clamp"), label='', value = TRUE)))
  }

maxent_MOD <- function(input, output, session, rvs) {
  reactive({
    
    if (is.null(rvs$occsGrp)) {
      rvs %>% writeLog(type = 'error', "Before building a model, partition 
                       occurrences in component 5.")
      return()
    }
    if (is.null(input$fcs)) {
      rvs %>% writeLog(type = 'error', "No feature classes selected.")
      return()
    }
    if (input$algMaxent == "maxent.jar") {
      if (!require('rJava')) {
        rvs %>% writeLog(type = "error", 'Package rJava cannot load.
               Please download the latest version of Java, and make sure it is the
                         correct version (e.g. 64-bit for a 64-bit system). After installing,
                         try "library(rJava)". If it loads properly, restart Wallace and try again.
                         If it does not, please consult www.github.com/wallaceecomod/wallace for
                         more tips on getting rJava to work.')
        return()
      }
    }
    
    if (is.null(input$fcs)) {
      rvs %>% writeLog(type = 'error', 'Select feature classes first.')
      return()
    }
    
    # record for RMD
    rvs$fcs <- input$fcs
    rvs$rms <- input$rms
    rvs$rmsStep <- input$rmsStep
    rvs$algMaxent <- input$algMaxent
    if (rvs$algMaxent == "maxnet") {
      rvs$clamp <- input$clamp
    } else if (rvs$algMaxent == "maxent.jar") {
      rvs$clamp <- T
    }
    
    # define the vector of RMs to input
    rms <- seq(input$rms[1], input$rms[2], input$rmsStep)  
    # create the Progress Bar object for ENMeval
    progress <- shiny::Progress$new()
    progress$set(message = "Evaluating ENMs...", value = 0)
    on.exit(progress$close())
    n <- length(rms) * length(input$fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
    
    jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    if (input$algMaxent == "maxent.jar") {
      if (!file.exists(jar)) {
        txt <- HTML(paste("To use Maxent, make sure you download,", strong("maxent.jar"), "from the",
                          a("AMNH Maxent webpage",
                            href = "http://biodiversityinformatics.amnh.org/open_source/maxent/",
                            target = "_blank"), "and place it in this directory:", br(), em(jar)))
        rvs %>% writeLog(type = 'error', txt)
        return()
      }
    }
   
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
    colnames(rvs$bgPts) <- names(occs.xy)
    e <- ENMeval::ENMevaluate(occ = occs.xy, env = rvs$bgMsk, bg.coords = rvs$bgPts,
                              RMvalues = rms, fc = input$fcs, method = 'user', 
                              occ.grp = rvs$occsGrp, bg.grp = rvs$bgGrp, 
                              bin.output = TRUE, clamp = rvs$clamp,
                              progbar = FALSE, updateProgress = updateProgress,
                              algorithm = input$algMaxent)
    
    if (rvs$clamp == T | rvs$algMaxent == "maxent.jar") {
      rvs %>% writeLog("Maxent ran successfully using", input$algMaxent, "and output evaluation results for", nrow(e@results), "clamped models.")
    } else if (rvs$clamp == F) {
      rvs %>% writeLog("Maxent ran successfully using", input$algMaxent, "and output evaluation results for", nrow(e@results), "unclampled models.")
    }
    return(e)
  })
}
