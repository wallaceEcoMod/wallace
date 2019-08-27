runGAM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title="These degrees of freedom represent the smoothing parameter for the splines corresponding to each variable.",
             numericInput(ns("degFr"), "Degrees of Freedom", value = 1))
  )
}

runGAM_MOD <- function(input, output, session) {
  reactive({
    for (sp in list(curSp())) {
      # ERRORS ####
      if(is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please
                               partition occurrences for cross-validation for ",
                               em(spName(occs())), ".")
        return()
      }

      # FUNCTION CALL ####
      mod.gam <- runGAM(spp[[sp]]$occs[,names(envs())], spp[[sp]]$bg[,names(envs())], input$degFr, sp, shinyLogs)
      # ensure the model object was returned before proceeding
      req(mod.gam)

      # LOAD INTO SPP ####
      spp[[sp]]$results <- mod.gam

      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "GAM"
      spp[[sp]]$rmm$model$gam$family <- "binominal"
      f <- as.character(mod.gam$formula)
      spp[[sp]]$rmm$model$gam$formula <- paste(f[2],f[1],f[3])
      spp[[sp]]$rmm$model$gam$notes <- "gam package implementation"
    }
  })
}

runGAM_TBL <- function(input, output, session) {
  output$evalTbls <- renderUI({
    output$gamSummary <- renderPrint(summary(spp[[curSp()]]$results))
    verbatimTextOutput("gamSummary")
  })
}

runGAM_INFO <- infoGenerator(modName = "Generalized Additive Model",
                             modAuts = "Johnny Modules, Alfred Russel Wallace",
                             pkgName = "gam")
