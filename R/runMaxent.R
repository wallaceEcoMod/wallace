#' \code{runMaxent} returns a formatted tibble of species occurrences with a selection of appropriate fields.
#' @title Run Maxent
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs
#' @param bg 
#' @param occsGrp 
#' @param bgGrp
#' @param bgMsk 
#' @param rms
#' @param rmsStep
#' @param fcs
#' @param shinyLogs
# @keywords
#'
# @examples
#'
#'
# @return 
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

runMaxent  <- function(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel, algMaxent,
                        shinyLogs = NULL) {
  if (is.null(occsGrp)) {
    shinyLogs %>% writeLog(type = 'error', "Before building a model, please partition 
                        occurrences for cross-validation.")
    return()
  }
  
  # if maxent.jar selected check for jar file and whether rJava can be loaded
  if (algMaxent == "maxent.jar") {
    # error for no maxent.jar in dismo directory
    jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    if(!file.exists(jar)) {
      shinyLogs %>% writeLog(type = 'error', "To use Maxent, make sure you 
                           download, ", strong("maxent.jar"), " from the ",
                             a("AMNH Maxent webpage", 
                               href="http://biodiversityinformatics.amnh.org/open_source/maxent/", 
                               target="_blank"), " and place it in this directory:", br(), em(jar))
      return()
    }
    
    if(!require('rJava')) {
      shinyLogs %>% writeLog(type = "error", 'Package rJava cannot load. 
               Please download the latest version of Java, and make sure it is the 
               correct version (e.g. 64-bit for a 64-bit system). After installing, 
               try "library(rJava)". If it loads properly, restart Wallace and try again.
               If it does not, please consult www.github.com/wallaceecomod/wallace for
               more tips on getting rJava to work.')
      return()
    }
  }  
  
  # define the vector of RMs to input
  rms.interval <- seq(rms[1], rms[2], rmsStep)  
  # create the Progress Bar object for ENMeval
  if (!is.null(shinyLogs)) {
    progress <- shiny::Progress$new()
    progress$set(message = paste0("Building/Evaluating ENMs for ", spName(occs), "..."), value = 0)
    on.exit(progress$close())
    n <- length(rms.interval) * length(fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
  } else {
    n <- length(rms.interval) * length(fcs)
    updateProgress <- FALSE
  }

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  
  # run ENMeval
  e <- ENMeval::ENMevaluate(occs.xy, bgMsk, bg.coords = bg.xy,
                            RMvalues = rms.interval, fc = fcs, method = 'user', 
                            occ.grp = occsGrp, bg.grp = bgGrp, 
                            bin.output = TRUE, clamp = clampSel,
                            progbar = FALSE, updateProgress = updateProgress,
                            algorithm = algMaxent)
  
  # name the output models in the model list
  names(e@models) <- e@results$settings
  # get the values of the prediction for each occ point
  occPredVals <- raster::extract(e@predictions, occs.xy)
  
  stats <- e@results[,1:16]
  statsBins <- e@results[,17:ncol(e@results)]
  
  endTxt <- paste(", using", algMaxent, "with clamping", ifelse(clampSel, "on.", "off."))
  
  shinyLogs %>% writeLog("Maxent ran successfully for ", em(spName(occs)), " and
                         output evaluation results for ", nrow(e@results), " models", endTxt)
  
  # output ENMeval object in list form to be compatible with other models
  e <- list(models=e@models, evalTbl=stats, evalTblBins=statsBins, predictions=e@predictions, occPredVals=occPredVals)
  
  return(e)
  
}
