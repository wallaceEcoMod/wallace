#' @title Build a GAM
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param
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

runGAM <- function(occs.vals, bg.vals, degFr, spName, shinyLogs=NULL) {
  if(!require(gam)) {
    shinyLogs %>% writeLog("Please install the gam package before running.")
    return()
  }
  # make vector of each variable wrapped in the spline function s() with
  # the assigned degrees of freedom
  degFrs <- paste0("s(", names(occs.vals), ", ", degFr, ")")
  # bind all environmental values together
  vals <- rbind(occs.vals, bg.vals)
  # make vector of 1's and 0's for identifying occurrence from background
  bin <- c(rep(1, nrow(occs.vals)), rep(0, nrow(bg.vals)))
  # put everything together in a table for modeling
  d <- data.frame(pa = bin, vals)
  # make formula
  f <- as.formula(paste("pa", paste(degFrs, collapse = " + "), sep = " ~ "))
  # run the GAM
  smartProgress(shinyLogs, message = paste("Running GAM for", spName), {
                mod <- gam(f, family = "binomial", data = d)
  })
  # write log message
  shinyLogs %>% writeLog("GAM ran successfully for ", spName, ".")
  # output model object
  return(mod)
}
