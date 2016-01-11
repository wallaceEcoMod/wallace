source("functions.R")

comp2_spThin <- function(thinDist) {
  if (is.null(values$df)) {
    writeLog("* WARNING: Obtain species occurrence localities first in Step 1.")
    return()
  }
  if (input$thinDist <= 0) {
    writeLog("* WARNING: Assign positive distance to thinning parameter.")
    return()
  }
  withProgress(message = "Spatially Thinning Localities...", {  # start progress bar
    output <- thin(values$df, 'latitude', 'longitude', 'species', thin.par = thinDist,
                   reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                   verbose = FALSE)
    values$prethinned <- values$df
    # pull thinned dataset with max records, not just the first in the list
    maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
    maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
    values$df <- values$df[as.numeric(rownames(maxThin)),]
    if (!is.null(values$inFile)) {
      thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
    }
  })
  writeLog(paste('* Total records thinned to [', nrow(values$df), '] localities.'))
  # render the thinned records data table
  output$occTbl <- DT::renderDataTable({DT::datatable(values$df[,1:4])})
}