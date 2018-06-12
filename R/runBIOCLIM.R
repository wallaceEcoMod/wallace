#' @export

runBIOCLIM  <- function(occs, bg, bgMask, shinyLogs = NULL) {
  
  bioclimEval <- function() {
    # RUN FULL DATA MODEL
    occs.xy <- occs %>% dplyr::select(longitude, latitude)
    # occs.env <- occs %>% dplyr::select(dplyr::starts_with('env_'))
    bg.xy <- bg %>% dplyr::select(longitude, latitude)
    full.mod <- dismo::bioclim(bgMask, occs.xy)
    pred <- dismo::predict(bgMask, full.mod)
    occPredVals <- matrix(dismo::predict(full.mod, full.mod@presence))
    colnames(occPredVals) <- 'BIOCLIM'
    
    # CREATE HOLDERS FOR RESULTS
    AUC.TEST <- double()
    AUC.DIFF <- double()
    OR10 <- double()
    ORmin <- double()
    
    # SET NUMBER OF TEST BINS
    nk <- length(unique(occs$partition))
    
    for (k in 1:nk) {
      
      # SPLIT TEST AND TRAIN DATA
      test.pts <- occs.xy[occs$partition == k, ]
      train.pts <- occs.xy[occs$partition != k, ]
      backg.pts <- bg.xy[bg$partition != k, ]
      mod <- dismo::bioclim(bgMask, train.pts)
      
      # GET AUC METRICS
      AUC.TEST[k] <- dismo::evaluate(p=test.pts, a=backg.pts, mod=mod, x=bgMask)@auc
      AUC.TRAIN <- dismo::evaluate(p=train.pts, a=backg.pts, mod=mod, x=bgMask)@auc
      AUC.DIFF[k] <- max(0, AUC.TRAIN - AUC.TEST[k])
      
      # GET PREDICTED VALUES AT OCCURRENCES FOR OMISSION RATE STATS
      train.pred <- dismo::predict(bgMask, mod)
      p.train <- raster::extract(train.pred, train.pts)
      p.test <- raster::extract(train.pred, test.pts)
      
      # FIND THRESHOLD FOR OR10
      if (nrow(train.pts) < 10) {
        n90 <- floor(nrow(train.pts) * 0.9)
      } else {
        n90 <- ceiling(nrow(train.pts) * 0.9)
      }
      
      # GET OMISSION RATE STATS
      train.thr.10 <- rev(sort(p.train))[n90]
      OR10[k] <- mean(p.test < train.thr.10)
      ORmin[k] <- mean(p.test < min(p.train))
    }
    
    # COMPILE AND SUMMARIZE RESULTS
    statsBins <- as.data.frame(rbind(AUC.TEST, AUC.DIFF, ORmin, OR10))
    statsBinsVar <- ENMeval::corrected.var(statsBins, nk)
    statsBinsAvg <- apply(statsBins, 1, mean)
    
    statsBins <- data.frame(t(statsBins[,1]), t(statsBins[,2]))
    bins <- sort(rep(paste0("Bin", 1:nk), 4))
    colnames(statsBins) <- paste0(bins, c("_test.AUC", "_diff.AUC", "_test.orMTP", "_test.or10pct"))
    row.names(statsBins) <- "BIOCLIM"
    
    stats <- t(data.frame(c(rbind(statsBinsAvg, statsBinsVar))))
    row.names(stats) <- "BIOCLIM"
    colnames(stats) <- c("avg.test.AUC", "var.test.AUC",
                             "avg.diff.AUC", "var.diff.AUC",
                             "avg.test.orMTP", "var.test.orMTP",
                             "avg.test.or10pct", "var.test.or10pct")
    
    preds <- raster::stack(pred)
    names(preds) <- "BIOCLIM"
    
    # THIS FORMAT FOR RETURNED DATA ATTEMPTS TO MATCH WHAT HAPPENS IN WALLACE ALREADY FOR ENMEVAL.
    return(list(models=list(BIOCLIM=full.mod), evalTbl=stats, evalTblBins=statsBins, predictions=preds, occPredVals=occPredVals))
  }
  
  smartProgress(shinyLogs, message = paste0("Running BIOCLIM for ", spName(occs), "..."), {  # start progress bar
    e <- bioclimEval()
  })
  
  shinyLogs %>% writeLog("BIOCLIM ran successfully for", em(spName(occs)), "and output evaluation results.")
  
  return(e)
}