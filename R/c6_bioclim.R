c6_bioclim  <- function(occs, bg, bgMsk, logs = NULL, shiny = FALSE) {
  
  bioclimEval <- function() {
    # RUN FULL DATA MODEL
    occs.xy <- occs %>% dplyr::select(longitude, latitude)
    bg.xy <- bg %>% dplyr::select(longitude, latitude)
    full.mod <- dismo::bioclim(bgMsk, occs.xy)
    pred <- dismo::predict(bgMsk, full.mod)
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
      mod <- dismo::bioclim(bgMsk, train.pts)
      
      # GET AUC METRICS
      AUC.TEST[k] <- dismo::evaluate(p=test.pts, a=backg.pts, mod=mod, x=bgMsk)@auc
      AUC.TRAIN <- dismo::evaluate(p=train.pts, a=backg.pts, mod=mod, x=bgMsk)@auc
      AUC.DIFF[k] <- max(0, AUC.TRAIN - AUC.TEST[k])
      
      # GET PREDICTED VALUES AT OCCURRENCES FOR OMISSION RATE STATS
      train.pred <- dismo::predict(bgMsk, mod)
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
    stats <- as.data.frame(rbind(AUC.TEST, AUC.DIFF, ORmin, OR10))
    stats <- cbind(apply(stats, 1, mean), ENMeval::corrected.var(stats, nk), stats)
    colnames(stats) <- c("Mean", "Variance", paste("Bin", 1:nk))
    rownames(stats) <- c("test.AUC", "diff.AUC","test.orMTP","test.or10pct")
    
    preds <- raster::stack(pred)
    names(preds) <- "BIOCLIM"
    
    # THIS FORMAT FOR RETURNED DATA ATTEMPTS TO MATCH WHAT HAPPENS IN WALLACE ALREADY FOR ENMEVAL.
    return(list(models=list(BIOCLIM=full.mod), results=stats, predictions=preds, occVals=occPredVals))
  }
  
  if(shiny == TRUE) {
    withProgress(message = paste0("Running BIOCLIM for ", spName(occs), "..."), {  # start progress bar
      e <- bioclimEval()
    })  
  } else {
    e <- bioclimEval()
  }
  
  logs %>% writeLog("BIOCLIM ran successfully for", em(spName(occs)), "and output evaluation results.")
  
  return(e)
}