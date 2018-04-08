c6_bioclim  <- function(occs, bg, occsGrp, bgGrp, bgMsk, logs = NULL, shiny = FALSE) {
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  
  BioClim_eval <- function (occs, bg.pts, occ.grp, bg.grp, env) {
    
    # RUN FULL DATA MODEL
    full.mod <- dismo::bioclim(env, occs)
    pred <- dismo::predict(env, full.mod)
    occPredVals <- matrix(dismo::predict(full.mod, full.mod@presence))
    colnames(occPredVals) <- 'BIOCLIM'
    
    # CREATE HOLDERS FOR RESULTS
    AUC.TEST <- double()
    AUC.DIFF <- double()
    OR10 <- double()
    ORmin <- double()
    
    # SET NUMBER OF TEST BINS
    nk <- length(unique(occ.grp))
    
    for (k in 1:nk) {
      
      # SPLIT TEST AND TRAIN DATA
      test.pts <- occs[occ.grp == k, ]
      train.pts <- occs[occ.grp != k, ]
      backg.pts <- bg.pts[bg.grp != k, ]
      mod <- dismo::bioclim(env, train.pts)
      
      # GET AUC METRICS
      AUC.TEST[k] <- dismo::evaluate(p=test.pts, a=backg.pts, mod=mod, x=env)@auc
      AUC.TRAIN <- dismo::evaluate(p=train.pts, a=backg.pts, mod=mod, x=env)@auc
      AUC.DIFF[k] <- max(0, AUC.TRAIN - AUC.TEST[k])
      
      # GET PREDICTED VALUES AT OCCURRENCES FOR OMISSION RATE STATS
      train.pred <- dismo::predict(env, mod)
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
    return(list(models=list(BIOCLIM=full.mod), results=stats, 
                predictions=preds, occVals=occPredVals))
  }
  
  e <- BioClim_eval(occs.xy, bg.xy, occsGrp, bgGrp, bgMsk)

  n <- formatSpName(occs$taxon_name[1])
  logs %>% writeLog("BIOCLIM ran successfully for", n, "and output evaluation results.")
  
  return(e)
}