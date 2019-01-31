
# sample workflow with module functions
# occs <- c1_queryDb("meles meles", "gbif", 100, doCitations = FALSE) 
# occs.thin <- c2_thinOccs(as.data.frame(occs$cleaned), 100)
# envs <- c3_worldclim(10, 1:19)
# ext <- c4_bgExtent(occs.thin, envs, "mcp", 1)  
# bgMsk <- c4_bgMask(occs.thin, envs, ext)
# bg <- c4_bgSample(occs.thin, bgMsk, 1000)
# grps <- c5_partitionOccs(occs.thin, bg, "block")
# m <- runMaxent(occs.thin, bg, grps$occ.grp, grps$bg.grp, bgMsk, 1:3, 1, c("L", "LQ"), TRUE, "maxnet")
