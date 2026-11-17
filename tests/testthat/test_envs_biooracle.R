#### COMPONENT 3: Obtain Environmental Data
#### MODULE: Bio-ORACLE Marine
context("Bio-ORACLE")

### catalogue (no network)
test_that("catalogue has the expected structure", {
  cat_bo <- envs_biooracle_layers()
  expect_is(cat_bo, "data.frame")
  expect_equal(names(cat_bo),
               c("var", "label", "units", "type", "years", "surf_only"))
  expect_false(any(duplicated(cat_bo$var)))
  expect_true(all(cat_bo$type %in% c("ocean", "terrain")))
  # terrain rows have no baseline years and are never surface-only
  expect_true(all(is.na(cat_bo$years[cat_bo$type == "terrain"])))
  expect_true(all(!cat_bo$surf_only[cat_bo$type == "terrain"]))
  # ocean rows all have a baseline period of the form YYYY_YYYY
  expect_true(all(grepl("^[0-9]{4}_[0-9]{4}$", cat_bo$years[cat_bo$type == "ocean"])))
  # the key marine variables are present
  expect_true(all(c("thetao", "so", "chl", "o2", "ph", "bathymetry_mean") %in% cat_bo$var))
})

### selection / naming rules (no network)
test_that("ocean variable resolves to dataset, field and layer name", {
  sel <- envs_biooracle_select("thetao", boDepth = "surf", boStat = "mean")
  expect_equal(nrow(sel), 1)
  expect_equal(sel$dataset_id, "thetao_baseline_2000_2019_depthsurf")
  expect_equal(sel$field, "thetao_mean")
  expect_equal(sel$layer, "thetao_mean_surf")
  expect_equal(sel$units, "degree_C")
})

test_that("benthic depth and statistic are encoded in names", {
  sel <- envs_biooracle_select(c("so", "chl"), boDepth = "mean", boStat = "range")
  expect_equal(sel$dataset_id, c("so_baseline_2000_2019_depthmean",
                                 "chl_baseline_2000_2018_depthmean"))
  expect_equal(sel$field, c("so_range", "chl_range"))
  expect_equal(sel$layer, c("so_range_mean", "chl_range_mean"))
})

test_that("surface-only variables fall back to surface for benthic requests", {
  sel <- envs_biooracle_select(c("siconc", "tas", "mlotst"), boDepth = "max", boStat = "ltmin")
  expect_true(all(grepl("_depthsurf$", sel$dataset_id)))
  expect_equal(sel$layer, c("siconc_ltmin_surf", "tas_ltmin_surf", "mlotst_ltmin_surf"))
})

test_that("terrain variables ignore depth and statistic", {
  sel <- envs_biooracle_select(c("bathymetry_mean", "slope"), boDepth = "min", boStat = "max")
  expect_equal(sel$dataset_id, rep("terrain_characteristics", 2))
  expect_equal(sel$field, c("bathymetry_mean", "slope"))
  expect_equal(sel$layer, c("bathymetry_mean", "slope"))
})

test_that("selection preserves order and mixes types", {
  sel <- envs_biooracle_select(c("bathymetry_mean", "thetao", "so"))
  expect_equal(sel$var, c("bathymetry_mean", "thetao", "so"))
  expect_equal(sel$layer, c("bathymetry_mean", "thetao_mean_surf", "so_mean_surf"))
  expect_false(any(duplicated(sel$layer)))
})

test_that("invalid selections error", {
  expect_error(envs_biooracle_select("notavar"), "Unknown Bio-ORACLE variable")
  expect_error(envs_biooracle_select("thetao", boDepth = "bottom"))
  expect_error(envs_biooracle_select("thetao", boStat = "median"))
})

### download function argument checks (no network)
test_that("error checks", {
  skip_if_not_installed("rerddap")
  expect_error(envs_biooracle(boVars = "thetao", boRes = ""),
               "Select a raster resolution.")
  expect_error(envs_biooracle(boVars = "thetao", boRes = 7),
               "Select a raster resolution.")
  expect_error(envs_biooracle(boVars = character(0), boRes = 1),
               "Select at least one Bio-ORACLE variable.")
})

### download (network)
test_that("output type checks", {
  skip_on_cran()
  skip_if_offline("erddap.bio-oracle.org")
  skip_if_not_installed("rerddap")
  envar <- c("thetao", "so", "bathymetry_mean")
  bo <- try(envs_biooracle(boVars = envar, boDepth = "surf", boStat = "mean",
                           boRes = 1), silent = TRUE)
  skip_if(inherits(bo, "try-error"), "Bio-ORACLE ERDDAP server not reachable")
  # the output is a RasterStack
  expect_is(bo, "RasterStack")
  # the number of layers is the same as the number of selected variables
  expect_equal(length(envar), raster::nlayers(bo))
  # the resolution and global extent are right
  expect_equal(raster::res(bo), c(1, 1))
  expect_equal(as.vector(raster::extent(bo)), c(-180, 180, -90, 90))
  # the names are right
  expect_equal(names(bo), c("thetao_mean_surf", "so_mean_surf", "bathymetry_mean"))
  # land is NA (center of Asia), open ocean has values (mid Atlantic)
  vals <- raster::extract(bo, data.frame(x = c(100, -30), y = c(45, 30)))
  expect_true(all(is.na(vals[1, ])))
  expect_true(all(!is.na(vals[2, ])))
  # bathymetry is at or below sea level
  expect_true(raster::cellStats(bo[["bathymetry_mean"]], "max") <= 0)
})
