#### COMPONENT 1: Obtain Occurrence Data
#### MODULE: Query Database (Present) -- OBIS and GBIF + OBIS
context("queryDb OBIS")

## columns of the cleaned table produced by occs_queryDb() for every database
cleaned_cols <- c("occID", "scientific_name", "longitude", "latitude", "country",
                  "state_province", "locality", "year", "record_type",
                  "catalog_number", "institution_code", "elevation",
                  "uncertainty", "pop")

## synthetic fixtures mimicking spocc::occ() GBIF output and occs_queryObis()
gbif_fx <- data.frame(
  name = "Mola mola",
  longitude = c(-70.12345, -70.2, -70.3, NA),
  latitude  = c(41.12345, 41.2, 41.3, 41.4),
  eventDate = as.Date(c("2020-06-01", "2019-07-02", NA, "2018-01-01")),
  year = c(2020L, 2019L, 2017L, 2018L),
  occurrenceID = c("urn:gbif:1", "urn:gbif:2", NA, "urn:gbif:4"),
  basisOfRecord = "HUMAN_OBSERVATION",
  stringsAsFactors = FALSE)

obis_fx <- data.frame(
  name = "Mola mola",
  # 1: same id as gbif 1 (dropped by id)
  # 2: same rounded xy + same day as gbif 2 (dropped by xy+date)
  # 3: same xy as gbif 2 but different day (kept)
  # 4: same xy+year as gbif 3, which has no eventDate (dropped by xy+year)
  # 5: coordinates differ beyond 4th decimal (kept)
  # 6: no coordinates (kept here; removed later by occs_queryDb)
  longitude = c(-70.5, -70.20001, -70.2, -70.3, -70.2004, NA),
  latitude  = c(41.5, 41.20004, 41.2, 41.3, 41.2, 41.6),
  eventDate = c("2021-01-01", "2019-07-02T10:00:00", "2019-07-03", NA, "2019-07-02", NA),
  year = c(2021L, 2019L, 2019L, 2017L, 2019L, 2016L),
  occurrenceID = c("urn:gbif:1", "obis:2", "obis:3", "", "obis:5", NA),
  depth = c(10, 20, 30, 40, 50, 60),
  stringsAsFactors = FALSE)

### dedup rule
test_that("occs_mergeGbifObis keeps all GBIF rows and drops OBIS duplicates", {
  m <- occs_mergeGbifObis(gbif_fx, obis_fx)
  expect_is(m, "data.frame")
  # all 4 gbif rows first, then the 3 retained obis rows (3, 5, 6)
  expect_equal(m$source, c(rep("gbif", 4), rep("obis", 3)))
  expect_equal(m$occurrenceID[m$source == "obis"], c("obis:3", "obis:5", NA))
  # rownames are reset so occID in occs_queryDb() is 1..n
  expect_equal(rownames(m), as.character(1:7))
  # union of columns, NA where a source lacks the column
  expect_true(all(c("depth", "basisOfRecord", "source") %in% names(m)))
  expect_true(all(is.na(m$depth[m$source == "gbif"])))
  expect_true(all(is.na(m$basisOfRecord[m$source == "obis"])))
})

test_that("occurrenceID match ignores missing or empty ids", {
  g <- data.frame(longitude = 1, latitude = 1, occurrenceID = c(NA, ""),
                  eventDate = NA, year = 2000L)
  o <- data.frame(longitude = c(2, 3), latitude = c(2, 3),
                  occurrenceID = c(NA, ""), eventDate = NA, year = 2001L)
  m <- occs_mergeGbifObis(g, o)
  expect_equal(sum(m$source == "obis"), 2)
})

test_that("coordinate rounding digits controls the xy+date match", {
  g <- data.frame(longitude = -70.2, latitude = 41.2, eventDate = "2019-07-02",
                  year = 2019L, occurrenceID = "g")
  o <- data.frame(longitude = -70.2004, latitude = 41.2, eventDate = "2019-07-02",
                  year = 2019L, occurrenceID = "o")
  expect_equal(sum(occs_mergeGbifObis(g, o, digits = 4)$source == "obis"), 1)
  expect_equal(sum(occs_mergeGbifObis(g, o, digits = 3)$source == "obis"), 0)
})

test_that("records without coordinates are never matched by xy+date", {
  g <- data.frame(longitude = NA, latitude = NA, eventDate = "2019-07-02",
                  year = 2019L, occurrenceID = "g")
  o <- data.frame(longitude = NA, latitude = NA, eventDate = "2019-07-02",
                  year = 2019L, occurrenceID = "o")
  expect_equal(sum(occs_mergeGbifObis(g, o)$source == "obis"), 1)
})

test_that("missing optional columns are tolerated and Date/character dates combine", {
  g <- data.frame(longitude = -70.2, latitude = 41.2, eventDate = as.Date("2019-07-02"))
  o <- data.frame(longitude = c(-70.2, -71), latitude = c(41.2, 42),
                  eventDate = c("2019-07-02", "2019-07-03"))
  m <- occs_mergeGbifObis(g, o)
  expect_equal(m$source, c("gbif", "obis"))
  expect_is(m$eventDate, "character")
})

### network tests
test_that("occs_queryObis honors the record limit and standardizes fields", {
  skip_on_cran()
  skip_if_offline("api.obis.org")
  skip_if_not_installed("jsonlite")
  q <- try(occs_queryObis("Mola mola", occNum = 50), silent = TRUE)
  skip_if(inherits(q, "try-error"), "OBIS API not reachable")
  expect_equal(nrow(q$occs), 50)
  expect_true(q$found >= 50)
  expect_true(all(c("name", "longitude", "latitude", "country", "stateProvince",
                    "locality", "year", "basisOfRecord", "catalogNumber",
                    "institutionCode", "elevation", "coordinateUncertaintyInMeters",
                    "occurrenceID", "depth", "source") %in% names(q$occs)))
  expect_true(all(q$occs$source == "obis"))
  expect_is(q$occs$longitude, "numeric")
})

test_that("occs_queryDb obis and gbif_obis produce the GBIF column schema", {
  skip_on_cran()
  skip_if_offline("api.obis.org")
  skip_if_offline("api.gbif.org")
  skip_if_not_installed("jsonlite")
  out_obis <- try(occs_queryDb("Mola mola", "obis", 50), silent = TRUE)
  skip_if(inherits(out_obis, "try-error"), "OBIS API not reachable")
  expect_equal(names(out_obis$Mola_mola$cleaned), cleaned_cols)
  expect_equal(nrow(out_obis$Mola_mola$orig), 50)
  expect_true(all(out_obis$Mola_mola$orig$source == "obis"))
  expect_false(any(duplicated(out_obis$Mola_mola$cleaned[, c("longitude", "latitude")])))

  out_both <- try(occs_queryDb("Mola mola", "gbif_obis", 50), silent = TRUE)
  skip_if(inherits(out_both, "try-error"), "GBIF or OBIS not reachable")
  expect_equal(names(out_both$Mola_mola$cleaned), cleaned_cols)
  expect_setequal(unique(out_both$Mola_mola$orig$source), c("gbif", "obis"))
  expect_equal(sum(out_both$Mola_mola$orig$source == "gbif"), 50)
  # every cleaned record points back to a row of the original download
  expect_true(all(out_both$Mola_mola$cleaned$occID %in% out_both$Mola_mola$orig$occID))
})
