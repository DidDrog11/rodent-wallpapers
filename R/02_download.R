# Download occurrence records for the selected species.
# Full run: one GBIF download (needs GBIF_USER, GBIF_PWD and GBIF_EMAIL in
# .Renviron), which carries a DOI for the wallpaper caption.
# Smoke run: the first 5000 records GBIF returns per species. That is not a
# random sample and must never be used for the real maps.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))
keep_cols <- c("gbifID", "speciesKey", "species", "countryCode", "decimalLatitude", "decimalLongitude",
               "coordinateUncertaintyInMeters", "basisOfRecord", "year")

if (smoke) {
  pieces <- list()
  for (k in species$speciesKey) {
    found <- rgbif::occ_search(speciesKey = k, hasCoordinate = TRUE, hasGeospatialIssue = FALSE,
                               occurrenceStatus = "PRESENT", limit = 5000)$data
    found <- as.data.table(found)
    pieces[[as.character(k)]] <- found[, intersect(keep_cols, names(found)), with = FALSE]
  }
  occ <- rbindlist(pieces, fill = TRUE)
  doi <- "smoke test sample, no DOI"
} else {
  stopifnot("GBIF credentials missing from .Renviron" = nzchar(Sys.getenv("GBIF_USER")))

  # Request the download once and remember its key, so a rerun picks it up
  key_file <- file.path(dir_gbif, "download_key.txt")
  if (!file.exists(key_file)) {
    request <- rgbif::occ_download(
      rgbif::pred_in("speciesKey", species$speciesKey),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred("occurrenceStatus", "PRESENT"),
      rgbif::pred_not(rgbif::pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
      format = "SIMPLE_CSV")
    writeLines(as.character(request), key_file)
  }
  key <- readLines(key_file)[1]
  rgbif::occ_download_wait(key, status_ping = 60)

  zip_file <- rgbif::occ_download_get(key, path = dir_gbif, overwrite = FALSE)
  csv_file <- unzip(zip_file, exdir = dir_gbif)
  occ <- fread(csv_file, select = keep_cols, sep = "\t", quote = "")
  doi <- rgbif::occ_download_meta(key)$doi
}

saveRDS(occ, file.path(dir_work, "occurrences_raw.rds"))
writeLines(doi, file.path(dir_work, "gbif_doi.txt"))
print(occ[, .N, by = species])
