# Clean and thin the records: drop unplaceable and imprecise records, keep a
# random 20% per species and country, then one record per 5' grid cell.

source(here::here("R", "00_setup.R"))

occ <- readRDS(file.path(dir_work, "occurrences_raw.rds"))
setnames(occ, c("decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters"), c("lat", "lon", "uncertainty_m"))
n_raw <- occ[, .(n_raw = .N), by = speciesKey]

# Records that cannot be placed: missing, null island, latitude equal to
# longitude (a common transcription error), or out of range
occ <- occ[!is.na(lat) & !is.na(lon) & !(lat == 0 & lon == 0) & lat != lon & abs(lat) <= 90 & abs(lon) <= 180]

# Fossils and captive animals do not describe where the species lives
occ <- occ[!basisOfRecord %in% c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN")]

# Imprecise records, where precision is stated
occ <- occ[is.na(uncertainty_m) | uncertainty_m <= max_uncertainty_m]
n_clean <- occ[, .(n_clean = .N), by = speciesKey]

# A random 20% per species and country, at least one record each
occ[is.na(countryCode) | countryCode == "", countryCode := "unknown"]
occ[, keep := seq_len(.N) %in% sample(.N, ceiling(.N * country_fraction)), by = .(speciesKey, countryCode)]
occ <- occ[keep == TRUE][, keep := NULL]

# One record per predictor grid cell, so dense cities and museum localities
# do not dominate the model
template <- rast(file.path(dir_pred, "predictors.tif"))[[1]]
occ[, cell := cellFromXY(template, cbind(lon, lat))]
thinned <- unique(occ[!is.na(cell)], by = c("speciesKey", "cell"))

saveRDS(thinned, file.path(dir_work, "occurrences_thinned.rds"))

n_thinned <- thinned[, .(n_thinned = .N), by = speciesKey]
thin_summary <- merge(n_raw, n_clean, by = "speciesKey", all = TRUE)
thin_summary <- merge(thin_summary, n_thinned, by = "speciesKey", all = TRUE)
fwrite(thin_summary, file.path(dir_work, "thinning_summary.csv"))
print(thin_summary)
