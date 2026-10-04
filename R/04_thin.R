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

# Records per calendar month, from the cleaned set before thinning removes the
# signal. Southern-hemisphere records are kept as they are, not shifted.
seasonality <- occ[month %in% 1:12, .N, by = .(speciesKey, month)]
fwrite(seasonality, file.path(dir_work, "seasonality.csv"))

# A random 20% per species and country, at least one record each
occ[is.na(countryCode) | countryCode == "", countryCode := "unknown"]
occ[, keep := seq_len(.N) %in% sample(.N, ceiling(.N * country_fraction)), by = .(speciesKey, countryCode)]
occ <- occ[keep == TRUE][, keep := NULL]

# One record per predictor grid cell, so dense cities and museum localities
# do not dominate the model
template <- rast(file.path(dir_pred, "predictors.tif"))[[1]]
occ[, cell := cellFromXY(template, cbind(lon, lat))]
thinned <- unique(occ[!is.na(cell)], by = c("speciesKey", "cell"))

# Drop stray records: groups of 3-degree cells more than cluster_km from the
# rest of the species' records and holding fewer than min_cluster_records.
# Real introductions (beaver in Patagonia, coypu in Korea) hold 15 or more;
# misplaced records (bank vole in Madagascar) hold a handful.
coarse <- rast(res = 3)
thinned[, coarse_cell := cellFromXY(coarse, cbind(lon, lat))]
thinned[, cluster := NA_integer_]
for (k in unique(thinned$speciesKey)) {
  cell_n <- thinned[speciesKey == k, .N, by = coarse_cell]
  cell_xy <- xyFromCell(coarse, cell_n$coarse_cell)
  group <- if (nrow(cell_xy) > 1) cutree(hclust(as.dist(distance(cell_xy, lonlat = TRUE)), method = "single"), h = cluster_km * 1000) else 1L
  thinned[speciesKey == k, cluster := group[match(coarse_cell, cell_n$coarse_cell)]]
}
thinned[, cluster_n := .N, by = .(speciesKey, cluster)]
strays <- thinned[cluster_n < min_cluster_records, .(n_strays = .N), by = speciesKey]
thinned <- thinned[cluster_n >= min_cluster_records][, c("coarse_cell", "cluster", "cluster_n") := NULL]

saveRDS(thinned, file.path(dir_work, "occurrences_thinned.rds"))

n_thinned <- thinned[, .(n_thinned = .N), by = speciesKey]
thin_summary <- merge(n_raw, n_clean, by = "speciesKey", all = TRUE)
thin_summary <- merge(thin_summary, n_thinned, by = "speciesKey", all = TRUE)
thin_summary <- merge(thin_summary, strays, by = "speciesKey", all.x = TRUE)
thin_summary[is.na(n_strays), n_strays := 0L]
fwrite(thin_summary, file.path(dir_work, "thinning_summary.csv"))
print(thin_summary)
