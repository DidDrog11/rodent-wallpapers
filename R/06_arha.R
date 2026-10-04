# Cross-reference each species against the latest Project ArHa database:
# studies, individuals tested, pathogens detected, and sampling sites; then
# IUCN outlines and COMBINE life history from the ArHa repositories.
# Reads arenavirus_hantavirus and arha-macroecology; writes nothing there.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))

db_dir <- here("..", "arenavirus_hantavirus", "data", "database")
db_file <- sort(list.files(db_dir, pattern = "^Project_ArHa_database_.*\\.rds$", full.names = TRUE), decreasing = TRUE)[1]
message("Using ", basename(db_file))
db <- readRDS(db_file)
host <- as.data.table(db$host)
pathogen <- as.data.table(db$pathogen)

# Match hosts by GBIF key, falling back to the species name
host[, host_record_id := as.character(host_record_id)]
host[, speciesKey := fifelse(gbif_id %in% species$speciesKey, as.integer(gbif_id),
                             species$speciesKey[match(host_species, species$species)])]
host_match <- host[!is.na(speciesKey)]

# Studies and individuals sampled
sampled <- host_match[, .(arha_studies = uniqueN(study_id), arha_individuals = sum(number_of_hosts, na.rm = TRUE)), by = speciesKey]

# Tests and detections
tests <- merge(pathogen[, .(host_record_id = as.character(host_record_id), pathogen_species_cleaned, pathogen_family,
                            assay, number_tested, number_positive)],
               host_match[, .(host_record_id, speciesKey)], by = "host_record_id")
tested <- tests[, .(arha_tested = sum(number_tested, na.rm = TRUE), arha_positive = sum(number_positive, na.rm = TRUE)), by = speciesKey]

# Positives resolved to one pathogen species are listed by name. Those left
# unresolved (no species, or several candidates joined by " | ") are pooled.
detected <- tests[number_positive > 0]
resolved <- !is.na(detected$pathogen_species_cleaned) & !grepl("|", detected$pathogen_species_cleaned, fixed = TRUE)
detected[, pathogen := fifelse(resolved, pathogen_species_cleaned, "Not resolved to species")]
detections <- detected[, .(n_positive = sum(number_positive), assays = paste(sort(unique(as.character(assay))), collapse = ", ")),
                       by = .(speciesKey, pathogen)]
setorder(detections, speciesKey, -n_positive)

# Sampling sites with usable coordinates
sites <- unique(host_match[coord_status == "valid" & !is.na(latitude) & !is.na(longitude),
                           .(speciesKey, lat = latitude, lon = longitude)])

arha <- merge(species[, .(speciesKey, species)], sampled, by = "speciesKey", all.x = TRUE)
arha <- merge(arha, tested, by = "speciesKey", all.x = TRUE)
arha[, arha_database := basename(db_file)]

fwrite(arha, file.path(dir_work, "arha_summary.csv"))
fwrite(detections, file.path(dir_work, "arha_detections.csv"))
fwrite(sites, file.path(dir_work, "arha_sites.csv"))
print(arha)
print(detections)

# IUCN expert ranges, held in the ArHa repository for its host species only.
# IUCN spatial data may not be redistributed: these stay in data/, which is
# never committed.
iucn <- vect(readRDS(here("..", "arenavirus_hantavirus", "data", "iucn_ranges.rds")))
iucn <- iucn[iucn$SCI_NAME %in% species$species]
if (nrow(iucn) > 0) {
  iucn <- aggregate(iucn, by = "SCI_NAME")
  iucn <- simplifyGeom(iucn, tolerance = 0.05)
  iucn$speciesKey <- species$speciesKey[match(iucn$SCI_NAME, species$species)]
  writeVector(iucn[, "speciesKey"], file.path(dir_work, "iucn_ranges.gpkg"), overwrite = TRUE)
}
message("IUCN ranges found for ", nrow(iucn), " of ", nrow(species), " species")

# Life history from COMBINE (reported values only, so gaps stay gaps).
# Coding checked against known species: activity 1 nocturnal, 2 mixed,
# 3 diurnal; hibernation_torpor 1 hibernates or uses torpor.
combine <- fread(here("..", "arha-macroecology", "data", "external", "combine_data_reported.csv"))
our_species <- species$species
combine <- combine[iucn2020_binomial %in% our_species]

fmt_mass <- function(g) if (is.na(g)) NA_character_ else if (g >= 1000) sprintf("%.1f kg", g / 1000) else sprintf("%.0f g", g)
fmt_num <- function(x, digits = 1) if (is.na(x)) NA_character_ else formatC(x, format = "f", digits = digits, drop0trailing = TRUE)
activity_words <- c("1" = "Nocturnal", "2" = "Day and night", "3" = "Diurnal")
stratum_words <- c(G = "Ground", S = "Ground and trees", Ar = "Trees", A = "Air", M = "Water")

traits <- list()
for (i in seq_len(nrow(combine))) {
  cb <- combine[i]
  diet_parts <- c(Plants = cb$dphy_plant, Invertebrates = cb$dphy_invertebrate, Vertebrates = cb$dphy_vertebrate)
  diet_parts <- diet_parts[!is.na(diet_parts) & diet_parts > 0]
  diet <- if (length(diet_parts)) paste(sprintf("%s %.0f%%", names(diet_parts), diet_parts)[order(-diet_parts)], collapse = " · ") else NA_character_
  litter <- if (is.na(cb$litter_size_n)) NA_character_ else
    paste0(fmt_num(cb$litter_size_n), " young", if (!is.na(cb$litters_per_year_n)) paste0(", ", fmt_num(cb$litters_per_year_n), " litters a year") else "")
  rows <- data.table(
    label = c("Weight", "Body length", "Active", "Hibernates or torpor", "Lives on", "Diet", "Litters", "Longest lived"),
    value = c(fmt_mass(cb$adult_mass_g),
              if (is.na(cb$adult_body_length_mm)) NA_character_ else paste(fmt_num(cb$adult_body_length_mm / 10), "cm"),
              unname(activity_words[as.character(cb$activity_cycle)]),
              if (is.na(cb$hibernation_torpor)) NA_character_ else if (cb$hibernation_torpor == 1) "Yes" else "No",
              unname(stratum_words[cb$foraging_stratum]),
              diet, litter,
              if (is.na(cb$max_longevity_d)) NA_character_ else paste(fmt_num(cb$max_longevity_d / 365.25), "years")))
  rows <- rows[!is.na(value)]
  rows[, speciesKey := species$speciesKey[match(cb$iucn2020_binomial, species$species)]]
  traits[[i]] <- rows
}
traits <- rbindlist(traits)
fwrite(traits, file.path(dir_work, "traits.csv"))
print(traits[, .N, by = speciesKey])
