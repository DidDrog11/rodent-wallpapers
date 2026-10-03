# Cross-reference each species against the latest Project ArHa database:
# studies, individuals tested, pathogens detected, and sampling sites.
# Reads the arenavirus_hantavirus repository; writes nothing there.

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
