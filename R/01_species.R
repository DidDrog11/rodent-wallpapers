# Rank rodent species by the number of georeferenced GBIF records and keep the
# top n_species. Needs no GBIF login.

source(here::here("R", "00_setup.R"))

# Record counts per species, from GBIF's facet search
counts <- rgbif::occ_count(taxonKey = rodentia_key, hasCoordinate = TRUE, hasGeospatialIssue = FALSE,
                           facet = "speciesKey", facetLimit = 100)
counts <- as.data.table(counts)
counts <- counts[, .(speciesKey = as.integer(speciesKey), n_gbif = as.numeric(count))]
setorder(counts, -n_gbif)
top <- counts[seq_len(n_species)]
top[, rank := .I]

# Names and family from the GBIF taxonomy
top[, `:=`(species = NA_character_, genus = NA_character_, family = NA_character_)]
for (i in seq_len(nrow(top))) {
  usage <- rgbif::name_usage(key = top$speciesKey[i])$data
  top$species[i] <- usage$canonicalName
  top$genus[i] <- usage$genus
  top$family[i] <- usage$family
}

# The smoke test keeps only a couple of them
if (smoke) top <- top[rank %in% smoke_ranks]

fwrite(top, file.path(dir_work, "species.csv"))
print(top[, .(rank, species, family, n_gbif)])
