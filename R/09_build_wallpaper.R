# Assemble the Wallpaper Engine project: the page from wallpaper/, the images,
# and a manifest of everything the page shows. The manifest is written twice:
# manifest.js for the page (loaded by a script tag, which works from local
# files) and manifest.json for other readers such as the life-organiser card.

source(here::here("R", "00_setup.R"))

start_date <- "2026-10-04"   # day one of the rotation

species <- fread(file.path(dir_work, "species.csv"))
thin <- fread(file.path(dir_work, "thinning_summary.csv"))
brt <- fread(file.path(dir_work, "brt_summary.csv"))
arha <- fread(file.path(dir_work, "arha_summary.csv"))
detections <- fread(file.path(dir_work, "arha_detections.csv"))
wiki <- fread(file.path(dir_work, "wikipedia.csv"))
renders <- fread(file.path(dir_work, "renders.csv"))
importance <- fread(file.path(dir_work, "brt_importance.csv"))
seasonality <- fread(file.path(dir_work, "seasonality.csv"))
traits <- fread(file.path(dir_work, "traits.csv"))
doi <- readLines(file.path(dir_work, "gbif_doi.txt"))[1]
web <- readRDS(file.path(dir_work, "web.rds"))

# Years the records span, for the replay counter
occ <- readRDS(file.path(dir_work, "occurrences_thinned.rds"))
occ_years <- split(occ$year, occ$speciesKey)

# The big globe view (0-based) facing the circular mean longitude of the records
big_globe_start <- function(lon) {
  centre <- atan2(mean(sin(lon * pi / 180)), mean(cos(lon * pi / 180))) * 180 / pi
  round(((180 - centre) %% 360) / (360 / globe_frames)) %% globe_frames
}

entries <- list()
for (i in seq_len(nrow(species))) {
  k <- species$speciesKey[i]
  w <- wiki[speciesKey == k]
  a <- arha[speciesKey == k]
  d <- detections[speciesKey == k]
  r <- renders[speciesKey == k]

  # Top six predictors by relative influence, in plain words
  imp <- importance[speciesKey == k][order(-rel_inf)][1:6]
  imp <- imp[!is.na(variable)]
  top_importance <- lapply(seq_len(nrow(imp)), function(j) list(label = unname(predictor_labels[imp$variable[j]]),
                                                                value = round(imp$rel_inf[j], 1)))

  # Records in each calendar month, January first
  by_month <- seasonality[speciesKey == k]
  months <- by_month$N[match(1:12, by_month$month)]
  months[is.na(months)] <- 0

  tr <- traits[speciesKey == k]

  entries[[i]] <- list(
    key = k, rank = species$rank[i], species = species$species[i], family = species$family[i],
    common_name = w$common_name, wiki_url = w$wiki_url, wiki_sentence = w$wiki_sentence,
    n_gbif = species$n_gbif[i], n_thinned = thin[speciesKey == k]$n_thinned, gbif_doi = doi,
    top_predictors = brt[speciesKey == k]$top_predictors,
    hue = r$hue[1], record_colour = record_colour(r$hue[1]), projection = r$projection[1], has_iucn_range = isTRUE(r$has_range[1]),
    importance = top_importance, months = months,
    traits = lapply(seq_len(nrow(tr)), function(j) list(label = tr$label[j], value = tr$value[j])),
    images = as.list(setNames(r$image, r$screen)),
    points = as.list(setNames(r$points, r$screen)),
    year_range = range(occ_years[[as.character(k)]], na.rm = TRUE),
    globe = if (is.na(r$globe[1]) || !nzchar(r$globe[1])) NULL else list(
      sprite = r$globe[1], frames = globe_frames, cols = globe_cols, frame_px = globe_px,
      place = lapply(setNames(seq_len(nrow(r)), r$screen), function(j) list(x = r$globe_x[j], y = r$globe_y[j], size = r$globe_size[j]))),
    big_globe = if (is.na(r$big_globe[1]) || !nzchar(r$big_globe[1])) NULL else list(
      frames = strsplit(r$big_globe[1], ";")[[1]],
      # Start facing the records: frame f is centred on longitude 180 - (f - 1) * 360 / globe_frames
      start = big_globe_start(occ[speciesKey == k]$lon),
      place = lapply(setNames(seq_len(nrow(r)), r$screen), function(j) list(x = r$big_x[j], y = r$big_y[j], size = r$big_size[j]))),
    silhouette = web[[as.character(k)]]$silhouette,
    card_images = list(wikipedia = web[[as.character(k)]]$wiki_image, inaturalist = web[[as.character(k)]]$inat_photos),
    arha = list(studies = a$arha_studies, individuals = a$arha_individuals, tested = a$arha_tested,
                positive = a$arha_positive,
                detections = lapply(seq_len(nrow(d)), function(j) as.list(d[j, .(pathogen, n_positive, assays)]))))
}

manifest <- list(start_date = start_date, built = format(Sys.time(), "%Y-%m-%d %H:%M"), smoke = smoke,
                 site_colour = site_colour, species = entries)
manifest_json <- jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
writeLines(manifest_json, file.path(dir_out, "manifest.json"))
writeLines(c("window.RODENTS = ", manifest_json, ";"), file.path(dir_out, "manifest.js"))

# The page itself, and a preview image for Wallpaper Engine's browser
file.copy(list.files(here("wallpaper"), full.names = TRUE), dir_out, overwrite = TRUE)
# Screen sizes the images were drawn for, so the page can scale positions
writeLines(sprintf("window.RODENT_SCREENS = %s;", jsonlite::toJSON(lapply(screens, function(s) s[c("width", "height")]), auto_unbox = TRUE)),
           file.path(dir_out, "screens.js"))
file.copy(file.path(dir_out, renders[screen == "hd"]$image[1]), file.path(dir_out, "preview.png"), overwrite = TRUE)

message("Wallpaper written to ", dir_out)
