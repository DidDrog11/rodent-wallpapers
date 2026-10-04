# A simple boosted regression tree (BRT) per species: thinned presences against
# background points, on 19 bioclim variables and elevation.
#
# Choices, made for a wallpaper rather than for inference:
# - Accessible area: every cell within access_km of a presence (1-degree grid).
#   The model is fitted and predicted only there, so it never paints glow onto
#   continents the species has not reached.
# - Background: target-group points, the thinned records of the other species
#   inside the accessible area. They share the recording bias of the presences,
#   which partly cancels it. Topped up with random cells where there are few.
# - Weights: presences and background carry equal total weight.
# - Fixed settings for every species: tree complexity 3, learning rate 0.01,
#   bag fraction 0.5, number of trees by 5-fold cross-validation.
# - Display stretch: suitability is rescaled between its 10th and 99th
#   percentiles within the area, and faded to zero over the outer fade_km.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))
occ <- readRDS(file.path(dir_work, "occurrences_thinned.rds"))
preds <- rast(file.path(dir_pred, "predictors.tif"))
coarse <- rast(res = 1)
limit_m <- access_km * 1000

fits <- list()
importance <- list()
for (i in seq_len(nrow(species))) {
  k <- species$speciesKey[i]
  pres <- occ[speciesKey == k]
  message(species$species[i], ": ", nrow(pres), " thinned presences")

  # Accessible area: distance to the nearest presence, on a 1-degree grid then
  # smoothed onto the predictor grid
  near <- rast(coarse)
  near[unique(cellFromXY(coarse, cbind(pres$lon, pres$lat)))] <- 1
  dist_m <- resample(distance(near), preds[[1]], method = "bilinear")
  area_ext <- ext(trim(ifel(dist_m <= limit_m, 1, NA)))
  preds_area <- crop(preds, area_ext)
  dist_area <- crop(dist_m, area_ext)
  in_area <- values(dist_area, mat = FALSE) <= limit_m & !is.na(values(preds_area[[1]], mat = FALSE))

  # Presence cells, capped for speed
  pres_cells <- unique(cellFromXY(preds_area, cbind(pres$lon, pres$lat)))
  pres_cells <- pres_cells[!is.na(pres_cells) & in_area[pres_cells]]
  if (length(pres_cells) > max_presences) pres_cells <- sample(pres_cells, max_presences)

  # Target-group background, topped up with random cells
  other <- occ[speciesKey != k]
  bg_cells <- unique(cellFromXY(preds_area, cbind(other$lon, other$lat)))
  bg_cells <- setdiff(bg_cells[!is.na(bg_cells) & in_area[bg_cells]], pres_cells)
  n_target_group <- length(bg_cells)
  if (length(bg_cells) > n_background) bg_cells <- sample(bg_cells, n_background)
  if (length(bg_cells) < min_background) {
    spare <- setdiff(which(in_area), c(pres_cells, bg_cells))
    bg_cells <- c(bg_cells, sample(spare, min(length(spare), min_background - length(bg_cells))))
  }

  # Model data, with presences and background weighted equally in total
  model_data <- rbind(data.frame(pa = 1, preds_area[pres_cells]), data.frame(pa = 0, preds_area[bg_cells]))
  model_data <- model_data[complete.cases(model_data), ]
  n_pres <- sum(model_data$pa == 1)
  n_bg <- sum(model_data$pa == 0)
  w <- ifelse(model_data$pa == 1, 1, n_pres / n_bg)

  fit <- gbm::gbm(pa ~ ., data = model_data, weights = w, distribution = "bernoulli", n.trees = 4000,
                  interaction.depth = 3, shrinkage = 0.01, bag.fraction = 0.5, cv.folds = 5, n.cores = 5)
  best_trees <- gbm::gbm.perf(fit, method = "cv", plot.it = FALSE)

  # Predict inside the accessible area, fade at its edge, stretch for display
  suit <- predict(preds_area, fit, n.trees = best_trees, type = "response", na.rm = TRUE)
  fade <- clamp((limit_m - dist_area) / (fade_km * 1000), 0, 1)
  suit <- mask(suit * fade, dist_area <= limit_m, maskvalues = FALSE)
  q <- quantile(values(suit, mat = FALSE), c(0.10, 0.99), na.rm = TRUE)
  suit <- clamp((suit - q[1]) / (q[2] - q[1]), 0, 1)
  names(suit) <- "suitability"
  writeRaster(suit, file.path(dir_sdm, paste0(k, ".tif")), overwrite = TRUE)

  influence <- summary(fit, n.trees = best_trees, plotit = FALSE)
  importance[[i]] <- data.table(speciesKey = k, variable = as.character(influence$var), rel_inf = influence$rel.inf)
  fits[[i]] <- data.table(speciesKey = k, n_presences = n_pres, n_background = n_bg, n_target_group = n_target_group,
                          best_trees = best_trees, cv_deviance = min(fit$cv.error),
                          top_predictors = paste(head(as.character(influence$var), 3), collapse = ", "))
}

fit_summary <- rbindlist(fits)
fwrite(fit_summary, file.path(dir_work, "brt_summary.csv"))
fwrite(rbindlist(importance), file.path(dir_work, "brt_importance.csv"))
print(fit_summary)
