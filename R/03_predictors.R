# Climate, elevation and country borders. Downloaded once and cached in
# data/predictors, shared by smoke and full runs.

source(here::here("R", "00_setup.R"))

pred_file <- file.path(dir_pred, "predictors.tif")
if (!file.exists(pred_file)) {
  bio <- geodata::worldclim_global(var = "bio", res = res_arcmin, path = dir_pred)
  names(bio) <- paste0("bio", 1:19)
  elev <- geodata::elevation_global(res = res_arcmin, path = dir_pred)
  names(elev) <- "elev"
  writeRaster(c(bio, elev), pred_file)
}

# Hillshade from the elevation layer, lit from the north-west, for shading
# land. Elevation is exaggerated eightfold: at 5' a cell is about 9 km across,
# so real slopes come out nearly flat and the relief would not show.
shade_file <- file.path(dir_pred, "hillshade_z8.tif")
if (!file.exists(shade_file)) {
  elev <- rast(pred_file)[["elev"]] * 8
  slope_aspect <- terrain(elev, v = c("slope", "aspect"), unit = "radians")
  hill <- shade(slope_aspect[["slope"]], slope_aspect[["aspect"]], angle = 40, direction = 315)
  writeRaster(hill, shade_file)
}

# GADM country outlines (level 0), simplified by geodata for small-scale maps
border_file <- file.path(dir_pred, "gadm_countries.gpkg")
if (!file.exists(border_file)) {
  countries <- geodata::world(resolution = 2, level = 0, path = dir_pred)
  writeVector(countries, border_file)
}

print(rast(pred_file))
