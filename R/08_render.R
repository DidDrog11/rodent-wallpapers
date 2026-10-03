# Render one map image per species and screen: pure black, dim GADM country
# borders, modelled suitability as a neon glow, thinned GBIF records as faint
# dots and Project ArHa sampling sites as amber rings. Text other than panel
# labels is not drawn here; the wallpaper page lays it over the image so it
# can move.
#
# Layout:
# - Regional species: one map filling the screen, in a Lambert azimuthal
#   equal-area projection centred on the species.
# - Species spread across the globe: the range is split into regions (cells
#   within cluster_km of each other), the three regions with most records get
#   a panel each in their own centred projection, and a small Equal Earth
#   world map in the corner of the main panel shows the whole range.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))
occ <- readRDS(file.path(dir_work, "occurrences_thinned.rds"))
sites <- fread(file.path(dir_work, "arha_sites.csv"))
countries <- vect(file.path(dir_pred, "gadm_countries.gpkg"))

land_level <- 0.035        # land brightness on the black ocean
border_colour <- "#2b2b2b"
frame_colour <- "#1c1c1c"
label_colour <- "#6e6e6e"
gap_px <- 24               # space between panels and round the edge
cluster_km <- 1500
eqearth <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m"
globe <- densify(as.polygons(ext(-180, 180, -90, 90), crs = "EPSG:4326"), 100000)

# A centred equal-area projection
laea <- function(lon0, lat0) sprintf("+proj=laea +lat_0=%d +lon_0=%d +datum=WGS84 +units=m", round(lat0), round(lon0))

# Country borders that project cleanly: for a centred projection, only the
# hemisphere around the centre
borders_for <- function(crs_map, lon0 = NA, lat0 = NA) {
  if (is.na(lon0)) return(project(countries, crs_map))
  window <- ext(max(-180, lon0 - 100), min(180, lon0 + 100), max(-90, lat0 - 70), min(90, lat0 + 70))
  project(crop(countries, window), crs_map)
}

# Pad an extent, widen it to at least min_span_km so a small region is not
# magnified past the 5' climate grid, and fit it to the aspect of a w x h panel
min_span_km <- 3000
fit_extent <- function(focus, w, h, pad, min_span = min_span_km * 1000) {
  fx <- c(focus$xmin, focus$xmax) + c(-1, 1) * pad * (focus$xmax - focus$xmin)
  fy <- c(focus$ymin, focus$ymax) + c(-1, 1) * pad * (focus$ymax - focus$ymin)
  if (diff(fx) < min_span && diff(fy) < min_span) fx <- mean(fx) + c(-1, 1) * min_span / 2
  if (diff(fx) / diff(fy) < w / h) {
    fx <- mean(fx) + c(-1, 1) * diff(fy) * w / h / 2
  } else {
    fy <- mean(fy) + c(-1, 1) * diff(fx) * h / w / 2
  }
  ext(fx[1], fx[2], fy[1], fy[2])
}

# Colour one panel: land a shade above black, suitability in the hue rising to
# near white at the top of the scale, and a halo in the hue from a blurred copy
compose_panel <- function(suit, crs_map, panel_ext, w, h, borders, hue_rgb, mask_globe = FALSE) {
  tmpl <- rast(panel_ext, ncols = w, nrows = h, crs = crs_map)
  s_map <- subst(project(suit, tmpl, method = "bilinear"), NA, 0)
  # Outside the Equal Earth outline the inverse projection wraps round and
  # would paint a second copy of the range
  if (mask_globe) s_map <- s_map * rasterize(project(globe, crs_map), tmpl, background = 0)
  small <- aggregate(s_map, 4, mean)
  glow <- focal(small, w = focalMat(small, 2 * res(small)[1], type = "Gauss"), fun = "sum", na.rm = TRUE)
  glow <- subst(resample(glow, tmpl, method = "bilinear"), NA, 0)
  land <- rasterize(borders, tmpl, field = 1, background = 0)

  v <- values(s_map, mat = FALSE)
  g <- values(glow, mat = FALSE)
  l <- values(land, mat = FALSE)
  a <- v^1.4
  white <- 0.65 * v^5
  rgb_vals <- matrix(0, nrow = length(v), ncol = 3)
  for (ch in 1:3) {
    rgb_vals[, ch] <- pmin(1, land_level * l * (1 - a) + (hue_rgb[ch] * (1 - white) + white) * a + hue_rgb[ch] * g * 0.5)
  }
  rgb_map <- rast(tmpl, nlyrs = 3)
  values(rgb_map) <- rgb_vals * 255
  rgb_map
}

# Draw a coloured panel into a pixel rectangle (x0, y0 from the top left) of a
# W x H device, with borders, records, sites and an optional label and frame
draw_panel <- function(rgb_map, rect, W, H, borders, pres_v, site_v, site_cex, label = NULL, frame = FALSE, first = FALSE) {
  par(fig = c(rect$x0 / W, (rect$x0 + rect$w) / W, 1 - (rect$y0 + rect$h) / H, 1 - rect$y0 / H), new = !first)
  plotRGB(rgb_map, r = 1, g = 2, b = 3, scale = 255, maxcell = ncell(rgb_map), mar = c(0, 0, 0, 0), axes = FALSE, smooth = FALSE)
  lines(borders, col = border_colour, lwd = 0.6)
  if (!is.null(pres_v)) points(pres_v, pch = 16, cex = 0.12, col = adjustcolor("white", 0.22))
  if (!is.null(site_v)) points(site_v, pch = 1, cex = site_cex, lwd = 0.9, col = adjustcolor(site_colour, 0.85))
  usr <- par("usr")
  if (frame) rect(usr[1], usr[3], usr[2], usr[4], border = frame_colour, lwd = 1.5)
  if (!is.null(label)) {
    text(usr[1] + 0.025 * (usr[2] - usr[1]), usr[4] - 0.03 * (usr[4] - usr[3]), label, adj = c(0, 1),
         col = label_colour, cex = 1.15 * H / 1080, family = "Segoe UI")
  }
}

# Panel rectangles inside the map part of the screen: one large panel on the
# left and the rest stacked on the right
layout_panels <- function(n, area) {
  g <- gap_px
  x <- area$x0 + g
  y <- area$y0 + g
  w <- area$w - 2 * g
  h <- area$h - 2 * g
  if (n == 1) return(list(list(x0 = x, y0 = y, w = w, h = h)))
  main_w <- round((w - g) * 0.6)
  side_w <- w - g - main_w
  side_h <- floor((h - g * (n - 2)) / (n - 1))
  rects <- list(list(x0 = x, y0 = y, w = main_w, h = h))
  for (j in seq_len(n - 1)) rects[[j + 1]] <- list(x0 = x + main_w + g, y0 = y + (j - 1) * (side_h + g), w = side_w, h = side_h)
  rects
}

renders <- list()
for (i in seq_len(nrow(species))) {
  k <- species$speciesKey[i]
  hue <- hues[(species$rank[i] - 1) %% length(hues) + 1]
  hue_rgb <- col2rgb(hue)[, 1] / 255
  suit <- rast(file.path(dir_sdm, paste0(k, ".tif")))
  pres_dt <- occ[speciesKey == k]
  pres_ll <- vect(pres_dt, geom = c("lon", "lat"), crs = "EPSG:4326")
  site_rows <- sites[speciesKey == k]
  site_ll <- if (nrow(site_rows) > 0) vect(site_rows, geom = c("lon", "lat"), crs = "EPSG:4326") else NULL

  # Global when the accessible area is very wide or tall, or spans an ocean: a
  # run of 40 or more degrees of longitude with nothing in it, inside an area
  # over 90 degrees wide
  e <- ext(suit)
  lon_occupied <- which(colSums(!is.na(as.matrix(aggregate(suit, 12, max, na.rm = TRUE), wide = TRUE))) > 0)
  widest_gap <- if (length(lon_occupied) > 1) max(diff(lon_occupied)) - 1 else 0
  lon_span <- e$xmax - e$xmin
  is_global <- lon_span > 160 || (e$ymax - e$ymin) > 110 || (lon_span > 90 && widest_gap >= 40)

  # Regions: 3-degree cells of the accessible area, grouped when within
  # cluster_km of each other, ranked by number of records
  regions <- list()
  if (is_global) {
    coarse <- aggregate(suit, 36, max, na.rm = TRUE)
    cell_ids <- cells(coarse)
    cell_xy <- xyFromCell(coarse, cell_ids)
    group <- cutree(hclust(as.dist(distance(cell_xy, lonlat = TRUE)), method = "single"), h = cluster_km * 1000)
    pres_dt[, region := group[match(cellFromXY(coarse, cbind(lon, lat)), cell_ids)]]
    pres_dt[, country := extract(countries, pres_ll)$NAME_0]
    ranked <- pres_dt[!is.na(region), .N, by = region][order(-N)]
    for (r in head(ranked$region, 3)) {
      in_region <- pres_dt[region == r]
      lon0 <- mean(in_region$lon)
      lat0 <- mean(in_region$lat)
      top_countries <- in_region[!is.na(country), .N, by = country][order(-N)]$country
      corners <- cell_xy[group == r, , drop = FALSE]
      corners <- rbind(corners + 1.5, corners - 1.5, cbind(corners[, 1] + 1.5, corners[, 2] - 1.5), cbind(corners[, 1] - 1.5, corners[, 2] + 1.5))
      corners[, 2] <- pmax(-89, pmin(89, corners[, 2]))
      crs_r <- laea(lon0, lat0)
      regions[[length(regions) + 1]] <- list(crs = crs_r, lon0 = lon0, lat0 = lat0,
                                             focus = ext(project(vect(corners, crs = "EPSG:4326"), crs_r)),
                                             label = paste(head(top_countries, 2), collapse = " · "))
    }
    projection_label <- "Regional panels in Lambert azimuthal equal-area; inset Equal Earth"
  } else {
    lon0 <- mean(c(e$xmin, e$xmax))
    lat0 <- mean(c(e$ymin, e$ymax))
    area_xy <- crds(suit, na.rm = TRUE)
    if (nrow(area_xy) > 50000) area_xy <- area_xy[sample(nrow(area_xy), 50000), ]
    crs_r <- laea(lon0, lat0)
    regions[[1]] <- list(crs = crs_r, lon0 = lon0, lat0 = lat0, focus = ext(project(vect(area_xy, crs = "EPSG:4326"), crs_r)), label = NULL)
    projection_label <- sprintf("Lambert azimuthal equal-area, centred %d°%s %d°%s", abs(round(lat0)), if (lat0 >= 0) "N" else "S",
                                abs(round(lon0)), if (lon0 >= 0) "E" else "W")
  }

  for (scr in names(screens)) {
    s <- screens[[scr]]
    W <- s$width
    H <- s$height
    site_cex <- 0.8 * H / 1080
    map_area <- list(x0 = 0, y0 = 0,
                     w = if (s$text_side == "right") round(W * (1 - s$text_share)) else W,
                     h = if (s$text_side == "bottom") round(H * (1 - s$text_share)) else H)
    img_file <- file.path(dir_img, paste0(k, "_", scr, ".png"))
    ragg::agg_png(img_file, width = W, height = H, units = "px", background = "black")

    if (!is_global) {
      # One map over the whole screen, with the species fitted into the map area
      reg <- regions[[1]]
      fitted <- fit_extent(reg$focus, map_area$w, map_area$h, 0.06)
      px <- (fitted$xmax - fitted$xmin) / map_area$w
      screen_ext <- if (s$text_side == "right") {
        ext(fitted$xmin, fitted$xmin + W * px, fitted$ymin, fitted$ymax)
      } else {
        ext(fitted$xmin, fitted$xmax, fitted$ymax - H * px, fitted$ymax)
      }
      borders <- borders_for(reg$crs, reg$lon0, reg$lat0)
      rgb_map <- compose_panel(suit, reg$crs, screen_ext, W, H, borders, hue_rgb)
      draw_panel(rgb_map, list(x0 = 0, y0 = 0, w = W, h = H), W, H, borders, project(pres_ll, reg$crs),
                 if (is.null(site_ll)) NULL else project(site_ll, reg$crs), site_cex, first = TRUE)
    } else {
      # A panel per region, then the world inset in the main panel's corner
      rects <- layout_panels(length(regions), map_area)
      for (j in seq_along(regions)) {
        reg <- regions[[j]]
        rect_j <- rects[[j]]
        panel_ext <- fit_extent(reg$focus, rect_j$w, rect_j$h, 0.08)
        borders <- borders_for(reg$crs, reg$lon0, reg$lat0)
        rgb_map <- compose_panel(suit, reg$crs, panel_ext, rect_j$w, rect_j$h, borders, hue_rgb)
        draw_panel(rgb_map, rect_j, W, H, borders, project(pres_ll, reg$crs),
                   if (is.null(site_ll)) NULL else project(site_ll, reg$crs), site_cex,
                   label = reg$label, frame = TRUE, first = (j == 1))
      }
      main <- rects[[1]]
      inset_w <- round(main$w * 0.3)
      inset_h <- round(inset_w / 2.05)
      inset <- list(x0 = main$x0 + gap_px, y0 = main$y0 + main$h - inset_h - gap_px, w = inset_w, h = inset_h)
      world_ext <- fit_extent(ext(project(globe, eqearth)), inset_w, inset_h, 0.02, min_span = 0)
      world_borders <- borders_for(eqearth)
      rgb_world <- compose_panel(suit, eqearth, world_ext, inset_w, inset_h, world_borders, hue_rgb, mask_globe = TRUE)
      draw_panel(rgb_world, inset, W, H, world_borders, NULL, NULL, site_cex, frame = TRUE)
    }
    dev.off()

    renders[[length(renders) + 1]] <- data.table(speciesKey = k, screen = scr, image = file.path("img", basename(img_file)),
                                                 hue = hue, projection = projection_label)
  }
  message("Rendered ", species$species[i], if (is_global) paste0(" (", length(regions), " regional panels)") else "")
}

render_summary <- rbindlist(renders)
fwrite(render_summary, file.path(dir_work, "renders.csv"))
