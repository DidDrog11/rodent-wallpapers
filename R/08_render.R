# Render one map image per species and screen: pure black, dim GADM country
# borders, modelled suitability as a neon glow and Project ArHa sampling sites
# as amber rings. Records are not drawn: their screen positions are written for
# the page, which draws and replays them by year. Text other than panel
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
hillshade <- rast(file.path(dir_pred, "hillshade.tif"))
iucn_file <- file.path(dir_work, "iucn_ranges.gpkg")
iucn <- if (file.exists(iucn_file)) vect(iucn_file) else NULL

land_level <- 0.035        # land brightness on the black ocean
flat_shade <- sin(40 * pi / 180)  # hillshade of flat ground for a 40-degree sun
coast_tint <- c(0.012, 0.022, 0.05)  # faint deep blue in the sea near coasts
border_colour <- "#2b2b2b"
range_colour <- adjustcolor("#d0d0d0", 0.4)
frame_colour <- "#1c1c1c"
label_colour <- "#6e6e6e"
gap_px <- 24               # space between panels and round the edge
eqearth <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m"
earth_radius <- 6378137
globe <- densify(as.polygons(ext(-180, 180, -90, 90), crs = "EPSG:4326"), 100000)

# A centred equal-area projection
laea <- function(lon0, lat0) sprintf("+proj=laea +lat_0=%d +lon_0=%d +datum=WGS84 +units=m", round(lat0), round(lon0))
deg <- function(x, pos, neg) sprintf("%d°%s", abs(round(x)), if (x >= 0) pos else neg)

# The longitude-latitude window whose lines project cleanly: the hemisphere
# round the centre of a Lambert projection, or everything but the seam of a
# recentred Equal Earth
laea_window <- function(lon0, lat0) ext(max(-180, lon0 - 100), min(180, lon0 + 100), max(-90, lat0 - 70), min(90, lat0 + 70))
project_for <- function(v, crs_map, window = NULL) {
  if (is.null(v) || nrow(v) == 0) return(NULL)
  if (!is.null(window)) v <- crop(v, window)
  if (nrow(v) == 0) return(NULL)
  project(v, crs_map)
}
borders_for <- function(crs_map, window = NULL) project_for(countries, crs_map, window)

# A single map for one range: Lambert azimuthal equal-area centred on it, or,
# for a range more than 120 degrees of longitude wide, Equal Earth centred on
# it and cropped to it, which keeps straight edges where Lambert would curve
single_region <- function(lon_range, lat_range, area_xy) {
  lon0 <- mean(lon_range)
  lat0 <- mean(lat_range)
  if (diff(lon_range) > 120) {
    crs_r <- sprintf("+proj=eqearth +lon_0=%d +datum=WGS84 +units=m", round(lon0))
    window <- ext(max(-180, lon0 - 179), min(180, lon0 + 179), -90, 90)
    label <- paste("Equal Earth, centred", deg(lon0, "E", "W"))
  } else {
    crs_r <- laea(lon0, lat0)
    window <- laea_window(lon0, lat0)
    label <- paste("Lambert azimuthal equal-area, centred", deg(lat0, "N", "S"), deg(lon0, "E", "W"))
  }
  list(crs = crs_r, window = window, focus = ext(project(vect(area_xy, crs = "EPSG:4326"), crs_r)), label = NULL,
       projection_label = label)
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

# A Gaussian blur at quarter resolution, back on the panel grid
blur <- function(x, tmpl, sigma_cells) {
  small <- aggregate(x, 4, mean)
  b <- focal(small, w = focalMat(small, sigma_cells * res(small)[1], type = "Gauss"), fun = "sum", na.rm = TRUE)
  subst(resample(b, tmpl, method = "bilinear"), NA, 0)
}

# Colour one panel: land a shade above black and lit by the terrain, a faint
# blue haze in the sea along coasts, suitability in the hue rising to near
# white at the top of the scale, and a halo in the hue from a blurred copy
compose_panel <- function(suit, crs_map, panel_ext, w, h, borders, hue_rgb, mask = "none") {
  tmpl <- rast(panel_ext, ncols = w, nrows = h, crs = crs_map)
  s_map <- subst(project(suit, tmpl, method = "bilinear"), NA, 0)
  # Outside the Equal Earth outline the inverse projection wraps round and
  # would paint a second copy of the range; outside a globe's disc there is
  # nothing to paint
  inside <- switch(mask,
                   eqearth = rasterize(project(globe, crs_map), tmpl, background = 0),
                   disc = (init(tmpl, "x")^2 + init(tmpl, "y")^2) <= earth_radius^2,
                   NULL)
  if (!is.null(inside)) s_map <- s_map * inside
  glow <- blur(s_map, tmpl, 2)
  land <- rasterize(borders, tmpl, field = 1, background = 0)
  coast <- blur(land, tmpl, 3) * (1 - land)
  if (!is.null(inside)) coast <- coast * inside
  relief <- clamp(subst(project(hillshade, tmpl, method = "bilinear"), NA, flat_shade) / flat_shade, 0.4, 1.6)

  v <- values(s_map, mat = FALSE)
  g <- values(glow, mat = FALSE)
  l <- values(land, mat = FALSE)
  co <- values(coast, mat = FALSE)
  rl <- values(relief, mat = FALSE)
  a <- v^1.4
  white <- 0.65 * v^5
  rgb_vals <- matrix(0, nrow = length(v), ncol = 3)
  for (ch in 1:3) {
    base <- land_level * l * rl + coast_tint[ch] * co
    rgb_vals[, ch] <- pmin(1, base * (1 - a) + (hue_rgb[ch] * (1 - white) + white) * a + hue_rgb[ch] * g * 0.5)
  }
  rgb_map <- rast(tmpl, nlyrs = 3)
  values(rgb_map) <- rgb_vals * 255
  rgb_map
}

# Draw a coloured panel into a pixel rectangle (x0, y0 from the top left) of a
# W x H device, with borders, records, sites and an optional label and frame
draw_panel <- function(rgb_map, rect, W, H, borders, pres_v, site_v, site_cex, label = NULL, frame = FALSE, first = FALSE,
                       range_v = NULL) {
  par(fig = c(rect$x0 / W, (rect$x0 + rect$w) / W, 1 - (rect$y0 + rect$h) / H, 1 - rect$y0 / H), new = !first)
  plotRGB(rgb_map, r = 1, g = 2, b = 3, scale = 255, maxcell = ncell(rgb_map), mar = c(0, 0, 0, 0), axes = FALSE, smooth = FALSE)
  lines(borders, col = border_colour, lwd = 0.6)
  if (!is.null(range_v)) lines(range_v, col = range_colour, lwd = 1.1 * H / 1080, lty = "22")
  if (!is.null(pres_v)) points(pres_v, pch = 16, cex = 0.12, col = adjustcolor("white", 0.22))
  if (!is.null(site_v)) points(site_v, pch = 1, cex = site_cex, lwd = 0.9, col = adjustcolor(site_colour, 0.85))
  usr <- par("usr")
  if (frame) rect(usr[1], usr[3], usr[2], usr[4], border = frame_colour, lwd = 1.5)
  if (!is.null(label)) {
    text(usr[1] + 0.025 * (usr[2] - usr[1]), usr[4] - 0.03 * (usr[4] - usr[3]), label, adj = c(0, 1),
         col = label_colour, cex = 1.15 * H / 1080, family = "Segoe UI")
  }
}

# Screen pixel positions and years of the records drawn in a panel. The page
# draws the records itself, so it can replay them by year.
points_on_screen <- function(pts_v, panel_ext, rect) {
  xy <- crds(pts_v)
  px <- rect$x0 + (xy[, 1] - panel_ext$xmin) / (panel_ext$xmax - panel_ext$xmin) * rect$w
  py <- rect$y0 + (panel_ext$ymax - xy[, 2]) / (panel_ext$ymax - panel_ext$ymin) * rect$h
  keep <- is.finite(px) & is.finite(py) & px >= rect$x0 & px < rect$x0 + rect$w & py >= rect$y0 & py < rect$y0 + rect$h
  data.table(x = round(px[keep], 1), y = round(py[keep], 1), year = pts_v$year[keep])
}

# A spinning globe for species shown in panels: globe_frames orthographic
# views round the world, packed in a grid sprite the page steps through.
# Borders are cut to the visible hemisphere before projecting.
render_globe <- function(suit, hue_rgb, lat0, file) {
  rows <- ceiling(globe_frames / globe_cols)
  W <- globe_cols * globe_px
  H <- rows * globe_px
  r <- earth_radius * 1.03
  rim <- seq(0, 2 * pi, length.out = 240)
  ragg::agg_png(file, width = W, height = H, units = "px", background = "black")
  for (f in seq_len(globe_frames)) {
    lon0 <- 180 - (f - 1) * 360 / globe_frames   # centre moves west, so the Earth turns eastward as it does
    crs_o <- sprintf("+proj=ortho +lat_0=%d +lon_0=%.1f +datum=WGS84 +units=m", round(lat0), lon0)
    hemisphere <- buffer(vect(cbind(lon0, lat0), crs = "EPSG:4326"), width = 9.6e6)
    borders <- project(crop(countries, hemisphere), crs_o)
    rgb_globe <- compose_panel(suit, crs_o, ext(-r, r, -r, r), globe_px, globe_px, borders, hue_rgb, mask = "disc")
    rect_f <- list(x0 = ((f - 1) %% globe_cols) * globe_px, y0 = ((f - 1) %/% globe_cols) * globe_px, w = globe_px, h = globe_px)
    draw_panel(rgb_globe, rect_f, W, H, borders, NULL, NULL, 0, first = (f == 1))
    lines(earth_radius * cos(rim), earth_radius * sin(rim), col = "#2a2a2a", lwd = 1)
  }
  dev.off()
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
  hue <- if (species$family[i] %in% names(family_hues)) family_hues[[species$family[i]]] else other_hue
  hue_rgb <- col2rgb(hue)[, 1] / 255
  suit <- rast(file.path(dir_sdm, paste0(k, ".tif")))
  pres_dt <- occ[speciesKey == k]
  pres_ll <- vect(pres_dt, geom = c("lon", "lat"), crs = "EPSG:4326")
  site_rows <- sites[speciesKey == k]
  site_ll <- if (nrow(site_rows) > 0) vect(site_rows, geom = c("lon", "lat"), crs = "EPSG:4326") else NULL
  range_ll <- if (is.null(iucn)) NULL else iucn[iucn$speciesKey == k]

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
      top_countries <- in_region[!is.na(country), .N, by = country][order(-N)]$country
      corners <- cell_xy[group == r, , drop = FALSE]
      corners <- rbind(corners + 1.5, corners - 1.5, cbind(corners[, 1] + 1.5, corners[, 2] - 1.5), cbind(corners[, 1] - 1.5, corners[, 2] + 1.5))
      corners[, 2] <- pmax(-89, pmin(89, corners[, 2]))
      reg <- single_region(range(corners[, 1]), range(corners[, 2]), corners)
      reg$label <- paste(head(top_countries, 2), collapse = " · ")
      regions[[length(regions) + 1]] <- reg
    }
    layout_mode <- "panels"
    projection_label <- "Regional panels in equal-area projections; inset Equal Earth"
    # When the largest region's records chain round the world (brown rat, house
    # mouse) the species gets a single world map. When there is only one
    # region and it spans a wide continent (red squirrel), a single map of it.
    region_xy <- cell_xy[group == ranked$region[1], , drop = FALSE]
    if (diff(range(region_xy[, 1])) > 200) {
      layout_mode <- "world"
      regions <- list(list(crs = eqearth, window = NULL, focus = ext(project(globe, eqearth)), label = NULL))
      projection_label <- "Equal Earth"
    } else if (length(regions) == 1) {
      layout_mode <- "single"
      regions[[1]] <- single_region(range(region_xy[, 1]) + c(-1.5, 1.5), range(region_xy[, 2]) + c(-1.5, 1.5), region_xy)
      projection_label <- regions[[1]]$projection_label
    }
  } else {
    layout_mode <- "single"
    area_xy <- crds(suit, na.rm = TRUE)
    if (nrow(area_xy) > 50000) area_xy <- area_xy[sample(nrow(area_xy), 50000), ]
    regions[[1]] <- single_region(c(e$xmin, e$xmax), c(e$ymin, e$ymax), area_xy)
    projection_label <- regions[[1]]$projection_label
  }

  # The spinning globe replaces the static world inset for species in panels
  globe_file <- NA_character_
  if (layout_mode == "panels") {
    globe_file <- file.path(dir_img, paste0("globe_", k, ".png"))
    render_globe(suit, hue_rgb, max(-30, min(40, mean(pres_dt$lat))), globe_file)
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
    on_screen <- list()
    globe_rect <- NULL

    if (layout_mode != "panels") {
      # One map over the whole screen, with the species (or the world) fitted
      # into the map area
      reg <- regions[[1]]
      is_world <- layout_mode == "world"
      fitted <- fit_extent(reg$focus, map_area$w, map_area$h, if (is_world) 0.02 else 0.06, min_span = if (is_world) 0 else min_span_km * 1000)
      px <- (fitted$xmax - fitted$xmin) / map_area$w
      screen_ext <- if (s$text_side == "right") {
        ext(fitted$xmin, fitted$xmin + W * px, fitted$ymin, fitted$ymax)
      } else {
        ext(fitted$xmin, fitted$xmax, fitted$ymax - H * px, fitted$ymax)
      }
      full_rect <- list(x0 = 0, y0 = 0, w = W, h = H)
      borders <- borders_for(reg$crs, reg$window)
      rgb_map <- compose_panel(suit, reg$crs, screen_ext, W, H, borders, hue_rgb, mask = if (is_world) "eqearth" else "none")
      draw_panel(rgb_map, full_rect, W, H, borders, NULL,
                 if (is.null(site_ll)) NULL else project(site_ll, reg$crs), site_cex, first = TRUE,
                 range_v = project_for(range_ll, reg$crs, reg$window))
      on_screen[[1]] <- points_on_screen(project(pres_ll, reg$crs), screen_ext, full_rect)
    } else {
      # A panel per region, and a place for the globe in the main panel's corner
      rects <- layout_panels(length(regions), map_area)
      for (j in seq_along(regions)) {
        reg <- regions[[j]]
        rect_j <- rects[[j]]
        panel_ext <- fit_extent(reg$focus, rect_j$w, rect_j$h, 0.08)
        borders <- borders_for(reg$crs, reg$window)
        rgb_map <- compose_panel(suit, reg$crs, panel_ext, rect_j$w, rect_j$h, borders, hue_rgb)
        draw_panel(rgb_map, rect_j, W, H, borders, NULL,
                   if (is.null(site_ll)) NULL else project(site_ll, reg$crs), site_cex,
                   label = reg$label, frame = TRUE, first = (j == 1), range_v = project_for(range_ll, reg$crs, reg$window))
        on_screen[[j]] <- points_on_screen(project(pres_ll, reg$crs), panel_ext, rect_j)
      }
      main <- rects[[1]]
      side <- round(min(main$w * 0.26, main$h * 0.4))
      globe_rect <- list(x = main$x0 + gap_px, y = main$y0 + main$h - side - gap_px, size = side)
    }
    dev.off()

    # Records for the page to draw and replay, oldest first
    pts <- rbindlist(on_screen)[order(year, na.last = FALSE)]
    pts_file <- file.path(dir_out, "pts", paste0(k, "_", scr, ".js"))
    dir.create(dirname(pts_file), showWarnings = FALSE)
    writeLines(sprintf('window.RODENT_POINTS = {x: [%s], y: [%s], year: [%s]};',
                       paste(pts$x, collapse = ","), paste(pts$y, collapse = ","),
                       paste(ifelse(is.na(pts$year), "null", pts$year), collapse = ",")), pts_file)

    renders[[length(renders) + 1]] <- data.table(speciesKey = k, screen = scr, image = file.path("img", basename(img_file)),
                                                 points = file.path("pts", basename(pts_file)),
                                                 hue = hue, projection = projection_label,
                                                 has_range = !is.null(range_ll) && nrow(range_ll) > 0,
                                                 globe = if (is.na(globe_file)) NA_character_ else file.path("img", basename(globe_file)),
                                                 globe_x = if (is.null(globe_rect)) NA else globe_rect$x,
                                                 globe_y = if (is.null(globe_rect)) NA else globe_rect$y,
                                                 globe_size = if (is.null(globe_rect)) NA else globe_rect$size)
  }
  message("Rendered ", species$species[i], ": ", projection_label)
}

render_summary <- rbindlist(renders)
fwrite(render_summary, file.path(dir_work, "renders.csv"))
