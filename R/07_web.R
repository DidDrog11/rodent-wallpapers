# What the web knows about each species:
# - Wikipedia: common name, link, first sentence and lead image
# - iNaturalist: up to three Creative Commons photos, with attribution, for the
#   life-organiser card (never shown on the wallpaper)
# - PhyloPic: a silhouette for the wallpaper, from the species if there is one,
#   else its genus, else its family. Only CC0, public domain and CC BY images;
#   the file is downloaded so the wallpaper never fetches from the web.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))
user_agent <- "rodent-wallpapers/0.1 (https://github.com/DidDrog11/rodent-wallpapers)"

get_json <- function(url) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200) return(NULL)
  httr2::resp_body_json(resp)
}

licence_label <- function(url) {
  if (grepl("publicdomain/zero", url)) "CC0" else if (grepl("publicdomain/mark", url)) "Public domain" else
    if (grepl("licenses/by/", url)) "CC BY" else NA_character_
}

wiki <- list()
web <- list()
for (i in seq_len(nrow(species))) {
  k <- species$speciesKey[i]
  sp <- species$species[i]

  # Wikipedia summary
  page <- get_json(paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", utils::URLencode(gsub(" ", "_", sp))))
  if (!is.null(page)) {
    common <- if (tolower(page$title) == tolower(sp)) NA_character_ else page$title
    wiki_url <- page$content_urls$desktop$page
    sentence <- sub("^(.*?[a-z)]\\.)\\s.*$", "\\1", page$extract, perl = TRUE)
    wiki_image <- if (!is.null(page$thumbnail)) page$thumbnail$source else NA_character_
  } else {
    common <- NA_character_
    wiki_url <- paste0("https://www.gbif.org/species/", k)
    sentence <- NA_character_
    wiki_image <- NA_character_
  }
  wiki[[i]] <- data.table(speciesKey = k, common_name = common, wiki_url = wiki_url, wiki_sentence = sentence)

  # iNaturalist: Creative Commons photos only
  photos <- list()
  found <- get_json(paste0("https://api.inaturalist.org/v1/taxa?rank=species&per_page=1&q=", utils::URLencode(sp)))
  # The top species match for the name; iNaturalist may file it under a synonym
  if (!is.null(found) && length(found$results) > 0) {
    taxon <- get_json(paste0("https://api.inaturalist.org/v1/taxa/", found$results[[1]]$id))
    for (tp in taxon$results[[1]]$taxon_photos) {
      if (length(photos) == 3) break
      if (is.null(tp$photo$license_code)) next
      photos[[length(photos) + 1]] <- list(url = sub("/square\\.", "/medium.", tp$photo$url), attribution = tp$photo$attribution,
                                           licence = toupper(tp$photo$license_code),
                                           page = paste0("https://www.inaturalist.org/photos/", tp$photo$id))
    }
  }

  # PhyloPic silhouette: species, then genus, then family
  usage <- rgbif::name_usage(key = k)$data
  silhouette <- NULL
  for (level in c("species", "genus", "family")) {
    key <- switch(level, species = k, genus = usage$genusKey, family = usage$familyKey)
    node <- get_json(sprintf("https://api.phylopic.org/resolve/gbif.org/species?objectIDs=%s&embed_primaryImage=true", key))
    img <- if (is.null(node)) NULL else node$`_embedded`$primaryImage
    if (is.null(img)) next
    licence <- licence_label(img$`_links`$license$href)
    if (is.na(licence)) next
    raster_files <- img$`_links`$rasterFiles
    file_url <- raster_files[[length(raster_files)]]$href   # the smallest, 512 px wide
    local_file <- file.path(dir_img, paste0("silhouette_", k, ".png"))
    httr2::request(file_url) |> httr2::req_user_agent(user_agent) |> httr2::req_perform(path = local_file)
    silhouette <- list(image = file.path("img", basename(local_file)), level = level, depicted = img$`_links`$self$title,
                       contributor = img$`_links`$contributor$title, licence = licence)
    break
  }

  web[[as.character(k)]] <- list(wiki_image = wiki_image, inat_photos = photos, silhouette = silhouette)
  message(sp, ": silhouette ", if (is.null(silhouette)) "none" else paste(silhouette$level, silhouette$depicted),
          "; ", length(photos), " iNaturalist photos")
  Sys.sleep(0.5)
}

fwrite(rbindlist(wiki), file.path(dir_work, "wikipedia.csv"))
saveRDS(web, file.path(dir_work, "web.rds"))
