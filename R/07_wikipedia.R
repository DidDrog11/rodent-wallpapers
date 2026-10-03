# Common name, link and a one-line description from the English Wikipedia
# summary for each species. Falls back to the GBIF species page.

source(here::here("R", "00_setup.R"))

species <- fread(file.path(dir_work, "species.csv"))
user_agent <- "rodent-wallpapers/0.1 (https://github.com/DidDrog11/rodent-wallpapers)"

wiki <- list()
for (i in seq_len(nrow(species))) {
  sp <- species$species[i]
  url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", utils::URLencode(gsub(" ", "_", sp)))
  resp <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) == 200) {
    page <- httr2::resp_body_json(resp)
    common <- if (tolower(page$title) == tolower(sp)) NA_character_ else page$title
    first_sentence <- sub("^(.*?[a-z)]\\.)\\s.*$", "\\1", page$extract, perl = TRUE)
    wiki[[i]] <- data.table(speciesKey = species$speciesKey[i], common_name = common,
                            wiki_url = page$content_urls$desktop$page, wiki_sentence = first_sentence)
  } else {
    wiki[[i]] <- data.table(speciesKey = species$speciesKey[i], common_name = NA_character_,
                            wiki_url = paste0("https://www.gbif.org/species/", species$speciesKey[i]), wiki_sentence = NA_character_)
  }
  Sys.sleep(0.2)
}

wiki <- rbindlist(wiki)
fwrite(wiki, file.path(dir_work, "wikipedia.csv"))
print(wiki[, .(speciesKey, common_name, wiki_url)])
