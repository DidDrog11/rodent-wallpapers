# Rodent wallpapers

A desktop wallpaper that shows one rodent species a day: a simple modelled distribution from GBIF records, drawn as a neon glow on a black map, with what Project ArHa knows about the species as an arenavirus or hantavirus host. It is a visualisation, not an analysis. The models are fitted with fixed settings and nobody checks them species by species.

## Pipeline

Run `run_pipeline.cmd` on the PC, or `run_pipeline.cmd smoke` for a two-species test. Each script logs to `logs/`.

| Script | What it does |
|---|---|
| `R/00_setup.R` | Settings shared by every script |
| `R/01_species.R` | Ranks rodent species by georeferenced GBIF records and keeps the top 30 |
| `R/02_download.R` | One GBIF download for those species, with a DOI. Needs a GBIF login in `.Renviron` |
| `R/03_predictors.R` | WorldClim bioclim and elevation at 5 arc-minutes; GADM country borders |
| `R/04_thin.R` | Drops unplaceable, imprecise, fossil and captive records; keeps 20% per species and country; then one record per grid cell |
| `R/05_brt.R` | One boosted regression tree per species |
| `R/06_arha.R` | Studies, tests, detections and sampling sites from the latest Project ArHa database; IUCN outlines and COMBINE life history from the ArHa repositories (all read only) |
| `R/07_web.R` | Wikipedia name, link, sentence and image; iNaturalist photos for the card; PhyloPic silhouette |
| `R/08_render.R` | One image per species for each screen shape |
| `R/09_build_wallpaper.R` | Assembles the Wallpaper Engine project in `output/full/` |

## Stray records

After thinning, each species' records are grouped into regions: 3-degree cells within 1,500 km of each other. A detached region holding fewer than 15 thinned records is treated as misplaced or misidentified and dropped. Across the first full run this split cleanly: real introduced populations (North American beaver in Patagonia and Finland, coypu in Korea, house mouse in Hawaii, muskrat in Tierra del Fuego) held 15 or more, and strays (bank vole in Madagascar and North America) held 10 or fewer. The number dropped per species is in `data/full/thinning_summary.csv`.

## Modelling choices

These are settings for a picture, written down so they are not implicit.

- Accessible area: cells within 500 km of a presence. Fitting and prediction are confined to it, and suitability fades to zero over its outer 200 km.
- Background: target-group points, the thinned records of the other 29 species inside the accessible area, at most 10,000. They carry roughly the same recording bias as the presences. Random cells top them up to 5,000 where there are too few.
- Weights: presences and background carry equal total weight.
- BRT: tree complexity 3, learning rate 0.01, bag fraction 0.5, number of trees by five-fold cross-validation, at most 4,000.
- Display: suitability is stretched between its 10th and 99th percentiles within the accessible area. The scale is relative within each species, not comparable between them.

## Look

Pure black for the OLED panel, dim grey text, one neon hue per species. Each hue was checked against the amber of the ArHa site rings for colour-blind separation. The map shifts a few pixels each hour and the text every two hours, so no element sits on the same pixels all day.

A species with one regional range gets a single map in a Lambert azimuthal equal-area projection centred on it. A species spread across oceans or continents is split into regions (cells within 1,500 km of each other); the three regions with most records each get a panel in their own centred projection, labelled with their top countries, and a small Equal Earth world map in the corner of the main panel shows the whole range. Panels never zoom in closer than 3,000 km across, so the 5' climate grid does not show.

Land is shaded by terrain from the elevation layer, and the sea carries a faint blue haze along coasts. Where Project ArHa holds an IUCN expert range for the species it is drawn as a dashed grey outline. IUCN spatial data may not be redistributed, so the outlines and the images that contain them stay in `data/` and `output/`, which are never committed.

Colour follows family: squirrels cyan, cricetids violet, murids magenta, beavers blue, others mint.

The text panel adds life history from COMBINE (reported values only; Soria et al. 2021), records by calendar month from the cleaned GBIF records, and the six predictors the model leaned on most, by relative influence.

## Motion

The page draws the thinned GBIF records itself, from screen positions written by `R/08_render.R`. A few seconds after loading, and every ten minutes after that, it clears them and replays them oldest first over about 50 seconds, each flaring briefly in the species' hue, with a counter showing the year reached. Between replays nothing moves, so the GPU idles.

Species shown in regional panels get a spinning globe in the corner of the main panel: 48 orthographic views pre-rendered in R into one sprite, cross-faded every 2.5 seconds, one turn every two minutes.

A PhyloPic silhouette sits above the species name: the species' own if PhyloPic has one, else its genus's, else its family's (CC0, public domain or CC BY only). The credit line names the taxon drawn when it is not the species itself.

Open `index.html?species=N` in a browser to preview species N; Wallpaper Engine never passes that.

## Rodent of the day card

The life-organiser dashboard reads `output/full/manifest.json` and shows the same species on its Today tab, with a link to Wikipedia. Both use the same rule: days since `start_date`, modulo the number of species, in local time.

## Where it runs

The PC builds everything. The finished `output/full/` folder is copied to Wallpaper Engine on the laptop, where the monitors are. The page picks the species by date, so nothing needs to run daily.
