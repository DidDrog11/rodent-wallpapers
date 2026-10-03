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
| `R/06_arha.R` | Studies, tests, detections and sampling sites from the latest Project ArHa database (read only) |
| `R/07_wikipedia.R` | Common name, link and first sentence from Wikipedia |
| `R/08_render.R` | One image per species for each screen shape |
| `R/09_build_wallpaper.R` | Assembles the Wallpaper Engine project in `output/full/` |

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

## Rodent of the day card

The life-organiser dashboard reads `output/full/manifest.json` and shows the same species on its Today tab, with a link to Wikipedia. Both use the same rule: days since `start_date`, modulo the number of species, in local time.

## Where it runs

The PC builds everything. The finished `output/full/` folder is copied to Wallpaper Engine on the laptop, where the monitors are. The page picks the species by date, so nothing needs to run daily.
