# Settings shared by every script. Each script sources this first.
# Set the environment variable RW_SMOKE=1 for the smoke test: two species and a
# small, non-random GBIF sample instead of the full download.

library(here)
library(terra)
library(data.table)

smoke <- Sys.getenv("RW_SMOKE", "0") == "1"

# Over SSH R takes the profile folder as home and misses Documents\.Renviron,
# where the GBIF login lives, so read it explicitly when needed
documents_renviron <- file.path(Sys.getenv("USERPROFILE"), "Documents", ".Renviron")
if (!nzchar(Sys.getenv("GBIF_USER")) && file.exists(documents_renviron)) readRenviron(documents_renviron)

# Species: the top n rodent species by georeferenced GBIF records
rodentia_key <- 1459L
n_species <- 30L
smoke_ranks <- c(3L, 8L)   # grey squirrel (global), bank vole (regional, ArHa host)

# Thinning
res_arcmin <- 5               # WorldClim resolution, about 9 km at the equator
country_fraction <- 0.2       # share of records kept per species and country
max_uncertainty_m <- 20000    # drop records less precise than this, where stated
cluster_km <- 1500            # records closer than this belong to one region
min_cluster_records <- 15     # smaller detached regions are treated as strays

# Model
access_km <- 500              # accessible area: within this distance of a presence
fade_km <- 200                # suitability fades to zero over the outer fade_km
max_presences <- 10000
n_background <- 10000         # target-group background points, at most
min_background <- 5000        # topped up with random cells if fewer

# Screens: pixel size, and where the text column or band sits
screens <- list(
  uw = list(width = 3440, height = 1440, text_side = "right", text_share = 0.24),
  hd = list(width = 1920, height = 1080, text_side = "bottom", text_share = 0.22))

# Neon hue by family, so a colour comes to mean a family over the month, and
# the fixed colour for ArHa sites. Each hue was checked against the site
# colour for colour-blind separation.
family_hues <- c(Sciuridae = "#00e5ff", Cricetidae = "#9b7bff", Muridae = "#ff3df0", Castoridae = "#3d9bff")
other_hue <- "#4dffa6"
site_colour <- "#ffb020"

# Plain names for the predictors, for the variable importance bars
predictor_labels <- c(
  bio1 = "Mean annual temp.", bio2 = "Daily temp. range", bio3 = "Isothermality",
  bio4 = "Temp. seasonality", bio5 = "Hottest month max.", bio6 = "Coldest month min.",
  bio7 = "Annual temp. range", bio8 = "Wettest quarter temp.", bio9 = "Driest quarter temp.",
  bio10 = "Warmest quarter temp.", bio11 = "Coldest quarter temp.", bio12 = "Annual rainfall",
  bio13 = "Wettest month rain", bio14 = "Driest month rain", bio15 = "Rainfall seasonality",
  bio16 = "Wettest quarter rain", bio17 = "Driest quarter rain", bio18 = "Warmest quarter rain",
  bio19 = "Coldest quarter rain", elev = "Elevation")

# Paths. Smoke runs keep their own data so they can never feed the real run.
run_name <- if (smoke) "smoke" else "full"
dir_work <- here("data", run_name)
dir_gbif <- file.path(dir_work, "gbif")
dir_sdm <- file.path(dir_work, "sdm")
dir_pred <- here("data", "predictors")
dir_out <- here("output", run_name)
dir_img <- file.path(dir_out, "img")
for (d in c(dir_work, dir_gbif, dir_sdm, dir_pred, dir_out, dir_img)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

set.seed(20261004)
