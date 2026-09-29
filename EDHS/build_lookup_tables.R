### BUILD ADMIN LOOKUP TABLES — EDHS
# Maps EDHS region names -> OCHA admin1 p-codes, and geocodes every unique
# survey cluster (GPS point) to its admin1/2/3 polygon by point-in-polygon
# spatial join.
#
# EDHS only reports location as a region name (adm1_name/region_name) plus a
# per-cluster GPS coordinate (lat/lon) — there are no zone/woreda text fields
# to fuzzy-match against (unlike the MPDSR facility data this folder was
# originally built for), so admin2/admin3 assignment has to come from the
# coordinates rather than name-matching.
#
# Run with the working directory set to this folder.
# Outputs: lookup_admin1.csv, lookup_clusters.csv

library(readr)
library(dplyr)
library(sf)

# ── ADMIN 1 ──────────────────────────────────────────────────────────────────
# Hand-coded: 11 harmonised region names appear across the EDHS 2000-2016
# waves (mirrors the adm1_harmonised recoding in explore_edhs.R — keep both
# in sync if either changes)

admin1_lookup <- tribble(
  ~dataset_region,      ~adm1_name,           ~adm1_pcode, ~match_notes,
  "Addis Ababa",        "Addis Ababa",         "ET14",      "",
  "Afar",               "Afar",                "ET02",      "",
  "Amhara",             "Amhara",              "ET03",      "",
  "Benishangul-Gumuz",  "Benishangul-Gumuz",   "ET06",      "",
  "Dire Dawa",          "Dire Dawa",           "ET15",      "",
  "Gambella",           "Gambela",             "ET12",      "spelling variant",
  "Harari",             "Harari",              "ET13",      "",
  "Oromia",             "Oromia",              "ET04",      "",
  "SNNPR",              NA,                    NA,          "pre-2023 region surveyed by EDHS 2000-2016, since split into Central Ethiopia (ET07), South Ethiopia (ET08), South West Ethiopia (ET11) and Sidama (ET16) — no single current polygon covers it; SNNPR women are still placed at admin2/admin3 via the cluster geocoding below",
  "Somali",             "Somali",              "ET05",      "",
  "Tigray",             "Tigray",              "ET01",      ""
)

write.csv(admin1_lookup, "lookup_admin1.csv", row.names = FALSE)
cat("Saved lookup_admin1.csv\n")

# ── CLUSTER GEOCODING (admin1 / admin2 / admin3) ─────────────────────────────
# One GPS point per survey cluster (cluster_number x survey_year). Join against
# the admin3 polygons, which already carry the full admin1/admin2/admin3
# hierarchy, so a single spatial join geocodes all three levels at once.
#
# DHS displaces cluster coordinates for confidentiality (~2km urban / ~5km
# rural, occasionally up to 10km), so a small number of points fall just
# outside every polygon — these are snapped to the nearest polygon, capped at
# 10km (DHS's stated maximum jitter) to avoid spurious matches.

clusters <- read_csv("../Survey/edhs_individual_clean.csv",
                     col_select = c(survey_year, cluster_number, lat, lon),
                     show_col_types = FALSE) %>%
  distinct(survey_year, cluster_number, lat, lon) %>%
  filter(!is.na(lat), !is.na(lon), !(lat == 0 & lon == 0))

cat("Geocoding", nrow(clusters), "unique survey clusters by GPS coordinates...\n")

pts  <- st_as_sf(clusters, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
adm3 <- read_sf("eth_admin3.shp", quiet = TRUE) %>%
  select(adm3_name, adm3_pcode, adm2_name, adm2_pcode, adm1_name, adm1_pcode)

geocoded     <- st_join(pts, adm3, join = st_intersects)
match_method <- if_else(is.na(geocoded$adm3_pcode), NA_character_, "intersects")

miss <- which(is.na(geocoded$adm3_pcode))
if (length(miss) > 0) {
  nn       <- st_nearest_feature(pts[miss, ], adm3)
  nn_dist  <- as.numeric(st_distance(pts[miss, ], adm3[nn, ], by_element = TRUE))
  nn_attrs <- st_drop_geometry(adm3)[nn, ]
  within_jitter <- nn_dist <= 10000

  fill <- miss[within_jitter]
  geocoded$adm3_name[fill]  <- nn_attrs$adm3_name[within_jitter]
  geocoded$adm3_pcode[fill] <- nn_attrs$adm3_pcode[within_jitter]
  geocoded$adm2_name[fill]  <- nn_attrs$adm2_name[within_jitter]
  geocoded$adm2_pcode[fill] <- nn_attrs$adm2_pcode[within_jitter]
  geocoded$adm1_name[fill]  <- nn_attrs$adm1_name[within_jitter]
  geocoded$adm1_pcode[fill] <- nn_attrs$adm1_pcode[within_jitter]

  match_method[fill]                    <- paste0("nearest_", round(nn_dist[within_jitter]), "m")
  match_method[miss[!within_jitter]]    <- "no_match"
}

lookup_clusters <- geocoded %>%
  st_drop_geometry() %>%
  mutate(match_method = match_method) %>%
  select(survey_year, cluster_number, lat, lon,
         adm3_name, adm3_pcode, adm2_name, adm2_pcode, adm1_name, adm1_pcode,
         match_method)

write.csv(lookup_clusters, "lookup_clusters.csv", row.names = FALSE)
cat("Saved lookup_clusters.csv —", nrow(lookup_clusters), "geocoded clusters\n")

cat("\n=== Cluster geocoding summary ===\n")
print(table(lookup_clusters$match_method, useNA = "ifany"))
