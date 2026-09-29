### SPATIAL COUNT DATASETS — EDHS
# Joins EDHS individual + stillbirth records to the OCHA admin shapefiles via
# the lookup tables and saves one GeoPackage per admin level with the women
# surveyed (n_women), stillbirth count (n_stillbirths) and stillbirth rate per
# 1,000 women (sb_rate) — the same population/summary as sb_by_adm1 in
# explore_edhs.R, just at all three admin levels instead of admin1 only.
#
# admin1 is matched by harmonised region name via lookup_admin1.csv.
# admin2/admin3 are matched via each woman's survey cluster, geocoded by GPS
# point-in-polygon join in lookup_clusters.csv (see build_lookup_tables.R) —
# EDHS has no zone/woreda names to match against directly.
#
# Run with the working directory set to this folder.

library(readr)
library(dplyr)
library(sf)

# ── Load EDHS data ────────────────────────────────────────────────────────────
indiv      <- read_csv("../Survey/edhs_individual_clean.csv", show_col_types = FALSE)
stillbirth <- read_csv("../Survey/edhs_stillbirth_clean.csv", show_col_types = FALSE)

indiv <- indiv %>% mutate(uid = paste0(survey_year, "_", case_id))

# One row per woman with known stillbirth status — same population as sb_by_adm1
# (survey_year.x / cluster_number come from indiv; "survey_year" collides with
# the stillbirth file and is suffixed by left_join — see explore_edhs.R)
women <- left_join(indiv, stillbirth, by = "uid") %>%
  filter(!is.na(had_stillbirth)) %>%
  mutate(adm1_harmonised = case_when(
    tolower(adm1_name) %in% c("addis ababa", "addis", "addis abeba") ~ "Addis Ababa",
    tolower(adm1_name) %in% c("afar region", "afar", "affar")        ~ "Afar",
    tolower(adm1_name) %in% c("amhara region", "amhara")             ~ "Amhara",
    tolower(adm1_name) %in% c("benishangul gumuz", "ben-gumz")       ~ "Benishangul-Gumuz",
    tolower(adm1_name) %in% c("dire dawa")                           ~ "Dire Dawa",
    tolower(adm1_name) %in% c("gambella", "gambela")                 ~ "Gambella",
    tolower(adm1_name) %in% c("harari")                              ~ "Harari",
    tolower(adm1_name) %in% c("oromiya region", "oromiya")           ~ "Oromia",
    tolower(adm1_name) %in% c("snnpr", "snnp")                       ~ "SNNPR",
    tolower(adm1_name) %in% c("somali")                              ~ "Somali",
    tolower(adm1_name) %in% c("tigray region", "tigray")             ~ "Tigray",
    TRUE ~ adm1_name
  )) %>%
  rename(survey_year = survey_year.x)

lkp1      <- read.csv("lookup_admin1.csv", stringsAsFactors = FALSE)
lkp_clust <- read.csv("lookup_clusters.csv", stringsAsFactors = FALSE) %>%
  select(survey_year, cluster_number, adm2_pcode, adm3_pcode)

adm1_shp <- read_sf("eth_admin1.shp", quiet = TRUE)
adm2_shp <- read_sf("eth_admin2.shp", quiet = TRUE)
adm3_shp <- read_sf("eth_admin3.shp", quiet = TRUE)

# ── ADMIN 1 ───────────────────────────────────────────────────────────────────
women1 <- women %>% left_join(lkp1, by = c("adm1_harmonised" = "dataset_region"))

n_snnp <- sum(is.na(women1$adm1_pcode))
if (n_snnp > 0) {
  message("Admin1: ", n_snnp, " women coded as SNNPR have no single current admin1 ",
          "polygon (region split post-2023) and are excluded from this layer — ",
          "they ARE included at admin2/admin3 via the cluster geocoding.")
}

counts1 <- women1 %>%
  filter(!is.na(adm1_pcode)) %>%
  group_by(adm1_pcode) %>%
  summarise(n_women       = n(),
            n_stillbirths = sum(had_stillbirth, na.rm = TRUE),
            sb_rate       = n_stillbirths / n_women * 1000,
            .groups = "drop")

spatial_admin1 <- adm1_shp %>%
  left_join(counts1, by = "adm1_pcode") %>%
  mutate(n_women = coalesce(n_women, 0L), n_stillbirths = coalesce(n_stillbirths, 0L)) %>%
  select(adm1_name, adm1_pcode, n_women, n_stillbirths, sb_rate, geometry)

st_write(spatial_admin1, "spatial_admin1.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("Saved spatial_admin1.gpkg —", nrow(spatial_admin1), "polygons\n")
print(as.data.frame(spatial_admin1)[, c("adm1_name", "adm1_pcode", "n_women", "n_stillbirths", "sb_rate")])

# ── ADMIN 2 / ADMIN 3 ─────────────────────────────────────────────────────────
# Each woman inherits her admin2/admin3 codes from her survey cluster's geocode
women23 <- women %>% inner_join(lkp_clust, by = c("survey_year", "cluster_number"))

n_ungeocoded <- nrow(women) - nrow(women23)
if (n_ungeocoded > 0) {
  message("Admin2/3: ", n_ungeocoded, " women belong to clusters with no usable ",
          "GPS coordinates and are excluded from the admin2/admin3 layers.")
}

counts2 <- women23 %>%
  filter(!is.na(adm2_pcode)) %>%
  group_by(adm2_pcode) %>%
  summarise(n_women       = n(),
            n_stillbirths = sum(had_stillbirth, na.rm = TRUE),
            sb_rate       = n_stillbirths / n_women * 1000,
            .groups = "drop")

spatial_admin2 <- adm2_shp %>%
  left_join(counts2, by = "adm2_pcode") %>%
  mutate(n_women = coalesce(n_women, 0L), n_stillbirths = coalesce(n_stillbirths, 0L)) %>%
  select(adm2_name, adm2_pcode, adm1_name, adm1_pcode, n_women, n_stillbirths, sb_rate, geometry)

st_write(spatial_admin2, "spatial_admin2.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("\nSaved spatial_admin2.gpkg —", nrow(spatial_admin2), "polygons\n")
spatial_admin2 %>%
  st_drop_geometry() %>%
  filter(n_women > 0) %>%
  arrange(desc(n_stillbirths)) %>%
  head(20) %>%
  print()

counts3 <- women23 %>%
  filter(!is.na(adm3_pcode)) %>%
  group_by(adm3_pcode) %>%
  summarise(n_women       = n(),
            n_stillbirths = sum(had_stillbirth, na.rm = TRUE),
            sb_rate       = n_stillbirths / n_women * 1000,
            .groups = "drop")

spatial_admin3 <- adm3_shp %>%
  left_join(counts3, by = "adm3_pcode") %>%
  mutate(n_women = coalesce(n_women, 0L), n_stillbirths = coalesce(n_stillbirths, 0L)) %>%
  select(adm3_name, adm3_pcode, adm2_name, adm2_pcode, adm1_name, adm1_pcode,
         n_women, n_stillbirths, sb_rate, geometry)

st_write(spatial_admin3, "spatial_admin3.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("\nSaved spatial_admin3.gpkg —", nrow(spatial_admin3), "polygons\n")
cat("Woredas with at least one woman surveyed:", sum(spatial_admin3$n_women > 0), "\n")
spatial_admin3 %>%
  st_drop_geometry() %>%
  filter(n_women > 0) %>%
  arrange(desc(n_stillbirths)) %>%
  head(20) %>%
  print()

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n=== Coverage summary ===\n")
cat("Total women with known stillbirth status:", nrow(women), "\n")
cat("Mapped at admin1:", sum(counts1$n_women), "\n")
cat("Mapped at admin2:", sum(counts2$n_women), "\n")
cat("Mapped at admin3:", sum(counts3$n_women), "\n")
