### SPATIAL COUNT DATASETS
# Joins MPDSR records to OCHA admin shapefiles via lookup tables.
# Outputs one GeoPackage per admin level with a column `n_records`.
#
# NOTE: counts are based on the REPORTING FACILITY location (Region, ZonName,
# woreda). If you want to map by deceased's RESIDENCE instead, swap those
# columns for Region1, ZonName1, woreda1 below.

library(readxl)
library(sf)
library(dplyr)

# ── Load raw data ─────────────────────────────────────────────────────────────
df <- read_excel("Final data set_mpdsr.xlsx", sheet = "Final data set_1")

lkp1 <- read.csv("lookup_admin1.csv", stringsAsFactors = FALSE)
lkp2 <- read.csv("lookup_admin2.csv", stringsAsFactors = FALSE)
lkp3 <- read.csv("lookup_admin3.csv", stringsAsFactors = FALSE)

adm1_shp <- read_sf("OCHA/eth_admin1.shp")
adm2_shp <- read_sf("OCHA/eth_admin2.shp")
adm3_shp <- read_sf("OCHA/eth_admin3.shp")

# ── ADMIN 1 ───────────────────────────────────────────────────────────────────
counts1 <- df %>%
  left_join(lkp1, by = c("Region" = "dataset_region")) %>%
  filter(!is.na(adm1_pcode)) %>%          # drop SNNP (no p-code)
  count(adm1_pcode, name = "n_records")

# Count the SNNP rows separately for reporting
n_snnp <- df %>%
  left_join(lkp1, by = c("Region" = "dataset_region")) %>%
  filter(is.na(adm1_pcode)) %>%
  nrow()
if (n_snnp > 0) {
  message("Admin1: ", n_snnp,
          " rows coded as SNNP have no shapefile polygon and are excluded from the spatial layer.")
}

spatial_admin1 <- adm1_shp %>%
  left_join(counts1, by = "adm1_pcode") %>%
  mutate(n_records = coalesce(n_records, 0L)) %>%
  select(adm1_name, adm1_pcode, n_records, geometry)

st_write(spatial_admin1, "spatial_admin1.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("Saved spatial_admin1.gpkg —", nrow(spatial_admin1), "polygons\n")
print(as.data.frame(spatial_admin1)[, c("adm1_name","adm1_pcode","n_records")])

# ── ADMIN 2 ───────────────────────────────────────────────────────────────────
# Keep only rows that matched to a p-code
lkp2_matched <- lkp2 %>% filter(!is.na(adm2_pcode))

counts2 <- df %>%
  left_join(lkp2_matched, by = c("ZonName" = "dataset_zone")) %>%
  filter(!is.na(adm2_pcode)) %>%
  count(adm2_pcode, name = "n_records")

n_unmatched2 <- df %>%
  left_join(lkp2_matched, by = c("ZonName" = "dataset_zone")) %>%
  filter(is.na(adm2_pcode)) %>%
  nrow()
if (n_unmatched2 > 0) {
  message("Admin2: ", n_unmatched2,
          " rows have no matched zone p-code (e.g. SNNP zones, ambiguous North Shewa, Western Tigray) and are excluded.")
}

spatial_admin2 <- adm2_shp %>%
  left_join(counts2, by = "adm2_pcode") %>%
  mutate(n_records = coalesce(n_records, 0L)) %>%
  select(adm2_name, adm2_pcode, adm1_name, adm1_pcode, n_records, geometry)

st_write(spatial_admin2, "spatial_admin2.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("\nSaved spatial_admin2.gpkg —", nrow(spatial_admin2), "polygons\n")
spatial_admin2 %>%
  as.data.frame() %>%
  select(-geometry) %>%
  filter(n_records > 0) %>%
  arrange(desc(n_records)) %>%
  head(50) %>%
  print()

# ── ADMIN 3 ───────────────────────────────────────────────────────────────────
# Use only high/medium/exact matches for admin3; flag low quality
lkp3_good <- lkp3 %>%
  filter(match_quality %in% c("exact", "high", "medium"),
         !is.na(adm3_pcode))

counts3 <- df %>%
  left_join(lkp3_good, by = c("woreda" = "dataset_woreda")) %>%
  filter(!is.na(adm3_pcode)) %>%
  count(adm3_pcode, name = "n_records")

n_unmatched3 <- df %>%
  left_join(lkp3_good, by = c("woreda" = "dataset_woreda")) %>%
  filter(is.na(adm3_pcode)) %>%
  nrow()
if (n_unmatched3 > 0) {
  message("Admin3: ", n_unmatched3,
          " rows excluded (woreda match quality too low or no match). Check lookup_admin3.csv for 'low_review_needed' entries.")
}

spatial_admin3 <- adm3_shp %>%
  left_join(counts3, by = "adm3_pcode") %>%
  mutate(n_records = coalesce(n_records, 0L)) %>%
  select(adm3_name, adm3_pcode, adm2_name, adm2_pcode,
         adm1_name, adm1_pcode, n_records, geometry)

st_write(spatial_admin3, "spatial_admin3.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("\nSaved spatial_admin3.gpkg —", nrow(spatial_admin3), "polygons\n")
cat("Woredas with at least one record:", sum(spatial_admin3$n_records > 0), "\n")
spatial_admin3 %>%
  as.data.frame() %>%
  select(-geometry) %>%
  filter(n_records > 0) %>%
  arrange(desc(n_records)) %>%
  head(30) %>%
  print()

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n=== Record coverage summary ===\n")
cat("Total rows in dataset:", nrow(df), "\n")
cat("Mapped at admin1:", sum(counts1$n_records), "\n")
cat("Mapped at admin2:", sum(counts2$n_records), "\n")
cat("Mapped at admin3:", sum(counts3$n_records), "\n")
