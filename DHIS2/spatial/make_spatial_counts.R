### SPATIAL COUNT DATASET — DHIS2
# Joins the DHIS2 zone x month panel (dhis2_stillbirth_clean.csv) to the OCHA
# admin1 shapefile via lookup_admin1.csv and saves a GeoPackage with, per
# region: the number of zone-month rows reported (n_zone_months), the total
# stillbirths recorded (n_stillbirths) and the mean stillbirths per zone-month
# (mean_sb) — the spatial counterpart of the national time series plotted at
# the end of prelim_explore.R.
#
# region_name is the only DHIS2 location field that maps cleanly and
# completely to the OCHA polygons (see build_lookup_tables.R for why
# admin2/admin3 aren't attempted), so this produces admin1 only.
#
# Run with the working directory set to this folder.

library(readr)
library(dplyr)
library(sf)

# ── Load DHIS2 panel ─────────────────────────────────────────────────────────
panel <- read_csv("../dhis2_stillbirth_clean.csv",
                  col_select = c(org_unit_id, org_unit_name, region_name, date, still_births),
                  show_col_types = FALSE)

lkp1     <- read.csv("lookup_admin1.csv", stringsAsFactors = FALSE)
adm1_shp <- read_sf("eth_admin1.shp", quiet = TRUE)

# ── ADMIN 1 ──────────────────────────────────────────────────────────────────
panel1 <- panel %>% left_join(lkp1, by = c("region_name" = "dataset_region"))

n_unmatched <- sum(is.na(panel1$adm1_pcode))
if (n_unmatched > 0) {
  message(n_unmatched, " zone-month rows have no matched region p-code and are excluded from the spatial layer.")
}

counts1 <- panel1 %>%
  filter(!is.na(adm1_pcode)) %>%
  group_by(adm1_pcode) %>%
  summarise(n_zone_months = n(),
            n_stillbirths = sum(still_births, na.rm = TRUE),
            mean_sb       = n_stillbirths / n_zone_months,
            .groups = "drop")

spatial_admin1 <- adm1_shp %>%
  left_join(counts1, by = "adm1_pcode") %>%
  mutate(n_zone_months = coalesce(n_zone_months, 0L),
         n_stillbirths = coalesce(n_stillbirths, 0L)) %>%
  select(adm1_name, adm1_pcode, n_zone_months, n_stillbirths, mean_sb, geometry)

st_write(spatial_admin1, "spatial_admin1.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("Saved spatial_admin1.gpkg —", nrow(spatial_admin1), "polygons\n")
print(as.data.frame(spatial_admin1)[, c("adm1_name", "adm1_pcode", "n_zone_months", "n_stillbirths", "mean_sb")])

# ── Summary ──────────────────────────────────────────────────────────────────
cat("\n=== Coverage summary ===\n")
cat("Total zone-month rows in panel:  ", nrow(panel), "\n")
cat("Mapped at admin1:                ", sum(counts1$n_zone_months), "\n")
cat("Total stillbirths in panel:      ", sum(panel$still_births, na.rm = TRUE), "\n")
cat("Total stillbirths mapped:        ", sum(counts1$n_stillbirths), "\n")
