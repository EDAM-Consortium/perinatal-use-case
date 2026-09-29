
#### PROLAPSE PREVALENCE & INCIDENCE ANALYSIS
### Policy question: What is the estimated prevalence and incidence of
###   uterovaginal prolapse at national and sub-regional levels?
###
### Administrative level: Level 3 (Zonal) — chosen because the key variable
###   "GC40.3 Uterovaginal prolapse" is reported by 123–137 of 174 zones,
###   giving adequate sub-regional coverage. Level 2 (14 regions) is too coarse;
###   Level 4 (Wereda) is too sparse for prevalence estimation.
###
### Study window: 2015–2018.  The DHIS2 dataset contains two coding eras:
###   - 2010–2014: ICD-10-like codes (N80-1137, K55-873/874)
###   - 2015–2018: ICD-11 codes (GC40.3, GC40.2, GC40, DB31.2, DB53)
###   GC40.3 reaches consistent national coverage only from 2015 onward.
###   Both eras are shown in the temporal trend; primary analysis uses 2015–2018.
###
### IMPORTANT CAVEAT:  These are HMIS facility-reported case counts, not
###   population-based data.  They represent diagnosed/treated cases presenting
###   to health facilities, likely a fraction of true burden.  True prevalence
###   and incidence rates require population denominators (not in this dataset).
###   Outputs should be interpreted as "facility-reported case burden."

## Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(scales)
library(ggrepel)

## VARIABLE DEFINITIONS 
PROLAPSE_IDS <- c(
  # ICD-11 uterovaginal / pelvic (primary focus)
  "ykGyWdc6qG2",   # GC40.3 — Uterovaginal prolapse        ← KEY variable
  "w3yjQC21alO",   # GC40.2 — Prolapse of vaginal apex
  "VV6shblSmaH",   # GC40   — Pelvic organ prolapse (unspecified)
  # ICD-10-era female genital
  "PV3qk3Pq6YT",   # N80-1137 — Female genital prolapse (pre-2015)
  # ICD-11 rectal
  "oPUwDiz9jAU",   # DB31.2  — Rectal prolapse
  # ICD-10-era rectal
  "g5chXpY2vYW",   # K55-874 — Rectal prolapse
  # ICD-11 anal
  "Nbqk9NOhpw3",   # DB53    — Anal prolapse
  # ICD-10-era anal
  "cmZDOp42uaF"    # K55-873 — Anal prolapse
)

SHORT_NAMES <- c(
  "ykGyWdc6qG2" = "GC40.3 Uterovaginal",
  "w3yjQC21alO" = "GC40.2 Vaginal apex",
  "VV6shblSmaH" = "GC40 Pelvic organ",
  "PV3qk3Pq6YT" = "N80-1137 Female genital",
  "oPUwDiz9jAU" = "DB31.2 Rectal (ICD-11)",
  "g5chXpY2vYW" = "K55-874 Rectal (ICD-10)",
  "Nbqk9NOhpw3" = "DB53 Anal (ICD-11)",
  "cmZDOp42uaF" = "K55-873 Anal (ICD-10)"
)

ICD11_IDS    <- c("ykGyWdc6qG2", "w3yjQC21alO", "VV6shblSmaH",
                  "oPUwDiz9jAU", "Nbqk9NOhpw3")
UTEROVAG_IDS <- c("ykGyWdc6qG2")   # key outcome for policy question
STUDY_YEARS  <- 2015:2018

## LOAD DATA 
raw_zonal   <- read_csv("data/fistula_level3_Zonal.csv",   show_col_types = FALSE)
raw_region  <- read_csv("data/fistula_level2_Regional.csv", show_col_types = FALSE)

## PARSE PERIOD 
parse_period <- function(df) {
  df |>
    mutate(
      period_chr = as.character(period),
      year       = as.integer(str_sub(period_chr, 1, 4)),
      month      = as.integer(str_sub(period_chr, 5, 6)),
      date       = as.Date(paste(year, month, "01", sep = "-"))
    )
}

raw_zonal  <- parse_period(raw_zonal)
raw_region <- parse_period(raw_region)

## EXTRACT REGION FROM HIERARCHY 
raw_zonal <- raw_zonal |>
  mutate(region_id = str_split_fixed(org_unit_hierarchy, "/", 3)[, 2])

region_names <- tribble(
  ~region_id,     ~region_name,
  "yb9NKGA8uqt",  "Amhara",
  "XU2wpLlX4Vk",  "Oromia",
  "tDoLtk2ylu4",  "Central Ethiopia",
  "xNUoZIrGKxQ",  "Sidama",
  "a2QIIR2UXcd",  "South Ethiopia",
  "Gmw0DJLXGtx",  "Tigray",
  "HIlnt7Qj8do",  "South West Ethiopia",
  "PCKGSJoNHXi",  "Somali",
  "UFtGyqJMEZh",  "Afar",
  "Fccw8uMlJHN",  "Benishangul-Gumuz",
  "moBiwh9h5Ce",  "Gambella",
  "yY9BLUUegel",  "Addis Ababa",
  "G9hDiPNoB7d",  "Dire Dawa",
  "b9nYedsL8te",  "Harari"
)

raw_zonal <- raw_zonal |> left_join(region_names, by = "region_id")

## FILTER TO PROLAPSE VARIABLES 
prolapse_zonal  <- raw_zonal  |> filter(data_element_id %in% PROLAPSE_IDS) |>
  mutate(short_name = SHORT_NAMES[data_element_id])

prolapse_region <- raw_region |> filter(data_element_id %in% PROLAPSE_IDS) |>
  mutate(short_name = SHORT_NAMES[data_element_id])

## COMPLETENESS ASSESSMENT 
# Total possible zone-months per year
zones_per_year <- prolapse_zonal |>
  filter(year >= 2010, year <= 2018) |>
  group_by(year) |>
  summarise(n_zones_total = n_distinct(org_unit_name), .groups = "drop") |>
  mutate(possible_zone_months = n_zones_total * 12)

# Non-zero zone-months per variable per year
completeness <- prolapse_zonal |>
  filter(year >= 2010, year <= 2018) |>
  group_by(year, data_element_id, short_name) |>
  summarise(
    zone_months_total    = n(),                          # rows present (reported)
    zone_months_nonzero  = sum(value > 0, na.rm = TRUE), # non-zero entries
    total_cases          = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(zones_per_year, by = "year") |>
  mutate(
    pct_nonzero = round(100 * zone_months_nonzero / possible_zone_months, 1)
  ) |>
  select(year, short_name, possible_zone_months, zone_months_nonzero, pct_nonzero, total_cases)

cat("\n=== Completeness by variable (zone-months with non-zero entries) ===\n")
print(completeness |> arrange(short_name, year), n = Inf)

## PRIMARY ANALYSIS DATASET (2015–2018, ICD-11 era) 
focal <- prolapse_zonal |> filter(year %in% STUDY_YEARS)

## NATIONAL TIME SERIES (ALL YEARS, MONTHLY) 
# Include full 2010–2018 to show coding transition
monthly_national <- prolapse_zonal |>
  filter(year >= 2010, year <= 2018, !is.na(date)) |>
  group_by(date, year, data_element_id, short_name) |>
  summarise(monthly_cases = sum(value, na.rm = TRUE), .groups = "drop")

annual_national <- prolapse_zonal |>
  filter(year >= 2010, year <= 2018) |>
  group_by(year, data_element_id, short_name) |>
  summarise(annual_cases = sum(value, na.rm = TRUE), .groups = "drop")

## ZONAL SUMMARY (PRIMARY 2015–2018) 
# Aggregate across ICD-11 uterovaginal variables (GC40.3 + GC40.2 + GC40)
zonal_summary <- focal |>
  filter(data_element_id %in% ICD11_IDS) |>
  group_by(org_unit_id, org_unit_name, region_id, region_name,
           data_element_id, short_name) |>
  summarise(total_cases = sum(value, na.rm = TRUE),
            annual_avg  = total_cases / length(STUDY_YEARS),
            .groups = "drop")

# Combined uterovaginal (GC40.3 only — the most specific ICD-11 code)
zonal_utero <- focal |>
  filter(data_element_id == "ykGyWdc6qG2") |>
  group_by(org_unit_id, org_unit_name, region_id, region_name) |>
  summarise(
    total_cases   = sum(value, na.rm = TRUE),
    annual_avg    = total_cases / length(STUDY_YEARS),
    n_months_rpt  = sum(value > 0),
    .groups = "drop"
  ) |>
  mutate(has_cases = total_cases > 0)

## REGIONAL SUMMARY 
regional_utero <- focal |>
  filter(data_element_id == "ykGyWdc6qG2") |>
  group_by(region_name) |>
  summarise(
    total_cases  = sum(value, na.rm = TRUE),
    annual_avg   = total_cases / length(STUDY_YEARS),
    n_zones_rpt  = n_distinct(org_unit_name[value > 0]),
    .groups = "drop"
  ) |>
  arrange(desc(total_cases))

regional_all <- focal |>
  filter(data_element_id %in% ICD11_IDS) |>
  group_by(region_name, data_element_id, short_name) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop")

cat("\n=== Regional uterovaginal prolapse burden (GC40.3), 2015–2018 ===\n")
print(regional_utero)

## TOP ZONES BY BURDEN
top_zones <- zonal_utero |>
  filter(has_cases) |>
  arrange(desc(total_cases)) |>
  slice_head(n = 20)

cat("\n=== Top 20 zones: uterovaginal prolapse cases (2015–2018) ===\n")
print(top_zones |> select(org_unit_name, region_name, total_cases, annual_avg))

## SAVE SUMMARY TABLES 
write_csv(zonal_utero,     "prolapse/summaries/zonal_uterovaginal_summary.csv")
write_csv(regional_utero,  "prolapse/summaries/regional_uterovaginal_summary.csv")
write_csv(annual_national, "prolapse/summaries/annual_national_all_prolapse.csv")
write_csv(completeness,    "prolapse/summaries/reporting_completeness.csv")

## SET PLOT THEME 
theme_prolapse <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, color = "grey40"),
    text              = element_text(face = "bold"),
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "cm")
  )

# Palettes — pinks/purples consistent with fistula analysis
pal_type <- c(
  "GC40.3 Uterovaginal"     = "#7B2D8B",
  "GC40.2 Vaginal apex"     = "#C06FAB",
  "GC40 Pelvic organ"       = "#E8A0C8",
  "N80-1137 Female genital" = "#4A90D9",
  "DB31.2 Rectal (ICD-11)"  = "#2C7873",
  "K55-874 Rectal (ICD-10)" = "#52B69A",
  "DB53 Anal (ICD-11)"      = "#B5838D",
  "K55-873 Anal (ICD-10)"   = "#D4A5A5"
)

## PLOT 1: National time series (all 8 variables, 2010–2018)
# Group into coding eras for context
p_time_all <- annual_national |>
  filter(annual_cases > 0) |>
  mutate(short_name = factor(short_name, levels = names(pal_type))) |>
  ggplot(aes(year, annual_cases, colour = short_name)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 2) +
  annotate("rect", xmin = 2014.5, xmax = 2015.5, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = 2015, y = Inf, vjust = 1.5, size = 3.5,
           label = "ICD-11\nrollout", colour = "grey40", fontface = "bold") +
  scale_colour_manual(values = pal_type, name = NULL) +
  scale_x_continuous(breaks = 2010:2018) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "National annual prolapse cases: all variables, 2010–2018",
    subtitle = "Coding transition (ICD-10 → ICD-11) visible as cross-over in 2014–2015",
    x = NULL, y = "Annual reported cases"
  ) +
  theme_prolapse +
  guides(colour = guide_legend(ncol = 4))

## PLOT 2: Focus — uterovaginal prolapse trends, monthly 2015–2018
p_time_utero <- monthly_national |>
  filter(data_element_id %in% UTEROVAG_IDS, year %in% STUDY_YEARS) |>
  mutate(monthly_cases = if_else(monthly_cases == 0, NA_real_, monthly_cases)) |>
  ggplot(aes(date, monthly_cases)) +
  geom_line(colour = "#7B2D8B", linewidth = 0.9, alpha = 0.85, na.rm = TRUE) +
  geom_smooth(method = "loess", span = 0.4, se = TRUE,
              colour = "#4A90D9", fill = "#4A90D9", alpha = 0.2, linewidth = 0.7) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "National monthly cases: GC40.3 Uterovaginal prolapse, 2015–2018",
    subtitle = "Purple = monthly total; blue = LOESS trend with 95% CI",
    x = NULL, y = "Monthly reported cases"
  ) +
  theme_prolapse +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## PLOT 3: Reporting completeness
# Heatmap 
p_completeness_heat <- completeness |>
  filter(year >= 2013, year <= 2018) |>
  mutate(short_name = factor(short_name, levels = names(pal_type))) |>
  ggplot(aes(year, short_name, fill = pct_nonzero)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = paste0(pct_nonzero, "%")), size = 3.5, colour = "white", fontface = "bold") +
  scale_fill_gradient(low = "#E8A0C8", high = "#7B2D8B",
                      name = "% zone-months\nnon-zero") +
  scale_x_continuous(breaks = 2013:2018) +
  labs(
    title    = "Reporting completeness: % of possible zone-months with non-zero entries",
    subtitle = "Denominator = n distinct zones in year × 12 months",
    x = NULL, y = NULL
  ) +
  theme_prolapse +
  theme(panel.grid = element_blank())


plot_data <- completeness |>
  mutate(
    short_name = factor(short_name, levels = var_order),
    # label for annotation: only show % if > 2% to avoid clutter
    pct_label  = if_else(pct_nonzero >= 2, paste0(pct_nonzero, "%"), "")
  )

p_completeness_line <- ggplot(plot_data, aes(x = year, y = pct_nonzero,
                                        colour = short_name, group = short_name)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(aes(size = zone_months_nonzero), alpha = 0.8) +
  geom_text_repel(
    aes(label = pct_label),
    size          = 3.2,
    fontface      = "bold",
    show.legend   = FALSE,
    box.padding   = 0.4,
    point.padding = 0.3,
    segment.color = "grey70",
    segment.size  = 0.3,
    max.overlaps  = 20,
    min.segment.length = 0.2
  ) +
  scale_colour_manual(values = pal_completeness, name = NULL) +
  scale_size_continuous(
    name   = "Zone-months\n(non-zero)",
    range  = c(1.5, 7),
    breaks = c(10, 50, 100, 200)
  ) +
  scale_x_continuous(breaks = 2010:2018) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Data completeness by prolapse variable, 2010–2018",
    subtitle = "% of possible zone-months with non-zero entries; point size = absolute zone-month count",
    x = NULL, y = "% zone-months non-zero"
  ) +
  theme_fistula +
  theme(base_size = 15) +
  guides(
    colour = guide_legend(ncol = 2, override.aes = list(size = 3)),
    size   = guide_legend(ncol = 2)
  )

## PLOT 4: Regional bar chart (GC40.3, 2015–2018) 
p_region_bar <- regional_utero |>
  filter(!is.na(region_name)) |>
  mutate(region_name = fct_reorder(region_name, total_cases)) |>
  ggplot(aes(total_cases, region_name)) +
  geom_col(fill = "#7B2D8B", alpha = 0.85) +
  geom_col(aes(x = annual_avg), fill = "#4A90D9", width = 0.5, alpha = 0.9) +
  geom_text(aes(label = comma(total_cases)), hjust = -0.1, size = 3.5,
            colour = "grey30", fontface = "bold") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Regional burden: GC40.3 Uterovaginal prolapse, 2015–2018",
    subtitle = "Dark bar = cumulative 4-year total; blue = annual average",
    x = "Reported cases", y = NULL
  ) +
  theme_prolapse

## PLOT 5: Zonal distribution — top 30 zones 
p_zonal_top <- zonal_utero |>
  filter(has_cases) |>
  filter(str_detect(org_unit_name, regex("zone", ignore_case = TRUE))) |>
  arrange(desc(total_cases)) |>
  slice_head(n = 10) |>
  mutate(
    zone_label = paste0(str_trunc(org_unit_name, 25), " (", region_name, ")"),
    zone_label = fct_reorder(zone_label, total_cases)
  ) |>
  ggplot(aes(total_cases, zone_label)) +
  geom_col(fill = "#7B2D8B", alpha = 0.85) +
  geom_text(aes(label = comma(total_cases)), hjust = -0.1, size = 3.5, fontface = "bold",
            colour = "grey30") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 10 zones by uterovaginal prolapse burden (GC40.3), 2015–2018",
    subtitle = "Cumulative 4-year reported cases; label = region",
    x = "Cumulative cases (2015–2018)", y = NULL
  ) +
  theme_prolapse +
  theme(axis.text.y = element_text(size = 9))

## PLOT 6: All ICD-11 variables by region (stacked/faceted) 
p_type_region <- regional_all |>
  filter(!is.na(region_name)) |>
  mutate(
    short_name  = factor(short_name, levels = names(pal_type)),
    region_name = fct_reorder(region_name, total_cases,
                              .fun = sum, .desc = TRUE)
  ) |>
  ggplot(aes(region_name, total_cases, fill = short_name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = pal_type, name = NULL) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Prolapse type composition by region, 2015–2018",
    subtitle = "ICD-11 variables only; stacked by prolapse type",
    x = NULL, y = "Cumulative reported cases"
  ) +
  theme_prolapse +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.key.width = unit(0.8, "cm")
  ) +
  guides(fill = guide_legend(ncol = 3))

## ── ASSEMBLE PANELS ──────────────────────────────────────────────────────────
panel_A <- (p_time_all / p_time_utero) +
  plot_annotation(
    title = "Figure 1 — National temporal trends in prolapse reporting",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

panel_B <- (p_completeness_heat / p_completeness_line) +
  plot_annotation(
    title = "Figure 2 — Reporting completeness by variable and year",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

panel_C <- (p_region_bar | p_type_region) +
  plot_annotation(
    title = "Figure 3 — Regional burden and type composition, 2015–2018",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

panel_D <- p_zonal_top +
  plot_annotation(
    title = "Figure 4 — Sub-regional (zonal) uterovaginal prolapse burden",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

## ── MAP (admin2 = zone level) ─────────────────────────────────────────────────
eth_zones   <- st_read("spatial/eth_admin2.shp", quiet = TRUE)
eth_regions <- st_read("spatial/eth_admin1.shp", quiet = TRUE)
eth_outline <- ne_countries(country = "Ethiopia", scale = "medium", returnclass = "sf")

# DHIS2 zone names differ from shapefile adm2_name in several ways:
#  (a) DHIS2 appends " Zone" / " zone" — strip it
#  (b) Spelling variants (Guraghe/Gurage, Siltie/Silte, Wello/Wollo, etc.)
#  (c) City/town units (Bahir Dar Town, sub-cities) don't correspond to zone polygons
# Strategy: strip " Zone" suffix, normalise case, then apply a manual crosswalk
# for remaining mismatches.  City-level units that have no zone polygon are left
# unmatched (they will render as grey on the map).

zone_crosswalk <- tribble(
  ~dhis2_clean,                          ~shp_adm2,
  # Spelling variants
  "Gurage",                              "Guraghe",
  "East Gurage",                         "East Guraghe",
  "Silte",                               "Siltie",
  "East Gojjam",                         "East Gojam",
  "North Gojjam",                        "North Gojam",
  "West Gojjam",                         "West Gojam",
  "North Wollo",                         "North Wello",
  "South Wollo",                         "South Wello",
  "Waghimera",                           "Wag Hamra",
  "Dawa",                                "Daawa",
  "Kaffa",                               "Kefa",
  "Horo Guduru Wollega",                 "Horo Gudru Wellega",
  "West Wollega",                        "West Wellega",
  "Aari",                                "Ari",
  "Wolaita",                             "Wolayita",
  "Dollo",                               "Doolo",
  "Sitti",                               "Siti",
  "Dawro",                               "Dawuro",
  "Liben",                               "Liban",
  "Shagar City",                         "Shager City",
  # City / town admin units
  "Bahir Dar Town",                      "Bahir Dar town Admin",
  "Hawassa City Administration",         "Hawassa town Admin",
  # Unit names that include non-zone suffixes
  "West Shewa Zone Health Office",       "West Shewa",
  "Tembaro Special Woreda",              "Tembaro Special",
  "Kebena Special Woreda",               "Kebena Special",
  # Ambiguous Amhara North Shewa (map to Amhara polygon)
  "North Shewa",                         "North Shewa (AM)"
)

join_data <- zonal_utero |>
  mutate(
    dhis2_clean = str_trim(org_unit_name) |>
      str_remove_all("(?i) zone$")
  ) |>
  left_join(zone_crosswalk, by = "dhis2_clean") |>
  mutate(shp_key = coalesce(shp_adm2, dhis2_clean))

map_data <- eth_zones |>
  left_join(join_data, by = c("adm2_name" = "shp_key"))

# How many zones matched?
n_matched <- sum(!is.na(map_data$total_cases))
cat(sprintf("\nMap join: %d / %d zone polygons matched to DHIS2 data\n",
            n_matched, nrow(eth_zones)))

map_base <- function() {
  list(
    geom_sf(data = eth_regions, fill = NA, colour = "grey60", linewidth = 0.4),
    geom_sf(colour = "white", linewidth = 0.2),
    theme_void(base_size = 13),
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "grey40"),
      legend.position = "right",
      legend.title  = element_text(face = "bold", size = 10),
      legend.text   = element_text(size = 9)
    )
  )
}

# Panel E-1: total uterovaginal cases by zone
p_map_total <- ggplot(map_data) +
  map_base() +
  aes(fill = total_cases) +
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "grey88",
                       name = "Cumulative\ncases", labels = comma,
                       trans = "sqrt") +
  labs(
    title    = "Uterovaginal prolapse burden by zone, 2015–2018",
    subtitle = "GC40.3; grey = no data; colour scale square-root transformed"
  )

# Panel E-2: binary reporting (any vs none)
p_map_coverage <- ggplot(map_data) +
  map_base() +
  aes(fill = factor(case_when(
    is.na(has_cases)    ~ "No match",
    has_cases == TRUE   ~ "Cases reported",
    TRUE                ~ "Zero cases"
  ), levels = c("Cases reported", "Zero cases", "No match"))) +
  scale_fill_manual(
    values = c("Cases reported" = "#7B2D8B",
               "Zero cases"     = "#E8A0C8",
               "No match"       = "grey88"),
    name = NULL, na.value = "grey88"
  ) +
  labs(
    title    = "Zones reporting uterovaginal prolapse (GC40.3), 2015–2018",
    subtitle = "Purple = ≥1 case; pink = reporting but zero cases; grey = no match"
  )

panel_E <- (p_map_total | p_map_coverage) +
  plot_annotation(
    title = "Figure 5 — Zonal maps: uterovaginal prolapse burden and coverage",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Region-level map data
eth_regions_map <- eth_regions |>
  mutate(region_name = recode(adm1_name,
                              "Gambela" = "Gambella"   # align shapefile name to DHIS2
  )) |>
  left_join(regional_utero, by = "region_name")

n_matched_region <- sum(!is.na(eth_regions_map$total_cases))
cat(sprintf("\nRegion map join: %d / %d region polygons matched\n",
            n_matched_region, nrow(eth_regions)))

# Panel E-3: total cases by region
p_map_region <- ggplot(eth_regions_map) +
  map_base() +
  aes(fill = total_cases) +
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "grey88",
                       name = "Cumulative\ncases", labels = comma,
                       trans = "sqrt") +
  labs(
    title    = "Uterovaginal prolapse burden by region, 2015–2018",
    subtitle = "GC40.3; grey = no data; colour scale square-root transformed"
  )

# Panel E-4: annual average by region
p_map_region_avg <- ggplot(eth_regions_map) +
  map_base() +
  aes(fill = annual_avg) +
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "grey88",
                       name = "Annual\naverage", labels = comma) +
  labs(
    title    = "Mean annual uterovaginal prolapse cases by region, 2015–2018",
    subtitle = "GC40.3; grey = no data"
  )

## ── NATIONAL SUMMARY STATISTICS ──────────────────────────────────────────────
cat("\n=== NATIONAL SUMMARY: Uterovaginal prolapse (GC40.3), 2015–2018 ===\n")

national_utero <- focal |>
  filter(data_element_id == "ykGyWdc6qG2") |>
  summarise(
    total_cases_4yr    = sum(value, na.rm = TRUE),
    annual_avg         = total_cases_4yr / length(STUDY_YEARS),
    n_zones_ever_rpt   = n_distinct(org_unit_name[value > 0]),
    n_zones_total      = n_distinct(org_unit_name),
    pct_zones_rpt      = n_zones_ever_rpt / n_zones_total * 100,
    peak_year          = focal |>
      filter(data_element_id == "ykGyWdc6qG2") |>
      group_by(year) |> summarise(s = sum(value, na.rm = TRUE)) |>
      slice_max(s, n = 1) |> pull(year)
  )

cat(sprintf(
  "  Total cases (2015–2018):     %s\n  Annual average:              %s\n  Zones ever reporting:        %d / %d (%.0f%%)\n  Peak year:                   %d\n",
  comma(national_utero$total_cases_4yr),
  comma(round(national_utero$annual_avg)),
  national_utero$n_zones_ever_rpt,
  national_utero$n_zones_total,
  national_utero$pct_zones_rpt,
  national_utero$peak_year
))

