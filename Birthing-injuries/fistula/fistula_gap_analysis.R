
#### FISTULA GEOGRAPHIC & SERVICE DELIVERY GAP ANALYSIS
### Research Question 1: Where are the gaps between fistula burden and access
###   to prevention and repair services?
###
### Indicators:
###   [1] MAT_Obstetric Fistula Cases by Age        → burden
###   [2] MAT_Obstetric Fistula Cases Treated by Age → treatment uptake
###   [3] RMNCH - Obstetric fistula care provided    → service availability

## Load packages 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(scales)
library(ggrepel)

## Load data 
raw <- read_csv("data/fistula_level4_Wereda.csv", show_col_types = FALSE)

## Spatial and temporal clean 
# Parse period (YYYYMM → Date)
raw <- raw |>
  mutate(
    period_chr = as.character(period),
    year       = as.integer(str_sub(period_chr, 1, 4)),
    month      = as.integer(str_sub(period_chr, 5, 6)),
    date       = as.Date(paste(year, month, "01", sep = "-"))
  )

# Extract region ID from hierarchy (level 2 of national/region/woreda)
raw <- raw |>
  mutate(region_id = str_split_fixed(org_unit_hierarchy, "/", 3)[, 2])

# Region name lookup (identified from facility/woreda names in each region)
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

raw <- raw |> left_join(region_names, by = "region_id")

# Focal indicators only
INDICATOR_CASES    <- "MAT_Obstetric Fistula Cases by Age"
INDICATOR_TREATED  <- "MAT_Obstetric Fistula Cases Treated by Age"
INDICATOR_CARE     <- "RMNCH - Obstetric fistula care provided"

focal <- raw |>
  filter(data_element_name %in% c(INDICATOR_CASES, INDICATOR_TREATED, INDICATOR_CARE)) |>
  mutate(
    indicator_short = case_when(
      data_element_name == INDICATOR_CASES   ~ "cases",
      data_element_name == INDICATOR_TREATED ~ "treated",
      data_element_name == INDICATOR_CARE    ~ "care_provided"
    )
  )

# Age bands relevant to obstetric fistula (exclude cord-prolapse indicators)
fistula_age_bands <- c("10 - 14 Years", "15 - 19 Years",
                       "20 - 24 Years", "25 - 29 Years", ">= 30 Years")

# Study window: all three indicators have consistent reporting 2015–2018.
# 2014 is a ramp-up year (cases reported but care_provided near-zero); 
# 2019+ data are essentially absent. Apply this filter to all analyses.
STUDY_YEARS <- 2015:2018

focal <- focal |> filter(year %in% STUDY_YEARS)

## WOREDA-LEVEL SUMMARY (2015–2018)
# Aggregate by woreda × indicator, summing across study period and age bands
woreda_summary <- focal |>
  filter(
    indicator_short %in% c("cases", "treated") &
      category_option_combo_name %in% fistula_age_bands |
    indicator_short == "care_provided"
  ) |>
  group_by(org_unit_id, org_unit_name, region_id, region_name, indicator_short) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

# Wide format
woreda_wide <- woreda_summary |>
  pivot_wider(names_from = indicator_short, values_from = total, values_fill = 0)

# Derived metrics
# NOTE: care_provided values are small integers (median = 1, max = 31) representing
# facility-level service availability contacts, not surgical repair counts. The Nov 2016
# jump from ~150 to ~1,900 reporting facilities reflects a DHIS2 rollout, not a real
# surge in services. care_provided is therefore treated as a binary coverage indicator
# (does this woreda have any facility offering fistula care?) rather than a volume measure.
woreda_wide <- woreda_wide |>
  mutate(
    treatment_rate      = if_else(cases > 0, treated / cases, NA_real_),
    unrepaired          = pmax(cases - treated, 0),
    has_any_care        = care_provided > 0,
    has_any_cases       = cases > 0,
    high_burden_no_care = cases > 0 & (is.na(care_provided) | care_provided == 0)
  )

write_csv(woreda_wide, "woreda_summary.csv")

## REGION-LEVEL SUMMARY 
region_summary <- woreda_wide |>
  group_by(region_id, region_name) |>
  summarise(
    n_woredas          = n(),
    total_cases        = sum(cases),
    total_treated      = sum(treated),
    region_treat_rate  = total_treated / pmax(total_cases, 1),
    n_woredas_any_care = sum(has_any_care),
    pct_covered        = n_woredas_any_care / n_woredas,
    n_high_burden_gap  = sum(high_burden_no_care),
    pct_gap            = n_high_burden_gap / n_woredas,
    .groups = "drop"
  )
write_csv(region_summary, "region_summary.csv")

## TEMPORAL TRENDS (2015–2018)
# focal is already filtered to STUDY_YEARS; just aggregate
monthly_national <- focal |>
  filter(
    indicator_short %in% c("cases", "treated") &
      category_option_combo_name %in% fistula_age_bands |
    indicator_short == "care_provided"
  ) |>
  group_by(date, year, indicator_short) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(date))

# Age-stratified burden
age_trend <- focal |>
  filter(
    indicator_short == "cases",
    category_option_combo_name %in% fistula_age_bands
  ) |>
  group_by(year, category_option_combo_name) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

## CORRELATION ANALYSIS 
# Among woredas with any reported cases: does treated correlate with care_provided?
cor_data <- woreda_wide |>
  filter(cases > 0, !is.na(care_provided))

cor_cases_treated  <- cor(cor_data$cases,        cor_data$treated,       use = "complete.obs")
cor_treated_care   <- cor(cor_data$treated,       cor_data$care_provided, use = "complete.obs")
cor_cases_care     <- cor(cor_data$cases,         cor_data$care_provided, use = "complete.obs")

cat(sprintf(
  "Correlations (facilities with cases > 0, n = %d):\n  cases ~ treated:        r = %.3f\n  treated ~ care_provided: r = %.3f\n  cases ~ care_provided:   r = %.3f\n",
  nrow(cor_data), cor_cases_treated, cor_treated_care, cor_cases_care
))

## PLOTS 
# Set aesthetics
theme_fistula <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, color = "grey40"),
    text              = element_text(face = "bold"),
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "cm")
  )

# Main palette: blues, purples, pinks
pal3       <- c(cases = "#7B2D8B", treated = "#4A90D9", care_provided = "#C06FAB")
age_colours <- c("#2C3E7A", "#4A90D9", "#7B2D8B", "#C06FAB", "#E8A0C8")
pal_care    <- c("Care available" = "#4A90D9", "No care reported" = "#C06FAB")
pal_quadrant <- c(
  "High burden\nLow treatment\n(priority gap)" = "#7B2D8B",
  "High burden\nHigh treatment"                 = "#4A90D9",
  "Low burden\nLow treatment"                   = "#C06FAB",
  "Low burden\nHigh treatment"                  = "#E8A0C8"
)
pal_bar <- c("Cases" = "#7B2D8B", "Treated" = "#4A90D9")
pal_dot <- c("% Facilities with care" = "#C06FAB")

# National time series: cases vs treated only
p_time <- monthly_national |>
  filter(indicator_short %in% c("cases", "treated")) |>
  mutate(
    value = if_else(value == 0, NA_real_, value),
    indicator_label = factor(indicator_short,
                             levels = c("cases", "treated"),
                             labels = c("Fistula cases (all ages)", "Cases treated (all ages)"))
  ) |>
  ggplot(aes(date, value, colour = indicator_label)) +
  geom_line(linewidth = 0.8, alpha = 0.85, na.rm = TRUE) +
  scale_colour_manual(values = c("Fistula cases (all ages)" = "#7B2D8B",
                                 "Cases treated (all ages)" = "#4A90D9"),
                      name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "National trends: fistula burden vs treatment",
    subtitle = "Monthly national totals, 2015–2018",
    x = NULL, y = "Monthly count"
  ) +
  theme_fistula +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Age-stratified annual cases
age_order <- c("10 - 14 Years", "15 - 19 Years", "20 - 24 Years",
               "25 - 29 Years", ">= 30 Years")

p_age <- age_trend |>
  mutate(age_band = factor(category_option_combo_name, levels = age_order)) |>
  ggplot(aes(year, value, fill = age_band)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = age_colours, name = "Age band") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Annual fistula cases by age band, 2015–2018",
    subtitle = "Stacked counts; burden concentrated in younger women",
    x = NULL, y = "Annual cases"
  ) +
  theme_fistula

# Woreda-level scatter: cases vs treated, coloured by binary care coverage
# care_provided treated as binary: does this woreda have any facility reporting care?
p_scatter <- cor_data |>
  ggplot(aes(cases, treated)) +
  geom_point(alpha = 0.6, size = 2.5, colour = "#4A90D9") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey30", linewidth = 0.6) +
  scale_x_log10(labels = comma, oob = squish) +
  scale_y_log10(labels = comma, oob = squish) +
  coord_equal() +
  annotate("text", x = Inf, y = 1, hjust = 1.05, vjust = -0.3, fontface = "bold",
           label = sprintf("r(cases,treated) = %.2f", cor_cases_treated),
           size = 4, colour = "grey30") +
  labs(
    title    = "Cases vs treated by facility (log scale)",
    subtitle = "Points above the diagonal: more cases than treated",
    x = "Total reported cases (log)", y = "Total treated (log)"
  ) +
  theme_fistula

# Treatment rate distribution by facility
p_treat_rate <- woreda_wide |>
  filter(cases >= 5, !is.na(treatment_rate), treatment_rate <= 1) |>
  ggplot(aes(treatment_rate)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.05,
                 fill = "#4A90D9", colour = "white", alpha = 0.8) +
  geom_density(colour = "#7B2D8B", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  coord_cartesian(ylim = c(0, 2)) + 
  labs(
    title    = "Distribution of facility-level treatment rates",
    subtitle = "Facilities with ≥5 reported cases; treatment rate capped at 100%",
    x = "Treatment rate (treated / cases)", y = "Density"
  ) +
  theme_fistula

# Region-level bar chart: cases, treated (counts) + care coverage (% woredas)
# care_provided shown as % woredas with any facility reporting, not a raw count,
# consistent with treating it as a binary coverage indicator.
scale_factor <- max(region_summary$total_cases)

p_gap_bar <- region_summary |>
  mutate(region_name = fct_reorder(region_name, -total_cases),
         care_dot    = pct_covered * scale_factor) |>
  ggplot(aes(x = region_name)) +
  geom_col(aes(y = total_cases,   fill = "Cases")) +
  geom_col(aes(y = total_treated, fill = "Treated"), width = 0.5) +
  geom_point(aes(y = care_dot, colour = "% Facilities with care"), size = 3) +
  scale_fill_manual(values = pal_bar, name = NULL) +
  scale_colour_manual(values = pal_dot, name = NULL) +
  scale_y_continuous(
    labels = comma,
    sec.axis = sec_axis(~ . / scale_factor, labels = percent,
                        name = "Facility with care coverage (%)")
  ) +
  labs(
    title    = "Region-level burden vs treatment vs care coverage",
    subtitle = "Bars = cumulative cases/treated 2015–2018; dots = % facilities reporting care",
    x = NULL, y = "Cumulative count"
  ) +
  theme_fistula +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Gap classification: 4-quadrant plot (cases × treatment rate)
# Quadrants: high burden / low treatment = highest priority gap
median_cases <- median(woreda_wide$cases[woreda_wide$cases > 0], na.rm = TRUE)
median_tr    <- median(woreda_wide$treatment_rate[woreda_wide$cases >= 5], na.rm = TRUE)

p_quadrant <- woreda_wide |>
  filter(cases >= 5, !is.na(treatment_rate)) |>
  mutate(
    quadrant = case_when(
      cases >= median_cases & treatment_rate <  median_tr ~ "High burden\nLow treatment\n(priority gap)",
      cases >= median_cases & treatment_rate >= median_tr ~ "High burden\nHigh treatment",
      cases <  median_cases & treatment_rate <  median_tr ~ "Low burden\nLow treatment",
      TRUE                                                ~ "Low burden\nHigh treatment"
    )
  ) |>
  ggplot(aes(cases, treatment_rate, colour = quadrant)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_vline(xintercept = median_cases, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = median_tr,    linetype = "dashed", colour = "grey50") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = percent) +
  scale_colour_manual(values = pal_quadrant, name = NULL) +
  labs(
    title    = "Facility-level gap classification",
    subtitle = "Dashed lines = medians; purple = priority service delivery gaps",
    x = "Total fistula cases (log)", y = "Treatment rate"
  ) +
  theme_fistula +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

# High-burden gap woredas: care provided = 0 
top_gap_woredas <- woreda_wide |>
  filter(high_burden_no_care) |>
  arrange(desc(cases)) |>
  mutate(
    woreda_label = paste0(org_unit_name, " (", region_name, ")"),
    woreda_label = fct_reorder(woreda_label, cases)
  )

p_gap_woredas <- top_gap_woredas |>
  ggplot(aes(cases, woreda_label)) +
  geom_col(aes(fill = unrepaired), colour = "white") +
  geom_col(aes(x = treated), fill = "#4A90D9", width = 0.6) +
  scale_fill_gradient(low = "#E8A0C8", high = "#7B2D8B",
                      name = "Unrepaired cases") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "High-burden locations with zero care provision",
    subtitle = "Full bar = total cases; blue overlay = treated; purple fill = unrepaired\nLocation label includes region in parentheses",
    x = "Cumulative cases (2015–2018)", y = NULL
  ) +
  theme_fistula +
  theme(legend.position = "right")

# ASSEMBLE 
# Panel A: temporal overview
panel_A <- (p_time / p_age) +
  plot_annotation(
    title = "Figure 1 — National temporal trends, 2015–2018",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Panel B: gap analysis
panel_B <- (p_scatter | p_treat_rate) /
           (p_quadrant | p_gap_bar) +
  plot_annotation(
    title = "Figure 2 — Correlation and gap analysis (woreda level, 2015–2018)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Panel C: priority woredas
p_gap_woredas

## SUMMARY TABLE 
gap_summary <- woreda_wide |>
  summarise(
    n_woredas_total       = n(),
    n_with_cases          = sum(has_any_cases),
    n_with_care           = sum(has_any_care),
    n_high_burden_gap     = sum(high_burden_no_care),
    pct_high_burden_gap   = mean(high_burden_no_care) * 100,
    total_cases           = sum(cases),
    total_treated         = sum(treated),
    total_unrepaired      = sum(unrepaired),
    national_treat_rate   = sum(treated) / pmax(sum(cases), 1) * 100,
    median_treatment_rate = median(treatment_rate[cases >= 5], na.rm = TRUE) * 100
  )

cat("\n=== NATIONAL GAP SUMMARY ===\n")
print(t(gap_summary))

write_csv(gap_summary, "gap_summary.csv")

## MAP
# ethiopia_dhis2_level1_points.geojson contains DHIS2 region-level centroids.
# We join region_summary directly on the DHIS2 id field, avoiding the need to
# match smaller spatial units. Choropleth panels: (A) treatment rate by region,
# (B) % woredas with care coverage, (C) % high-burden gap woredas.

eth_regions <- st_read("spatial/eth_admin1.shp")

# Join on DHIS2 region id - adjust field name if needed (common options: id, uid, DHIS2_UID)
region_summary <- region_summary |> mutate(region_name = recode(region_name, "Gambella" = "Gambela"))
map_data <- eth_regions |>
  left_join(region_summary, by = c("adm1_name" = "region_name"))

# Ethiopia background for context (from rnaturalearth)
eth_outline <- ne_countries(country = "Ethiopia", scale = "medium", returnclass = "sf")

map_base <- function() {
  list(
    geom_sf(data = eth_outline, fill = "grey92", colour = "grey60", linewidth = 0.3),
    geom_sf(colour = "white", linewidth = 0.3),
    geom_sf_label(aes(label = adm1_name), size = 2.5, label.padding = unit(0.1, "lines"),
                  label.size = 0, fill = alpha("white", 0.7), colour = "grey20",
                  fontface = "bold"),
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

# Panel A: treatment rate
p_map_treat <- ggplot(map_data) +
  map_base() +
  aes(fill = region_treat_rate) +
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "grey85",
                       name = "Treatment\nrate", labels = percent, limits = c(0, 1)) +
  labs(title = "Fistula treatment rate by region",
       subtitle = "Treated / cases, cumulative 2015–2018")

# Panel B: care coverage (% facilities with any care reporting)
p_map_care <- ggplot(map_data) +
  map_base() +
  aes(fill = pct_covered) +
  scale_fill_distiller(palette = "BuPu", direction = 1, na.value = "grey85",
                       name = "Facilities\nwith care", labels = percent) +
  labs(title = "Care facility coverage by region",
       subtitle = "% of facilities with ≥1 reporting fistula care")

# Panel C: % high-burden gap facilities
p_map_gap <- ggplot(map_data) +
  map_base() +
  aes(fill = pct_gap) +
  scale_fill_distiller(palette = "RdPu", direction = 1, na.value = "grey85",
                       name = "Gap\nfacilities", labels = percent) +
  labs(title = "Service delivery gap by region",
       subtitle = "% of facilities with cases but no care reported")

panel_D <- (p_map_treat | p_map_care | p_map_gap) +
  plot_annotation(
    title = "Figure 3 — Regional maps: treatment rate, care coverage, and gap (2015–2018)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

panel_D

