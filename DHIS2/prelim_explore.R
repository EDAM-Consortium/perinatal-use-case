#### DHIS2 ETHIOPIA STILLBIRTH RISK FACTOR ANALYSIS
#### Unit of analysis: zone (level 3) x month
#### Outcome: stillbirth count per zone-month
#### This script: data preparation, cleaning, and descriptive comparison

## Load packages 
library(tidyverse)
library(purrr)
library(ggplot2)
library(dplyr)
library(sf)

##Variable lookup 
var_labels <- c(
  # Outcomes
  vqNLnporiZ1 = "still_births",
  cFlulEeuKRw = "iufd",
  lz1lT9MFQd4 = "single_stillbirth",
  weJvzEsHWWX = "live_births",
  # Maternal age / parity
  lotDArt3TsU = "anc1_by_age",
  dCyyyAiuofb = "anc4_by_age",
  oNLXqTifGKY = "teen_preg_positive",
  mZCqcHS5I0U = "teen_preg_rate",
  JTm0SnNGjms = "elderly_primigravida",
  T5gna2TEX32 = "grand_multiparity",
  AxkfDHsp3Fj = "poor_obstetric_history",
  # ANC coverage & timing
  NTidDdqjEde = "anc8_contacts",
  hJGEI95aFvT = "anc1_by_gest_week",
  cmav5iXqALq = "anc1_gest_week_count",
  FMHlxx3lBcZ = "anc4_by_gest_week",
  xE0gwDlZgl5 = "anc1_before_12wk_pct",
  zcCEgTBfMIP = "anc4_coverage_pct",
  hfqUwjltEcp = "anc8_coverage_pct",
  mzX0cipJBfa = "high_risk_preg_supervision",
  # Nutrition & micronutrients
  y7drS5G9QGT = "iron_folate_90d",
  XfbcACv3sB8 = "dewormed",
  nYcQoY7Kvda = "nutrition_counselled",
  BX8jhiWcgHY = "malnutrition_icd11",
  txgF0BhqQsA = "malnutrition_icd10",
  Gviy3f6fuSR = "anaemia_complicating_preg",
  CBJVSyU00mz = "low_weight_gain_icd11",
  eLRv5LpyuKm = "low_weight_gain_icd10",
  # Hypertensive disorders
  onIgJN7FFUn = "pre_eclampsia_icd11",
  NX3tXatAQBk = "pre_eclampsia_unspec",
  T269kPxN0G9 = "severe_pre_eclampsia",
  il18oInXFKx = "eclampsia_preg",
  wRC6NGAGjTp = "pre_eclampsia_on_chronic_htn",
  YBlOR5P2wXY = "pre_existing_htn",
  WERL7LjT4x5 = "unspec_maternal_htn",
  # Diabetes
  auXHGN5ToAC = "diabetes_preg_icd11",
  MMO5hO1SGEc = "gestational_diabetes",
  q1ruBjp1W86 = "diabetes_preg_icd10",
  # Infections
  kbVb7uFcWuH = "syphilis_tested",
  Q831ZIXPYSX = "syphilis_treated",
  XfkilS4FUL1 = "syphilis_complicating_preg",
  rlHrL4d6AN3 = "uti_preg",
  qgY0HMjUdLG = "pyelonephritis_preg",
  JBq6A6H5yiu = "genito_urinary_infection",
  Uy3sUGYDocU = "hiv_complicating_preg",
  # Haemorrhage & placenta
  hJaDtuUyuAq = "antepartum_haem_icd11",
  zF8m83W0plG = "antepartum_haem_icd10",
  IEjeuIGU58m = "placenta_praevia_haem",
  hM79L2Fffwb = "placenta_praevia_no_haem",
  Wa1XuXurAEh = "intrapartum_haem",
  xOMNqgdHMJq = "pph",
  # PROM
  qx7r284km0B = "prom",
  # Prolonged / obstructed labour
  KZeJvQis9Ng = "prolonged_1st_stage_icd11",
  pc1rrYIDEZX = "prolonged_2nd_stage_icd11",
  zgTTh8iGsQP = "prolonged_1st_stage_icd10",
  et6xmt51VFr = "prolonged_2nd_stage_icd10",
  eSvo4q5gSO5 = "obstructed_pelvic",
  uAyi1noyZWZ = "obstructed_malpresentation",
  FsCxrT3WGTz = "obstructed_labour_unspec",
  G8BtoAHKUI6 = "uterine_rupture_during_icd11",
  o0VKXcMiHAd = "uterine_rupture_during_icd10",
  dQ9ZONjeTuz = "uterine_rupture_before",
  # Fetal distress & asphyxia
  PG2RQZtyZv9 = "fetal_distress_icd11",
  VT49tM7NMm0 = "fetal_distress_icd10",
  Z37i0wwAvjH = "meconium_liquor",
  KSrBwgS4DQC = "intrauterine_hypoxia",
  # Cord
  aKcR4pbtypU = "cord_prolapse",
  LGNOxm3PPDs = "cord_complications_icd11",
  # Preterm & LBW
  XpqxTfhsVWL = "preterm_labour_icd11",
  FDTfcU6kqcY = "preterm_labour_icd10",
  vWJ2dJT7AtI = "preterm_newborn",
  HnLMGD0DnU0 = "low_birth_weight",
  j6GEdB3OXWZ = "lbw_or_premature_kmc",
  m0j9dD48jVP = "post_term_preg",
  # Fetal growth restriction
  Qn9jp4jiZPt = "poor_fetal_growth",
  RWulXW939kX = "fetal_hypoxia_signs",
  SH4gkjJ9Xnc = "slow_fetal_growth_newborn",
  # Multiple gestation
  G2h7hAurvnY = "twin_preg_icd11",
  G1Jh6qEesmr = "twin_preg_icd10",
  Ccg95wwkP2I = "polyhydramnios",
  pMaIDtVcadf = "amniotic_fluid_disorders",
  # Malpresentation
  yZweNVKF450 = "breech",
  lH6LbkboZsO = "transverse_oblique_lie",
  yRYcATaIBBZ = "cervical_incompetence",
  # Rhesus
  uYTTNfHAZ18 = "rhesus_isoimmunisation",
  # Delivery care
  lzhOxCHJMOV = "skilled_birth_attendance",
  VAwGaDJTcbm = "caesarean_section",
  # Fistula
  sRyFDCE2b3P = "obstetric_fistula_cases",
  Rc6kyqCLHUv = "obstetric_fistula_treated",
  # Substance use
  Qf7iQv49C4C = "substance_alcohol",
  vdFj1JNnuJA = "substance_khat",
  oZuiC0yCz8F = "substance_tobacco",
  lv3PWuVsAC5 = "substance_treated_pct",
  # Maternal deaths
  pj5uAe3P8EM = "maternal_deaths_facility",
  VbLl2M9cuzV = "maternal_deaths_community"
)

# Columns that are outcomes or identifiers — exclude from predictors
non_predictors <- c(
  "still_births", "single_stillbirth", "iufd", "live_births",
  "maternal_deaths_facility", "maternal_deaths_community"
)

## Load data 
level2_raw <- read_csv("data/maternal_level2_Regional.csv", show_col_types = FALSE)
level3_raw <- read_csv("data/maternal_level3_Zonal.csv",    show_col_types = FALSE)

## Reshape from long to wide 
reshape_dhis2 <- function(df) {
  df %>%
    mutate(value = suppressWarnings(as.numeric(value))) %>%
    group_by(data_element_id, period, org_unit_level, org_unit_id,
             org_unit_name, org_unit_hierarchy) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols     = c(period, org_unit_level, org_unit_id,
                      org_unit_name, org_unit_hierarchy),
      names_from  = data_element_id,
      values_from = value
    ) %>%
    rename_with(~ var_labels[.x], .cols = any_of(names(var_labels))) %>%
    mutate(
      year  = as.integer(substr(period, 1, 4)),
      month = as.integer(substr(period, 5, 6)),
      date  = as.Date(paste(year, month, "01", sep = "-"))
    )
}

wide2 <- reshape_dhis2(level2_raw)
wide3 <- reshape_dhis2(level3_raw)

## Verify and subset level 3 explicitly
cat("Levels in level2 file:", unique(wide2$org_unit_level), "\n")
cat("Levels in level3 file:", unique(wide3$org_unit_level), "\n")

wide3 <- wide3 %>% filter(org_unit_level == 3)
cat("Rows after level 3 filter:", nrow(wide3), "\n")

## Attach region names to zone data 
wide3 <- wide3 %>%
  mutate(region_id = map_chr(strsplit(org_unit_hierarchy, "/"), ~ tail(.x, 1)))

region_lookup <- wide2 %>%
  distinct(org_unit_id, org_unit_name) %>%
  rename(region_id = org_unit_id, region_name = org_unit_name)

wide3 <- wide3 %>%
  left_join(region_lookup, by = "region_id")

cat("Zones with matched region:", sum(!is.na(wide3$region_name)),
    "of", nrow(wide3), "\n")

## Filter to modelling rows 
# Keep all zone x month rows where stillbirth count was recorded
# Include zeros — genuine low/no stillbirth observations needed for comparison
model_df <- wide3 %>%
  filter(!is.na(still_births))

cat("\nRows for modelling:", nrow(model_df), "\n")
cat("Unique zones:", n_distinct(model_df$org_unit_id), "\n")
cat("Date range:", format(min(model_df$date)), "to", format(max(model_df$date)), "\n")
cat("\nStillbirth count distribution:\n")
summary(model_df$still_births)
cat("Zone-months with zero stillbirths:", sum(model_df$still_births == 0), "\n")
cat("Zone-months with at least one stillbirth:", sum(model_df$still_births > 0), "\n")

## Build predictor list dynamically 
# All variables that came through the reshape, excluding outcomes
predictor_cols <- setdiff(
  names(model_df)[names(model_df) %in% var_labels],
  non_predictors
)

cat("\nTotal predictors available:", length(predictor_cols), "\n")

## Missingness summary 
cat("\n--- Predictor missingness (%) ---\n")
missingness <- model_df %>%
  select(all_of(predictor_cols)) %>%
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(pct_missing)

print(missingness, n = Inf)

## High vs low stillbirth comparison 
sb_comparison <- model_df %>%
  mutate(sb_group = if_else(still_births > median(still_births), "high", "low")) %>%
  select(sb_group, all_of(predictor_cols)) %>%
  group_by(sb_group) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-sb_group, names_to = "variable", values_to = "mean") %>%
  pivot_wider(names_from = sb_group, values_from = mean) %>%
  mutate(
    difference = high - low,
    pct_higher = round((high - low) / low * 100, 1)
  ) %>%
  left_join(missingness, by = "variable") %>%
  arrange(desc(abs(difference)))

cat("\n--- Predictor means: high vs low stillbirth zone-months ---\n")
print(sb_comparison, n = Inf)

write_csv(sb_comparison, "sb_comparison_predictors.csv")

## Build and save clean dataset
# Identifiers + raw outcome + log outcome + all predictors
# NAs replaced with 0 — absent count = not recorded in DHIS2
# log1p transform handles both zeros and right skew
clean_data <- model_df %>%
  select(org_unit_id, org_unit_name, region_name, date, year, month,
         still_births, all_of(predictor_cols)) %>%
  mutate(across(all_of(predictor_cols), ~ replace_na(., 0))) %>%
  mutate(still_births_log = log1p(still_births))

cat("\nClean dataset dimensions:", nrow(clean_data), "rows x",
    ncol(clean_data), "columns\n")

write_csv(clean_data, "dhis2_stillbirth_clean.csv")

## Plot stillbirth data in time and space
# From the original plot I realised that the data is very sparse at the tails 
# So I am going to subset 
data <- read_csv("dhis2_stillbirth_clean.csv")
data <- data %>%
  filter(date >= as.Date("2010-05-01"),
         date <= as.Date("2018-09-01"))

# In time 
series_dat <- data %>% group_by(date) %>% 
  mutate(nat_sb = sum(still_births, na.rm = T)) %>% 
  ungroup() %>% select(date, nat_sb) %>% distinct()

ggplot(series_dat, aes(x = date, y = nat_sb)) + 
  geom_line(color = "#4A90D9", size = 1.5) + theme_minimal() + 
  labs(x = "Date", y = "Total No. of Stillbirths") + 
  theme(text = element_text(face = "bold", size = 13))

 # In space
# spatial_admin1.gpkg is pre-built by spatial/make_spatial_counts.R: it joins
# the zone x month panel to the OCHA admin1 polygons via region_name (the only
# DHIS2 location field that maps cleanly and completely — see the notes in
# spatial/build_lookup_tables.R for why zones/woredas aren't attempted) and
# aggregates n_zone_months/n_stillbirths/mean_sb per region.
adm1_sf <- read_sf("spatial/spatial_admin1.gpkg")

ggplot(adm1_sf) +
  geom_sf(aes(fill = n_stillbirths)) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90") +
  theme_minimal() +
  labs(fill = "Stillbirths") +
  theme(text = element_text(face = "bold", size = 13))










