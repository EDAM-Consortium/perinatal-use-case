
#### EXPLORING THE EDHS DATASET 
### DHS IS AN SURVEY BASED REPORTING SYSTEM FOR YEARS 2000, 2005, 2011, 2016 AND 2019 (INTERIM)
### HERE, I HAVE TAKEN ALL THE YEARS AND MERGED THEM INTO ONE FILE
### I HAVE EXTRACTED THE INDIVIDUAL, CHILDREN & BIRTH RECODE 
### I HAVE EXTRACTED BOTH THE GPS DATASETS AND THE SURVEY DATASETS 
### THE COLUMNS HAVE BEEN PRE-SCREENED AND SELECTED BASED ON POTENTIAL SB RISKS

## Load packages
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)      
library(corrplot)
library(sf)
library(tidymodels) 
library(xgboost)     
library(shapviz) 

## Using survey individual + stillbirth recode (not GPS)
## Survey data has ~2x more stillbirths than GPS (~474 vs ~237)
## which improves outcome prevalence for modelling
## Each row corresponds to one woman (individual recode)
## Stillbirth-derived variables (n_stillbirths, had_stillbirth etc.) 
## come from the pre-processed stillbirth file via merge

## Load data
indiv     <- read_csv("Survey/edhs_individual_clean.csv")
stillbirth <- read_csv("Survey/edhs_stillbirth_clean.csv")

## Construct UID on individual recode to match stillbirth file
indiv <- indiv %>%
  mutate(uid = paste0(survey_year, "_", case_id))

## Merge: left join keeps all women, stillbirth vars are NA for those not in stillbirth file
merged <- left_join(indiv, stillbirth, by = "uid")

## Clean and recode
merged_clean <- merged %>%
  # SELECT
  select(
    survey_year.x, lat, lon, adm1_name, urban_rural,
    respondent_current_age,
    education_level.x, literacy,
    wealth_index_quintile,
    total_children_ever_born, births_last_5yrs, age_at_first_birth,
    years_since_first_cohabitation, age_at_first_cohabitation_imputed,
    age_at_first_sex_imputed, months_since_last_birth, age_difference_husband,
    ever_had_terminated_pregnancy, pregnancy_end_completeness,
    num_other_terminated_pregnancies, months_since_last_termination,
    months_pregnant_at_loss, problem_after_delivery_or_stillbirth,
    visited_health_facility_12mo, last_facility_public_or_private,
    num_antenatal_visits, fp_ever_used_any_method,
    distance_to_facility_problem, health_insurance,
    ipv_pushed_shaken, ipv_slapped, ipv_punched_hit, ipv_kicked,
    ipv_choked_burnt, ipv_threatened_weapon, ipv_forced_sex_husband,
    ipv_forced_other_sexual_acts, ipv_any_physical_violence,
    ipv_any_sexual_violence, ipv_any_violence, ipv_injuries_sustained,
    dv_slapped, dv_kicked_dragged, dv_threatened_with_weapon,
    dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo,
    dv_emotionally_threatened, dv_forced_sex, dv_other_forced_sex,
    respondent_weight_kg, respondent_height_cm, respondent_bmi_x100, bmi,
    maternal_anemia_level, iron_tablets_during_pregnancy,
    antimalarial_chloroquine_pregnancy,
    currently_using_contraception, ever_used_family_planning,
    # stillbirth vars from stillbirth file (pre-derived)
    any_nonlive_birth, n_pregnancy_losses, n_early_losses,
    n_stillbirths, had_stillbirth
  ) %>%
  # HEALTHCARE CLEANING
  mutate(
    # visited_health_facility_12mo: 9 = missing
    visited_health_facility_12mo = if_else(
      visited_health_facility_12mo == 9, NA_real_,
      visited_health_facility_12mo
    ),
    # last_facility_public_or_private: 0 = no visit, 1 = public, 2 = private, 9 = missing
    last_facility_public_or_private = case_when(
      last_facility_public_or_private == 9   ~ NA_character_,
      last_facility_public_or_private == 0   ~ "none",
      last_facility_public_or_private == 1   ~ "public",
      last_facility_public_or_private == 2   ~ "private",
      is.na(last_facility_public_or_private) ~ NA_character_
    ),
    # num_antenatal_visits: DHS composite codes (1xx, 2xx); actual count = value %% 100
    # 998/999 = missing
    num_antenatal_visits = case_when(
      num_antenatal_visits %in% c(998, 999) ~ NA_real_,
      !is.na(num_antenatal_visits)           ~ num_antenatal_visits %% 100,
      TRUE                                   ~ NA_real_
    ),
    # had_stillbirth: comes in as logical (TRUE/FALSE) from stillbirth file, recode to 0/1
    had_stillbirth = as.numeric(had_stillbirth)
  ) %>%
  # IPV RECODE
  mutate(
    ipv_physical = if_else(
      rowSums(!is.na(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                               ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                               dv_kicked_dragged, dv_threatened_with_weapon,
                               dv_physically_hurt_since_age15,
                               dv_physically_hurt_last_12mo)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                         ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                         dv_kicked_dragged, dv_threatened_with_weapon,
                         dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_slapped) & dv_slapped > 0),
        1, 0)
    ),
    ipv_emotional = if_else(
      is.na(dv_emotionally_threatened), NA_real_,
      if_else(dv_emotionally_threatened == 1, 1, 0)
    ),
    ipv_sexual = if_else(
      rowSums(!is.na(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                               ipv_any_sexual_violence, dv_forced_sex)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                         ipv_any_sexual_violence, dv_forced_sex),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_other_forced_sex) & dv_other_forced_sex > 0),
        1, 0)
    )
  ) %>%
  # DROP RAW IPV/DV COLUMNS
  select(-c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit, ipv_kicked,
            ipv_choked_burnt, ipv_threatened_weapon, ipv_forced_sex_husband,
            ipv_forced_other_sexual_acts, ipv_any_physical_violence,
            ipv_any_sexual_violence, ipv_any_violence, ipv_injuries_sustained,
            dv_slapped, dv_kicked_dragged, dv_threatened_with_weapon,
            dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo,
            dv_emotionally_threatened, dv_forced_sex, dv_other_forced_sex)) %>%
  # RENAME
  rename(
    year            = survey_year.x,
    age             = respondent_current_age,
    education_level = education_level.x,
    age_first_union = age_at_first_cohabitation_imputed
  )

## Explore obstetric & sexual history 
# Scatterplot of parity v age
parity <- merged_clean %>% group_by(age, total_children_ever_born) %>% tally()

ggplot(parity, aes(x = age, y = total_children_ever_born, alpha = n)) + 
  geom_tile(fill = "#2B6CB0") +
  theme_minimal() + labs(x = "Age of Mother", y = "Parity") + 
  theme(text = element_text(face = "bold", size = 13),
        legend.position = "top")

# Histograms of age at first sex/birth
age_sex   <- merged_clean %>% group_by(age_at_first_sex_imputed) %>% tally()
age_birth <- merged_clean %>% group_by(age_at_first_birth) %>% tally()

age_birth <- na.omit(age_birth)
age_sex   <- age_sex %>% filter(age_at_first_sex_imputed > 4 & age_at_first_sex_imputed < 40)

P1 <- 
  ggplot(age_sex, aes(x = age_at_first_sex_imputed, y = n)) + 
  geom_bar(stat = "identity", fill = "#4A90D9") + 
  theme_minimal() + labs(x = "Age at First Sexual Intercourse", y = "No. of Women") + 
  theme(text = element_text(face = "bold", size = 13))

P2 <- 
  ggplot(age_birth, aes(x = age_at_first_birth, y = n)) + 
  geom_bar(stat = "identity", fill = "#4A90D9") + 
  theme_minimal() + labs(x = "Age at First Birth", y = "No. of Women") + 
  theme(text = element_text(face = "bold", size = 13))

ggarrange(P1, P2)

## Predictor distribution by stillbirth status
# Compares mean values of key predictors between livebirth and stillbirth groups
# Helps identify which variables have genuine discriminative signal
# and flags any implausible differences that may indicate coding issues
predictor_comparison <- merged_clean %>%
  filter(!is.na(had_stillbirth)) %>%
  mutate(had_stillbirth = factor(had_stillbirth,
                                 levels = c(0, 1),
                                 labels = c("livebirth", "stillbirth"))) %>%
  select(had_stillbirth, age, education_level, wealth_index_quintile,
         total_children_ever_born, age_at_first_birth, bmi,
         maternal_anemia_level, ipv_physical, ipv_emotional, ipv_sexual) %>%
  pivot_longer(-had_stillbirth) %>%
  group_by(name, had_stillbirth) %>%
  summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = had_stillbirth, values_from = mean) %>%
  mutate(difference = stillbirth - livebirth) %>%
  arrange(desc(abs(difference)))

print(predictor_comparison)

## Compute correlations across the predictors 
# Recode multi-category vars to binary for correlation
corr_data <- merged_clean %>%
  mutate(
    # currently_using_contraception: 0 = not using, 1-16 = method codes, 99 = missing
    contraception_current = case_when(
      currently_using_contraception == 99  ~ NA_real_,
      is.na(currently_using_contraception) ~ NA_real_,
      currently_using_contraception == 0   ~ 0,
      TRUE                                 ~ 1
    ),
    # fp_ever_used_any_method: clean 0/1 binary from DHS processing; 9 = missing
    fp_ever_used = case_when(
      is.na(fp_ever_used_any_method)   ~ NA_real_,
      fp_ever_used_any_method == 9     ~ NA_real_,
      TRUE                             ~ as.numeric(fp_ever_used_any_method)
    ),
    # num_antenatal_visits: already cleaned to actual count above
    # iron/antimalarial: binary 0/1; 8/9 -> NA
    # vitamin_a excluded: 90% missing and never co-occurs with iron_tablets
    # (asked at different survey waves), causing uncorrelatable pairs
    iron_tablets_during_pregnancy = if_else(
      iron_tablets_during_pregnancy %in% c(8, 9), NA_real_,
      iron_tablets_during_pregnancy
    ),
    antimalarial_chloroquine_pregnancy = if_else(
      antimalarial_chloroquine_pregnancy %in% c(8, 9), NA_real_,
      antimalarial_chloroquine_pregnancy
    )
  ) %>%
  select(
    age,
    education_level,
    literacy,
    total_children_ever_born,
    age_at_first_birth,
    age_first_union,
    months_since_last_birth,
    num_antenatal_visits,
    respondent_weight_kg,
    respondent_height_cm,
    maternal_anemia_level,
    iron_tablets_during_pregnancy,
    antimalarial_chloroquine_pregnancy,
    fp_ever_used,
    contraception_current,
    n_pregnancy_losses,
    n_early_losses,
    n_stillbirths
  )

# Labels (vitamin_a removed, order matches select above)
pretty_labels <- c(
  "Age",
  "Education level",
  "Literacy",
  "Children ever born",
  "Age at first birth",
  "Age at first union",
  "Months since last birth",
  "Antenatal visits",
  "Weight (kg)",
  "Height (cm)",
  "Anaemia level",
  "Iron tablets (pregnancy)",
  "Antimalarials (pregnancy)",
  "Ever used family planning",
  "Currently using contraception",
  "Pregnancy losses (total)",
  "Early pregnancy losses",
  "Stillbirths"
)

# Compute pairwise correlation matrix (pairwise complete obs)
# suppressWarnings: iron_tablets x n_stillbirths has zero variance in pairwise overlap
# (no women with iron tablet data had a recorded stillbirth); replaced with 0 below
corr_matrix <- suppressWarnings(
  cor(corr_data, use = "pairwise.complete.obs", method = "spearman")
)

# Replace NA and NaN with 0 — is.nan() alone misses NA values in R
corr_matrix[is.na(corr_matrix) | is.nan(corr_matrix)] <- 0

# Apply pretty labels
rownames(corr_matrix) <- pretty_labels
colnames(corr_matrix) <- pretty_labels

# Plot
par(font = 2)  # bold for all text elements
corrplot(
  corr_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",        # cluster similar variables together
  tl.col      = "black",
  tl.srt      = 45,
  tl.cex      = 1.0,
  cl.cex      = 0.9,
  addCoef.col = "black",         # show correlation values
  number.cex  = 0.6,
  col         = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  diag        = FALSE,
  mar         = c(0, 0, 2, 0),
  title       = "Spearman correlation matrix"
)

## Identify number of stillbirths and losses 
stillbirths_tbl <- merged_clean %>% group_by(n_stillbirths) %>% tally()
losses_tbl      <- merged_clean %>% group_by(n_pregnancy_losses) %>% tally()
early_loss_tbl  <- merged_clean %>% group_by(n_early_losses) %>% tally()

## Spatial map of stillbirths to admin 1
# Harmonise adm1_name across survey waves (inconsistent naming)
# then aggregate stillbirths per admin unit and join to shapefile
merged_clean <- merged_clean %>%
  mutate(adm1_harmonised = case_when(
    tolower(adm1_name) %in% c("addis ababa", "addis", "addis abeba") ~ "Addis Ababa",
    tolower(adm1_name) %in% c("afar region", "afar", "affar")        ~ "Afar",
    tolower(adm1_name) %in% c("amhara region", "amhara")             ~ "Amhara",
    tolower(adm1_name) %in% c("benishangul gumuz", "ben-gumz")       ~ "Benishangul-Gumuz",
    tolower(adm1_name) %in% c("dire dawa")                           ~ "Dire Dawa",
    tolower(adm1_name) %in% c("gambella", "gambela")                 ~ "Gambella",
    tolower(adm1_name) %in% c("harari")                              ~ "Harari",
    tolower(adm1_name) %in% c("oromiya region", "oromiya")           ~ "Oromia",
    tolower(adm1_name) %in% c("snnpr", "snnp")                      ~ "SNNPR",
    tolower(adm1_name) %in% c("somali")                              ~ "Somali",
    tolower(adm1_name) %in% c("tigray region", "tigray")             ~ "Tigray",
    TRUE ~ adm1_name
  ))

# Aggregate: total stillbirths and rate per 1,000 women per admin 1
sb_by_adm1 <- merged_clean %>%
  filter(!is.na(had_stillbirth)) %>%
  group_by(adm1_harmonised) %>%
  summarise(
    n_women       = n(),
    n_stillbirths = sum(had_stillbirth, na.rm = TRUE),
    sb_rate       = n_stillbirths / n_women * 1000  # per 1,000 women
  )

# Load admin 1 shapefile (same as MPDSR script)
# Note: check the name field in your shapefile — adjust "adm1_name" below if needed
adm1_sf <- read_sf("spatial/spatial_admin1.gpkg")

# Join stillbirth aggregates to shapefile
adm1_sf <- adm1_sf %>%
  left_join(sb_by_adm1, by = c("adm1_name" = "adm1_harmonised"))

# Map: total stillbirth count per admin 1
ggplot(adm1_sf) +
  geom_sf(aes(fill = n_stillbirths)) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90") +
  theme_minimal() +
  labs(fill = "Stillbirths") +
  theme(text = element_text(face = "bold", size = 13))

# Map: stillbirth rate per 1,000 women (adjusts for unequal sampling across regions)
# ggplot(adm1_sf) +
#   geom_sf(aes(fill = sb_rate)) +
#   scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90") +
#   theme_minimal() +
#   labs(fill = "Stillbirths\nper 1,000 women") +
#   theme(text = element_text(face = "bold", size = 13))

## XGBoost + SHAP
## MODEL 1: Who is at risk of stillbirth 
# Same approach but three key differences given larger dataset:
# 1. scale_pos_weight derived from real class ratio (~97:1), not hardcoded to 0.5
# 2. step_impute_median added to recipe — avoids listwise deletion of sparse vars
# 3. tree_depth increased to 4 (more data/predictors warrants slightly deeper trees)
# Rows with NA in had_stillbirth excluded (women with no pregnancy history recorded)
merged_clean <- merged %>%
  # SELECT
  select(
    survey_year.x, lat, lon, adm1_name, urban_rural,
    respondent_current_age,
    education_level.x, literacy,
    wealth_index_quintile,
    total_children_ever_born, births_last_5yrs, age_at_first_birth,
    years_since_first_cohabitation, age_at_first_cohabitation_imputed,
    age_at_first_sex_imputed, months_since_last_birth, age_difference_husband,
    visited_health_facility_12mo, last_facility_public_or_private,
    num_antenatal_visits, fp_ever_used_any_method,
    distance_to_facility_problem, health_insurance,
    ipv_pushed_shaken, ipv_slapped, ipv_punched_hit, ipv_kicked,
    ipv_choked_burnt, ipv_threatened_weapon, ipv_forced_sex_husband,
    ipv_forced_other_sexual_acts, ipv_any_physical_violence,
    ipv_any_sexual_violence, ipv_any_violence, ipv_injuries_sustained,
    dv_slapped, dv_kicked_dragged, dv_threatened_with_weapon,
    dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo,
    dv_emotionally_threatened, dv_forced_sex, dv_other_forced_sex,
    respondent_weight_kg, respondent_height_cm, bmi,
    maternal_anemia_level, iron_tablets_during_pregnancy,
    antimalarial_chloroquine_pregnancy,
    currently_using_contraception, ever_used_family_planning,
    # stillbirth vars from stillbirth file (pre-derived)
    any_nonlive_birth, n_pregnancy_losses, n_early_losses,
    n_stillbirths, had_stillbirth
  ) %>%
  # CLEANING AND RECODING
  mutate(
    # visited_health_facility_12mo: 9 = missing
    visited_health_facility_12mo = if_else(
      visited_health_facility_12mo == 9, NA_real_,
      visited_health_facility_12mo
    ),
    # last_facility_public_or_private: 0 = no visit, 1 = public, 2 = private, 9 = missing
    last_facility_public_or_private = case_when(
      last_facility_public_or_private == 9   ~ NA_character_,
      last_facility_public_or_private == 0   ~ "none",
      last_facility_public_or_private == 1   ~ "public",
      last_facility_public_or_private == 2   ~ "private",
      is.na(last_facility_public_or_private) ~ NA_character_
    ),
    # num_antenatal_visits: DHS composite codes (1xx, 2xx); actual count = value %% 100
    # 998/999 = missing
    num_antenatal_visits = case_when(
      num_antenatal_visits %in% c(998, 999) ~ NA_real_,
      !is.na(num_antenatal_visits)           ~ num_antenatal_visits %% 100,
      TRUE                                   ~ NA_real_
    ),
    # distance_to_facility_problem: character, "9" = missing
    distance_to_facility_problem = if_else(
      distance_to_facility_problem == "9", NA_character_,
      distance_to_facility_problem
    ),
    # health_insurance: character, "9" = missing
    health_insurance = if_else(
      health_insurance == "9", NA_character_,
      health_insurance
    ),
    # had_stillbirth: comes in as logical (TRUE/FALSE) from stillbirth file, recode to 0/1
    had_stillbirth = as.numeric(had_stillbirth),
    # literacy: 9 = missing
    literacy = if_else(literacy == 9, NA_real_, literacy),
    # bmi: stored x100 in DHS; 9999 = missing code; divide by 100 for real BMI units
    bmi = if_else(bmi >= 9999, NA_real_, bmi / 100),
    # maternal_anemia_level: stored as haemoglobin g/dL x1000 in DHS; 99999 = missing
    # divide by 1000 to get real g/dL then recode to standard DHS 4-level anaemia category:
    # 1 = severe (<7.0 g/dL), 2 = moderate (7.0-9.9), 3 = mild (10.0-10.9), 4 = not anaemic (>=11.0)
    maternal_anemia_level = case_when(
      maternal_anemia_level >= 99999       ~ NA_real_,
      maternal_anemia_level / 1000 <  7.0  ~ 1,
      maternal_anemia_level / 1000 < 10.0  ~ 2,
      maternal_anemia_level / 1000 < 11.0  ~ 3,
      maternal_anemia_level / 1000 >= 11.0 ~ 4,
      TRUE                                 ~ NA_real_
    )
  ) %>%
  # IPV RECODE
  mutate(
    ipv_physical = if_else(
      rowSums(!is.na(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                              ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                              dv_kicked_dragged, dv_threatened_with_weapon,
                              dv_physically_hurt_since_age15,
                              dv_physically_hurt_last_12mo)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                         ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                         dv_kicked_dragged, dv_threatened_with_weapon,
                         dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_slapped) & dv_slapped > 0),
        1, 0)
    ),
    ipv_emotional = if_else(
      is.na(dv_emotionally_threatened), NA_real_,
      if_else(dv_emotionally_threatened == 1, 1, 0)
    ),
    ipv_sexual = if_else(
      rowSums(!is.na(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                              ipv_any_sexual_violence, dv_forced_sex)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                         ipv_any_sexual_violence, dv_forced_sex),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_other_forced_sex) & dv_other_forced_sex > 0),
        1, 0)
    )
  ) %>%
  # DROP RAW IPV/DV COLUMNS
  select(-c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit, ipv_kicked,
            ipv_choked_burnt, ipv_threatened_weapon, ipv_forced_sex_husband,
            ipv_forced_other_sexual_acts, ipv_any_physical_violence,
            ipv_any_sexual_violence, ipv_any_violence, ipv_injuries_sustained,
            dv_slapped, dv_kicked_dragged, dv_threatened_with_weapon,
            dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo,
            dv_emotionally_threatened, dv_forced_sex, dv_other_forced_sex)) %>%
  # RENAME
  rename(
    year            = survey_year.x,
    age             = respondent_current_age,
    education_level = education_level.x,
    age_first_union = age_at_first_cohabitation_imputed
  )

# Class imbalance ratio for scale_pos_weight
# XGBoost uses this to upweight the minority (stillbirth) class
n_livebirth  <- sum(shap_data$had_stillbirth == "livebirth")
n_stillbirth <- sum(shap_data$had_stillbirth == "stillbirth")
weight_ratio <- sqrt(n_livebirth / n_stillbirth)

# Train/test split stratified on outcome
set.seed(42)
split      <- initial_split(shap_data, prop = 0.8, strata = had_stillbirth)
train_data <- training(split)
test_data  <- testing(split)

# 10-fold cross-validation stratified on outcome
cv_folds <- vfold_cv(train_data, v = 10, strata = had_stillbirth)

# Recipe
# step_novel:         handle factor levels unseen in training
# step_unknown:       recode NA in nominal vars to explicit "unknown" level
# step_dummy:         encode character/factor predictors
# step_impute_median: impute numeric NAs for vars where missing = unknown not structural
# step_zv:            remove zero-variance columns
xgb_recipe <- recipe(had_stillbirth ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(bmi, maternal_anemia_level,
                     age_at_first_birth, age_first_union,
                     months_since_last_birth) %>%
  step_zv(all_predictors())

# Model spec
# scale_pos_weight = sqrt of class ratio, balances sensitivity/specificity
# tree_depth = 4 slightly deeper than default given more data and predictors
xgb_spec <- boost_tree(
  trees          = 500,
  tree_depth     = 4,
  learn_rate     = 0.05,
  loss_reduction = 0.01,
  min_n          = 10
) %>%
  set_engine("xgboost", scale_pos_weight = weight_ratio) %>%
  set_mode("classification")

# Workflow
xgb_workflow <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

# Cross-validated performance
cv_results <- fit_resamples(
  xgb_workflow,
  resamples = cv_folds,
  metrics   = metric_set(roc_auc, average_precision),
  control   = control_resamples(save_pred = TRUE)
)

# Threshold-dependent metrics computed manually with correct event level
# threshold set at prevalence level (~1%) rather than default 0.5
# since model probabilities are calibrated to population prevalence
threshold  <- n_stillbirth / (n_stillbirth + n_livebirth)

cv_preds <- collect_predictions(cv_results) %>%
  mutate(.pred_class = factor(
    if_else(.pred_stillbirth >= threshold, "stillbirth", "livebirth"),
    levels = c("livebirth", "stillbirth")
  ))

# Pooled metrics across all folds
pooled_metrics <- bind_rows(
  roc_auc(cv_preds,           truth = had_stillbirth, .pred_stillbirth,      event_level = "second"),
  average_precision(cv_preds, truth = had_stillbirth, .pred_stillbirth,      event_level = "second"),
  sensitivity(cv_preds,       truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  specificity(cv_preds,       truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  ppv(cv_preds,               truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  npv(cv_preds,               truth = had_stillbirth, estimate = .pred_class, event_level = "second")
)

# Per-fold mean and SE
per_fold_metrics <- cv_preds %>%
  group_by(id) %>%
  summarise(
    roc_auc = roc_auc_vec(had_stillbirth, .pred_stillbirth, event_level = "second"),
    sens    = sensitivity_vec(had_stillbirth, .pred_class,   event_level = "second"),
    spec    = specificity_vec(had_stillbirth, .pred_class,   event_level = "second")
  ) %>%
  summarise(across(roc_auc:spec, list(mean = mean, se = ~sd(.)/sqrt(n()))))

print(pooled_metrics)
print(per_fold_metrics)

# Compare thresholds to find best sensitivity/specificity balance
# Run this after cv_results to inform threshold selection before final model
thresholds <- c(0.005, 0.008, 0.01, 0.015, 0.02, 0.03, 0.05)

threshold_comparison <- map_dfr(thresholds, function(thresh) {
  cv_preds_t <- collect_predictions(cv_results) %>%
    mutate(.pred_class = factor(
      if_else(.pred_stillbirth >= thresh, "stillbirth", "livebirth"),
      levels = c("livebirth", "stillbirth")
    ))
  tibble(
    threshold   = thresh,
    sensitivity = sensitivity_vec(cv_preds_t$had_stillbirth, cv_preds_t$.pred_class, event_level = "second"),
    specificity = specificity_vec(cv_preds_t$had_stillbirth, cv_preds_t$.pred_class, event_level = "second"),
    ppv         = ppv_vec(cv_preds_t$had_stillbirth,         cv_preds_t$.pred_class, event_level = "second"),
    f1          = f_meas_vec(cv_preds_t$had_stillbirth,      cv_preds_t$.pred_class, event_level = "second")
  )
})

print(threshold_comparison)

# Set threshold based on results above — update this value after inspecting output
# default suggestion: threshold that maximises F1, or lowest threshold with sens > 0.1
threshold <- 0.005

# Fit final model on full training set
final_fit <- fit(xgb_workflow, data = train_data)

# Test set evaluation with correct event level
test_preds <- augment(final_fit, test_data) %>%
  mutate(.pred_class = factor(
    if_else(.pred_stillbirth >= threshold, "stillbirth", "livebirth"),
    levels = c("livebirth", "stillbirth")
  ))

roc_auc(test_preds,  truth = had_stillbirth, .pred_stillbirth, event_level = "second")
conf_mat(test_preds, truth = had_stillbirth, estimate = .pred_class)

# Full test set performance summary
test_summary <- bind_rows(
  roc_auc(test_preds,     truth = had_stillbirth, .pred_stillbirth,      event_level = "second"),
  sensitivity(test_preds, truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  specificity(test_preds, truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  ppv(test_preds,         truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  npv(test_preds,         truth = had_stillbirth, estimate = .pred_class, event_level = "second"),
  f_meas(test_preds,      truth = had_stillbirth, estimate = .pred_class, event_level = "second")
)

print(test_summary)
conf_mat(test_preds, truth = had_stillbirth, estimate = .pred_class)

# Extract fitted xgboost model and baked training matrix for SHAP
xgb_model   <- extract_fit_engine(final_fit)
train_baked <- xgb_recipe %>%
  prep() %>%
  bake(new_data = train_data) %>%
  select(-had_stillbirth) %>%
  as.matrix()

shp <- shapviz(xgb_model, X_pred = train_baked)

## SHAP PLOTS
# Feature label lookup — defined once, reused across all plots
# note: leakage variables removed (ever_had_terminated_pregnancy,
# months_pregnant_at_loss, num_other_terminated_pregnancies,
# months_since_last_termination, pregnancy_end_completeness,
# problem_after_delivery_or_stillbirth)
feature_labels <- c(
  "age"                                        = "Age",
  "education_level"                            = "Education level",
  "literacy"                                   = "Literacy",
  "wealth_index_quintile"                      = "Wealth quintile",
  "urban_rural"                                = "Urban/rural",
  "age_at_first_birth"                         = "Age at first birth",
  "age_first_union"                            = "Age at first union",
  "age_at_first_sex_imputed"                   = "Age at first sex",
  "age_difference_husband"                     = "Age diff. (husband)",
  "total_children_ever_born"                   = "Children ever born",
  "births_last_5yrs"                           = "Births (last 5 yrs)",
  "months_since_last_birth"                    = "Months since last birth",
  "years_since_first_cohabitation"             = "Years cohabiting",
  "num_antenatal_visits"                       = "Antenatal visits",
  "visited_health_facility_12mo"               = "Visited facility (12mo)",
  "last_facility_public_or_private_none"       = "Facility: none",
  "last_facility_public_or_private_public"     = "Facility: public",
  "last_facility_public_or_private_unknown"      = "Facility: unknown",
  "distance_to_facility_problem_unknown"         = "Distance to facility: unknown",
  "health_insurance_unknown"                     = "Health insurance: unknown",
  "distance_to_facility_problem_big.problem"   = "Distance: big problem",
  "distance_to_facility_problem_small.problem" = "Distance: small problem",
  "health_insurance_yes"                       = "Has health insurance",
  "fp_ever_used_any_method"                    = "Ever used family planning",
  "respondent_weight_kg"                       = "Weight (kg)",
  "respondent_height_cm"                       = "Height (cm)",
  "bmi"                                        = "BMI",
  "maternal_anemia_level"                      = "Anaemia level",
  "iron_tablets_during_pregnancy"              = "Iron tablets (pregnancy)",
  "antimalarial_chloroquine_pregnancy"         = "Antimalarials (pregnancy)",
  "currently_using_contraception"              = "Currently using contraception",
  "ever_used_family_planning"                  = "Ever used family planning (alt)",
  "ipv_physical"                               = "IPV: physical",
  "ipv_emotional"                              = "IPV: emotional",
  "ipv_sexual"                                 = "IPV: sexual"
)

# Rename shapviz object once for dependence plots
shp_renamed <- shp
colnames(shp_renamed$X) <- dplyr::recode(colnames(shp_renamed$X), !!!feature_labels)
colnames(shp_renamed$S) <- dplyr::recode(colnames(shp_renamed$S), !!!feature_labels)

# Bar plot: mean |SHAP| feature importance
p <- sv_importance(shp, kind = "bar")
p$layers[[1]]$aes_params$fill <- "#4A90D9"
p$data$feature <- dplyr::recode(p$data$feature, !!!feature_labels)
p + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "Mean |SHAP value|", y = NULL)

# Beeswarm: direction and magnitude of each feature's effect
b <- sv_importance(shp, kind = "beeswarm")
b$data$feature <- dplyr::recode(b$data$feature, !!!feature_labels)
b + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "SHAP value", y = NULL)

# Dependence plots for top 5 features by mean |SHAP| importance
top5_features <- sv_importance(shp, kind = "bar")$data %>%
  arrange(desc(value)) %>%
  slice_head(n = 5) %>%
  pull(feature) %>%
  as.character() %>%
  dplyr::recode(!!!feature_labels)

sv_dependence(shp_renamed, v = top5_features) &
  theme_minimal() &
  theme(text = element_text(size = 11, face = "bold"))

# Waterfall for first stillbirth in test set
# Shows how each feature pushed the prediction above/below baseline for one woman
stillbirth_idx <- which(test_data$had_stillbirth == "stillbirth")[1]
sv_waterfall(shp, row_id = stillbirth_idx)

## MODEL 2: Binary classification - miscarriage v stillbirth
# Subset to women who had a recorded pregnancy loss with known gestation
# Outcome: late_loss (1 = stillbirth >= 7 months, 0 = earlier loss < 7 months)
# Question: among women with a pregnancy loss, what distinguishes
# stillbirths from earlier miscarriages?
loss_data <- merged %>%
  filter(!is.na(months_pregnant_at_loss) &
           months_pregnant_at_loss > 0 &
           months_pregnant_at_loss < 99) %>%
  mutate(
    # outcome: late loss (stillbirth) vs early loss (miscarriage)
    late_loss = factor(
      if_else(months_pregnant_at_loss >= 7, 1, 0),
      levels = c(0, 1),
      labels = c("early_loss", "stillbirth")
    ),
    # bmi: stored x100 in DHS; 9999 = missing code
    bmi = if_else(bmi >= 9999, NA_real_, bmi / 100),
    # maternal_anemia_level: haemoglobin g/dL x1000; 99999 = missing
    # recode to 4-level DHS anaemia category
    maternal_anemia_level = case_when(
      maternal_anemia_level >= 99999       ~ NA_real_,
      maternal_anemia_level / 1000 <  7.0  ~ 1,
      maternal_anemia_level / 1000 < 10.0  ~ 2,
      maternal_anemia_level / 1000 < 11.0  ~ 3,
      maternal_anemia_level / 1000 >= 11.0 ~ 4,
      TRUE                                 ~ NA_real_
    ),
    # literacy: 9 = missing
    literacy = if_else(literacy == 9, NA_real_, literacy),
    # last_facility_public_or_private: 0 = none, 1 = public, 2 = private, 9 = missing
    last_facility_public_or_private = case_when(
      last_facility_public_or_private == 9   ~ NA_character_,
      last_facility_public_or_private == 0   ~ "none",
      last_facility_public_or_private == 1   ~ "public",
      last_facility_public_or_private == 2   ~ "private",
      is.na(last_facility_public_or_private) ~ NA_character_
    ),
    # num_antenatal_visits: DHS composite codes; actual count = value %% 100
    num_antenatal_visits = case_when(
      num_antenatal_visits %in% c(998, 999) ~ NA_real_,
      !is.na(num_antenatal_visits)           ~ num_antenatal_visits %% 100,
      TRUE                                   ~ NA_real_
    ),
    # distance_to_facility_problem: "9" = missing
    distance_to_facility_problem = if_else(
      distance_to_facility_problem == "9", NA_character_,
      distance_to_facility_problem
    ),
    # health_insurance: "9" = missing
    health_insurance = if_else(
      health_insurance == "9", NA_character_,
      health_insurance
    ),
    # num_other_terminated_pregnancies: NA -> 0
    num_other_terminated_pregnancies = if_else(
      is.na(num_other_terminated_pregnancies), 0,
      num_other_terminated_pregnancies
    ),
    # currently_using_contraception: 0 = not using, 1+ = method code, 99 = missing
    currently_using_contraception = case_when(
      currently_using_contraception == 99  ~ NA_real_,
      is.na(currently_using_contraception) ~ NA_real_,
      currently_using_contraception == 0   ~ 0,
      TRUE                                 ~ 1
    ),
    # fp_ever_used_any_method: 9 = missing
    fp_ever_used_any_method = case_when(
      is.na(fp_ever_used_any_method)   ~ NA_real_,
      fp_ever_used_any_method == 9     ~ NA_real_,
      TRUE                             ~ as.numeric(fp_ever_used_any_method)
    ),
    # iron/antimalarial: 8/9 = missing
    iron_tablets_during_pregnancy = if_else(
      iron_tablets_during_pregnancy %in% c(8, 9), NA_real_,
      iron_tablets_during_pregnancy
    ),
    antimalarial_chloroquine_pregnancy = if_else(
      antimalarial_chloroquine_pregnancy %in% c(8, 9), NA_real_,
      antimalarial_chloroquine_pregnancy
    )
  ) %>%
  # IPV recode
  mutate(
    ipv_physical = if_else(
      rowSums(!is.na(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                              ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                              dv_kicked_dragged, dv_threatened_with_weapon,
                              dv_physically_hurt_since_age15,
                              dv_physically_hurt_last_12mo)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_pushed_shaken, ipv_slapped, ipv_punched_hit,
                         ipv_kicked, ipv_choked_burnt, ipv_threatened_weapon,
                         dv_kicked_dragged, dv_threatened_with_weapon,
                         dv_physically_hurt_since_age15, dv_physically_hurt_last_12mo),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_slapped) & dv_slapped > 0),
        1, 0)
    ),
    ipv_emotional = if_else(
      is.na(dv_emotionally_threatened), NA_real_,
      if_else(dv_emotionally_threatened == 1, 1, 0)
    ),
    ipv_sexual = if_else(
      rowSums(!is.na(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                              ipv_any_sexual_violence, dv_forced_sex)))) == 0,
      NA_real_,
      if_else(
        rowSums(across(c(ipv_forced_sex_husband, ipv_forced_other_sexual_acts,
                         ipv_any_sexual_violence, dv_forced_sex),
                       ~ . == 1 & !. %in% c(8, 9)), na.rm = TRUE) > 0 |
          (!is.na(dv_other_forced_sex) & dv_other_forced_sex > 0),
        1, 0)
    )
  ) %>%
  select(-age, -age_first_union) %>%
  rename(
    age             = respondent_current_age,
    education_level = education_level.x,
    age_first_union = age_at_first_cohabitation_imputed
  ) %>%
  select(
    late_loss,
    # demographic & socioeconomic
    age, education_level, literacy, wealth_index_quintile, urban_rural,
    # reproductive history
    age_at_first_birth, age_first_union, age_at_first_sex_imputed,
    age_difference_husband, total_children_ever_born, births_last_5yrs,
    months_since_last_birth, years_since_first_cohabitation,
    num_other_terminated_pregnancies,
    # healthcare access
    num_antenatal_visits, visited_health_facility_12mo,
    last_facility_public_or_private, distance_to_facility_problem,
    health_insurance, fp_ever_used_any_method,
    # anthropometrics
    respondent_weight_kg, respondent_height_cm, bmi,
    # clinical
    maternal_anemia_level, iron_tablets_during_pregnancy,
    antimalarial_chloroquine_pregnancy,
    currently_using_contraception, ever_used_family_planning,
    # violence
    ipv_physical, ipv_emotional, ipv_sexual
  )

cat("Women with pregnancy loss:", nrow(loss_data), "\n")
table(loss_data$late_loss)

# Class imbalance ratio
n_early      <- sum(loss_data$late_loss == "early_loss")
n_late       <- sum(loss_data$late_loss == "stillbirth")
weight_ratio_m2 <- sqrt(n_early / n_late)
cat("Class ratio (early:stillbirth):", round(n_early / n_late, 1), "\n")
cat("scale_pos_weight (sqrt):", round(weight_ratio_m2, 2), "\n")

# Train/test split stratified on outcome
set.seed(42)
split_m2      <- initial_split(loss_data, prop = 0.8, strata = late_loss)
train_data_m2 <- training(split_m2)
test_data_m2  <- testing(split_m2)

# 10-fold cross-validation stratified on outcome
cv_folds_m2 <- vfold_cv(train_data_m2, v = 10, strata = late_loss)

# Recipe
xgb_recipe_m2 <- recipe(late_loss ~ ., data = train_data_m2) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(bmi, maternal_anemia_level, respondent_weight_kg,
                     respondent_height_cm, age_at_first_birth,
                     age_first_union, months_since_last_birth) %>%
  step_zv(all_predictors())

# Model spec
# scale_pos_weight from sqrt of class ratio (~3.7:1, mild imbalance)
xgb_spec_m2 <- boost_tree(
  trees          = 500,
  tree_depth     = 4,
  learn_rate     = 0.05,
  loss_reduction = 0.01,
  min_n          = 10
) %>%
  set_engine("xgboost", scale_pos_weight = weight_ratio_m2) %>%
  set_mode("classification")

# Workflow
xgb_workflow_m2 <- workflow() %>%
  add_recipe(xgb_recipe_m2) %>%
  add_model(xgb_spec_m2)

# Cross-validated performance
cv_results_m2 <- fit_resamples(
  xgb_workflow_m2,
  resamples = cv_folds_m2,
  metrics   = metric_set(roc_auc, average_precision),
  control   = control_resamples(save_pred = TRUE)
)

# Threshold-free CV metrics
cv_preds_m2 <- collect_predictions(cv_results_m2)

pooled_metrics_m2 <- bind_rows(
  roc_auc(cv_preds_m2,           truth = late_loss, .pred_stillbirth,      event_level = "second"),
  average_precision(cv_preds_m2, truth = late_loss, .pred_stillbirth,      event_level = "second")
)

per_fold_metrics_m2 <- cv_preds_m2 %>%
  group_by(id) %>%
  summarise(
    roc_auc = roc_auc_vec(late_loss, .pred_stillbirth, event_level = "second")
  ) %>%
  summarise(across(roc_auc, list(mean = mean, se = ~sd(.)/sqrt(n()))))

print(pooled_metrics_m2)
print(per_fold_metrics_m2)

