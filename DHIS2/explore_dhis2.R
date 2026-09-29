#### DHIS2 ETHIOPIA STILLBIRTH RISK FACTOR ANALYSIS
#### Unit of analysis: zone x month
#### Outcome: log(stillbirths + 1) per zone-month (continuous regression)
#### Method: XGBoost regression + SHAP via shapviz
#### Reads from: dhis2_stillbirth_clean.csv (produced by prelim_explore.R)

## Load packages 
library(tidyverse)
library(tidymodels)
library(xgboost)
library(shapviz)
library(purrr)

## Variable lookup (for feature labels only) 
# Full set of 88 predictors — labels used for SHAP plot axis names
feature_labels <- c(
  # Maternal age / parity
  anc1_by_age                  = "ANC 1st visit by age",
  anc4_by_age                  = "ANC 4th visit by age",
  teen_preg_positive           = "Teen pregnancy (positive count)",
  teen_preg_rate               = "Teenage pregnancy rate",
  elderly_primigravida         = "Elderly primigravida",
  grand_multiparity            = "Grand multiparity",
  poor_obstetric_history       = "Poor obstetric history",
  # ANC coverage & timing
  anc8_contacts                = "ANC 8 contacts",
  anc1_by_gest_week            = "ANC 1st by gestational week",
  anc1_gest_week_count         = "ANC 1st gestational week count",
  anc4_by_gest_week            = "ANC 4th by gestational week",
  anc1_before_12wk_pct         = "ANC 1st before 12 wks (%)",
  anc4_coverage_pct            = "ANC 4+ coverage (%)",
  anc8_coverage_pct            = "ANC 8+ coverage (%)",
  high_risk_preg_supervision   = "High risk pregnancy supervision",
  # Nutrition & micronutrients
  iron_folate_90d              = "Iron-folate supplementation",
  dewormed                     = "Dewormed",
  nutrition_counselled         = "Nutrition counselled",
  malnutrition_icd11           = "Malnutrition (ICD-11)",
  malnutrition_icd10           = "Malnutrition (ICD-10)",
  anaemia_complicating_preg    = "Anaemia in pregnancy",
  low_weight_gain_icd11        = "Low weight gain (ICD-11)",
  low_weight_gain_icd10        = "Low weight gain (ICD-10)",
  # Hypertensive disorders
  pre_eclampsia_icd11          = "Pre-eclampsia (ICD-11)",
  pre_eclampsia_unspec         = "Pre-eclampsia (unspecified)",
  severe_pre_eclampsia         = "Severe pre-eclampsia",
  eclampsia_preg               = "Eclampsia",
  pre_eclampsia_on_chronic_htn = "Pre-eclampsia on chronic HTN",
  pre_existing_htn             = "Pre-existing hypertension",
  unspec_maternal_htn          = "Unspecified hypertension",
  # Diabetes
  diabetes_preg_icd11          = "Diabetes in pregnancy (ICD-11)",
  gestational_diabetes         = "Gestational diabetes",
  diabetes_preg_icd10          = "Diabetes in pregnancy (ICD-10)",
  # Infections
  syphilis_tested              = "Syphilis tested",
  syphilis_treated             = "Syphilis treated",
  syphilis_complicating_preg   = "Syphilis complicating pregnancy",
  uti_preg                     = "UTI in pregnancy",
  pyelonephritis_preg          = "Pyelonephritis in pregnancy",
  genito_urinary_infection     = "Genitourinary infection",
  hiv_complicating_preg        = "HIV complicating pregnancy",
  # Haemorrhage & placenta
  antepartum_haem_icd11        = "Antepartum haemorrhage (ICD-11)",
  antepartum_haem_icd10        = "Antepartum haemorrhage (ICD-10)",
  placenta_praevia_haem        = "Placenta praevia (with haem.)",
  placenta_praevia_no_haem     = "Placenta praevia (no haem.)",
  intrapartum_haem             = "Intrapartum haemorrhage",
  pph                          = "PPH",
  # PROM
  prom                         = "PROM",
  # Prolonged / obstructed labour
  prolonged_1st_stage_icd11    = "Prolonged 1st stage (ICD-11)",
  prolonged_2nd_stage_icd11    = "Prolonged 2nd stage (ICD-11)",
  prolonged_1st_stage_icd10    = "Prolonged 1st stage (ICD-10)",
  prolonged_2nd_stage_icd10    = "Prolonged 2nd stage (ICD-10)",
  obstructed_pelvic            = "Obstructed labour (pelvic)",
  obstructed_malpresentation   = "Obstructed labour (malpresentation)",
  obstructed_labour_unspec     = "Obstructed labour (unspecified)",
  uterine_rupture_during_icd11 = "Uterine rupture during (ICD-11)",
  uterine_rupture_during_icd10 = "Uterine rupture during (ICD-10)",
  uterine_rupture_before       = "Uterine rupture before labour",
  # Fetal distress & asphyxia
  fetal_distress_icd11         = "Fetal distress (ICD-11)",
  fetal_distress_icd10         = "Fetal distress (ICD-10)",
  meconium_liquor              = "Meconium in liquor",
  intrauterine_hypoxia         = "Intrauterine hypoxia",
  # Cord
  cord_prolapse                = "Cord prolapse",
  cord_complications_icd11     = "Cord complications (ICD-11)",
  # Preterm & LBW
  preterm_labour_icd11         = "Preterm labour (ICD-11)",
  preterm_labour_icd10         = "Preterm labour (ICD-10)",
  preterm_newborn              = "Preterm newborn",
  low_birth_weight             = "Low birth weight",
  lbw_or_premature_kmc         = "LBW / premature (KMC)",
  post_term_preg               = "Post-term pregnancy",
  # Fetal growth restriction
  poor_fetal_growth            = "Poor fetal growth",
  fetal_hypoxia_signs          = "Fetal hypoxia signs",
  slow_fetal_growth_newborn    = "Slow fetal growth (newborn)",
  # Multiple gestation
  twin_preg_icd11              = "Twin pregnancy (ICD-11)",
  twin_preg_icd10              = "Twin pregnancy (ICD-10)",
  polyhydramnios               = "Polyhydramnios",
  amniotic_fluid_disorders     = "Amniotic fluid disorders",
  # Malpresentation
  breech                       = "Breech presentation",
  transverse_oblique_lie       = "Transverse / oblique lie",
  cervical_incompetence        = "Cervical incompetence",
  # Rhesus
  rhesus_isoimmunisation       = "Rhesus isoimmunisation",
  # Delivery care
  skilled_birth_attendance     = "Skilled birth attendance",
  caesarean_section            = "Caesarean section",
  # Fistula
  obstetric_fistula_cases      = "Obstetric fistula cases",
  obstetric_fistula_treated    = "Obstetric fistula treated",
  # Substance use
  substance_alcohol            = "Alcohol use disorder",
  substance_khat               = "Khat use disorder",
  substance_tobacco            = "Tobacco use disorder",
  substance_treated_pct        = "Substance use treated (%)"
)

## Load clean data from prelim_explore.R 
# This file is already:
#   - subset to level 3 zones
#   - filtered to 2010-05-01 to 2018-09-01
#   - NAs replaced with 0 for all predictors
#   - log1p outcome included as still_births_log
clean_data <- read_csv("dhis2_stillbirth_clean.csv", show_col_types = FALSE)

cat("Loaded:", nrow(clean_data), "rows x", ncol(clean_data), "columns\n")
cat("Date range:", format(min(clean_data$date)), "to", format(max(clean_data$date)), "\n")
cat("Unique zones:", n_distinct(clean_data$org_unit_id), "\n")

## Build predictor list 
# All columns except identifiers and outcome columns
id_cols      <- c("org_unit_id", "org_unit_name", "region_name",
                  "date", "year", "month")
outcome_cols <- c("still_births", "still_births_log")

# Drop volume markers (zone size proxies — not clinical risk factors)
volume_markers <- c(
  "skilled_birth_attendance",
  "iron_folate_90d",
  "syphilis_tested",
  "dewormed",
  "caesarean_section",
  "anc1_by_age",
  "anc4_by_age",
  "anc1_by_gest_week",
  "anc1_gest_week_count",
  "anc4_by_gest_week",
  "anc8_contacts",
  "nutrition_counselled"
)

# Drop redundant variables — keep best representative from each correlated group
redundant_vars <- c(
  # Teen pregnancy — keep teen_preg_positive (377% diff), drop rate version
  "teen_preg_rate",
  
  # Preterm — keep preterm_newborn (100% diff), drop labour versions
  "preterm_labour_icd11",
  "preterm_labour_icd10",
  
  # ANC coverage — keep anc4_coverage_pct, drop anc8 (less complete, similar signal)
  "anc8_coverage_pct",
  "anc1_before_12wk_pct",   # only 6.6% difference, weak signal
  
  # Pre-eclampsia — keep pre_eclampsia_icd11 (131% diff), drop unspec and chronic htn
  "pre_eclampsia_unspec",
  "pre_eclampsia_on_chronic_htn",
  
  # Malnutrition — keep icd11 (155% diff, correct direction), drop icd10 (wrong direction)
  "malnutrition_icd10",
  
  # Low weight gain — keep icd11 (186% diff), drop icd10
  "low_weight_gain_icd10",
  
  # Diabetes — keep gestational_diabetes (65% diff), drop both icd versions
  "diabetes_preg_icd11",
  "diabetes_preg_icd10",
  
  # Prolonged labour — keep icd11 versions, drop icd10 duplicates
  "prolonged_1st_stage_icd10",
  "prolonged_2nd_stage_icd10",
  
  # Obstructed labour — keep obstructed_pelvic (65% diff), drop unspec and malpresentation
  "obstructed_labour_unspec",
  "obstructed_malpresentation",
  
  # Uterine rupture — keep uterine_rupture_during_icd11, drop icd10 and before
  "uterine_rupture_during_icd10",
  "uterine_rupture_before",
  
  # Fetal distress — keep icd11 (170% diff), drop icd10 (48% diff)
  "fetal_distress_icd10",
  
  # Twin pregnancy — keep icd10 (52% diff), drop icd11 (26% diff)
  "twin_preg_icd11",
  
  # Antepartum haem — keep icd11 (88% diff), drop icd10 (8% diff)
  "antepartum_haem_icd10",
  
  # Substance use — all near zero signal
  "substance_alcohol",
  "substance_khat",
  "substance_tobacco",
  "substance_treated_pct"
)

# Apply both exclusions in one step
vars_to_drop <- c(volume_markers, redundant_vars)
predictor_cols <- predictor_cols[!predictor_cols %in% vars_to_drop]

cat("Predictors after removing volume markers and duplicates:", length(predictor_cols), "\n")
print(predictor_cols)

## Build modelling dataset 
# Use log outcome — handles right skew and zeros
# Keep identifiers in dataset; recipe will assign them ID role
shap_data <- clean_data %>%
  select(org_unit_name, region_name, date,
         still_births_log, all_of(predictor_cols))

cat("\nOutcome distribution (log scale):\n")
summary(shap_data$still_births_log)

## Train/test split and cross-validation 
set.seed(42)
split      <- initial_split(shap_data, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)

cv_folds <- vfold_cv(train_data, v = 10)

## Recipe and model 
xgb_recipe <- recipe(still_births_log ~ ., data = train_data) %>%
  update_role(org_unit_name, region_name, date, new_role = "ID") %>%
  step_zv(all_predictors())

xgb_spec <- boost_tree(
  trees      = 500,
  tree_depth = 3,
  learn_rate = 0.05,
  min_n      = 10
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_workflow <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

## Cross-validated performance
cv_results <- fit_resamples(
  xgb_workflow,
  resamples = cv_folds,
  metrics   = metric_set(rmse, rsq, mae),
  control   = control_resamples(save_pred = TRUE)
)

cat("\n--- Cross-validated metrics ---\n")
collect_metrics(cv_results) %>% print()

## Fit final model and evaluate on test set
final_fit <- fit(xgb_workflow, data = train_data)

test_preds <- augment(final_fit, test_data)

cat("\n--- Test set metrics ---\n")
metrics(test_preds, truth = still_births_log, estimate = .pred) %>% print()

# Observed vs predicted plot (log scale)
ggplot(test_preds, aes(x = still_births_log, y = .pred)) +
  geom_point(alpha = 0.4, colour = "#4A90D9") +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Observed log(stillbirths + 1)",
       y = "Predicted log(stillbirths + 1)") +
  theme(text = element_text(face = "bold", size = 13))

## SHAP values
xgb_model <- extract_fit_engine(final_fit)

train_baked <- xgb_recipe %>%
  prep() %>%
  bake(new_data = train_data) %>%
  select(-still_births_log) %>%
  select(where(is.numeric)) %>%
  as.matrix()

shp <- shapviz(xgb_model, X_pred = train_baked)

## Rename shapviz object 
# Only rename columns that exist in the model
present_labels <- feature_labels[names(feature_labels) %in% colnames(shp$X)]

shp_renamed <- shp
colnames(shp_renamed$X) <- dplyr::recode(colnames(shp_renamed$X), !!!present_labels)
colnames(shp_renamed$S) <- dplyr::recode(colnames(shp_renamed$S), !!!present_labels)

## SHAP plots
# Bar plot — global importance
p <- sv_importance(shp, kind = "bar")
p$layers[[1]]$aes_params$fill <- "#4A90D9"
p$data$feature <- dplyr::recode(p$data$feature, !!!present_labels)
p + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "Mean |SHAP value|", y = NULL)

p <- sv_importance(shp, kind = "bar", max_display = length(predictor_cols))
p$data$feature <- dplyr::recode(p$data$feature, !!!present_labels)

shap_importance <- p$data %>%
  select(feature, mean_shap = value) %>%
  arrange(desc(mean_shap))

write_csv(shap_importance, "shap_importance.csv")

# Beeswarm — importance + direction
b <- sv_importance(shp, kind = "beeswarm")
b$data$feature <- dplyr::recode(b$data$feature, !!!present_labels)
b + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "SHAP value", y = NULL)

# Dependence plots for all predictors
sv_dependence(shp_renamed, v = unname(present_labels)) &
  theme_minimal() &
  theme(text = element_text(size = 11, face = "bold"))

# Waterfall for zone-month with highest stillbirth count in test set
high_idx <- which.max(test_data$still_births_log)
sv_waterfall(shp, row_id = high_idx)
