
#### EXPLORING THE MPDSR DATASET 
### MPDSR IS AN INCIDENT BASED REPORTING SYSTEM, WHEN WOMEN DIE DURING PREGNANCY THEY ARE REPORTED HERE
### IT ALSO GIVES BIRTH OUTCOME, IF THE WOMEN MADE IT TO LABOUR 

## Load packages
library(dplyr)
library(ggplot2)
library(zoo)
library(sf)
library(tidyr)
library(tidymodels)
library(xgboost)
library(shapviz)
library(readxl)

## Load data
mpdsr <- read_excel("Final data set_mpdsr.xlsx",
                    sheet = "Final data set_1")

## Plot time-series 
# Time-series of all data 
time_series <- mpdsr %>% group_by(Date_Re, Month_Rep, Year_Rep) %>% tally()

time_series <- time_series %>% 
  mutate(Year_Rep = case_when(Year_Rep == 12 ~ 2012, 
                              Year_Rep == 13 ~ 2013,
                              Year_Rep != 12 | 13 ~ Year_Rep)) %>%
  group_by(Month_Rep, Year_Rep) %>% 
  mutate(monthly_reports = sum(n)) %>%
  ungroup() %>% 
  mutate(yearmon = as.yearmon(paste(Year_Rep, Month_Rep, sep = "-"), "%Y-%m"))

ggplot(time_series, aes(x = yearmon, y = monthly_reports)) + 
  geom_line(color = "#4A90D9", size = 1.5) + theme_minimal() + 
  labs(x = "Date", y = "Monthly Incidents") + 
  theme(text = element_text(face = "bold", size = 13))

# Time-series by rural/urban
time_series2 <- mpdsr %>% group_by(Date_Re, Month_Rep, Year_Rep, Residenceofdeceased) %>% tally()

time_series2 <- time_series2 %>% 
  mutate(Year_Rep = case_when(Year_Rep == 12 ~ 2012, 
                              Year_Rep == 13 ~ 2013,
                              Year_Rep != 12 | 13 ~ Year_Rep)) %>%
  group_by(Month_Rep, Year_Rep, Residenceofdeceased) %>% 
  mutate(monthly_reports = sum(n)) %>%
  ungroup() %>% 
  mutate(yearmon = as.yearmon(paste(Year_Rep, Month_Rep, sep = "-"), "%Y-%m"))

ggplot(time_series2, aes(x = yearmon, y = monthly_reports, color = Residenceofdeceased)) + 
  geom_line(size = 1.5) + theme_minimal() + 
  labs(x = "Date", y = "Monthly Incidents", color = "Residence") + 
  theme(text = element_text(face = "bold", size = 13),
        legend.position = "top") + 
  scale_color_manual(values = c("#2B6CB0", "#7EB8E8"))

## Plot a map
# At admin 1
adm1 <- read_sf("spatial/spatial_admin1.gpkg")

ggplot(adm1) +
  geom_sf(aes(fill = n_records)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() + 
  labs(fill = "Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

# At admin 2
adm2 <- read_sf("spatial/spatial_admin2.gpkg")

ggplot(adm2) +
  geom_sf(aes(fill = n_records)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() + 
  labs(fill = "Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

# At admin 3
adm3 <- read_sf("spatial/spatial_admin3.gpkg")

ggplot(adm3) +
  geom_sf(aes(fill = n_records)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() + 
  labs(fill = "Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

## Histogram of age of mother 
age_mother <- mpdsr %>% group_by(AgeAtDeathinyears) %>% tally()

ggplot(age_mother, aes(x = AgeAtDeathinyears, y = n)) + 
  geom_bar(stat = "identity", fill = "#4A90D9") + 
  theme_minimal() + labs(x = "Age of Mother", y = "No. of Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

## Barplot 
# of time of death
t_death <- mpdsr %>% 
  mutate(`Time of Death` = case_when(
    `Time of Death` == "Post Partum"   ~ "PostPartum",
    `Time of Death` == "intrapartum"   ~ "IntraPartum",
    `Time of Death` == "antipartum"    ~ "AntePartum",
    `Time of Death` == "Intra-partum"  ~ "IntraPartum",
    `Time of Death` == "post partum"   ~ "PostPartum",
    `Time of Death` == "Postpartum"    ~ "PostPartum",
    `Time of Death` == "Antepartum"    ~ "AntePartum",
    TRUE ~ `Time of Death`
  )) %>%
  group_by(`Time of Death`) %>% tally() %>%
  ungroup()

t_death <- na.omit(t_death)

ggplot(t_death, aes(x = `Time of Death`, y = n)) + 
  geom_bar(stat = "identity", fill = "#4A90D9") + 
  theme_minimal() + labs(x = "Stage", y = "No. of Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

# of cause of death
cause <- mpdsr %>% group_by(`final classification`) %>% tally()
cause <- na.omit(cause)

cause <- cause %>% 
  filter(`final classification` != "concidental cuase") %>%
  mutate(`final classification` = case_when(
    `final classification` == "Abortive pregnacy outcome" ~ "Abortive pregnacy outcome", 
    `final classification` == "HDP" ~ "Hypertensive", 
    `final classification` == "Non obestratrics complication" ~ "Non-obstetric complication", 
    `final classification` == "Obstetric haemorrhage"  ~ "Obstetric haemorrhage", 
    `final classification` == "Other Obestratrics  Complication"  ~ "Other obstetric complication", 
    `final classification` == "Pregnacy related infection"  ~ "Pregnacy related infection", 
    `final classification` == "Unticipated complication of management"  ~ "Management complication",
    `final classification` == "Unknow /undetermined"  ~ "Unknown"
  ))

ggplot(cause, aes(x = reorder(`final classification`, n), y = n)) + 
  geom_bar(stat = "identity", fill = "#4A90D9") + 
  theme_minimal() + labs(x = "Cause of Death", y = "No. of Deaths") + 
  theme(text = element_text(face = "bold", size = 13)) + 
  coord_flip()

## Scatterplot of age vs gravidity/parity
gravid <- mpdsr %>% group_by(AgeAtDeathinyears, Gravidity) %>% tally()
parity <- mpdsr %>% group_by(AgeAtDeathinyears, Parity) %>% tally()

p1 <- 
ggplot(gravid, aes(x = AgeAtDeathinyears, y = Gravidity, alpha = n)) + 
  geom_tile(fill = "#2B6CB0") +
  theme_minimal() + labs(x = "Age of Mother", y = "Gravidity") + 
  theme(text = element_text(face = "bold", size = 13),
        legend.position = "top")

p2 <- 
ggplot(parity, aes(x = AgeAtDeathinyears, y = Parity, alpha = n)) + 
  geom_tile(fill = "#2B6CB0") + 
  theme_minimal() + labs(x = "Age of Mother", y = "Parity") + 
  theme(text = element_text(face = "bold", size = 13),
        legend.position = "top")

ggpubr::ggarrange(p1, p2, ncol = 1)

### EXPLORE DIFFERENCES IN THE BABIES WHICH DID NOT SURVIVE 
# Extract my deaths with birth outcomes 
stillbirth <- mpdsr %>% filter(IfDeliveredOutcome %in% c("Stillbirth", "Still birth", 2))
livebirth <- mpdsr %>% filter(IfDeliveredOutcome %in% c("Live birth", "livebirth", 1))

# Transform to binary outcomes 
stillbirth$IfDeliveredOutcome <- 1
livebirth$IfDeliveredOutcome <- 0

# Merge these back together to form one dataset and identify predictors 
shap <- bind_rows(
  livebirth  %>% mutate(outcome = 0),
  stillbirth %>% mutate(outcome = 1)
) %>%
  mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("livebirth", "stillbirth")))

shap_clean <- shap %>% 
  select(outcome,
         AgeAtDeathinyears, Residenceofdeceased, LevelofEducation,
         Gravidity, Parity, IfYesNumberofANCvisits, DO1hemorrhage, DO2Obstructedlabor,
         Direct_causes, IND2Malaria, IND3HIV, IND4TB, IND5others,
         DELAY_ONE, DELAY_TWO, DELAY_THREE) %>%
  mutate(
    LevelofEducation = as.numeric(case_when(
      LevelofEducation == "3 Elementery"        ~ "3",
      LevelofEducation == "Illiterate"          ~ "0",
      LevelofEducation == "No formal education" ~ "1",
      LevelofEducation == "Not Know"            ~ NA_character_,
      TRUE                                      ~ LevelofEducation
    )),
    LevelofEducation       = replace_na(LevelofEducation, 0),
    IfYesNumberofANCvisits = replace_na(IfYesNumberofANCvisits, 0),
    Direct_causes          = replace_na(Direct_causes, 0),
    Gravidity              = replace_na(Gravidity, 0),
    Parity                 = replace_na(Parity, 0),
    IDs = if_else(
      if_any(c(IND2Malaria, IND3HIV, IND4TB, IND5others), ~ tolower(.x) == "yes"),
      1, 0
    ),
    delay = if_else(
      if_any(c(DELAY_ONE, DELAY_TWO, DELAY_THREE), ~ !is.na(.x) & .x > 0),
      1, 0
    ),
    DO1hemorrhage      = if_else(tolower(DO1hemorrhage)      == "yes", 1, 0, missing = 0),
    DO2Obstructedlabor = if_else(tolower(DO2Obstructedlabor) == "yes", 1, 0, missing = 0)
  ) %>%
  select(-c(DELAY_ONE, DELAY_TWO, DELAY_THREE,
            IND2Malaria, IND3HIV, IND4TB, IND5others)) %>%
  rename(mothers_age    = AgeAtDeathinyears,
         residence      = Residenceofdeceased,
         education      = LevelofEducation,
         anc_number     = IfYesNumberofANCvisits,
         haemorrhage    = DO1hemorrhage,
         obstructed     = DO2Obstructedlabor,
         cause_of_death = Direct_causes)

# See how gravidity/partity vs age differed among babies which died 
p1 <- ggplot(shap_clean, aes(x = mothers_age, y = Gravidity)) + 
  geom_bin_2d() +
  scale_fill_gradient(name = "n", low = "#c6dbef", high = "#2B6CB0") +
  theme_minimal() + labs(x = "Age of Mother", y = "Gravidity") + 
  theme(text = element_text(face = "bold", size = 13), legend.position = "top")

p2 <- ggplot(shap_clean, aes(x = mothers_age, y = Parity)) + 
  geom_bin_2d() +
  scale_fill_gradient(name = "n", low = "#c6dbef", high = "#2B6CB0") +
  theme_minimal() + labs(x = "Age of Mother", y = "Parity") + 
  theme(text = element_text(face = "bold", size = 13), legend.position = "top")

p3 <- ggpubr::ggarrange(p1, p2, ncol = 1)

# Histogram of mothers age
p4 <- ggplot(shap_clean, aes(x = mothers_age)) + 
  geom_bar(fill = "#4A90D9") +
  theme_minimal() + labs(x = "Age of Mother", y = "No. of Deaths") + 
  theme(text = element_text(face = "bold", size = 13))

ggpubr::ggarrange(p3, p4, ncol = 2)

# Define a testing and training set 
set.seed(42)
split <- initial_split(shap_clean, prop = 0.8, strata = outcome)
train_data <- training(split)
test_data <- testing(split)

# Set up a 10 fold cross-validation 
cv_folds <- vfold_cv(train_data, v = 10, strata = outcome)

# Recipe (preprocessing) 
xgb_recipe <- recipe(outcome ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%       # handle unseen factor levels
  step_dummy(all_nominal_predictors()) %>%        # encode any remaining categoricals
  step_zv(all_predictors())                       # remove zero-variance columns

# Model spec with class weighting 
# scale_pos_weight = n_livebirth / n_stillbirth to handle imbalance
weight_ratio <- nrow(livebirth) / nrow(stillbirth)

xgb_spec <- boost_tree(
  trees          = 500,
  tree_depth     = 3,
  learn_rate     = 0.05,
  loss_reduction = 0.01,
  min_n          = 10
) %>%
  set_engine("xgboost", scale_pos_weight = 0.5) %>%
  set_mode("classification")

# Specify the workflow 
xgb_workflow <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

# Cross-validated performance 
cv_results <- fit_resamples(
  xgb_workflow,
  resamples = cv_folds,
  metrics   = metric_set(roc_auc, average_precision, sensitivity, specificity),
  control   = control_resamples(save_pred = TRUE)
)

collect_metrics(cv_results)

# Fit final model on full training set 
final_fit <- fit(xgb_workflow, data = train_data)

# Test set evaluation
test_preds <- augment(final_fit, test_data)
roc_auc(test_preds, truth = outcome, .pred_stillbirth)
conf_mat(test_preds, truth = outcome, estimate = .pred_class)

# Extract fitted xgboost model and prepped training matrix SHAP values
xgb_model  <- extract_fit_engine(final_fit)
train_baked <- xgb_recipe %>%
  prep() %>%
  bake(new_data = train_data) %>%
  select(-outcome) %>%
  as.matrix()

shp <- shapviz(xgb_model, X_pred = train_baked)

# SHAP plots 
# Feature label lookup — defined once, reused across all plots
feature_labels <- c(
  "mothers_age"     = "Mother's age",
  "education"       = "Education",
  "Gravidity"       = "Gravidity",
  "Parity"          = "Parity",
  "anc_number"      = "ANC visits",
  "haemorrhage"     = "Haemorrhage",
  "obstructed"      = "Obstructed labour",
  "cause_of_death"  = "Cause of death",
  "IDs"             = "Infectious disease",
  "delay"           = "Care-seeking delay",
  "residence_Urban" = "Urban residence"
)

# Rename shapviz object once for dependence plots
shp_renamed <- shp
colnames(shp_renamed$X) <- recode(colnames(shp_renamed$X), !!!feature_labels)
colnames(shp_renamed$S) <- recode(colnames(shp_renamed$S), !!!feature_labels)

# Bar plot
p <- sv_importance(shp, kind = "bar")
p$layers[[1]]$aes_params$fill <- "#4A90D9"
p$data$feature <- recode(p$data$feature, !!!feature_labels)
p + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "Mean |SHAP value|", y = NULL)

# Beeswarm
b <- sv_importance(shp, kind = "beeswarm")
b$data$feature <- recode(b$data$feature, !!!feature_labels)
b + theme_minimal() +
  theme(text = element_text(size = 13, face = "bold")) +
  labs(x = "SHAP value", y = NULL)

# Dependence plots (all predictors)
sv_dependence(shp_renamed, v = unname(feature_labels)) &
  theme_minimal() &
  theme(text = element_text(size = 11, face = "bold"))

# To look at 1v1 comparisons 
# In one direction
# sv_dependence(shp_renamed, v = "Mother's age", color_var = "Gravidity") +
#   theme_minimal() +
#   theme(text = element_text(size = 13, face = "bold")) +
#   labs(x = "Mother's age", y = "SHAP value", colour = "Gravidity")
# And in the other direction 
# sv_dependence(shp_renamed, v = "Gravidity", color_var = "Mother's age") +
#   theme_minimal() +
#   theme(text = element_text(size = 13, face = "bold")) +
#   labs(x = "Gravidity", y = "SHAP value", colour = "Mother's age")

# Waterfall for first stillbirth in test set
stillbirth_idx <- which(test_data$outcome == "stillbirth")[1]
sv_waterfall(shp, row_id = stillbirth_idx)

