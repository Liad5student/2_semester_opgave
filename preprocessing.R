# Workflow

# 1. Load data             # Læs filer fra fx CSV, Excel, database, API
# 2. Merge datasets        # Join flere datakilder (demografi, køb, logs...)
# 3. Clean data            # Ryd op: navne, NA, duplicates, outliers
# 4. Feature Engineering   # Skab nye variable, fx "dage siden sidste køb"
# 5. EDA                   # Forstå churn, fordelinger, sammenhænge
# 6. Preprocessing         # Split, recipe, dummy, scale, håndtér ubalance
# 7. Modelling             # Fit/tune modeller (logistic, RF, XGBoost...)
# 8. Evaluate metrics      # ROC, confusion matrix, AUC, fortolkning

# 9. Main (Quarto)

# ------------------------------------------------------------------------------
# Pacman
# ------------------------------------------------------------------------------

# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
  dplyr,         # til datamanipulation
  tidyr,         # til fx split af kolonner
  stringr,       # til teksthåndtering
  tidyverse,     # samlet pakke til dataanalyse (inkl. ggplot2, tibble, mm.)
  skimr,         # oversigtsstatistik
  readxl,        # læsning af Excel-filer
  hms,           # håndtering af tid
  ggpubr,        # ggplot med publikationstema
  ggfortify,     # autoplot til modeller
  GGally,        # udvidelse af ggplot, fx ggpairs
  gridExtra,     # arrangering af plots
  hrbrthemes,    # moderne ggplot-temaer
  table1,        # flot tabel 1 output
  tidymodels,    # samlet ML-framework
  themis,        # håndtering af ubalancerede data
  broom,         # konvertering af modeller til tidy-format
  caret,         # klassisk ML-framework
  discrim,       # diskriminantanalyse
  glmnet,        # regulerede regressioner
  kknn,          # k-NN klassifikation/regression
  naivebayes,    # naive bayes modeller
  kernlab,       # kernelbaserede metoder inkl. SVM
  xgboost,       # gradient boosting
  ranger,        # hurtig random forest
  gbm,           # gradient boosting machines
  randomForest,  # klassisk random forest
  rpart,         # beslutningstræer
  leaps,         # modelselektionsmetoder
  car,           # diagnostik for lineære modeller
  PerformanceAnalytics, # performance-metrics (især finans)
  rvest,         # web scraping
  httr,          # HTTP requests
  jsonlite,      # arbejde med JSON
  rjson,         # alternativ JSON-pakke
  rlist,         # listehåndtering
  Rcrawler,      # web crawling
  rmarkdown,     # rapportgenerering
  knitr,         # knit Rmd-dokumenter
  future         # parallelisering og asynkron kodning
  
)

# Helper-funktion: Henter og opdaterer .rds-filer fra de andre branches
source("load_all_data.R")

# Henter .rds-fil fra branchet EDA
eda <- readRDS("data/eda.R")

feature_engineering <- readRDS("data/feature_engineering.rds")

# ------------------------------------------------------------------------------
# 6. Preprocessing
# ------------------------------------------------------------------------------

set.seed(2025)

churn_split <- initial_split(feature_engineering, prop = 0.8, strata = churn)
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

churn_folds <- vfold_cv(churn_train, v = 10, strata = churn)

churn_recipe <- 
  recipe(churn ~ ., data = churn_train) |>
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_downsample(churn)  # Brug evt. step_smote(churn) hvis ekstrem ubalance

# ------------------------------------------------------------------------------
# 7. Modelling
# ------------------------------------------------------------------------------

# Model specs
rf_spec <- rand_forest(mtry = tune(), min_n = tune()) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

xgb_spec <- boost_tree(trees = tune(), mtry = tune(), learn_rate = tune()) |>
  set_engine("xgboost") |>
  set_mode("classification")

log_reg_spec <- logistic_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("classification")

knn_spec <- nearest_neighbor(neighbors = tune(), weight_func = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

nb_spec <- naive_Bayes(smoothness = tune(), Laplace = tune()) |>
  set_engine("naivebayes") |>
  set_mode("classification")

svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")

# Samlet workflow set
churn_workflow_set <- workflow_set(
  preproc = list(churn_recipe = churn_recipe),
  models = list(
    rf = rf_spec,
    xgboost = xgb_spec,
    logistic = log_reg_spec,
    knn = knn_spec,
    naive_bayes = nb_spec,
    svm_rbf = svm_spec
  )
)

# ------------------------------------------------------------------------------
# 8. Evaluate metrics
# ------------------------------------------------------------------------------

churn_metrics <- metric_set(accuracy, roc_auc, f_meas, sens, spec)

grid_ctrl <- control_grid(
  verbose = TRUE,
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

plan(multisession)
strt.time <- Sys.time()

churn_results <- churn_workflow_set |> 
  workflow_map(
    resamples = churn_folds,
    grid = 5,
    metrics = churn_metrics,
    control = grid_ctrl,
    seed = 2025
  )

Sys.time() - strt.time
plan(sequential)

# Sammenlign resultater
churn_results |> 
  rank_results(select_best = TRUE) |> 
  select(wflow_id, .metric, mean) |> 
  pivot_wider(names_from = .metric, values_from = mean) |> 
  arrange(-f_meas)

autoplot(churn_results, select_best = TRUE)


# -------------------------------------------------------------------------
# Plot modeller efter deres performance
# -------------------------------------------------------------------------


# Din tibble, hvis ikke du allerede har den i en variabel:
metrics_df <- tibble::tibble(
  wflow_id = c("churn_recipe_rf", "churn_recipe_xgboost", "churn_recipe_svm_rbf",
               "churn_recipe_logistic", "churn_recipe_knn", "churn_recipe_naive_bayes"),
  accuracy = c(0.825, 0.827, 0.845, 0.807, 0.743, 0.710),
  f_meas   = c(0.724, 0.723, 0.708, 0.697, 0.592, 0.287),
  roc_auc  = c(0.877, 0.881, 0.439, 0.858, 0.778, 0.855),
  sens     = c(0.777, 0.766, 0.643, 0.755, 0.634, 0.272),
  spec     = c(0.845, 0.852, 0.929, 0.828, 0.788, 0.893)
)

# Gør labels lidt pænere
metrics_focus <- metrics_long %>%
  filter(metric %in% c("accuracy", "f_meas", "roc_auc")) %>%
  mutate(metric = case_when(
    metric == "accuracy" ~ "Accuracy",
    metric == "f_meas" ~ "F1-score",
    metric == "roc_auc" ~ "ROC AUC",
    TRUE ~ metric
  ))

# Nr. 1: BarPlot med værdier for denne 3 metrikker

ggplot(metrics_focus, aes(x = metric, y = value, fill = metric)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(value, 3)), vjust = -0.3, size = 3.5) +
  facet_wrap(~ wflow_id) +
  ylim(0, 1.05) +
  labs(
    title = "Model performance (Accuracy, F1 og ROC AUC)",
    x = NULL,
    y = "Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set3")


# Nr. 2: Linje plot 

ggplot(metrics_focus, aes(x = wflow_id, y = value, color = metric, group = metric)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(value, 3)), vjust = -0.7, size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Sammenligning på tværs af modeller",
    x = "Model",
    y = "Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Nr. 3: Heatmap pr. model og metrik 

ggplot(metrics_focus, aes(x = metric, y = wflow_id, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Performance heatmap pr. model og metrik",
    x = "Metric",
    y = "Model"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# -------------------------------------------------------------------------
# Plot for xgboost og Random forest med de vigtigste variabler 
# -------------------------------------------------------------------------

# Hent tuning-resultater for rf og xgboost
rf_result <- churn_results %>% extract_workflow_set_result("churn_recipe_rf")
xgb_result <- churn_results %>% extract_workflow_set_result("churn_recipe_xgboost")

# Hent workflow (før det er fit)
rf_workflow <- churn_results %>% extract_workflow("churn_recipe_rf")
xgb_workflow <- churn_results %>% extract_workflow("churn_recipe_xgboost")

# Vælg bedste parametre og fit modellen
best_rf <- rf_workflow %>%
  finalize_workflow(select_best(rf_result, metric = "f_meas")) %>%
  fit(data = churn_train)

best_xgb <- xgb_workflow %>%
  finalize_workflow(select_best(xgb_result, metric = "f_meas")) %>%
  fit(data = churn_train)

# Feature importance
vip_rf <- vi(extract_fit_parsnip(best_rf)) %>% mutate(model = "Random Forest")
vip_xgb <- vi(extract_fit_parsnip(best_xgb)) %>% mutate(model = "XGBoost")

# Kombinér og vis kun top 10 vigtigste variabler pr. model
vip_combined <- bind_rows(vip_rf, vip_xgb) %>%
  group_by(model) %>%
  slice_max(order_by = Importance, n = 10) %>%
  ungroup() %>%
  mutate(Variable = str_wrap(Variable, width = 25))

# Plot med labels og tekstrotation optimeret
ggplot(vip_combined, aes(x = reorder(Variable, Importance), y = Importance, fill = model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Importance, 2)), hjust = -0.1, size = 3) +
  facet_wrap(~ model, scales = "free") +
  coord_flip() +
  labs(
    title = "Top 10 vigtigste variabler pr. model",
    x = "Variabel",
    y = "Vigtighed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) #ektra space til labels

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(preprocessing, "data/preprocessing.rds")

