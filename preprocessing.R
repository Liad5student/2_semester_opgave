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

churn_split <- initial_split(df_feature, prop = 0.8, strata = churn)
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

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(preprocessing, "data/preprocessing.rds")

