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

# Henter .rds-filen fra branchet preprocessing
preprocessing <- readRDS("data/preprocessing.rds")

# ------------------------------------------------------------------------------
# 7. Modelling
# ------------------------------------------------------------------------------

# Fra BJarne - skal rettes til:

logistic_spec <- logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("classification")

rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")

xgb_spec <- boost_tree(mtry = tune(), trees = 1000, learn_rate = tune(), tree_depth = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

churn_models <- workflow_set(
  preproc = list(churn = churn_recipe),
  models = list(logistic = logistic_spec, rf = rf_spec, xgb = xgb_spec)
)

churn_metrics <- metric_set(accuracy, roc_auc, f_meas, sens, spec)

plan(multisession)
churn_results <- churn_models |> 
  workflow_map(
    seed = 2024,
    resamples = churn_folds,
    grid = 7,
    control = control_grid(
      verbose = TRUE,
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    ),
    metrics = churn_metrics
  )
plan(sequential)







# ------------------------------------------------------------------
# End
# ------------------------------------------------------------------

saveRDS(modelling, "data/modelling.rds")