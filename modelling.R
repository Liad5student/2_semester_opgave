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

# ------------------------------------------------------------------------------
# 7. Modelling
# ------------------------------------------------------------------------------

preprocessing <- readRDS("data/preprocessing.rds")

# Fra BJarne - skal rettes til:

# ------------------------------------------------------------------
# Modeldefinitioner
# ------------------------------------------------------------------

# 3.1. Lineær model
linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# 3.2. Lasso model
lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# 3.3. Ridge model
ridge_model <- linear_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# 3.4. Elastic Net model
elastic_net_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# 3.5. Random Forest model med ranger
rf_ranger_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") 

# 3.6. XGBoost-model med mtry
xgb_model <- boost_tree(mtry = tune(), trees = 1000, tree_depth = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# ------------------------------------------------------------------
# Workflow for modeller (uden XGBoost)
# ------------------------------------------------------------------
# Vi bruger igen workflow set, som kan inkludere flere modeller og 
# flere recipes.
wf_set <- workflow_set(
  preproc = list(scaled = recipe_scaled, reduceret = recipe_reduceret),
  models = list(
    linear      = linear_model, 
    lasso       = lasso_model, 
    ridge       = ridge_model, 
    elastic_net = elastic_net_model, 
    rf          = rf_ranger_model,
    xgbst       = xgb_model
  ),
  cross = TRUE # Betyder at hver model anvendes på hver recipe,
  # der bliver 6*2=12 modeller.
)

# Valg betrikker
vff_metrics <- metric_set(rmse, mae, rsq)

# ------------------------------------------------------------------
# Tuning af workflow_set 
# ------------------------------------------------------------------

# Så bliver der givet gas:
plan(multisession, workers = parallel::detectCores() - 1)
strt.time <- Sys.time()

fit_workflows <- wf_set %>%  
  workflow_map(
    seed      = 42, # For reproducerbarhed, da nogle af modellerne 
    # bruger fx tilfældige variabler i hvert split, og seed giver
    # mulighed for at reproducere resultaterne
    grid      = 10,       # 10 grid-værdier til hver tuning.
    resamples = vff_boost,
    metrics = vff_metrics
  )

print(Sys.time() - strt.time)
# Der lettes på gashåndtaget
plan(sequential)







# ------------------------------------------------------------------
# End
# ------------------------------------------------------------------

saveRDS(modelling, "data/modelling.rds")