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

# Læser .rds ind fra branchet Modelling:
modelling <- readRDS("data/modelling.rds")

# ------------------------------------------------------------------------------
# 8. Evaluate metrics
# ------------------------------------------------------------------------------

# Taget fra Bjarnes fil - skal tilpasses:

# Overblik over bedste modeller
churn_results %>% 
  rank_results(select_best = TRUE) %>%
  select(wflow_id, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  arrange(-f_meas)

# Plot
autoplot(churn_results, select_best = TRUE)

# Udvælg og fit den bedste model
best_result <- churn_results |>
  extract_workflow_set_result("churn_logistic") |>
  select_best(metric = "f_meas")

final_wf <- churn_results |>
  extract_workflow("churn_logistic") |>
  finalize_workflow(best_result)

# Evaluer på testdata
last_fit_result <- final_wf |> 
  last_fit(split = churn_split, metrics = churn_metrics)

last_fit_result |> collect_metrics()

# Confusion matrix
last_fit_result |> 
  collect_predictions() |> 
  conf_mat(truth = churn, estimate = .pred_class)

# Fit hele modellen på alle data
final_model <- fit(final_wf, churn_data)

# Prediktion på ny kunde
new_customer <- tribble(
  ~tenure_months, ~monthly_fee, ~num_logins, ~num_clicks, ~num_support_tickets, ~contract_type, ~region,
  12, 199, 30, 250, 3, "Monthly", "East"
)

predict(final_model, new_data = new_customer, type = "prob")






evaluate_metrics <-

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(evaluate_metrics, "data/evaluate_metrics.rds")