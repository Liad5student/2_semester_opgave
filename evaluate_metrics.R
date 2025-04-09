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

# En vurdering af hvor godt modellerne med forskellige tuning parameters
# klarer sig.

fit_workflows |> 
  autoplot(type = "wflow_id")

collect_metrics(fit_workflows)
# rank_results(fit_workflows, rank_metric = "rmse", select_best = TRUE)

# Funktionen collect_metrics() bruges til at hente og formatere resultaterne 
# fra et sæt af workflows, der er blevet fittet og evalueret. Når du har 
# udført modeltræning og tuning vha workflows, kan du bruge denne funktion 
# til at opsummere præstationsmetrikkerne for de forskellige modeller, 
# der er blevet fittet i workflow-sættet.

# Vi sorterer modellerne bedste pba rmse, og tager den øverst (den bedste)
# og pull hiver den bedste værdi ud.
best_wf_id <- fit_workflows %>% 
  rank_results(
    rank_metric = "rmse",
    select_best = TRUE
  ) %>% 
  dplyr::slice(1) %>% 
  pull(wflow_id)

# wf_best <- extract_workflow(fit_workflows, id = best_wf_id)


# ------------------------------------------------------------------
# Extract the best workflow, træning på alle træningsdata, og test på testdata
# ------------------------------------------------------------------

# Udtræk tuningresultaterne for den bedste workflow-ID
best_results <- fit_workflows %>%
  extract_workflow_set_result(best_wf_id)

# Vælg de bedste parametre baseret på RMSE
best_params <- select_best(best_results, metric = "rmse")

# Udtræk selve workflowet (ikke tuningresultaterne)
best_workflow <- fit_workflows %>%
  extract_workflow(id = best_wf_id)

# Afslut workflowet med de bedste parametre
final_workflow <- finalize_workflow(best_workflow, best_params)


# final_workflow: er det workflow, der indeholder den bedste model 
# og de bedste parametre, som er blevet identificeret gennem tuning 
# og evaluering. Denne funktion anvendes til at udføre den endelige 
# fitting af modellen på hele træningsdatasættet og evaluere den på 
# testdatasættet. Det simulerer processen, hvor man efter at have 
# bestemt den bedste model, fitter den endeligt og derefter vurderer 
# dens præstation på testdata. split-argumentet bruges til at
# identificere, hvad der er træning, og hvad der er test data.

wf_best_final_fit <- final_workflow %>% 
  last_fit(
    split = vff_split
  )

wf_best_final_fit


# ------------------------------------------------------------------
# Visualiseringer
# ------------------------------------------------------------------

# Sammenligner vores Y med vores Yhat. Data skulle gerne ligge på
# den stiplede linje.
collect_predictions(wf_best_final_fit) |> 
  ggplot(aes(tilskuere, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.8, color = "midnightblue") +
  coord_fixed()

# Hvor vigtige er variablerne:
extract_workflow(wf_best_final_fit) %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "col")

# Testdatametrikker:
wf_best_final_fit %>% 
  collect_metrics()

# ------------------------------------------------------------------
# Endelige model
# ------------------------------------------------------------------

# Nu laver vi den endelige model, estimeret på alle data, med 
# de fundne tuningparmetre:
final_model <- fit(final_workflow, vffXXXX)
final_model

# Her er en ny observation:)
new_vff <- tribble(~sæson, ~runde, ~hold, ~akk_forskel, ~sum_af_tre, 
                   ~akk_point_lag, ~akk_egnemål_lag, ~rang_lag, ~udehold_rang_lag, 
                   ~tid_kat, ~tilskuere_sidste_møde, ~temperatur, ~vind, 
                   ~nedbør_seneste_7_timer, ~ferie,
                   "2023/2024", 22, "VFF-AGF", -7, 6, 21, 31, 8, 3, 
                   "hverdag_tidligt", 4094., 19.7, 1, 0, "1")   

# Som vi predikter værdien af:)
predict(final_model, new_data = new_vff)






evaluate_metrics <-

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(evaluate_metrics, "data/evaluate_metrics.rds")