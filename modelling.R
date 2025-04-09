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


# ------------------------------------------------------------------
# 1. Pakker, dataindlæsning og forberedelse
# ------------------------------------------------------------------

pacman::p_load(
  tidyverse, tidymodels, themis, table1, ggpubr, broom, ggfortify,
  GGally, PerformanceAnalytics, car, caret, skimr, discrim, glmnet,
  kknn, naivebayes, kernlab, xgboost, gridExtra, rpart, future,
  ranger, rmarkdown, rvest, httr, jsonlite, rlist, rjson,
  Rcrawler, hrbrthemes, knitr, hms, leaps, readxl, gbm,
  randomForest, stringr
)

vffXXXX <- readRDS("data/vff_hjemmekampe.RDS") |> 
  select(-c(akk_egnemål, akk_point, hold01, ugedag, sommerferie, vinterferie, 
            efterårsferie, år, helligdag_dummy)) |> 
  mutate_if(is.character, as.factor)

# Jeg har fjernet enkelte af variablerne for at gøre det nemmere, og for at fjerne
# tydelig perfekt multikollinearitet.

# summary(vffXXXX)
# str(vffXXXX)


# Split data i træning og test
set.seed(2025)
vff_split <- initial_split(vffXXXX, prop = 0.8, strata = tilskuere)
vff_train <- training(vff_split)
vff_test  <- testing(vff_split)

# Resampling-metode er bootstrapping
vff_boost <- bootstraps(vff_train, times = 25, strata = tilskuere)  
# Bootstrapping er en statistisk metode, der involverer gentagen 
# sampling fra et (trænings)datasæt med tilbagelægning. Herved får
# man altid det samme antal observationer til træning af tuningparametrene, 
# men de forskellige stikprøver kan indeholde forskellige observationer og
# forskelligt antal observationer, hvilket giver mulighed for at evaluere 
# modellen på forskellige datasæt og dermed forbedre valideringen.

# ------------------------------------------------------------------
# 2. Opskrifter (recipes)
# ------------------------------------------------------------------


recipe_scaled <- recipe(tilskuere ~ ., data = vff_train) |> 
  # Definerer opskriften, hvor tilskuere er outcome, og alle 
  # andre variabler (.) er prædiktorer.
  step_nzv(all_numeric(), -all_outcomes()) |> 
  # Fjerner numeriske variabler med næsten ingen varians (near-zero 
  # variance). Disse variabler bidrager sjældent til modellen, da de 
  # næsten ikke varierer.
  step_corr(all_numeric_predictors(), threshold = 0.85) |> 
  # Fjerner stærkt korrelerede numeriske prædiktorer 
  # (korrelationskoefficient > 0.85). Dette reducerer redundans i data
  # og hjælper med at undgå multikollinearitet.
  step_novel(all_nominal_predictors()) |> 
  # Håndterer nye niveauer i kategoriske variabler, der ikke var til 
  # stede i træningsdata. Dette sikrer, at modellen kan håndtere ukendte 
  # kategorier i fremtidige data.
  step_normalize(all_numeric(), -all_outcomes()) |>  
  # Normaliserer numeriske variabler ved at skalere dem til en 
  # standardnormal fordeling (gennemsnit = 0, standardafvigelse = 1).
  # Dette er vigtigt for modeller, der er følsomme over for skala, som 
  # fx lineær regression 
  step_YeoJohnson(all_numeric(), -all_outcomes()) |>  
  # Anvender en Yeo-Johnson-transformation på numeriske variabler for at 
  # gøre dem mere normale i deres fordeling. Dette kan forbedre modellens 
  # præstation, især hvis data er skæve.
  step_other(all_nominal(), threshold = 0.05) |>   
  # Kombinerer sjældne kategorier i kategoriske variabler (frekvens < 5%) 
  # til en fælles kategori ("other"). Dette reducerer støj fra sjældne 
  # niveauer.
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 
# Konverterer kategoriske variabler til one-hot encoded dummy-variabler. 
# Hver kategori bliver repræsenteret som en binær variabel (0 eller 1).

# Der er lavet to alternative recipes, som kan laves endnu mere 
# avancerede ved fx at tilføje tuning til fx ovenstående thresholds.
recipe_reduceret <- recipe(tilskuere ~ ., data = vff_train) |> 
  step_nzv(all_numeric(), -all_outcomes()) |> 
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)  


# ------------------------------------------------------------------
# 3. Modeldefinitioner
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
# 4. Workflow for modeller (uden XGBoost)
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
# 5. Tuning af workflow_set 
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
# 7. Evaluate model fits
# ------------------------------------------------------------------

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
# 8. Extract the best workflow, træning på alle træningsdata, og test på testdata
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
# 9. Visualiseringer
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
# 10. Endelige model
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









saveRDS(modelling, "data/modelling.rds")