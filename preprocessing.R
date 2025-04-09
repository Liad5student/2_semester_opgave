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
# 6. Preprocessing
# ------------------------------------------------------------------------------

eda <- readRDS("data/eda.R")

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
# Recipes
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



# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(preprocessing, "data/preprocessing.rds")