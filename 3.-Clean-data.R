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
  stringr,        # til teksthåndtering
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

# Husk setWD()

# Henter og opdaterer alle .rds-filer fra alle branches
source("load_all_data.R")

# Indlæser .rds-filen fra branch 2.: Merge datasets
merge_datasets <- readRDS("data/merge_datasets.rds")

# ------------------------------------------------------------------------------
# 3. Clean data
# ------------------------------------------------------------------------------

merge_datasets

# ------------------------------------------------------------------------------
# Dataklargøring og datarensning starter her
# Klippet fra 'joins_modeller' og indsat her i clean data delen (Martin)
# ------------------------------------------------------------------------------

# Fjerner variabler som ikke vurderes relevante for churn-analyse
merge_datasets <- merge_datasets |>
  select(-TitleChanged, -LocationChanged, -CreatedBy, -Firstname,
         -UserRole, -Initials, -ContactLastUpdated)

# Erstatter NA-værdier i event-relaterede kolonner med "Ingen event"
merge_datasets <- merge_datasets |>
  mutate(across(
    c(MeetingLength, EventExternalId, EventPublicId, Description, 
      LocationId, MaxParticipants, EventLength, EventId),
    ~ if_else(is.na(.), "Ingen event", as.character(.))
  ))

# Erstatter NA i antal ansatte med "Ukendt"
merge_datasets <- merge_datasets |>
  mutate(Employees = if_else(is.na(Employees), "Ukendt", as.character(Employees)))

# Erstatter NA i NACECode med "Ukendt"
merge_datasets <- merge_datasets |>
  mutate(NACECode = if_else(is.na(NACECode), "Ukendt", as.character(NACECode)))

# Splitter NACECode i to: kode og branche – og håndterer "Ukendt" særskilt
merge_datasets <- merge_datasets |>
  mutate(
    Nacecode = if_else(NACECode == "Ukendt", "Ukendt", str_extract(NACECode, "^[0-9]+")),
    Nacebranche = if_else(NACECode == "Ukendt", "Ukendt", str_remove(NACECode, "^[0-9]+\\s*"))
  )

# Fjerner den oprindelige NACECode-kolonne, da vi har splittet den op
merge_datasets <- merge_datasets |>
  select(-NACECode)

# Tjekker hvor der stadig er NA-værdier tilbage i datasættet
colSums(is.na(merge_datasets))

# Fjerner rækker med NA-værdier (kan også overvejes at håndteres individuelt)
merge_datasets <- na.omit(merge_datasets)

merge_datasets_unique <- merge_datasets |> 
  distinct(PNumber, .keep_all = TRUE)
names(merge_datasets_unique)
length(unique(merge_datasets$PNumber))



# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(merge_dataset_unique, "data/clean_data.rds")
