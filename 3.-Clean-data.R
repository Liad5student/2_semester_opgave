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

# Husk setWD()

# Henter og opdaterer alle .rds-filer fra alle branches
source("load_all_data.R")

# Indlæser .rds-filen fra branch 2.: Merge datasets
merge_datasets <- readRDS("data/merge_datasets.rds")

# ------------------------------------------------------------------------------
# 3. Clean data
# ------------------------------------------------------------------------------

# Giver overblik over datastrukturen og variabeltyper
glimpse(merge_datasets)

# Tjekker for manglende værdier (NA) i alle variabler
na_count <- merge_datasets |> 
  summarise(across(everything(), ~ sum(is.na(.)))) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "na_count")

# Rydder op i variabelnavne: fjerner tal, specialtegn og whitespace
names(merge_datasets) <- names(merge_datasets) |>
  str_remove("^[0-9]+_1*\\s*") |>      # Fjerner startende tal/1-taller
  str_replace_all("[ /\\-]+", "_") |> # Erstatter mellemrum og specialtegn med _
  str_replace_all("_+", "_") |>       # Fjerner dobbelte underscores
  str_remove("_$") |>                 # Fjerner underscore i slutningen
  str_trim()                          # Trim whitespace

# Udskriver de rensede kolonnenavne
print(names(merge_datasets))

# Fjerner ID’er og variabler som ikke skal bruges til analyse
clean_data <- merge_datasets |> 
  dplyr::select(-ContactId, -CompanyOwnerId, -EventExternalId,
                -EventPublicId, -LocationId, -Tekstfelt, -CompanyType)

# Erstatter irrelevante tekstværdier med NA, konverterer til numerisk,
# og udfylder NA med "Ukendt" (tekst) eller 0 (tal)
clean_data <- clean_data |>
  mutate(
    across(
      c(CVR, Nacecode, PostalCode, PNumber, MaxParticipants, EventLength, Employees),
      ~ as.numeric(ifelse(.x %in% c(" ", "", "Tom", "Ukendt", "Ingen event"), NA, .x))
    ),
    across(where(is.character), ~ replace_na(.x, "Ukendt")),  # Tekst: NA → "Ukendt"
    across(where(is.numeric), ~ replace_na(.x, 0))             # Tal: NA → 0
  )

# Konverterer datoer til korrekt datoformat
CompanyDateStamp <- as.Date(clean_data$CompanyDateStamp, format = "%Y-%m-%d")
Kontaktdato      <- as.Date(clean_data$Kontaktdato, format = "%Y-%m-%d")

# Viser datastruktur efter rensning
glimpse(clean_data)


# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(clean_data, "data/clean_data.rds")

