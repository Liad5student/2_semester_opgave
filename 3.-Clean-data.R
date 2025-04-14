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

# Tjekker datastrukturen
glimpse(merge_datasets)


# Tjekker for NA værdier 
na_count <- merge_datasets |> 
  summarise(across(everything(), ~ sum(is.na(.)))) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "na_count")

# Det er ikke noget problem med NA værdier i dataene, 
# så vi kan gå videre til næste trin

# Fjernelse af irrelevante tegn og tal fra variabelnavne
names(merge_datasets) <- names(merge_datasets) |>
  str_remove("^[0-9]+_1*\\s*") |>
  str_replace_all("[ /\\-]+", "_") |>
  str_replace_all("_+", "_") |>
  str_remove("_$") |>
  str_trim()

# Tjeker navne 
names(merge_datasets)

# Fjern ID’er og target (som ikke skal bruges som features)

clean_data <- merge_datasets |>
  dplyr::select(-ContactId, -CompanyOwnerId, -EventId,
                -EventExternalId, -EventPublicId, -LocationId, 
                -Tekstfelt, -CompanyType)

# Fjern "Ukendt" og lav til NA for variabel Employees

clean_data <- clean_data |>
  dplyr::mutate(across(where(is.character), ~ na_if(.x, "Ukendt"))) |>
  tidyr::drop_na()

# Konverter variabler til de rigtige datatyper

clean_data <- clean_data |>
  mutate(across(c(CVR, Nacecode, PostalCode, PNumber, 
                  MaxParticipants, EventLength, Employees),
                ~ ifelse(.x %in% c(" ", "", "Tom", "Ukendt", "Ingen event"), 
                         NA, .x))) |>
  mutate(across(c(CVR, Nacecode, PostalCode, PNumber, 
                  MaxParticipants, EventLength, Employees), as.numeric))

# Konverterer til tid format 
CompanyDateStamp = as.Date(clean_data$CompanyDateStamp, format = "%Y-%m-%d")
Kontaktdato  = as.Date(clean_data$Kontaktdato, format = "%Y-%m-%d")

# Tjekker datasæt struktur
glimpse(clean_data)

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(clean_data, "data/clean_data.rds")
