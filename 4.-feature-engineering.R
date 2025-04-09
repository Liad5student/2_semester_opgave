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

# Husk at sætte working directory

# Helper-funktion: Henter de opdaterede .rds-filer fra de andre branches

source("load_all_data.R")

# ------------------------------------------------------------------------------
# 4. Feature Engineering
# ------------------------------------------------------------------------------

# Load data frame fra branch 3. Clean data
clean_data <- readRDS("data/clean_data.rds")


# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------
saveRDS(feature_engineering, "data/feature_engineering.rds")
