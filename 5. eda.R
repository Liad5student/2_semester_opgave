# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 5. EDA
# ------------------------------------------------------------------------------
readRDS("data/feature_engineering.rds")








saveRDS(eda, "data/eda.rds")