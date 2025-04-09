# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 4. Feature Engineering
# ------------------------------------------------------------------------------
readRDS("data/clean_data.rds")








saveRDS(feature_enginering, "data/feature_engineering.rds")