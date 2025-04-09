# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------
#readRDS("data/load_data.rds")







saveRDS(load_data, "data/load_data.rds")