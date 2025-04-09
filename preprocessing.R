# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 6. Preprocessing
# ------------------------------------------------------------------------------
readRDS("data/eda.rds")








saveRDS(preprocessing, "data/preprocessing.rds")