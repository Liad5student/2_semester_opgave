# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 7. Modelling
# ------------------------------------------------------------------------------
readRDS("data/preprocessing.rds")








saveRDS(modelling, "data/modelling.rds")