# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 3. Clean data
# ------------------------------------------------------------------------------
loadRDS("data/merge_datasets.rds")






saveRDS(clean_data, "data/clean_data.rds")
