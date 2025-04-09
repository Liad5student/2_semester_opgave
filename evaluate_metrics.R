# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
    dplyr,     # til datamanipulation
    tidyr,     # til fx split af kolonner
    stringr    # til teksthåndtering
)

# ------------------------------------------------------------------------------
# 8. Evaluate metrics
# ------------------------------------------------------------------------------
readRDS("data/modelling.rds")








saveRDS(evaluate_metrics, "data/evaluate_metrics.rds")