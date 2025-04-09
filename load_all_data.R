# ------------------------------------------------------------------------------
# Indlæs alle .rds-filer fra tidligere branches via helper-funktionen
# ------------------------------------------------------------------------------

source("load_helpers.R")

loaded_data         <- get_rds_from_branch("1.-Load-data", "loaded_data.rds")
merged_data         <- get_rds_from_branch("2.-Merge-datasets", "merge_datasets.rds")
cleaned_data        <- get_rds_from_branch("3.-Clean-data", "clean_data.rds")
features            <- get_rds_from_branch("4.-Feature-Engineering", "feature_engineering.rds")
eda_output          <- get_rds_from_branch("5.-EDA", "eda_summary.rds")
preprocessed_data   <- get_rds_from_branch("6.-Preprocessing", "preprocessed_data.rds")
model_results       <- get_rds_from_branch("7.-Modelling", "model_results.rds")
metrics             <- get_rds_from_branch("8.-Evaluate-metrics", "evaluation_metrics.rds")