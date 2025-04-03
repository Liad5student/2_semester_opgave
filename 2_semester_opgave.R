# Indlæser og loader pakker
library(dplyr)

# Indlæs det gemte objekt fra .rds-filen
merged_unique <- readRDS("merged_unique.rds")

# Ændrer variabel navne
merged_unique <- merged_unique |> 
  mutate(across(where(is.logical), ~ as.integer(.)))
