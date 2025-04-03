# Indlæser og loader pakker
library(dplyr)

# Indlæs det gemte objekt fra .rds-filen
merged_unique <- readRDS("merged_unique.rds")

# Ændrer true/false til 1/0 så vi kan bruge det i churn analysen
merged_unique <- merged_unique |> 
  mutate(across(where(is.logical), ~ as.integer(.)))
