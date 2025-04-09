# Workflow

# 1. Load data             # Læs filer fra fx CSV, Excel, database, API
# 2. Merge datasets        # Join flere datakilder (demografi, køb, logs...)
# 3. Clean data            # Ryd op: navne, NA, duplicates, outliers
# 4. Feature Engineering   # Skab nye variable, fx "dage siden sidste køb"
# 5. EDA                   # Forstå churn, fordelinger, sammenhænge
# 6. Preprocessing         # Split, recipe, dummy, scale, håndtér ubalance
# 7. Modelling             # Fit/tune modeller (logistic, RF, XGBoost...)
# 8. Evaluate metrics      # ROC, confusion matrix, AUC, fortolkning

# 9. Main (Quarto)

# ------------------------------------------------------------------------------
# Pacman
# ------------------------------------------------------------------------------

# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
  dplyr,         # til datamanipulation
  tidyr,         # til fx split af kolonner
  stringr,       # til teksthåndtering
  tidyverse,     # samlet pakke til dataanalyse (inkl. ggplot2, tibble, mm.)
  skimr,         # oversigtsstatistik
  readxl,        # læsning af Excel-filer
  hms,           # håndtering af tid
  ggpubr,        # ggplot med publikationstema
  ggfortify,     # autoplot til modeller
  GGally,        # udvidelse af ggplot, fx ggpairs
  gridExtra,     # arrangering af plots
  hrbrthemes,    # moderne ggplot-temaer
  table1,        # flot tabel 1 output
  tidymodels,    # samlet ML-framework
  themis,        # håndtering af ubalancerede data
  broom,         # konvertering af modeller til tidy-format
  caret,         # klassisk ML-framework
  discrim,       # diskriminantanalyse
  glmnet,        # regulerede regressioner
  kknn,          # k-NN klassifikation/regression
  naivebayes,    # naive bayes modeller
  kernlab,       # kernelbaserede metoder inkl. SVM
  xgboost,       # gradient boosting
  ranger,        # hurtig random forest
  gbm,           # gradient boosting machines
  randomForest,  # klassisk random forest
  rpart,         # beslutningstræer
  leaps,         # modelselektionsmetoder
  car,           # diagnostik for lineære modeller
  PerformanceAnalytics, # performance-metrics (især finans)
  rvest,         # web scraping
  httr,          # HTTP requests
  jsonlite,      # arbejde med JSON
  rjson,         # alternativ JSON-pakke
  rlist,         # listehåndtering
  Rcrawler,      # web crawling
  rmarkdown,     # rapportgenerering
  knitr,         # knit Rmd-dokumenter
  future         # parallelisering og asynkron kodning
  
)

# Husk setWD()

# Henter og opdaterer alle .rds-filer fra alle branches
source("load_all_data.R")

# Indlæser .rds-filen fra branch 2.: Merge datasets
merge_datasets <- readRDS("data/merge_datasets.rds")

# ------------------------------------------------------------------------------
# 3. Clean data
# ------------------------------------------------------------------------------

merge_datasets

# Klippet fra 'joins_modeller' og indsat her i clean data delen (Martin)

# Fokus: Unikke virksomheder via PNumber (produktionsenhedsnummer)
# Det giver os 2966 unikke observationer
merged_df <- merged_df |>
  select(-z_companies_1_Firmanavn_1, -z_contacts_1_Email_1)  # Fjerner anonymiserede data

# Omdøber kolonner for at forenkle og standardisere navne
colnames(merged_df) <- c(
  "BusinessCouncilMember",
  "CompanyDateStamp",
  "CompanyId",
  "CompanyType",
  "CVR",
  "Employees",
  "PostalCode",
  "CompanyTypeName",
  "PNumber", "Country",
  "NACECode",
  "CompanyStatus",
  "AdvertisingProtected",
  "ContactId",
  "CompanyOwnerId",
  "ContactLastUpdated",
  "TitleChanged",
  "LocationChanged",
  "CreatedBy",
  "MeetingLength",
  "Firstname",
  "UserRole",
  "Initials",
  "EventExternalId",
  "EventPublicId",
  "Description",
  "LocationId",
  "MaxParticipants",
  "EventLength",
  "EventId"
)

# Beholder kun én række pr. produktionsenhed (PNumber)
merged_unique <- merged_df |>
  distinct(PNumber, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# Dataklargøring og datarensning starter her
# Klippet fra 'joins_modeller' og indsat her i clean data delen (Martin)
# ------------------------------------------------------------------------------

# Fjerner variabler som ikke vurderes relevante for churn-analyse
merge_datasets <- merge_datasets |>
  select(-TitleChanged, -LocationChanged, -CreatedBy, -Firstname,
         -UserRole, -Initials, -ContactLastUpdated)

# Erstatter NA-værdier i event-relaterede kolonner med "Ingen event"
merge_datasets <- merge_datasets |>
  mutate(across(
    c(MeetingLength, EventExternalId, EventPublicId, Description, 
      LocationId, MaxParticipants, EventLength, EventId),
    ~ if_else(is.na(.), "Ingen event", as.character(.))
  ))

# Erstatter NA i antal ansatte med "Ukendt"
merge_datasets <- merge_datasets |>
  mutate(Employees = if_else(is.na(Employees), "Ukendt", as.character(Employees)))

# Erstatter NA i NACECode med "Ukendt"
merge_datasets <- merge_datasets |>
  mutate(NACECode = if_else(is.na(NACECode), "Ukendt", as.character(NACECode)))

# Splitter NACECode i to: kode og branche – og håndterer "Ukendt" særskilt
merge_datasets <- merge_datasets |>
  mutate(
    Nacecode = if_else(NACECode == "Ukendt", "Ukendt", str_extract(NACECode, "^[0-9]+")),
    Nacebranche = if_else(NACECode == "Ukendt", "Ukendt", str_remove(NACECode, "^[0-9]+\\s*"))
  )

# Fjerner den oprindelige NACECode-kolonne, da vi har splittet den op
merge_datasets <- merge_datasets |>
  select(-NACECode)

# Tjekker hvor der stadig er NA-værdier tilbage i datasættet
colSums(is.na(merge_datasets))

# Fjerner rækker med NA-værdier (kan også overvejes at håndteres individuelt)
merge_datasets <- na.omit(merge_datasets)

merge_datasets <- merge_datasets |> 
  distinct(PNumber, .keep_all = TRUE)


# Maria C. Koder: ---------------------------------------------------------


glimpse(merge_datasets)

# Tjekker for NA værdier 

na_count <- merge_datasets %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count")

# Det er ikke noget problem med NA værdier i dataene, 
# så vi kan gå videre til næste trin

# Tjekker for duplicates

duplicates_count <- merge_datasets %>%
  group_by(CompanyId) %>%
  summarise(duplicates = n()) %>%
  filter(duplicates > 1)

# Fjerne duplicates

clean_data <- merge_datasets %>%
  distinct(CompanyId, .keep_all = TRUE)

# Antag at dit datasæt hedder df
names(clean_data) <- names(clean_data) %>%
  # Fjern indledende ID'er (f.eks. "26481_1")
  str_remove("^[0-9]+_1*\\s*") %>%
  # Erstat mellemrum, /, - og lign. med _
  str_replace_all("[ /\\-]+", "_") %>%
  # Fjern dobbelte underscores
  str_replace_all("_+", "_") %>%
  # Fjern evt. afsluttende underscore
  str_remove("_$") %>%
  # Trim whitespace og lav til lowercase
  str_trim()


# Tjek de nye navne
names(clean_data)

# Fjern ID’er og target (som ikke skal bruges som features)

clean_data <- clean_data %>%
  select(-CompanyId, -ContactId, -CompanyOwnerId, -CVR, -PNumber,-EventId,
         -EventExternalId,-EventPublicId,-LocationId, -Tekstfelt, -CompanyType)

# Fjern "Tom", "Ukendt", "Ingen event" og lav til NA 

clean_data <- clean_data %>%
  mutate(across(where(is.character), ~na_if(.x, "Ukendt")))

# Fjerner NA værdier 

clean_data <- clean_data %>%
  drop_na()

# Konverter 'Employees' til numerisk

clean_data$Employees = as.numeric(clean_data$Employees)

glimpse(clean_data)

# Vi skal konverterer flere kolonner til den rigtige format fordi der 
# er flere der er chr i stedet for numerisk mens vi har stadig "ingen event" i variabler
# der skal være numerisk såsom meetinglength, max participants, EventLength 

# Hvad skal vi laver med denne variabler? Jeg har prøvet at fjerne alle der har Tom
# ingen event og ukendt og det kun er 60 observationer tilbage :(


# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(merge_datasets, "data/clean_data.rds")