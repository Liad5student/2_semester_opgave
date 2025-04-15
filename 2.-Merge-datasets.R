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
  stringr,        # til teksthåndtering
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

# Husk at sætte working directory

# Helper-funktion: Hent og læs en .rds-fil fra en anden branch

source("load_all_data.R")

# ------------------------------------------------------------------------------
# 1. Merge datasets
# ------------------------------------------------------------------------------

# Indlæser alle nødvendige datasæt
meetings <- readRDS("data/meetings.rds")
events <- readRDS("data/events.rds")
event_participants <- readRDS("data/event_participants.rds")
company_contacts <- readRDS("data/company_contacts.rds")
all_contact <- readRDS("data/all_contact.rds")
all_companies <- readRDS("data/all_companies.rds")
old_projects <- readRDS("data/old_projects.rds")

# ------------------------------------------------------------------------------
# 2. JOIN af tabeller
# ------------------------------------------------------------------------------


# Fjerner dubletter og beholder første forekomst i hvert dataset
meetings_unique <- meetings |>
  group_by(CompanyId) |> 
  summarise(across(everything(), first))     # Første møde pr. virksomhed

events_unique <- events |>
  group_by(Cvr) |> 
  summarise(across(everything(), first))     # Første event pr. virksomhed

event_participants_unique <- event_participants |>
  group_by(Cvr) |> 
  summarise(across(everything(), first))     # Første deltagerinfo pr. virksomhed

# Samler alle datasæt via left_joins og fjerner dublet-kolonner
merged_df <- all_companies |> 
  left_join(company_contacts, by = "CompanyId") |>     # Join kontaktpersoner
  left_join(all_contact, by = "contactId") |>          # Join kontaktinfo
  left_join(meetings_unique, by = "CompanyId") |>      # Join mødedata
  rename(Cvr = "z_companies_1_CVR-nummer_1") |>        # Standardiser CVR-navn
  left_join(events_unique, by = "Cvr") |>              # Join eventinfo
  left_join(event_participants_unique, by = "Cvr") |>  # Join deltagerinfo
  select(-ends_with(".y"), -ends_with(".x"))           # Fjerner dublet-kolonner


# ------------------------------------------------------------------------------
# 3. Dataklargøring og datarensning starter her
# ------------------------------------------------------------------------------

# Viser overblik over datasættet og datatyper
glimpse(merged_df)

# Fokus: Unikke virksomheder via PNumber (produktionsenhedsnummer)
# Det giver os 2966 unikke observationer
merged_df <- merged_df |> 
  select(-z_companies_1_Firmanavn_1, -z_contacts_1_Email_1)  
# Fjerner anonymiserede data

# Omdøber kolonner for at forenkle og standardisere navne
colnames(merged_df) <- c(
  "BusinessCouncilMember", "CompanyDateStamp", "CompanyId", "CompanyType",
  "CVR", "Employees", "PostalCode", "CompanyTypeName", "PNumber", "Country",
  "NACECode", "CompanyStatus", "AdvertisingProtected", "ContactId",
  "CompanyOwnerId", "ContactLastUpdated", "TitleChanged", "LocationChanged",
  "CreatedBy", "MeetingLength", "Firstname", "UserRole", "Initials",
  "EventExternalId", "EventPublicId", "Description", "LocationId",
  "MaxParticipants", "EventLength", "EventId"
)

# Beholder unikke virksomheder, fjerner irrelevante kolonner,
# og udfylder NA i eventdata
merged_unique <- merged_df |>
  distinct(PNumber, .keep_all = TRUE) |>  # Beholder én række pr. PNumber
  select(-TitleChanged, -LocationChanged, -CreatedBy, -Firstname,  
         # Fjerner irrelevante variabler
         -UserRole, -Initials, -ContactLastUpdated) |>
  mutate(across(  # Erstatter NA i event-kolonner med "Ingen event"
    c(MeetingLength, EventExternalId, EventPublicId, Description, 
      LocationId, MaxParticipants, EventLength, EventId),
    ~ if_else(is.na(.), "Ingen event", as.character(.))
  ))

merged_unique <- merged_unique |> 
  mutate(
    MeetingLength = ifelse(MeetingLength == "Ingen event", "0 mins", MeetingLength),
    MeetingLength = as.numeric(str_remove(MeetingLength, " mins"))
  )
  

# Erstatter NA, splitter NACECode, fjerner original kolonne,
# og fjerner rækker med NA
merged_unique <- merged_unique |>
  mutate(
    Employees   = if_else(is.na(Employees), "Ukendt", as.character(Employees)),
    # NA -> "Ukendt"
    NACECode    = if_else(is.na(NACECode),  "Ukendt", as.character(NACECode)),
    # NA -> "Ukendt"
    Nacecode    = if_else(NACECode == "Ukendt", "Ukendt", 
                          str_extract(NACECode, "^[0-9]+")),                 
    # Hent kode
    Nacebranche = if_else(NACECode == "Ukendt", "Ukendt", 
                          str_remove(NACECode, "^[0-9]+\\s*"))               
    # Hent branche
  ) |>
  select(-NACECode) |>  # Fjerner original NACECode-kolonne
  na.omit()             # Fjerner rækker med NA-værdier


# Tjekker hvor der stadig er NA-værdier tilbage i datasættet
colSums(is.na(merged_unique))


# Gemmer det rensede og unikke datasæt til senere brug
saveRDS(merged_unique, "merged_unique.rds")

# ------------------------------------------------------------------------------
# 4. Hvis vi vil have old_projects merged med i den samlede merge
# ------------------------------------------------------------------------------

# Omdøb SMVContactId til ContactId
old_projects <- old_projects |>
  rename(ContactId = SMVContactId)

# Gem kolonnenavne fra old_projects (ekskl. ContactId)
old_project_cols <- setdiff(names(old_projects), "ContactId")
cols_to_fill <- setdiff(old_project_cols, c("Id", "SMVCompanyId", "SharedWith"))

# Merge med merged_unique og fjern unødvendige kolonner
merged_unique_old_projects <- merged_unique |>
  left_join(old_projects, by = "ContactId") |> # Merger på ContactId
  select(-Id, -SMVCompanyId, -SharedWith) |> # Fjerner udnødvendige kolonner
mutate(across(all_of(cols_to_fill), 
              ~ if_else(is.na(.), "Tom", as.character(.)))) |>
  #Erstater NA'er med "Tom" i de relevante kolonner
  distinct(PNumber, .keep_all = TRUE) # Behold én række per Pnumber


# Tjek for NA-værdier
colSums(is.na(merged_unique_old_projects))

# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------

saveRDS(merged_unique_old_projects, "data/merge_datasets.rds")

