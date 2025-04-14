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

# Husk at sætte working directory

# Helper-funktion: Henter de opdaterede .rds-filer fra de andre branches

source("load_all_data.R")

# ------------------------------------------------------------------------------
# 4. Feature Engineering
# ------------------------------------------------------------------------------

# Load data frame fra branch 3. Clean data
clean_data <- readRDS("data/clean_data.rds")

glimpse(clean_data) #Bruger glimpse til at få et hurtigt overblik over data

# Feature engineering

# CompanyDateStamp – opret en ny feature: Virksomhedens alder

feature_engineering <- clean_data %>%
  mutate(
    company_age_years = as.numeric(difftime(Sys.Date(), as.Date(CompanyDateStamp), units = "days")) %/% 365
  )


# Antal events pr. år 

events_per_year <- feature_engineering %>%
  filter(EventLength != "Ingen event") %>%                     # Kun deltagelser
  mutate(event_year = year(CompanyDateStamp)) %>%              # Træk årstal ud
  group_by(CompanyId, event_year) %>%                          
  summarise(events = n(), .groups = "drop")                    # Tæl events

# Employees – antal ansatte (efter konvertering til numerisk)

feature_engineering <- feature_engineering %>%
  mutate(
    Employees = as.numeric(str_replace_all(Employees, "\\.", "")), # Fjern punktummer
    Employees = as.numeric(str_replace_all(Employees, "\\s+", "")) # Fjern mellemrum
  )

# CompanyTypeName – A/S, ApS, osv.

feature_engineering <- feature_engineering %>%
  mutate(
    CompanyTypeName = str_replace_all(CompanyTypeName, "A/S", "Aktieselskab"),
    CompanyTypeName = str_replace_all(CompanyTypeName, "ApS", "Anpartsselskab"),
    CompanyTypeName = str_replace_all(CompanyTypeName, "IVS", "Iværksætterselskab"),
    CompanyTypeName = str_replace_all(CompanyTypeName, "P/S", "Partnerselskab"),
    CompanyTypeName = str_replace_all(CompanyTypeName, "K/S", "Kommanditselskab")
  )

# Nacebranche – brancher laver kategorier 
# 
# feature_engineering <- feature_engineering %>%
#   mutate(
#     Nacebranche = str_replace_all(Nacebranche, "Sundhed", "Sundhed og socialvæsen"),
#     Nacebranche = str_replace_all(Nacebranche, "Bygge og anlæg", "Byggeri"),
#     Nacebranche = str_replace_all(Nacebranche, "Transport", "Transport og logistik"),
#     Nacebranche = str_replace_all(Nacebranche, "Detailhandel", "Detailhandel"),
#     Nacebranche = str_replace_all(Nacebranche, "IT", "IT og teknologi")
#   )
# 
# feature_engineering$Nacebranche # Tjekker fordelingen af Nacebranche

# Virksomhedens besøg – opret en ny feature: Har haft kontakt

feature_engineering <- feature_engineering %>%
  mutate(
    har_haft_kontakt = if_else(
      Virksomhedsbesøg != "Tom" | 
        Telefonkontakt != "Tom" | 
        Konsulent_Navn != "Tom" | 
        Notat != "Tom" | 
        Kontaktdato != "Tom",
      "Ja", "Nej"
    )
  )

# Deltaget i event – opret en ny feature: Har deltaget i event

feature_engineering  <- feature_engineering  %>%
  mutate(deltaget_i_event = if_else(EventLength != "Ingen event", "Ja", "Nej"))

# Gennemsnit af deltagere – opret en ny feature: Gennemsnitligt antal deltagere

feature_engineering  <- feature_engineering %>%
  mutate(MaxParticipants_num = as.numeric(MaxParticipants))

max_participants_stats <- feature_engineering %>%
  filter(!is.na(MaxParticipants_num)) %>%
  group_by(CompanyId) %>%
  summarise(gennemsnit_max_deltagere = mean(MaxParticipants_num))

# Skeber kategorier for at samle alle de variabler med true og false fra 
# områder hvor virksomheder søger hjælp 


feature_engineering <- feature_engineering %>%
  # Sørg for at konvertere kolonnerne til logiske værdier (TRUE/FALSE)
  mutate(across(matches("^\\d+_1"), ~ .x != "FALSE" & .x != "Tom")) %>%
  mutate(
    # Opretter en enkelt variabel, der kategoriserer virksomheden baseret på de 8 områder
    hjælp_kategori = case_when(
      (as.logical(Kundeportefølje) | as.logical(Forretningsmodel) | as.logical(Forretningsidé) | as.logical(Produktportefølje)) ~ "Strategi Udvikling",
      (as.logical(Markedsføring) | as.logical(Branding) | as.logical(Kommunikation_og_PR)) ~ "Marketing og Kommunikation",
      (as.logical(Salg) | as.logical(Eksport) | as.logical(Markedsposition)) ~ "Salg og Eksport",
      (as.logical(Medarbejdere) | as.logical(Netværk) | as.logical(Samarbejdspartnere) | as.logical(Ejer_og_bestyrelse)) ~ "Organisation og Ledelse",
      (as.logical(Økonomistyring) | as.logical(Finansiering) | as.logical(Kapitalfond) | as.logical(Vækstfonden) | as.logical(Innovationsfonden)) ~ "Økonomi og Finansiering",
      (as.logical(Leverance_og_projektstyring) | as.logical(IT_systemer) | as.logical(Faciliteter) | as.logical(Forretningsgange)) ~ "Drift og Systemer",
      (as.logical(Juridiske_forhold) | as.logical(Ejerskifte_og_generationsskifte)) ~ "Jura og Struktur",
      (as.logical(EU_Kontoret_i_DK_Interreg) | as.logical(Erhvervshuset) | as.logical(FN_1) | as.logical(Andre_nationale_ordninger)) ~ "Støtteordninger",
      
      # Adaugă noile categorii
      (as.logical(Uddannelse_kompetenceudvikling) | 
         as.logical(Vidensordninger) | 
         as.logical(IV_Vejledning) | 
         as.logical(Virksomhedsbesøg_Virksomhed_under_3_år) | 
         as.logical(I_Værkstedet) | 
         as.logical(Klippekort_Udleveret) | 
         as.logical(Væksthjul_Screening) | 
         as.logical(Agro_Business_Park) | 
         as.logical(Konsulent_virksomhed_uden_for_Kommunen_DK) | 
         as.logical(Lokal_konsulent_eller_virksomhed) | 
         as.logical(Indenrigsministeriet_The_Trade_Council) | 
         as.logical(Produktudviklin)) ~ "Andre Hjælpeordninger",
      
      TRUE ~ "Ingen specifik hjælp"
    )
  ) %>%
  # Fjern de gamle variabler, som ikke længere er nødvendige
  select(-c(
    Kundeportefølje, Forretningsmodel, Forretningsidé, Produktportefølje,
    Markedsføring, Branding, Kommunikation_og_PR,
    Salg, Eksport, Markedsposition,
    Medarbejdere, Netværk, Samarbejdspartnere, Ejer_og_bestyrelse,
    Økonomistyring, Finansiering, Kapitalfond, Vækstfonden, Innovationsfonden,
    Leverance_og_projektstyring, IT_systemer, Faciliteter, Forretningsgange,
    Juridiske_forhold, Ejerskifte_og_generationsskifte,
    EU_Kontoret_i_DK_Interreg, Erhvervshuset, FN_1, Andre_nationale_ordninger,
    Uddannelse_kompetenceudvikling, Vidensordninger, IV_Vejledning, 
    Virksomhedsbesøg_Virksomhed_under_3_år, I_Værkstedet, 
    Klippekort_Udleveret, Væksthjul_Screening, Agro_Business_Park, 
    Konsulent_virksomhed_uden_for_Kommunen_DK, Lokal_konsulent_eller_virksomhed, 
    Indenrigsministeriet_The_Trade_Council, Produktudviklin
  ))

# Tjek resultatet
glimpse(feature_engineering)


# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------
saveRDS(feature_engineering, "data/feature_engineering.rds")
