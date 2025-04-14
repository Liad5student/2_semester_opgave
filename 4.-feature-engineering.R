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

# Load data frame fra branch 3. Clean data
clean_data <- readRDS("data/clean_data.rds")

# ------------------------------------------------------------------------------
# 4. Feature Engineering
# ------------------------------------------------------------------------------

glimpse(clean_data) #Bruger glimpse til at få et hurtigt overblik over data

# Opret en ny feature: Medlem i antal år
feature_engineering <- clean_data |>
  mutate(
    medlem_antal_år = round(
      as.numeric(difftime(Sys.Date(), as.Date(CompanyDateStamp), 
                          units = "days")) / 365, 
      0
    )
  )

# Opret en ny feature: Deltaget i antal events

feature_engineering <- feature_engineering |> 
  group_by(PNumber) |> 
  mutate(Antal_events = n_distinct(EventId)) |> 
  ungroup() |> 
  relocate(Antal_events, .after = EventLength)

names(feature_engineering)  
  
  
# Employees – antal ansatte (efter konvertering til numerisk)

feature_engineering <- feature_engineering |>
  mutate(
    Employees = as.numeric(str_replace_all(Employees, "\\.", "")), 
    # Fjern punktummer
    Employees = as.numeric(str_replace_all(Employees, "\\s+", "")) 
    # Fjern mellemrum
  )

# CompanyTypeName – A/S, ApS, osv.

feature_engineering <- feature_engineering |>
  mutate(
CompanyTypeName = str_replace_all(CompanyTypeName, "A/S", "Aktieselskab"),
CompanyTypeName = str_replace_all(CompanyTypeName, "ApS", "Anpartsselskab"),
CompanyTypeName = str_replace_all(CompanyTypeName, "IVS", "Iværksætterselskab"),
CompanyTypeName = str_replace_all(CompanyTypeName, "P/S", "Partnerselskab"),
CompanyTypeName = str_replace_all(CompanyTypeName, "K/S", "Kommanditselskab")
  )

# Læs NACE-lookup og omdøb kolonner
nace_lookup <- read_delim("data/nace_branchenavne.csv", delim = ";") |> 
  select(KODE, TITEL) |> 
  rename(Nace_kort = KODE, Branche_navn = TITEL)

# Lav en ny kolonne med de første to cifre af Nacecode
feature_engineering <- feature_engineering |> 
  mutate(Nace_kort = substr(Nacecode, 1, 2)) |> 
  select(-Nacebranche) |> 
  left_join(nace_lookup, by = "Nace_kort") |> 
  mutate(
    Branche_navn = replace_na(Branche_navn, "Ukendt"),
    Branche_navn = as.factor(Branche_navn)
  ) %>%
  select(-Nacecode, -Nace_kort) |> 
  relocate(Branche_navn, .after = PNumber)

# Virksomhedens besøg – opret en ny feature: Har haft kontakt

feature_engineering <- feature_engineering |>
  mutate(
    har_haft_kontakt = if_else(
      Virksomhedsbesøg != "Tom" | 
        Telefonkontakt != "Tom" | 
        Konsulent_Navn != "Tom" | 
        Notat != "Tom" | 
        Kontaktdato != "Tom",
      "Ja", "Nej")) |>  
  select(-Virksomhedsbesøg, -Telefonkontakt,
               - Konsulent_Navn, -Notat, -Kontaktdato)

# Deltaget i event – opret en ny feature: Har deltaget i event

feature_engineering <- feature_engineering |>
  mutate(
    deltaget_i_event = if_else(
      as.numeric(EventLength) > 0, 
      "Ja", 
      "Nej"
    )
  )




# test <- feature_engineering |> 
#   filter(PNumber == 1022227854)




# Skeber kategorier for at samle alle de variabler med true og false fra 
# områder hvor virksomheder søger hjælp 


feature_engineering <- feature_engineering |>
  # Sørg for at konvertere kolonnerne til logiske værdier (TRUE/FALSE)
  mutate(across(matches("^\\d+_1"), ~ .x != "FALSE" & .x != "Tom")) |>
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
      
      # Tilføjelse af de nye kategorier
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
  ) |>
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

# test2 <- feature_engineering |> 
#   filter(PNumber == 1022227854) |> 
#   pull(hjælp_kategori)

# Beholder kun igangværende virksomheder
feature_engineering <- feature_engineering %>%
  filter(CompanyStatus %in% c("Aktiv", "NORMAL"))
# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------
saveRDS(feature_engineering, "data/feature_engineering.rds")
