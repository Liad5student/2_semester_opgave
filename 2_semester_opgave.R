# Indlæser nødvendige pakker med pacman (installerer automatisk hvis mangler)
pacman::p_load(
  dplyr,     # til datamanipulation
  tidyr,     # til fx split af kolonner
  stringr    # til teksthåndtering
)

# Indlæser alle nødvendige datasæt
meetings <- readRDS("data/meetings.rds")
events <- readRDS("data/events.rds")
event_participants <- readRDS("data/event_participants.rds")
company_contacts <- readRDS("data/company_contacts.rds")
all_contact <- readRDS("data/all_contact.rds")
all_companies <- readRDS("data/all_companies.rds")
old_projects <- readRDS("data/old_projects.rds")

# ------------------------------------------------------------------------------
# JOIN af tabeller
# ------------------------------------------------------------------------------

# Starter med at merge 'all_companies' og 'company_contacts' på 'CompanyId'
# Fjerner derefter dubletter fra join (kolonner med .x og .y)
merged_df <- all_companies |> 
  left_join(company_contacts, by = "CompanyId") |> 
  select(-ends_with(".y")) |> 
  select(-ends_with(".x"))

# Merger videre med kontaktdata fra 'all_contact' (via contactId)
merged_df <- merged_df |> 
  left_join(all_contact, by = "contactId") |> 
  select(-ends_with(".y")) |> 
  select(-ends_with(".x"))

# Fjerner dubletter i 'meetings' og beholder kun første registrering per virksomhed
meetings_unique <- meetings |> 
  group_by(CompanyId) |> 
  summarise(across(everything(), first))

# Merger mødedata på 'CompanyId'
merged_df <- merged_df |> 
  left_join(meetings_unique, by = "CompanyId") |> 
  select(-ends_with(".y")) |> 
  select(-ends_with(".x"))

# Omdøber kolonnen med CVR-nummer for konsistens
merged_df <- rename(merged_df, c("Cvr" = "z_companies_1_CVR-nummer_1"))

# Fjerner dubletter i 'events' og merger på 'Cvr'
events_unique <- events |> 
  group_by(Cvr)  |> 
  summarise(across(everything(), first))

merged_df <- merged_df |>
  left_join(events_unique, by = "Cvr") |>
  select(-ends_with(".y")) |>
  select(-ends_with(".x"))

# Fjerner dubletter i 'event_participants' og merger på 'Cvr'
event_participants_unique <- event_participants |>
  group_by(Cvr) |>
  summarise(across(everything(), first))

merged_df <- merged_df |>
  left_join(event_participants_unique, by = "Cvr") |>
  select(-ends_with(".y")) |>
  select(-ends_with(".x"))

# Viser overblik over datasættet og datatyper
glimpse(merged_df)

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
# ------------------------------------------------------------------------------

# Fjerner variabler som ikke vurderes relevante for churn-analyse
merged_unique <- merged_unique |>
  select(-TitleChanged, -LocationChanged, -CreatedBy, -Firstname,
         -UserRole, -Initials, -ContactLastUpdated)

# Erstatter NA-værdier i event-relaterede kolonner med "Ingen event"
merged_unique <- merged_unique |>
  mutate(across(
    c(MeetingLength, EventExternalId, EventPublicId, Description, 
      LocationId, MaxParticipants, EventLength, EventId),
    ~ if_else(is.na(.), "Ingen event", as.character(.))
  ))

# Erstatter NA i antal ansatte med "Ukendt"
merged_unique <- merged_unique |>
  mutate(Employees = if_else(is.na(Employees), "Ukendt", as.character(Employees)))

# Erstatter NA i NACECode med "Ukendt"
merged_unique <- merged_unique |>
  mutate(NACECode = if_else(is.na(NACECode), "Ukendt", as.character(NACECode)))

# Splitter NACECode i to: kode og branche – og håndterer "Ukendt" særskilt
merged_unique <- merged_unique |>
  mutate(
    Nacecode = if_else(NACECode == "Ukendt", "Ukendt", str_extract(NACECode, "^[0-9]+")),
    Nacebranche = if_else(NACECode == "Ukendt", "Ukendt", str_remove(NACECode, "^[0-9]+\\s*"))
  )

# Fjerner den oprindelige NACECode-kolonne, da vi har splittet den op
merged_unique <- merged_unique |>
  select(-NACECode)

# Tjekker hvor der stadig er NA-værdier tilbage i datasættet
colSums(is.na(merged_unique))

# Fjerner rækker med NA-værdier (kan også overvejes at håndteres individuelt)
merged_unique <- na.omit(merged_unique)

# Gemmer det rensede og unikke datasæt til senere brug
saveRDS(merged_unique, "merged_unique.rds")