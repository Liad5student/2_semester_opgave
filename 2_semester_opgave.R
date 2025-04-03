#JOIN tabeller 

# Bruger dplyr til at merge dataframes
pacman::p_load(
  dplyr,
  tidyr,
  stringr)

#indlæser data
meetings <- readRDS("data/meetings.rds")
events <- readRDS("data/events.rds")
event_participants <- readRDS("data/event_participants.rds")
company_contacts <- readRDS("data/company_contacts.rds")
all_contact <- readRDS("data/all_contact.rds")
all_companies <- readRDS("data/all_companies.rds")
old_projects <- readRDS("data/old_projects.rds")

#merger dataframes
merged_df <- all_companies %>%
  left_join(company_contacts, by = "CompanyId") %>%
  select(-ends_with(".y")) %>% # fjerner kolonner med .y
  select(-ends_with(".x"))  #fjerner kolonner med .x


merged_df <- merged_df %>%
  left_join(all_contact, by = "contactId") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) 


meetings_unique <- meetings %>% # bruger det fordi vi har duplicates værdier 
  group_by(CompanyId) %>%
  summarise(across(everything(), first))  # first row of each group

merged_df <- merged_df %>%
  left_join(meetings_unique, by = "CompanyId")%>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) 

merged_df <- rename(merged_df, c("Cvr" = "z_companies_1_CVR-nummer_1"))

events_unique <- events %>% # bruger det fordi vi har duplicates værdier 
  group_by(Cvr) %>%
  summarise(across(everything(), first))  # first row of each group

merged_df <- merged_df %>%
  left_join(events_unique, by = "Cvr") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) 

event_participants_unique <- event_participants %>% # bruger det fordi vi har duplicates værdier 
  group_by(Cvr) %>%
  summarise(across(everything(), first))  # first row of each group

merged_df <- merged_df %>%
  left_join(event_participants_unique, by = "Cvr") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) 

#visualisering af data
glimpse(merged_df) # viser alle kolonner og deres datatype


# Hvis vi fokuserer på PNumber (produktionsenhedsnummer = unikke adresser hvor CVR nr. har aktivitet),
# så undgår vi, at der er mange CVR nr. der går igen i merged_df og kun har de unikke virksomheder, som de har registreret.
# Med den metode har vi 2966 observationer, som er unikke at arbejde videre med.

# Fjerner variabler der var med anonyme data
merged_df <- merged_df %>%
  select (-z_companies_1_Firmanavn_1) 

merged_df <- merged_df %>%
  select (-z_contacts_1_Email_1) 

# Omdøber kolonner 
colnames(merged_df) <- c(
  "BusinessCouncilMember",
  "CompanyDateStamp",
  "CompanyId",
  "CompanyType",
  "CVR",
  "Employees",
  "PostalCode",
  "CompanyTypeName",
  "PNumber",
  "Country",
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

# Gør så ingen CVR nr. går igen vha. unikt p-nummer
merged_unique <- merged_df %>%
  distinct(PNumber, .keep_all = TRUE)

# Indsæt NACECode
# Opdel med branchekode og navn

# Fjerner de variabler med NA-værdier, som ikke er så relevante for om de churner
merged_unique <- merged_unique |> 
  dplyr::select(-TitleChanged, -LocationChanged, -CreatedBy, -Firstname, -UserRole, -Initials, -ContactLastUpdated) |>
# Erstatter NA-værdi med "Ingen event" i kolonnerne angående event
  mutate(across(
    c(MeetingLength, EventExternalId, EventPublicId, Description, 
      LocationId, MaxParticipants, EventLength, EventId),
    ~ if_else(is.na(.), "Ingen event", as.character(.))
  )) |>
# Erstatter NA-værdi med "Ukendt" i Employees kolonnen
  mutate(Employees = if_else(is.na(Employees), "Ukendt", as.character(Employees)))

# Erstatter NA-værdi med "Ukendt" i Employees kolonnen
merged_unique <- merged_unique |> 
mutate(NACECode = if_else(is.na(NACECode), "Ukendt", as.character(NACECode)))





merged_unique <- merged_unique %>%
  mutate(
    Nacecode = if_else(NACECode == "Ukendt", "Ukendt", str_extract(NACECode, "^[0-9]+")),
    Nacebranche = if_else(NACECode == "Ukendt", "Ukendt", str_remove(NACECode, "^[0-9]+\\s*"))
  )



merged_unique <- merged_unique |> 
  dplyr::select(-NACECode)

# Tjekker for NA-værdier
colSums(is.na(merged_unique))

# Fjerner NA-værdier
merged_unique <- na.omit(merged_unique)

saveRDS(merged_unique, "merged_unique.rds")
