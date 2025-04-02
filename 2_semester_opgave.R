#JOIN tabeller 

# Bruger dplyr til at merge dataframes
library(dplyr)
library(tidyr)


#indlæser data
meetings <- readRDS("")
events <- readRDS("")
event_participants <- readRDS("")
company_contacts <- readRDS("")
all_contact <- readRDS("")
all_companies <- readRDS("")


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


#gemme filen som rds
saveRDS(merged_df, "merged_df.rds") # gemmer filen som rds fil 


# fjerne na værdier 
 
merged_df <- drop_na(merged_df)# fjerner NA værdier

#Fjerne variabler der var med anonyme data
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

merged_df <- drop_na(merged_df)

