#loading required packages
used.packages <- c("stringr","sf","ggplot2","dplyr","osmdata","forcats",
                   "scales","lubridate","tidyr","viridis","igraph","rnaturalearthdata")

for (pkg in used.packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#Load the data set (file needs to be in the directory)
nigeria <- read.csv("data/1997-01-01-2025-01-01-Nigeria.csv")

#Data Cleansing
##labels and levels for factors
source.scale.levels <- c("International", "National", "National-International",
                         "National-Regional", "Regional", "Regional-International",
                         "Subnational", "Subnational-International", "Subnational-National",
                         "Subnational-Regional", "Local partner-International",
                         "Local partner-Other", "New media", "New media-International",
                         "New media-National", "New media-Regional", "New media-Subnational",
                         "Other", "Other-International", "Other-National", "Other-New media",
                         "Other-Regional", "Other-Subnational", "")

source.scale.labels <- source.scale.levels
source.scale.labels[length(source.scale.labels)] <- "NULL"

event.type.levels <- c("Strategic developments", "Riots", "Violence against civilians",
                       "Battles", "Explosions/Remote violence", "Protests", "")

event.type.labels <- event.type.levels
event.type.labels[length(event.type.labels)] <- "NULL"

sub.event.type.levels <- c("", "Abduction/forced disappearance", "Agreement", "Air/drone strike",
                           "Armed clash", "Arrests", "Attack", "Change to group/activity",
                           "Disrupted weapons use", "Excessive force against protesters",
                           "Government regains territory", "Grenade", "Headquarters or base established",
                           "Looting/property destruction", "Mob violence", "Non-state actor overtakes territory",
                           "Non-violent transfer of territory", "Other", "Peaceful protest", 
                           "Protest with intervention", "Remote explosive/landmine/IED", "Sexual violence",
                           "Shelling/artillery/missile attack", "Suicide bomb", "Violent demonstration")

sub.event.type.labels <- sub.event.type.levels
sub.event.type.labels[1] <- "NULL"

##recoding variables
nigeria.restructured <- nigeria %>% 
  mutate(event_id_cnty = event_id_cnty %>%
           str_trim() %>% 
           str_remove_all("[[:alpha:][:space:][:punct:]]") %>% 
           as.integer(),
         year = year %>% 
           as.integer(),
         event_date = mdy(event_date),
         event_type = event_type %>% 
           str_remove_all("[[:digit:]]") %>% 
           factor(levels = event.type.levels,
                  labels = event.type.labels),
         sub_event_type = sub_event_type %>% 
           str_remove_all("\"") %>% 
           factor(levels = sub.event.type.levels,
                  labels = sub.event.type.labels),
         civilian_targeting = civilian_targeting %>%
           str_remove_all("[[:punct:]]") %>%
           factor(levels = c("", "Civilian targeting"),
                  labels = c("no targeting", "civilian targeting")),
         location = location %>% 
           str_remove_all("\""),
         source_scale = source_scale %>% 
           str_remove_all("\"") %>% 
           factor(levels = source.scale.levels,
                  labels = source.scale.labels),
         fatalities = fatalities %>% 
           str_remove_all("[[:alpha:][:space:][:punct:]]") %>%
           as.integer(),
         population_best = population_best %>% 
           as.integer() %>%
           replace_na(0),
         actor1 = actor1 %>% 
           str_trim() %>% 
           str_to_lower()
  ) %>%
  filter(
    between(year, 1997, 2025) | is.na(year),
    between(latitude, -90, 90) | is.na(latitude),
    between(longitude, -180, 180) | is.na(longitude)
  ) %>%
  select(-c(notes, region, timestamp, time_precision, geo_precision))


#removing rows with NAs in event_id_cnty
nigeria.restructured <- nigeria.restructured %>%
  mutate(
    actor1 = replace_na(actor1, "Unknown"),
    event_id_cnty = as.character(event_id_cnty),
    event_type = as.character(event_type)
  ) %>%
  drop_na(event_id_cnty, event_type, actor1)


##pivot wider to get conflict tuples and not just one actor
nigeria.wide <- nigeria.restructured %>%
  group_by(event_id_cnty, event_type) %>%
  mutate(row = dense_rank(actor1)) %>%
  pivot_wider(
    names_from = row,
    values_from = actor1,
    names_prefix = "actor",
    values_fn = first 
  ) %>%
  ungroup()

nigeria.wide <- nigeria.wide %>%
  mutate(across(starts_with("actor"), as.character),  
         across(starts_with("actor"), ~ replace_na(., "Unknown")))  


##put different actors into different actor groups, code convention by ACLED
nigeria.merged <- nigeria.wide %>%
  group_by(across(-starts_with("actor"))) %>%
  summarise(
    actor1 = first(na.omit(actor1)),
    actor2 = first(na.omit(actor2)),
    .groups = "drop"
  ) %>% mutate(
    actor_group1 = case_when(
      str_detect(actor1, "military forces of nigeria|police forces of nigeria|government of nigeria|
                 nigeria customs service|NSCDC: Nigeria Security and Civil Defence Corps|
                 national drug law enforcement agency|nigeria immigration service|multinational joint task force|
                 military forces of chad|military forces of niger|military forces of cameroon") ~ "State Forces",
      str_detect(actor1, "boko haram|islamic state west africa province|ansaru|islamic state sahel province") ~ "Rebel Groups",
      str_detect(actor1, "unidentified armed group|indigenous peoples of biafra|OPC: Oodua Peoples Congress|
                 ijaw freedom fighters|yoruba nation agitators|NDLM: Niger Delta Liberation Movement") ~ "Political Militias",
      str_detect(actor1, "communal militia|ethnic militia|fulani ethnic militia|tiv ethnic militia|
                 jukun ethnic militia|irigwe ethnic militia|alago ethnic militia|gbagyi ethnic militia|
                 yoruba ethnic militia|ambu ethnic militia|eki ethnic militia") ~ "Identity Militias",
      str_detect(actor1, "rioters") ~ "Rioters",
      str_detect(actor1, "protesters|nigeria labour congress") ~ "Protesters",
      str_detect(actor1, "civilians") ~ "Civilians",
      str_detect(actor1, "private security forces|african nature investors|KSVG: Katsina State Vigilance Group|
                 ebube agu corps|amotekun corps|zamfara state community protection guards") ~ "External/Other Forces",
      TRUE ~ "External/Other Forces"),
    actor_group2 = case_when(
      str_detect(actor2, "military forces of nigeria|police forces of nigeria|government of nigeria|
                 nigeria customs service|NSCDC: Nigeria Security and Civil Defence Corps|
                 national drug law enforcement agency|nigeria immigration service|multinational joint task force|
                 military forces of chad|military forces of niger|military forces of cameroon") ~ "State Forces",
      str_detect(actor2, "boko haram|islamic state west africa province|ansaru|islamic state sahel province") ~ "Rebel Groups",
      str_detect(actor2, "unidentified armed group|indigenous peoples of biafra|OPC: Oodua Peoples Congress|
                 ijaw freedom fighters|yoruba nation agitators|NDLM: Niger Delta Liberation Movement") ~ "Political Militias",
      str_detect(actor2, "communal militia|ethnic militia|fulani ethnic militia|tiv ethnic militia|jukun ethnic militia|
                 irigwe ethnic militia|alago ethnic militia|gbagyi ethnic militia|yoruba ethnic militia|
                 ambu ethnic militia|eki ethnic militia") ~ "Identity Militias",
      str_detect(actor2, "rioters") ~ "Rioters",
      str_detect(actor2, "protesters|nigeria labour congress") ~ "Protesters",
      str_detect(actor2, "civilians") ~ "Civilians",
      str_detect(actor2, "private security forces|african nature investors|KSVG: Katsina State Vigilance Group|
                 ebube agu corps|amotekun corps|zamfara state community protection guards") ~ "External/Other Forces",
      TRUE ~ "External/Other Forces"
      ))
