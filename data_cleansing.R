#loading required libraries
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("osmdata")) install.packages("osmdata")
if (!require("forcats")) install.packages("forcats")
if (!require("scales")) install.packages("scales")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")

library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(scales)
library(lubridate)
library(tidyr)

#Load the data set (file needs to be in the directory)
nigeria <- read.csv("1997-01-01-2025-01-01-Nigeria.csv")

#Data Cleansing
source_scale_levels <- c("International", "National", "National-International",
                    "National-Regional", "Regional", "Regional-International",
                    "Subnational", "Subnational-International", "Subnational-National",
                    "Subnational-Regional", "Local partner-International",
                    "Local partner-Other", "New media", "New media-International",
                    "New media-National", "New media-Regional", "New media-Subnational",
                    "Other", "Other-International", "Other-National", "Other-New media",
                    "Other-Regional", "Other-Subnational", "")

source_scale_labels <- source_scale_levels
source_scale_labels[length(source_scale_labels1)] <- "NULL"

event_type_levels <- c("Strategic developments", "Riots", "Violence against civilians",
                 "Battles", "Explosions/Remote violence", "Protests", "")

event_type_labels <- event_type_levels
event_type_labels[length(event_type_labels)] <- "NULL"

sub_event_type_levels <- c("", "Abduction/forced disappearance", "Agreement", "Air/drone strike",
                     "Armed clash", "Arrests", "Attack", "Change to group/activity",
                     "Disrupted weapons use", "Excessive force against protesters",
                     "Government regains territory", "Grenade", "Headquarters or base established",
                     "Looting/property destruction", "Mob violence", "Non-state actor overtakes territory",
                     "Non-violent transfer of territory", "Other", "Peaceful protest", 
                     "Protest with intervention", "Remote explosive/landmine/IED", "Sexual violence",
                     "Shelling/artillery/missile attack", "Suicide bomb", "Violent demonstration")

sub_event_type_labels <- sub_event_type_levels
sub_event_type_labels[1] <- "NULL"

nigeria.clean <- nigeria %>% 
  mutate(event_id_cnty = event_id_cnty %>%
           str_trim() %>% 
           str_remove_all("[[:alpha:][:space:][:punct:]]") %>% 
           as.integer(),
         event_date = mdy(event_date),
         time_precision = factor(time_precision,
                                 levels = c("1", "2", "3"),
                                 labels = c("most precise", "precise", "least precise")),
         event_type = event_type %>% 
           str_remove_all("[[:digit:]]") %>% 
           factor(levels = event_type_levels,
                  labels = event_type_labels),
         sub_event_type = sub_event_type %>% 
           str_remove_all("\"") %>% 
           factor(levels = sub_event_type_levels,
                  labels = sub_event_type_labels),
         civilian_targeting = civilian_targeting %>%
           str_remove_all("[[:punct:]]") %>%
           factor(levels = c("", "Civilian targeting"),
                  labels = c("no targeting", "civilian targeting")),
         region = region %>% 
           str_remove_all("\""),
         location = location %>% 
           str_remove_all("\""),
         geo_precision = geo_precision %>%
           factor(levels = c("1", "2", "3"),
                  labels = c("most precise", "precise", "least precise")),
         source_scale = source_scale %>% 
           str_remove_all("\"") %>% 
           factor(levels = source_scale_levels,
                  labels = source_scale_labels),
         fatalities = fatalities %>% 
           str_remove_all("[[:alpha:][:space:][:punct:]]") %>%
           as.integer(),
         population_best = population_best %>% 
           as.integer() %>%
           replace_na(0),
         actor1 = actor1 %>% 
           str_trim() %>% 
           str_to_lower(),
         actor_group = case_when(
           str_detect(actor1, "military forces of nigeria|police forces of nigeria") ~ "State Security Forces",
           str_detect(actor1, "communal militia|ethnic militia") ~ "Militia",
           str_detect(actor1, "unidentified armed group") ~ "Unidentified Armed Groups",
           str_detect(actor1, "islamic state west africa province|boko haram") ~ "Terrorist Groups",
           str_detect(actor1, "civilians|protesters|rioters") ~ "Civilians/Protesters",
           str_detect(actor1, "confraternity|cult") ~ "Cult Groups",
           TRUE ~ "Other")) %>%
  relocate(actor_group, .after = actor1) %>% 
  filter(
    between(year, 1997, 2025) | is.na(year),
    between(latitude, -90, 90) | is.na(latitude),
    between(longitude, -180, 180) | is.na(longitude)
  ) %>%
  select(-c(notes, region, timestamp))

nigeria.clean.nodupes <- nigeria.clean %>%
  distinct()

nigeria.no.nas  <- nigeria.clean %>%
  filter(if_all(everything(), ~ !is.na(.)))

überprüfen_actor_groups <- nigeria.no.nas %>%
  select(actor_group, actor1) %>% 
  filter(actor_group == "State Security Forces")
unique(überprüfen_actor_groups)

## Attempt pivot

## nigeria.wide <- nigeria.clean %>%
##   group_by(event_id_cnty, event_type) %>%   
##   mutate(row = row_number()) %>%      
##   pivot_wider(names_from = row, values_from = actor1, names_prefix = "actor") %>%
##   ungroup()

nigeria.clean <- nigeria.clean %>%
  mutate(
    actor1 = replace_na(actor1, "Unknown"),
    event_id_cnty = as.character(event_id_cnty),
    event_type = as.character(event_type)
  ) %>%
  drop_na(event_id_cnty, event_type, actor1)

nigeria.wide <- nigeria.clean %>%
  group_by(event_id_cnty, event_type) %>%   
  mutate(row = dense_rank(actor1)) %>%
  pivot_wider(
    names_from = row, 
    values_from = actor1, 
    names_prefix = "actor"
  ) %>%
  ungroup()


###bei event_type, sub_event_type und source scale gibt es keine NAs,
### sondern sind als "" gespeichert, wurden zu "NULL" umgeschrieben
### für bessere lesbarkeit und identifikation

#zum überprüfen der nas pro variable: sum(is.na(nigeria.clean$variable))

##> sum(is.na(nigeria$event_id_cnty))
##[1] 0
##> sum(is.na(nigeria$event_date))
##[1] 0
##> sum(is.na(nigeria$year))
##[1] 62792
##> sum(is.na(nigeria$time_precision))
##[1] 62793
##> sum(is.na(nigeria$event_type))
##[1] 0
###nas sind als " " gespeichert
##> sum(is.na(nigeria$sub_event_type))
##[1] 0

##> sum(is.na(nigeria$actor1))
##[1] 0
##> sum(is.na(nigeria$civilian_targeting))
##[1] 0
##> sum(is.na(nigeria$region))
##[1] 0
##> sum(is.na(nigeria$location))
##[1] 0
##> sum(is.na(nigeria$latitude))
##[1] 62794
##> sum(is.na(nigeria$longitude))
##[1] 62794
##> sum(is.na(nigeria$geo_precision))
##[1] 62794
##> sum(is.na(nigeria$source))
##[1] 0
##> sum(is.na(nigeria$source_scale))
##[1] 0
##> sum(is.na(nigeria$fatalities))
##[1] 0
##> sum(is.na(nigeria$timestamp))
##[1] 0
##> sum(is.na(nigeria$population_best))
##[1] 29987

###Sehr viele NAs bei year, 
###mit 1997 <= year <= 2025 nimmt man auch alle NAs von longitude latitude raus
###population_best NAs zu 0 umgewandelt, da diese weniger wichtig sind wenn sie fehlen, 
###solange nur diese variable fehlt
### doppelte zeilen müssen noch überprüft und evtl gelöscht werden