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

#loading in the data set
nigeria <- read.csv("C:/Users/Bob/Documents/Praxisprojekt2/1997-01-01-2025-01-01-Nigeria.csv")

str(nigeria)

#code convention: Use lowerCamelCase for functions, 
#dotted.case for variables, 
#and UpperCamelCase for classes

#data cleansing

nigeria.clean <- nigeria %>% 
  mutate(event_date = mdy(event_date),
         time_precision = factor(time_precision,
                                 levels = c("1", "2", "3"),
                                 labels = c("most precise", "precise", "least precise")),
         sub_event_type = sub_event_type %>% 
           str_remove_all("\""),
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
         fatalities = fatalities %>% 
           str_remove_all("[[:alpha:][:space:][:punct:]]") %>%
           as.integer(),
         population_best = population_best %>% 
           as.integer() %>% 
           replace_na(0)) %>%
  filter(year >= 1997 & year <= 2025,
         latitude >= -90 & latitude <= 90,
         longitude >= -180 & longitude <= 180) %>%
  select(-notes)


str(nigeria.clean)

sum(duplicated(nigeria.clean))
##1100 doppelte zeilen !!!

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

nigeria.no.nas  <- nigeria.clean %>%
  filter(if_all(everything(), ~ !is.na(.)))



