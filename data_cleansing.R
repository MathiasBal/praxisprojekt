#loading in the data set
nigeria <- read.csv("C:/Users/Bob/Documents/Praxisprojekt2/1997-01-01-2025-01-01-Nigeria.csv")

#loading required libraries
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("osmdata")) install.packages("osmdata")
if (!require("forcats")) install.packages("forcats")
if (!require("scales")) install.packages("scales")
if (!require("lubridate")) install.packages("lubridate")

library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(scales)
library(lubridate)


#code convention: Use lowerCamelCase for functions, 
#dotted.case for variables, 
#and UpperCamelCase for classes

#data cleansing

nigeria.clean <- nigeria %>% 
  mutate(event_date = mdy(event_date)) %>% 
  select(-notes)


nigeria.clean$sub_event_type <- str_remove_all(nigeria.clean$sub_event_type,
                                               "\"")
#2 ausprägungen kombiniert


nigeria.clean$civilian_targeting <- str_remove_all(nigeria.clean$civilian_targeting,
                                                   "[[:punct:]]")

nigeria.clean$civilian_targeting <- factor(nigeria.clean$civilian_targeting,
                                              levels = c("", "Civilian targeting"),
                                              labels = c("no targeting", "civilian targeting"))
#civilian targeting aufgeräumt und als factor


nigeria.clean$geo_precision <- as.factor(nigeria.clean$geo_precision)
#kann man als factor mit levels schreiben wenn wir wissen was er bedeutet

  
nigeria.clean$fatalities <- as.integer(str_remove_all(nigeria.clean$fatalities,
                                           "[[:alpha:][:space:][:punct:]]"))
#fatalities aufgeräumt und als integer nicht mehr chr gespeichert



#zum überprüfen der nas pro variable: sum(is.na(nigeria.clean$variable))

str(nigeria.clean)

nigeria.no.nas  <- nigeria.clean %>%
  filter(if_all(everything(), ~ !is.na(.)))



