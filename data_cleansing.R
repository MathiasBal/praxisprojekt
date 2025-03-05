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
  mutate(event_date = mdy(event_date))





unique(nigeria.clean$event_date)
#group by month mb
unique(nigeria.clean$year)
#6 und NA drinne
unique(nigeria.clean$time_precision)
#weird variable
View(table(nigeria.clean$event_type))
#sehr gut zum aufräumen
unique(nigeria.clean$sub_event_type)
#ist ok
unique(nigeria.clean$actor1)
#1440 actors...
unique(nigeria.clean$civilian_targeting)
#2 bezeichnungen für ja/nein jeweils
unique(nigeria.clean$region)
#unnötig
unique(nigeria.clean$location)
#5200 locations...
unique(nigeria.clean$geo_precision)
#1,2,3,NA
unique(nigeria.clean$source)
#2900 sources
unique(nigeria.clean$source_scale)
#can combine some
unique(nigeria.clean$fatalities)
#convert to int and remove strings
unique(nigeria.clean$timestamp)
#2100 timestamps
unique(nigeria.clean$population_best)
#8000 entries
