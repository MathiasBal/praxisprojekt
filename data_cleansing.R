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