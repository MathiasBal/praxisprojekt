# Libraries
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("osmdata")) install.packages("osmdata")
if (!require("forcats")) install.packages("forcats")
if (!require("scales")) install.packages("scales")

library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(scales)

# Data Prepping
anf.prak.ws24 <- anf.park.ws24
anf.prak.ws24$bj <- floor(anf.prak.ws24$bj)
wl <- anf.prak.ws24$WL

wl <- str_replace(wl, "beste", "3")
wl <- str_replace(wl, "gute", "2")
wl <- str_replace(wl, "durchschnittliche", "1")
anf.prak.ws24 <- transform(anf.prak.ws24, WL = as.integer(WL))

anf.prak.ws24 <- anf.prak.ws24 %>% 
  rename(net_rent_per_qm = nmqm) %>% 
  rename(space_per_qm = wfl.gekappt) %>% 
  rename(year_of_construction = bj) %>% 
  rename(landlord_type = vermietertyp) %>% 
  rename(contract_type = art.vertrag) %>% 
  rename(residential_area = WL) %>% 
  rename(is_central = Zentral) %>% 
  rename(is_new_contract = Neuvertrag) %>% 
  rename(first_occupancy = erstbezug) %>% 
  rename(district_name = SBez) %>% 
  rename(district_no = bezirk.nr) %>% 
  rename(avg_comparative_rent = ovm21) %>% 
  rename(house_type = Haustyp) %>% 
  rename(building_type = Gebäudetyp) %>% 
  rename(rent_increase_month = mieterhöhung_monat) %>% 
  rename(rent_increase_year = mieterhöhung_jahr) %>% 
  rename(start_lease_month = beginn_mietverh_monat) %>% 
  rename(start_lease_year = beginn_mietverh_jahr) %>% 
  mutate(district_name = recode(district_name, "Fledmoching" = "Feldmoching")) %>% 
  mutate(district_name = recode(district_name, "Aubing..." = "Aubing – Lochhausen – Langwied")) %>%
  mutate(district_name = recode(district_name, "Thalkirchen..." = "Thalkirchen-Obersendling-Forstenried-Fürstenried-Solln"))
