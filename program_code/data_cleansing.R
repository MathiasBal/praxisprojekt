# Libraries
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

# Data Prepping
anf.park.ws24 <- anf_park_ws24
anf.park.ws24$bj <- floor(anf.park.ws24$bj)
wl <- anf.park.ws24$WL

wl <- str_replace(wl, "beste", "3")
wl <- str_replace(wl, "gute", "2")
wl <- str_replace(wl, "durchschnittliche", "1")
anf.park.ws24 <- transform(anf.park.ws24, WL = as.integer(WL))

anf.park.ws24 <- anf.park.ws24 %>% 
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
  rename(price_driver = preistreiber)

# Annahme: Der DataFrame "anf_park_ws24" ist bereits im Arbeitsspeicher
# "Preistreiber" berechnen: 1 = Preistreiber, 0 = kein Preistreiber
data <- anf.park.ws24 %>%
  mutate(price_driver = ifelse(net_rent_per_qm > avg_comparative_rent, 1, 0))
