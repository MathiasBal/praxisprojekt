# Libraries
if (!require("stringr")) install.packages("stringr")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("osmdata")) install.packages("osmdata")

library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

# Durchschnittlicher Anteil der Preistreiber pro Bezirk berechnen
price_driver_by_district <- data %>%
  group_by(district_name) %>%
  summarise(share_price_driver = mean(price_driver, na.rm = TRUE))

# Lade Stadtbezirksgrenzen für München
munich_boundaries <- opq("München") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "10") %>%
  osmdata_sf()

# Stadtbezirksgrenzen extrahieren
munich_districts <- munich_boundaries$osm_multipolygons

# Verknüpfe Geodaten mit den berechneten Preistreiber-Daten
# Angenommen, munich_districts hat eine ID oder Namen der Bezirke, die mit `SBez` übereinstimmen
# Prüfen und anpassen!
munich_districts <- munich_districts %>%
  left_join(price_driver_by_district, by = c("name" = "district_name"))  # Passe `name` ggf. an

# Karte erstellen mit Preistreiber-Anteil
ggplot(data = munich_districts) +
  geom_sf(aes(fill = share_price_driver), color = "white") +
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "grey90", 
    name = "Preistreiber-Anteil"
  ) +
  labs(
    title = "Preistreiber im Münchner Mietwohnungsmarkt",
    subtitle = "Farbliche Hervorhebung der Bezirke mit hohem Preistreiber-Anteil",
    caption = "Quelle: Eigene Berechnung und OpenStreetMap"
  ) +
  theme_minimal()

ggplot(data, aes(x = residential_area)) +
  geom_bar() +
  labs(
    title = "Wohnlagen im Münchner Mietwohnungsmarkt",
    x = "Wohnlage",
    y = "Anzahl"
  ) +
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c("Durchschnittlich", "Gut", "Best")) +
  theme_minimal()
