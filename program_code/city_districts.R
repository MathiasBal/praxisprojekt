# Annahme: Der DataFrame "anf_park_ws24" ist bereits im Arbeitsspeicher
# "Preistreiber" berechnen: 1 = Preistreiber, 0 = kein Preistreiber
anf_park_ws24 <- anf.park.ws24 %>%
  mutate(preistreiber = ifelse(nmqm > ovm21, 1, 0))

# Durchschnittlicher Anteil der Preistreiber pro Bezirk berechnen
preistreiber_by_bezirk <- anf_park_ws24 %>%
  group_by(SBez) %>%
  summarise(anteil_preistreiber = mean(preistreiber, na.rm = TRUE))

# Geodaten der Münchner Stadtbezirke aus OpenStreetMap laden
if (!require("osmdata")) install.packages("osmdata")
library(osmdata)

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
  left_join(preistreiber_by_bezirk, by = c("name" = "SBez"))  # Passe `name` ggf. an

# Karte erstellen mit Preistreiber-Anteil
ggplot(data = munich_districts) +
  geom_sf(aes(fill = anteil_preistreiber), color = "white") +
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
