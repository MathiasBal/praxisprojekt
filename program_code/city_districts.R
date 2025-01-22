# Benötigte Pakete installieren und laden
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Annahme: Der DataFrame "anf_park_ws24" ist bereits im Arbeitsspeicher
# "Preistreiber" berechnen: 1 = Preistreiber, 0 = kein Preistreiber
anf_park_ws24 <- anf_park_ws24 %>%
  mutate(preistreiber = ifelse(nmqm > ovm21, 1, 0))

# Durchschnittlicher Anteil der Preistreiber pro Bezirk berechnen
preistreiber_by_bezirk <- anf_park_ws24 %>%
  group_by(SBez) %>%
  summarise(anteil_preistreiber = mean(preistreiber, na.rm = TRUE)) %>%
  arrange(desc(anteil_preistreiber))  # Sortieren der Bezirke absteigend

# Sortierung für Facets anpassen
preistreiber_by_bezirk$SBez <- factor(preistreiber_by_bezirk$SBez, levels = preistreiber_by_bezirk$SBez)

# Darstellung mit angepasster Farbskala
ggplot(data = preistreiber_by_bezirk, aes(x = 1, y = 1, fill = anteil_preistreiber)) +
  geom_tile(width = 0.9, height = 0.9) +  # Kästchen zeichnen
  geom_text(aes(label = scales::percent(anteil_preistreiber, accuracy = 1)), color = "white", size = 4) +  # Werte einfügen
  facet_wrap(~ SBez, ncol = 4) +  # Facetierung mit 4 Spalten
  scale_fill_gradient(low = "blue", high = "red", name = "Anteil\nPreistreiber", labels = scales::percent) +
  labs(
    title = "Anteil der Preistreiber pro Bezirk",
    subtitle = "Absteigend sortierte Bezirke nach Anteil der Preistreiber",
    x = NULL,
    y = NULL,
    caption = "Quelle: Eigene Berechnung"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Titel der Facets (Bezirke)
    axis.text = element_blank(),  # Keine Achsenbeschriftungen
    axis.ticks = element_blank(),  # Keine Achsenticks
    panel.grid = element_blank(),  # Keine Gitterlinien
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom"  # Legende unterhalb der Darstellung
  )
