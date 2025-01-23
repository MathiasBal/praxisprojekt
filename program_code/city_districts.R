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

# Darstellung mit optimierter Nutzung der Plot-Fläche und größerer Schrift
ggplot(data = preistreiber_by_bezirk, aes(x = SBez, y = anteil_preistreiber, fill = anteil_preistreiber)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +  # Höhenbalken zeichnen
  geom_text(
    aes(label = scales::percent(anteil_preistreiber, accuracy = 1)),
    position = position_stack(vjust = 0.5),  # Mittig im Balken platzieren
    color = "white", size = 5  # Schriftgröße der Werte erhöhen
  ) +
  facet_wrap(~ SBez, scales = "free_x", ncol = 5) +  # Facetierung mit 5 Spalten
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +  # Einheitliche Y-Achse für alle
  scale_fill_gradient(low = "blue", high = "red", name = "Anteil\nPreistreiber", labels = scales::percent) +
  labs(
    title = "Anteil der Preistreiber pro Bezirk",
    x = NULL,
    y = "Anteil der Preistreiber"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Größere Schrift für Facet-Titel
    axis.text.x = element_blank(),  # X-Achse Text entfernen
    axis.ticks.x = element_blank(),  # X-Achse Ticks entfernen
    panel.grid.major.x = element_blank(),  # Gitterlinien für X entfernen
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Größere Titel-Schrift
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"  # Legende unterhalb der Darstellung
  )
