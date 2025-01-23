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

# Sortierung der Bezirke anpassen
preistreiber_by_bezirk$SBez <- factor(preistreiber_by_bezirk$SBez, levels = preistreiber_by_bezirk$SBez)

# Darstellung als Punkte- und Liniendiagramm
ggplot(data = preistreiber_by_bezirk, aes(x = SBez, y = anteil_preistreiber, group = 1)) +
  geom_line(color = "black", size = 1) +  # Linie zwischen den Punkten
  geom_point(size = 4, color = "skyblue") +  # Punkte für jeden Wert
 
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +  # Einheitliche Y-Achse (0-100%)
  labs(
    x = "Bezirk",
    y = "Anteil der Preistreiber"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # X-Achse beschriften (schräg gestellt)
    axis.text.y = element_text(size = 10),  # Y-Achse beschriften
    axis.title.x = element_text(size = 12),  # X-Achsentitel
    axis.title.y = element_text(size = 12),  # Y-Achsentitel
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Titel
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel
    panel.grid.major.x = element_line(color = "grey90"),  # Dezente Gitterlinien für X
    panel.grid.major.y = element_line(color = "grey90")
  )
