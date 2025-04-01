# translation event types
event_translation <- c(
  "Battles" = "Kämpfe",
  "Explosions/Remote violence" = "Explosionen",
  "Protests" = "Proteste",
  "Riots" = "Aufstände",
  "Strategic developments" = "Strategische Angriffe",
  "Violence against civilians" = "Gewalt gegen Zivilisten"
)


# filtering
conflict_trends <- nigeria.merged %>%
  filter(!is.na(event_type) & event_type != "NULL",
         year < 2025) %>%
  group_by(year = year(event_date), event_type) %>%
  summarise(conflict_count = n(), .groups = "drop") %>%
  mutate(event_type_de = recode(event_type, !!!event_translation))


# line plot
ggplot(conflict_trends, aes(x = year, y = conflict_count, color = event_type_de, group = event_type_de)) +
  geom_line(size = 1.5, linewidth = 1.5) + 
  scale_color_manual(values = c(
    "Gewalt gegen Zivilisten" = "yellow",
    "Explosionen" = "blue",
    "Proteste" = "brown",
    "Strategische Angriffe" = "orange",
    "Aufstände" = "purple",
    "Kämpfe" = "red"
  )) + 
  labs(
    title = "Konflikttypen in Nigeria",
    x = "Jahr",
    y = "Anzahl der Konflikte",
    color = "Konflikttyp"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    panel.grid.minor = element_blank()  
  )
