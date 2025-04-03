# Civilian Targeting by Event Type

nigeria.merged %>%
  filter(event_type != "NULL") %>%
  filter(!is.na(civilian_targeting)) %>% 
  mutate(
    event_type = case_when(
      event_type == "Battles" ~ "Kämpfe",
      event_type == "Explosions/Remote violence" ~ "Explosionen",
      event_type == "Protests" ~ "Proteste",
      event_type == "Riots" ~ "Aufstände",
      event_type == "Strategic developments" ~ "Strategische Angriffe",
      event_type == "Violence against civilians" ~ "Gewalt gegen Zivilisten",
      TRUE ~ event_type
    )
  ) %>% 
  ggplot(
    aes(
      x = event_type, 
      fill = factor(civilian_targeting)
    )
  ) +
  geom_bar() +
  scale_fill_manual(
    values = c("gray", "red"), 
    labels = c("Nicht angegriffen", "Angegriffen")
  ) +
  labs(
    title = "Konflikttypen und Angriffe auf Zivilisten",
    x = "Konflikttyp",
    y = "Anzahl der Konflikte",
    fill = "Angriffe auf Zivilisten"
  ) +
  theme_minimal()
