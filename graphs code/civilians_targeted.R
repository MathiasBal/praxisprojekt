# Civilian Targeting by Event Type

civilians_targeted <- nigeria.merged %>% 
  filter(civilian_targeting == 2)

nigeria.merged %>%
  filter(event_type != "NULL") %>%
  filter(!is.na(civilian_targeting)) %>% 
  ggplot(
    aes(
      x = event_type, 
      fill = factor(civilian_targeting)
    )
  ) +
  geom_bar() +
  scale_fill_manual(
    values = c("gray", "red"), 
    labels = c("nicht angegriffen", "angegriffen")
  ) +
  labs(
    title = "Konflikttypen und Angriffe auf Zivilisten",
    x = "Konflikttyp",
    y = "Anzahl der Konflikte",
    fill = "Angriffe auf Zivilisten"
  ) +
  theme_minimal()

