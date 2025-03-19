# association type of conflict and conflict groups

# count numbers

data_counts <- nigeria.merged %>%
  pivot_longer(cols = c(actor_group1, actor_group2), 
               names_to = "actor_group", 
               values_to = "actor") %>%
  filter(event_type != "NULL") %>% 
  group_by(event_type, actor) %>%
  summarise(n = n(), .groups = "drop")


# facet wrap
ggplot(data_counts, aes(x = event_type, y = n, fill = event_type)) +
  geom_col(position = "dodge") +  
  facet_wrap(~ factor(actor, levels = c(
    "Civilians", "Protesters", "Rebel Groups", "State Forces", 
    "Identity Militias", "Political Militias", "Rioters", "External/Other Forces"
  )), labeller = as_labeller(c(
    "Civilians" = "Zivilisten",
    "Protesters" = "Protestierende",
    "Rebel Groups" = "Rebellengruppen",
    "State Forces" = "Staatliche Sicherheitskräfte",
    "Identity Militias" = "Identitätsmilizen",
    "Political Militias" = "Politische Milizen",
    "Rioters" = "Aufständische",
    "External/Other Forces" = "Externe/Sonstige Akteure"
  ))) +
  labs(
    title = "Zusammenhang Konfliktarten und Konfliktgruppen",
    x = NULL,
    y = "Anzahl der Konflikte",
    fill = "Konflikttyp"
  ) +
  scale_fill_manual(values = c(
    "Battles" = "red",
    "Explosions/Remote violence" = "blue",
    "Protests" = "brown",
    "Riots" = "purple",
    "Strategic developments" = "orange",
    "Violence against civilians" = "yellow"
  ), labels = c(
    "Battles" = "Kämpfe",
    "Explosions/Remote violence" = "Explosionen",
    "Protests" = "Proteste",
    "Riots" = "Aufstände",
    "Strategic developments" = "Strategische Angriffe",
    "Violence against civilians" = "Gewalt gegen Zivilisten"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
