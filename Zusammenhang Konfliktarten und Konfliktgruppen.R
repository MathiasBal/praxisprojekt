# association type of conflict and conflict groups

# graph 

# count numbers
data_counts <- nigeria.wide %>%
  count(event_type, actor_group) %>%
  filter(!is.na(event_type) & !is.na(actor_group))


# facet wrap
  ggplot(data_counts, aes(x = event_type, y = n, fill = event_type)) +
    geom_col(position = "dodge") +  
    facet_wrap(~ factor(actor_group, levels = c(
      "Civilians/Protesters", "Cult Groups", "Militia", 
      "State Security Forces", "Terrorist Groups", "Unidentified Armed Groups", "Other"
    )), labeller = as_labeller(c(
      "Civilians/Protesters" = "Zivilisten/Protestierende",
      "Cult Groups" = "Gewalttätige Sekten",
      "Militia" = "Milizen",
      "State Security Forces" = "Staatliche Sicherheitskräfte",
      "Terrorist Groups" = "Terrorgruppen",
      "Unidentified Armed Groups" = "Unbekannte bewaffnete Gruppen",
      "Other" = "Andere"
    ))) +
    labs(
      title = "Zusammenhang Konfliktarten und Konfliktgruppen",
      x = NULL,
      y = "Anzahl der Konflikte",
      fill = "Konflikttyp"
    ) +
    scale_fill_manual(values = c(
      "Battles" = "#E41A1C",
      "Explosions/Remote violence" = "#377EB8",
      "Protests" = "#4DAF4A",
      "Riots" = "#984EA3",
      "Strategic developments" = "#FF7F00",
      "Violence against civilians" = "#FFFF33"
    ), labels = c(
      "Battles" = "Kämpfe",
      "Explosions/Remote violence" = "Explosionen/Ferngewalt",
      "Protests" = "Proteste",
      "Riots" = "Randalierungen",
      "Strategic developments" = "Strategische Entwicklungen",
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
  
