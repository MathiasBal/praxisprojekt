#ZUSAMMENHANG KONFLIKTARTEN UND KONFLIKTGRUPPEN

#GRAFIK 1
# Balkendiagramm
ggplot(nigeria.no.nas, aes(x = event_type, fill = actor_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Zusammenhang Konfliktarten und Konfliktgruppen in Nigeria",
       x = "Konflikttyp",
       y = "Anzahl der Konflikte",
       fill = "Konfliktgruppe") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5))

#GRAFIK 2
#Anzahl berechnen
data_counts <- nigeria.no.nas %>%
  count(event_type, actor_group) %>%
  filter(!is.na(event_type) & !is.na(actor_group))


# Facet-Wrapped Balkendiagramm
ggplot(data_counts, aes(x = event_type, y = n, fill = event_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ actor_group, scales = "free_y") +  
  labs(title = "Zusammenhang Konfliktarten und Konfliktgruppen in Nigeria",
       x = "Konflikttyp", y = "Anzahl der Konflikte") +
  theme_minimal() 


#GRAFIK 3
#Pakete laden
install.packages("treemapify")
library(treemapify)


#Farbpalette
actor_colors <- c(
  "Civilians/Protesters" = "red",
  "Cult Groups" = "yellow",
  "Militia" = "green",
  "Other" = "purple",
  "State Security Forces" = "blue",
  "Terrorist Groups" = "orange",
  "Unidentified Armed Groups" = "black"
)


# Treemap
ggplot(data_counts, aes(area = n, fill = actor_group, label = actor_group)) +
  geom_treemap() +
  geom_treemap_text(
    place = "centre",
    colour = "white", 
    fontface = "bold",
    size = 10
  ) +
  scale_fill_manual(values = actor_colors) +
  labs(
    title = "Zusammenhang Konflikttypen und Konfliktgruppen in Nigeria",
    fill = "Konfliktgruppen"
  ) +
  theme_minimal() + 
  facet_wrap(~ event_type, scales = "free", ncol = 2) + 
  theme(
    strip.text = element_text(size = 14, face = "bold"), 
    legend.position = "bottom",  
    panel.spacing = unit(1.5, "lines"),  
    axis.text.x = element_text(size = 20, face = "bold")  
  )

