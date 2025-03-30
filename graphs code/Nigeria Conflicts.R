# graph Nigeria Conflicts


# load package
install.packages("rnaturalearthdata")
library(rnaturalearthdata)


# Nigeria Map
Nigeria <- ne_countries(scale = "medium", country = "Nigeria", returnclass = "sf")


# top conflicts
top_conflicts <- nigeria.merged %>%
  mutate(actor_group = ifelse(actor_group1 < actor_group2, 
                              paste(actor_group1, "vs", actor_group2), 
                              paste(actor_group2, "vs", actor_group1))) %>%
  count(actor_group, sort = TRUE) %>%
  top_n(6, n) %>% 
  pull(actor_group)


# big cities in Nigeria
big_cities <- data.frame(
  city = c("Lagos", "Abuja", "Kano", "Port Harcourt", "Kaduna", "Ibadan", "Maiduguri"),
  lon = c(3.3792, 7.4913, 8.5167, 7.0134, 7.4386, 3.8964, 13.1510),
  lat = c(6.5244, 9.0579, 12.0000, 4.8242, 10.5236, 7.3775, 11.8464)
)


# translation and filtering
nigeria_top_conflicts <- nigeria.merged %>%
  mutate(actor_group = ifelse(actor_group1 < actor_group2, 
                              paste(actor_group1, "vs", actor_group2), 
                              paste(actor_group2, "vs", actor_group1))) %>%
  filter(actor_group %in% top_conflicts) %>% 
  mutate(actor_group_de = recode(actor_group, "External/Other Forces vs Protesters" = "Externe/Sonstige Akteure vs Protestierende", 
                                 "Civilians vs Identity Militias" = "Zivilisten vs Identitätsmilizen",
                                 "Identity Militias vs State Forces" = "Identitätsmilizen vs Staatliche Sicherheitskräfte",
                                 "Civilians vs Political Militias" = "Zivilisten vs Politische Milizen",
                                 "Political Militias vs State Forces" = "Politische Milizen vs Staatliche Sicherheitskräfte",
                                 "Rebel Groups vs State Forces" = "Rebellengruppen vs Staatliche Sicherheitskräfte"))


# graph
ggplot() +
  geom_sf(data = Nigeria, fill = "gray90", color = "black", linewidth = 0.4) +
  geom_point(data = nigeria_top_conflicts, 
             aes(x = longitude, y = latitude, 
                 color = actor_group_de), size = 1.3) + 
  geom_point(data = big_cities, aes(x = lon, y = lat), 
             color = "black", size = 5) + 
  geom_label_repel(data = big_cities, aes(x = lon, y = lat, label = city),
                   size = 6, fontface = "bold", fill = "white", color = "black",
                   box.padding = 0.5, point.padding = 0.2, max.overlaps = 10) +
  scale_color_manual(values = c(
    "Externe/Sonstige Akteure vs Protestierende" = "brown",
    "Zivilisten vs Identitätsmilizen" = "blue",
    "Identitätsmilizen vs Staatliche Sicherheitskräfte" = "cyan",
    "Zivilisten vs Politische Milizen" = "red",
    "Politische Milizen vs Staatliche Sicherheitskräfte" = "purple",
    "Rebellengruppen vs Staatliche Sicherheitskräfte" = "gold"), guide = guide_legend(override.aes = list(size = 5), title.hjust = 0.5)) +
  labs(
    title = "Konflikte in Nigeria",
    x = "Längengrad",
    y = "Breitengrad",
    color = "Konfliktkonstellationen") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"))
