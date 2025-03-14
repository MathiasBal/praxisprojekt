# graph Nigeria Conflicts

# load packages
library(ggplot2)
library(sf)  
library(ggthemes)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)
library(dplyr)
library(ggrepel)

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

# translation and filtering
nigeria_top_conflicts <- nigeria.merged %>%
  mutate(actor_group = ifelse(actor_group1 < actor_group2, 
                              paste(actor_group1, "vs", actor_group2), 
                              paste(actor_group2, "vs", actor_group1))) %>%
  filter(actor_group %in% top_conflicts) %>%
  filter(!(actor_group %in% c("Other vs Protesters", "Protesters vs Other"))) %>%
  mutate(actor_group_de = recode(actor_group, "Civilians vs Identity Militias" = "Zivilisten vs Identitätsmilizen",
                                              "Identity Militias vs State Forces" = "Identitätsmilizen vs Staatliche Sicherheitskräfte",
                                              "Civilians vs Political Militias" = "Zivilisten vs Politische Milizen",
                                              "Political Militias vs State Forces" = "Politische Milizen vs Staatliche Sicherheitskräfte",
                                              "Rebel Groups vs State Forces" = "Rebellgruppen vs Staatliche Sicherheitskräfte"))

# Graph
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
    "Zivilisten vs Identitätsmilizen" = "blue",
    "Identitätsmilizen vs Staatliche Sicherheitskräfte" = "cyan",
    "Zivilisten vs Politische Milizen" = "red",
    "Politische Milizen vs Staatliche Sicherheitskräfte" = "green",
    "Rebellgruppen vs Staatliche Sicherheitskräfte" = "gold"), guide = guide_legend(override.aes = list(size = 5), title.hjust = 0.5)) +
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