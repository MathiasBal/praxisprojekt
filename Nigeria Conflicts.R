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


# big cities in Nigeria
big_cities <- data.frame(
  city = c("Lagos", "Abuja", "Kano", "Port Harcourt", "Kaduna", "Ibadan", "Maiduguri"),
  lon = c(3.3792, 7.4913, 8.5167, 7.0134, 7.4386, 3.8964, 13.1510),
  lat = c(6.5244, 9.0579, 12.0000, 4.8242, 10.5236, 7.3775, 11.8464)
)



# graph
ggplot() +
  geom_sf(data = Nigeria, fill = "gray90", color = "black", linewidth = 0.4) +
  geom_point(data = nigeria.no.nas, 
             aes(x = longitude, y = latitude, 
                 color = recode(actor_group,
                                "Civilians/Protesters" = "Zivilisten/Protestierende",
                                "Militia" = "Milizen",
                                "State Security Forces" = "Staatliche Sicherheitskräfte",
                                "Cult Groups" = "gewalttätige Sekten",
                                "Other" = "Andere",
                                "Terrorist Groups" = "Terrorgruppen",
                                "Unidentified Armed Groups" = "Unbekannte bewaffnete Gruppen")), size = 1.3) +
  geom_point(data = big_cities, aes(x = lon, y = lat), 
             color = "black", size = 5) + 
  geom_label_repel(data = big_cities, aes(x = lon, y = lat, label = city),
                   size = 6, fontface = "bold", fill = "white", color = "black",
                   box.padding = 0.5, point.padding = 0.2, max.overlaps = 10) +
  scale_color_manual(values = c(
    "Zivilisten/Protestierende" = "blue",
    "Milizen" = "cyan",
    "Staatliche Sicherheitskräfte" = "green",
    "gewalttätige Sekten" = "gold",
    "Andere" = "brown",
    "Terrorgruppen" = "red",
    "Unbekannte bewaffnete Gruppen" = "purple"
  ), guide = guide_legend(override.aes = list(size = 5))) +
  labs(
    title = "Konflikte in Nigeria",
    x = "Längengrad",
    y = "Breitengrad",
    color = "Konfliktgruppen"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
