#Verteilung der Wohnungsgrößen
library(dplyr)
library(ggplot2)
library(scales)
pricedrivers %>% 
  select(net_rent_per_qm, avg_comparative_rent, space_per_qm) %>% 
  filter(avg_comparative_rent < net_rent_per_qm) %>%
  mutate( 
    living_space_category = cut(
      space_per_qm,
      breaks = c(0, 30, 60, 90, 120, 150, 180),
      labels = c("1-30", "31-60", "61-90", "91-120", "121-150", "151-180"),
      include.lowest = TRUE
    )
  ) %>% 
  count(living_space_category) %>% 
  mutate(
    percentage = (n / sum(n)) * 100,
    percentage = round(percentage, 2)
  ) %>% 
  ggplot(aes(x = living_space_category, y = percentage / 100)) +
  geom_col(fill = "skyblue", width = 0.7) +
  geom_text(
    aes(label = paste0(percentage, "%")),
    vjust = -0.2,
    size = 3,
    color = "black") +
  labs(x = "Wohnungsgröße (m²)", y = "Anteil (%)", title = "Verteilung der Wohnungsgrößen") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.text = element_text(size = 10)
  )
