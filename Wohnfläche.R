library(dplyr)
library(ggplot2)
library(scales)
pricedrivers %>% 
  select(nmqm, ovm21, wfl.gekappt) %>% 
  filter(ovm21 < nmqm) %>%
  mutate( 
    living_space_category = cut(
      wfl.gekappt,
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
  labs(x = "Wohnungsgröße (m²)", y = "Anteil (%)", title = "Prozentuale Verteilung der Wohnungsgrößen") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.text = element_text(size = 10)
  )


#Anteil der Preistreiber deren Nettomieter pro m² über der mittleren ortsüblichen Vergleichsmiete liegt in %
Prozent <- pricedrivers %>% 
  select(nmqm, ovm21)

prozentdrüber <- (((Prozent$nmqm / Prozent$ovm21) -1) * 100)
prozentdrüber <- floor(prozentdrüber)
glimpse(prozentdrüber)


#Wohnfläche Verteilung auf Nettomiete pro m²
library(ggplot2)
ggplot(pricedrivers, aes(x = wfl.gekappt, y = prozentdrüber)) +
  geom_point(color = "black", alpha = 0.5, size = 0.8) +
  labs(x = "Wohnungsgröße (m²)", 
       y = " Anteil Preistreiber Nettomiete pro m² über ortsüblichen Vergleichsmiete (%)",
       title = "Verteilung der Wohnfläche in Verhältnis zur ortsüblichen Vergleichsmiete") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
