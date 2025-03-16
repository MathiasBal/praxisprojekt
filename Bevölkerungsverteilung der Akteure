library(ggplot2)
library(dplyr)


df <- nigeria.merged


df <- df %>% filter(!is.na(population_best))


boxplot <- ggplot(df, aes(x = actor_group1, y = population_best)) +
  geom_boxplot(outlier.shape = NA, fill = "steelblue", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 125000)) + 
  labs(title = "Bevölkerungsverteilung nach Akteursgruppen",
       x = "Akteursgruppe",
       y = "Bevölkerung am Ereignisort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(boxplot)
