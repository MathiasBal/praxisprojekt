
dataframe_acteurs_population <- nigeria.merged


dataframe_acteurs_population <- dataframe_acteurs_population %>% filter(!is.na(population_best))


boxplot_acteurs_population <- ggplot(dataframe_acteurs_population, aes(x = actor_group1, y = population_best)) +
  geom_boxplot(outlier.shape = NA, fill = "steelblue", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 125000)) + 
  labs(title = "Bevölkerungsanzahl an Ereignisorten der Akteursgruppen",
       x = "Akteure",
       y = "Bevölkerung am Ereignisort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(boxplot_acteurs_population)
