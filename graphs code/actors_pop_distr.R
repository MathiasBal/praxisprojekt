
dataframe.actors.population <- nigeria.merged


dataframe.actors.population <- dataframe.actors.population %>% filter(!is.na(population_best))


boxplot.actors.population <- ggplot(dataframe.actors.population, aes(x = actor_group1, y = population_best)) +
  geom_boxplot(outlier.shape = NA, fill = "#779776", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 125000)) + 
  labs(title = "Zusammenhang zwischen Akteursgruppen und der Bevölkerungsanzahl am Ereignisort",
       x = "Akteure",
       y = "Bevölkerung am Ereignisort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(boxplot.actors.population)
