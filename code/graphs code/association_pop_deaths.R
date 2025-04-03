#association between deaths and population size at event location

# filter for population and fatalities exist
dataframe.population.deaths.correlation <- subset(nigeria.merged, population_best > 0 & fatalities >= 0) 

# Scatterplot
ggplot(dataframe.population.deaths.correlation, aes(x = population_best, y = fatalities)) +
  geom_point(alpha = 0.5, color = "blue") + 
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Zusammenhang zwischen Bevölkerungszahl und Todesfällen",
    x = "Bevölkerungszahl",
    y = "Anzahl der Todesopfer"
  ) +
  theme_minimal()
