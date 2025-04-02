
df_population <- subset(nigeria_merged, population_best > 0 & fatalities >= 0) #Nur Datensätze wo die untersuchten Daten vorhanden sind

# Scatterplot
plot <- ggplot(df_population, aes(x = population_best, y = fatalities)) +
  geom_point(alpha = 0.5, color = "blue") + 
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Zusammenhang zwischen Bevölkerungszahl und Todesfälle",
    x = "Bevölkerungszahl",
    y = "Anzahl der Todesopfer"
  ) +
  theme_minimal()


print(plot)
