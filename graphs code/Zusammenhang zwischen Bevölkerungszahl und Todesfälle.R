library(ggplot2)

df <- nigeria.merged

# Daten filtern, um ungültige Werte zu vermeiden
df_filtered <- subset(df, population_best > 0 & fatalities >= 0)

# Scatterplot ohne Regressionslinie
plot <- ggplot(df_filtered, aes(x = population_best, y = fatalities)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatterplot ohne Linie
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Zusammenhang zwischen Bevölkerungszahl und Todesfälle",
    x = "Bevölkerungszahl",
    y = "Anzahl der Todesopfer"
  ) +
  theme_minimal()


print(plot)
