library(ggplot2)

df <- nigeria.merged


df_filtered <- subset(df, population_best > 0 & fatalities >= 0)

# Scatterplot
plot <- ggplot(df_filtered, aes(x = population_best, y = fatalities)) +
  geom_point(alpha = 0.5, color = "blue") + 
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Zusammenhang zwischen Bevölkerungszahl und Todesfälle",
    x = "Bevölkerungszahl",
    y = "Anzahl der Todesopfer"
  ) +
  theme_minimal()


print(plot)
