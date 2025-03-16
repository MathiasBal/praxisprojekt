# During what years were the most reports of violence in Nigeria?

## Total reports
nigeria.merged %>%
  count(year) %>% 
  ggplot(
    aes(
      x = year, 
      y = n
    )
  ) +
  labs(title = "Reports Over Time", x = "Year", y = "Number of reports") +
  geom_line() +
  geom_point() +
  theme_minimal()

