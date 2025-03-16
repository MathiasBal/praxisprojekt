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

## Reports per source

### Total

top_sources <- nigeria.merged %>% 
  count(source) %>% 
  top_n(5, n) %>% 
  pull(source)

nigeria.merged %>%
  filter(source %in% top_sources) %>% 
  count(year, source) %>% 
  ggplot(
    aes(
      x = year, 
      y = n,
      color = source,
      group = source
    )
  ) +
  labs(
    title = "Top Reports Over Time by News Source", 
    x = "Year", 
    y = "Number of reports",
    color = "News Source"
  ) +
  geom_line() +
  geom_point() +
  theme_minimal()
