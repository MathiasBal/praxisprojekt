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
  labs(title = "Reports Over Time", x = "Jahr", y = "Anzahl der Berichte") +
  geom_line() +
  geom_point() +
  theme_minimal()

## Reports per source

### Total

top.sources <- nigeria.merged %>% 
  count(source) %>% 
  top_n(5, n) %>% 
  pull(source)

nigeria.merged %>%
  filter(source %in% top.sources) %>%
  mutate(
    source = case_when(
      source == "Risk and Strategic Management, Corporation" ~ "RSM Corp.",
      TRUE ~ source
    )
  ) %>%
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
    title = "Verlauf der meistgenutzten Nachrichtenquellen",
    x = "Jahr",
    y = "Anzahl der Berichte",
    color = "Nachrichtenquelle"
  ) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, 5) 
  ) +
  theme_minimal()


### International

international.sources <- nigeria.merged %>% 
  filter(source_scale == "International") %>% 
  count(source) %>% 
  top_n(5, n) %>% 
  pull(source)

nigeria.merged %>%
  filter(source %in% international.sources) %>% 
  filter(year <= 2024) %>% 
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
    title = "Verlauf der meistgenutzten internationalen Nachrichtenquellen", 
    x = "Jahr", 
    y = "Anzahl der Berichte",
    color = "Nachrichtenquelle"
  ) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#ff6361", "burlywood4", "cadetblue4", "darkorchid4", "cornsilk4")) +
  theme_minimal()

### New Media 

new.media.sources <- nigeria.merged %>%
  filter(source_scale == "New media" | source_scale == "New media-National") %>%
  mutate(source = case_when(
    grepl("Twitter", source, ignore.case = TRUE) ~ "Twitter",
    TRUE ~ source
  )) %>%
  count(source) %>%
  top_n(5, n) %>%
  pull(source)

nigeria.merged %>%
  filter(source_scale == "New media" | source_scale == "New media-National") %>%
  filter(year <= 2024) %>% 
  mutate(source = case_when(
    grepl("Twitter", source, ignore.case = TRUE) ~ "Twitter",
    grepl("Telegram", source, ignore.case = TRUE) ~ "Telegram",
    TRUE ~ source
  )) %>%
  filter(source %in% new.media.sources) %>%
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
    title = "Verlauf der meistgenutzten New-Media-Nachrichtenquellen",
    x = "Jahr",
    y = "Anzahl der Berichte",
    color = "Nachrichtenquelle"
  ) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkgrey", "deepskyblue", "chartreuse4", "darkorange")) +
  theme_minimal()

