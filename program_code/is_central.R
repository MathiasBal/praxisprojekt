data_raw <- anf.prak.ws24
data_filtered <- pricedrivers

data_raw_summary <- data_raw %>%
  group_by(is_central) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percent = count / sum(count) * 100,
    group = "Raw"
  )

data_filtered_summary <- data_filtered %>%
  group_by(is_central) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percent = count / sum(count) * 100,
    group = "Filtered"
  )

data_raw_summary$is_central <- factor(data_raw_summary$is_central, levels = c(0, 1), labels = c("Dezentral", "Zentral"))
data_filtered_summary$is_central <- factor(data_filtered_summary$is_central, levels = c(0, 1), labels = c("Dezentral", "Zentral"))

ggplot(data_raw_summary, aes(x = group, y = percent, fill = is_central)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(
    x = "Dataset",
    y = "Prozent",
    fill = "Mietlage in Stadtbezirken"
  )

ggplot(data_filtered_summary, aes(x = group, y = percent, fill = is_central)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(
    x = "Dataset",
    y = "Prozent",
    fill = "Mietlage in Stadtbezirken"
  )


