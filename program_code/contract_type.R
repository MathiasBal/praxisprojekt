#Namen der verschiedenen Mietverträge
pd <- pricedrivers %>%
  mutate(contract_type = factor(
    contract_type,
    levels = c(1, 2, 3, 4),
    labels = c(
      "Index",
      "Staffel",
      "keine Regelung zur Mieterhöhung",
      "unbekannt"
    )
  )
  )

#genaue Anzahl (nur Preistreibern)
g.mietverträge <- pd
g.mietverträge_count <- g.mietverträge %>%
  select(net_rent_per_qm, avg_comparative_rent, contract_type) %>%
  filter(avg_comparative_rent < net_rent_per_qm) %>%
  group_by(contract_type) %>%
  summarise(count = n())


#Art des Mietvertrags (nur Preistreibern)
g.mietverträge %>%
  select(net_rent_per_qm, avg_comparative_rent, contract_type) %>%
  filter(avg_comparative_rent < net_rent_per_qm) %>%
  ggplot(aes(x = fct_infreq(contract_type))) +
  geom_bar(color = "skyblue",
           fill = "skyblue",
           width = 0.7) +
  geom_text(data = g.mietverträge_count,
            aes(x = contract_type, y = count, label = count),
            size = 3,
            vjust = -0.2) +
  labs(x = "Art des Mietvertrags", y = "Anzahl", title = "Anzahl der verschiedenen Mietverträge (nur Preistreibern)") +
  scale_y_continuous(breaks = seq(0, 1750, by= 250)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))


#genaue Anzahl
namen <- anf.prak.ws24 %>%
  mutate(contract_type = factor(
    contract_type,
    levels = c(1, 2, 3, 4),
    labels = c(
      "Index",
      "Staffel",
      "keine Regelung zur Mieterhöhung",
      "unbekannt"
    )
  )
  )

g.mietverträge_count2 <- namen %>%
  group_by(contract_type) %>%
  summarise(count = n())


#Art des Mietvertrags
g.mietverträge_count2 %>%
  ggplot(aes(x = fct_reorder(contract_type, count, .desc = TRUE), y = count)) +
  geom_bar(stat = "identity",
           color = "skyblue",
           fill = "skyblue",
           width = 0.7) +
  geom_text(aes(label = count),
            size = 3,
            vjust = -0.2) +
  labs(x = "Art des Mietvertrags", y = "Anzahl", title = "Anzahl der verschiedenen Mietverträge") +
  scale_y_continuous(breaks = seq(0, 2500, by= 500)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
