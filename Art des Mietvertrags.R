#Namen der verschiedenen Mietverträge
pd <- pricedrivers %>%
  mutate(contract_type = factor(
      contract_type,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Index",
        "Staffel",
        "keine Regelung",
        "unbekannt"
      )
    )
  )


#genaue Anzahl
g.mietverträge <- pd
g.mietverträge_count <- g.mietverträge %>%
  select(nmqm, ovm21, contract_type) %>%
  filter(ovm21 < nmqm) %>%
  group_by(contract_type) %>%
  summarise(count = n())


#Art des Mietvertrags
g.mietverträge %>% 
  select(nmqm, ovm21, contract_type) %>% 
  filter(ovm21 < nmqm) %>%
  ggplot(aes(x = fct_infreq(contract_type))) +
  geom_bar(color = "skyblue",
           fill = "skyblue",
           width = 0.7) +
  geom_text(data = g.mietverträge_count,
            aes(x = contract_type, y = count, label = count), 
            size = 3.5,
            vjust = -0.5) +
  labs(x = "Art des Mietvertrags", y = "Anzahl", title = "Anzahl der verschiedenen Mietverträge") +
  scale_y_continuous(breaks = seq(0, 1750, by= 250)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10))
