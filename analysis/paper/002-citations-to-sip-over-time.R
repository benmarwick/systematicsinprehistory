# What is the distribution of citations to SIP over time?

plot_citations_to_sip <-
srp_structured_tbl_1 %>%
  group_by(year_pub) %>%
  tally() %>%
  ggplot() +
  aes(year_pub,
      n) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year",
       y = "Number of citations to 'Systematics' per year") +
  theme_bw(base_size = 12)
