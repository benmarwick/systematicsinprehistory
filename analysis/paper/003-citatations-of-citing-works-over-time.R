# What is the distribution of citations of works that cites SIP?

top_five_cited <-
  srp_structured_tbl_1 %>%
  arrange(desc(citation_count)) %>%
  top_n(5, citation_count) %>%
  select(title_of_citing_work,
         citation_count) %>%
  rename(`Top five cited items` = title_of_citing_work,
         `Citation count` = citation_count) %>%
  mutate(`Top five cited items` = str_to_sentence(`Top five cited items`))

library(ggpmisc)
plot_citations_of_citing_works <-
srp_structured_tbl_1 %>%
  ggplot() +
  aes(citation_count) +
  geom_histogram() +
  labs(x = "Citations",
       y = "Count") +
  scale_x_log10() +
  ylim(0, 45) +
  theme_bw(base_size = 8) # +
  #annotate(geom = "table",
  #         x = 1,
  #         y = 45,
  #         label = list(top_five_cited),
  #         vjust = 1,
  #         hjust = 0)

library(cowplot)
ggdraw(plot_citations_to_sip) +
  draw_plot(plot_citations_of_citing_works,
            .2,   # x-pos
            .65,  # y-pos
            .6,   # width
            .3)   # height

