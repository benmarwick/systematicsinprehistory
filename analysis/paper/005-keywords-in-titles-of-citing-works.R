# What are the keywords in the titles of works that cite SIP?

library(tidytext)
title_word_list <-
  srp_structured_tbl_1 %>%
  mutate(article = title_of_citing_work) %>%
  select(article, title_of_citing_work) %>%
  unnest_tokens(word, title_of_citing_work) %>%
  filter(!word %in% c(stop_words$word,
                      "de", "la",
                      "archaeology",
                      "archaeological",
                      "prehistoric"))

plot_keywords <-
title_word_list %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  slice(1:50) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(y = "Number of citing works",
       x = "") +
  coord_flip() +
  theme_bw(base_size = 12)

library(cowplot)
ggdraw(plot_keywords) +
  draw_plot(plot_langs,
            .55,  # x-pos
            .15,  # y-pos
            .4,  # width
            .4)  # height
