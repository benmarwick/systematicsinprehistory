
keywords_and_langs_of_citing_works_fn <- function(srp_structured_tbl_1){


library(cld2)
# what language are the titles of the citing works?

srp_structured_tbl_1_lang <-
  srp_structured_tbl_1 %>%
  mutate(title_clean = str_squish(str_remove_all(title_of_citing_work, "[[:punct:]]"))) %>%
  mutate(lang = cld2::detect_language(title_clean, lang_code = FALSE))

# tally
srp_structured_tbl_1_lang_tally <-
  srp_structured_tbl_1_lang %>%
  mutate(lang = case_when(
    lang == "PORTUGUESE" ~ "SPANISH",
    lang == "GALICIAN" ~ "SPANISH",
    lang == "CHINESET" ~ "CHINESE",
    TRUE ~ as.character(lang)
  )) %>%
  mutate(lang = str_to_sentence(lang)) %>%
  group_by(lang) %>%
  tally(sort = TRUE) %>%
  drop_na() %>%
  filter(!lang %in% c("Lithuanian", "Afrikaans"))

# iterate over the tally...
# what seems not right?
# LITHUANIAN, AFRIKAANS

# What seems the same?
# GALICIAN, PORTUGUESE, SPANISH
library(scales)
plot_langs <-
  ggplot(srp_structured_tbl_1_lang_tally) +
  aes(n,
      reorder(lang, n)) +
  geom_col() +
  labs(x = "Number of citing works",
       y = "") +
  scale_x_continuous(trans= pseudo_log_trans(sigma = 10, base = 10)) +
  theme_bw(base_size = 12)


# What are the keywords in the titles of works that cite SIP?

library(tidytext)
title_word_list <-
  srp_structured_tbl_1 %>%
  mutate(article = title_of_citing_work) %>%
  select(article, title_of_citing_work) %>%
  unnest_tokens(word, title_of_citing_work) %>%
  filter(!word %in% c(stop_words$word,
                      "de", "la",
                      "north",
                      "south",
                      "east",
                      "eastern",
                      "west",
                      "en",
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
            .425,  # x-pos
            .17,  # y-pos
            .55,  # width
            .4)  # height

ggsave(here::here("analysis/figures/languages-and-keywords-in-titles-of-citing-works.png"),
       h = 10,
       w = 10)

}
