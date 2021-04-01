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
plot_langs <-
ggplot(srp_structured_tbl_1_lang_tally) +
  aes(n,
      reorder(lang, n)) +
  geom_col() +
  labs(x = "Number of citing works",
       y = "") +
  theme_bw(base_size = 10)

