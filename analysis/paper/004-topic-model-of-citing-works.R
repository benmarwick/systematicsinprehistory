
topic_model_of_citing_works_titles_fn <- function(srp_structured_tbl_1){

# generate a topic model from the titles

library(stm)
library(quanteda)
library(tidyverse)

custom_stopwords <- c(
  "looking",
  "archaeology",
  "archaeological",
  "sites",
  "michael",
  "using",
  "introduction",
  "problem",
  "perspectives",
  "de", "la",
  "north",
  "south",
  "east",
  "eastern",
  "west",
  "en",
  "archaeology",
  "archaeological",
  "prehistoric",
  "middle"
)

# prepare titles for topic model by converting to a document feature matrix
title_of_citing_work_dfm <-
srp_structured_tbl_1 %>%
  select(title_of_citing_work) %>%
  pull() %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  tokens_select(min_nchar = 3) %>%
  tokens_remove(c(stopwords("english"),
                  custom_stopwords)) %>%
  dfm()

# set random number for setting seed  to get same result for every run
set.seed(100)
library(Rtsne)

saaFit <- stm(title_of_citing_work_dfm,
              K = 0,
              control = list(tSNE_init.dims = 90),
              max.em.its = 50,
              init.type = "Spectral",
              seed = 100)

library(tidytext)
td_beta2 <- tidy(saaFit)
td_gamma2 <- tidy(saaFit,
                  matrix = "gamma",
                  document_names = rownames(title_of_citing_work_dfm))

library(ggthemes)

top_terms2 <- td_beta2 %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

gamma_terms2 <- td_gamma2 %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms2, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

tm2 <-
  gamma_terms2 %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic,
             gamma,
             label = str_wrap(terms, 55),
             fill = topic)) +
  scale_fill_viridis_d() +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0,
            vjust = 0.5,
            lineheight = .75,
            nudge_y = 0.0005,
            size = 5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "",
       subtitle = "") +
  theme_bw(base_size = 16)


# https://francescocaberlin.blog/2019/07/01/messing-around-with-stm-part-iiib-model-analysis/
ExSem_add <-
  as.data.frame(cbind(
    k = c(1:saaFit$settings$dim$K),
    exclusivity(saaFit),
    semanticCoherence(model = saaFit,
                      title_of_citing_work_dfm)
  ))

colnames(ExSem_add) <- c("k", "Exclusivity", "SemanticCoherence")

ExSem_add_gamma <-
  gamma_terms2 %>%
  mutate(k = parse_number(as.character(topic))) %>%
  left_join(ExSem_add)

library(ggrepel)
plotexcoer <-
  ggplot(ExSem_add_gamma,
         aes(SemanticCoherence,
             Exclusivity,
             colour = gamma,
             size = gamma)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = k),
                  force = 10,
                  bg.color = "white",
                  bg.r = 0.2) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "") +
  scale_colour_viridis_c(guide = "legend") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = c(.75,
                        .255)
  ) +
  theme(legend.title = element_text(size = 6),
        legend.text  = element_text(size = 6),
        legend.key.size = unit(1, "lines"))

library(cowplot)
library(gridGraphics)

# can we do some kind of clustering with t-sne or PCA?
td_gamma2_wide <-
td_gamma2 %>%
  pivot_wider(names_from = topic,
              values_from = gamma)

# PCA
library(FactoMineR)
library(factoextra)

td_gamma2_wide_pca <- PCA(td_gamma2_wide[ , -1],
                          scale.unit = TRUE,
                          graph = FALSE,
                          ncp = 50)
# t-SNA
library(Rtsne)

# compute t-SNE
tsne.norm = Rtsne(td_gamma2_wide_pca$ind$coord ,
                  check_duplicates = FALSE,
                  pca = FALSE)

# add output to raw data
srp_structured_tbl_1_tsne <-
srp_structured_tbl_1 %>%
  mutate(tsne1 = tsne.norm$Y[, 1],
         tsne2 = tsne.norm$Y[, 2])

# identify clusters
library(fpc)
ds.norm = dbscan(tsne.norm$Y,
                 eps = 2,
                 MinPts = 12) # explore this value
srp_structured_tbl_1_tsne$density = factor(ds.norm$cluster)

# what topics are in the clusters?
names(td_gamma2_wide) <- paste0("topic ", names(td_gamma2_wide))

srp_structured_tbl_1_tsne_topics <-
  srp_structured_tbl_1_tsne %>%
  bind_cols(td_gamma2_wide)

srp_structured_tbl_1_tsne_topics_in_clusters <-
  srp_structured_tbl_1_tsne_topics %>%
  filter(density != 0) %>%
  mutate(density = fct_drop(density, "0")) %>%
  select(starts_with("topic"),
         density,
         -`topic document`)

# most important topics in each cluster of the t-SNE
srp_structured_tbl_1_tsne_top_topics_in_clusters <-
  srp_structured_tbl_1_tsne_topics_in_clusters %>%
  group_by(density) %>%
  nest() %>%
  mutate(most_prominent_topic = map(data, ~colSums(.x))) %>%
  mutate(which_topic_max = map_chr(most_prominent_topic, ~names(which.max(.x))))

# join with t-SNE data
srp_structured_tbl_1_tsne_top_topics_in_clusters_for_plot <-
srp_structured_tbl_1_tsne_top_topics_in_clusters %>%
  select(density, which_topic_max) %>%
  right_join(srp_structured_tbl_1_tsne_topics)

# prepare labels
ds.cent = srp_structured_tbl_1_tsne_top_topics_in_clusters_for_plot %>%
  group_by(which_topic_max, density) %>%
  select(tsne1,  tsne2) %>%
  summarize_all(mean) %>%
  drop_na

# plot showing clusters identified by DBSCAN
tsne_plot <-
ggplot(srp_structured_tbl_1_tsne_top_topics_in_clusters_for_plot,
       aes(x = tsne1,
           y = tsne2,
           colour = density)) +
  geom_point(alpha = 0.3,
             size = 3) +
  theme_bw(base_size = 12) +
  geom_label_repel(aes(label = which_topic_max),
                   size = 3,
                   data = ds.cent) +
  guides(colour = FALSE)

# need to make the plot panel big to avoid some 'pin' error
plot_grid(tm2,
          plot_grid(tsne_plot,
                    plotexcoer,
                    nrow = 2,
                    align = c("hv")),
          nrow = 1,
          align = c("hv"),
          axis = c("tb"),
          rel_heights = c(1, 1),
          rel_widths = c(1.1, 0.9)) +
  panel_border(remove = TRUE)

ggsave(here::here('analysis/figures/topic-model-on-titles.jpg'),
       height = 13,
       width = 20,
       dpi = 600)

return(saaFit)

# extract network

# library(tidygraph)
# library(ggraph)
#
# top_n_topics <-
#   gamma_terms2 %>%
#   top_n(30, gamma) %>%
#   pull(topic) %>%
#   as.character() %>%
#   parse_number()
#
# mod.out.corr <- topicCorr(saaFit)
# plot.topicCorr(mod.out.corr)
# plot.topicCorr(mod.out.corr,
#      topics = top_n_topics)
}





