
# I went to https://scholar.google.com/scholar?cites=4127027525661109296&as_sdt=5,48&sciodt=0,48&hl=en
# and manually downloaded each search result page as a single HTML page.
# Inspector Gadget wouldn't work for these pages, so I used the Chrome Inspector to
# figure out the node IDs to extract data from.

prepare_the_data_fn <- function(){

library(tidyverse)
library(rvest)

# read in HTML files

srp <-
  list.files(here::here("analysis/data/raw_data/2021-06-14"),
             full.names = TRUE)

# ".gs_rt" title of citing work
# ".gs_ri" combination of things
# ".gs_a" author, year, journal title
# ".gs_fl" citation count
# ".gs_rs" snippet of text

title_of_citing_work <-
  map(srp,
      ~.x %>%
        read_html %>%
        html_nodes(".gs_rt") %>%
        html_text
  )  %>%
  unlist # 561

# drop title of SIP since we don't want that in here
title_of_citing_work <- title_of_citing_work[title_of_citing_work != "Systematics in prehistory"  ]
# 510 items

author_of_citing_work <-
  map(srp,
      ~.x %>%
        read_html %>%
        html_nodes(".gs_a") %>%
        html_text
  )  %>%
  unlist # 510

citationcount_of_citing_work <-
  map(srp,
      ~.x %>%
        read_html %>%
        html_nodes(".gs_ri") %>% # get full snipped
        html_text
  ) %>%
  unlist

# combine into a tibble
srp_structured_tbl <-
  tibble(author_of_citing_work = unlist(author_of_citing_work),
         title_of_citing_work = unlist(title_of_citing_work),
         citationcount_of_citing_work = citationcount_of_citing_work

  )

# Clean and extract more specific details

srp_structured_tbl_1 <-
  srp_structured_tbl %>%
  mutate(title_of_citing_work = tolower(str_remove_all(title_of_citing_work, "\\[.*\\]"))) %>%
  mutate(author = tolower(str_extract(author_of_citing_work,
                                      ".+?(?=-)"))) %>%
  mutate(author = str_squish(str_extract(author,
                                         " .*"))) %>%
  mutate(author = str_remove_all(author,
                                 " .{1} | .{2} ")) %>%
  mutate(citation_count = parse_number(str_extract(citationcount_of_citing_work,
                                                   "Cited by \\d+"))) %>%
  select(-citationcount_of_citing_work) %>%
  mutate(year_pub = str_extract_all(author_of_citing_work,
                                    "19\\d{2}|20\\d{2}")) %>%
  # some items have two years, like the authors birth year or something, we'll get the second year
  mutate(year_pub = map_chr(year_pub, ~ifelse(length(.x) == 2, .x[2], .x))) %>%
  mutate(year_pub = parse_integer(year_pub)) %>%
  # only items with a publication date after SIP was published, and only items with a date
  filter(year_pub >= 1971)

return(srp_structured_tbl_1)

}
