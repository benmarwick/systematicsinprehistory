
# load libraries we need here
library(targets)
library(tarchetypes) # for tar_render

# load the functions from our script files
my_script_files <- list.files(here::here("analysis/paper"),
                              pattern = ".R$",
                              full.names = TRUE)
purrr::walk(my_script_files, source)

# load libraries used in all functions
tar_option_set(packages = c("tidyverse", "quanteda"))

list(
  # prepares the google scholar data ready for analysis
  tar_target(prepare_the_data,        # name of target
             prepare_the_data_fn()),  # name of function to run

  # prepares citations over time plot
  tar_target(citations_over_time,                        # name of target
             citations_over_time_fn(prepare_the_data)),  # name of function to run

  # prepares keywords and langs of citing works plot
  tar_target(keywords_and_langs_of_citing_works,                        # name of target
             keywords_and_langs_of_citing_works_fn(prepare_the_data)),  # name of function to run

  # prepares topic model plot
  tar_target(topic_model_of_citing_works_titles,                        # name of target
             topic_model_of_citing_works_titles_fn(prepare_the_data)),  # name of function to run

  # knit Rmd to produce word doc of manuscript
  tarchetypes::tar_render(paper,
                          here::here("analysis/paper/paper.Rmd"))

  # end of the targets
  # run
  # targets::tar_make()
  # to run all code and knit Rmd file to Word docx
)
