# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/tidyverse:4.0.3

# required
MAINTAINER Ben Marwick <benmawick@gmail.com>

COPY . /systematicsinprehistory

# go into the repo directory
RUN . /etc/environment/ \
  && ls -asf \
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github('rstudio/renv')" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # run all the code
  && R -e "targets::tar_make()"
