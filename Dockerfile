# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/tidyverse:4.0.3

# required
MAINTAINER Ben Marwick <benmawick@gmail.com>

WORKDIR /systematicsinprehistory
COPY . /systematicsinprehistory

# go into the repo directory
<<<<<<< HEAD
RUN . /etc/environment \
  && ls -asf \
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
=======
RUN  R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
>>>>>>> e41f4a84f5fc42e2ea529f8367ad6cc9385a3600
  && R -e "remotes::install_github('rstudio/renv')" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # run all the code
  && R -e "targets::tar_make()"
