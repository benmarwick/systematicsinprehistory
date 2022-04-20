
<!-- README.md is generated from README.Rmd. Please edit that file -->

# systematicsinprehistory

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/benmarwick/systematicsinprehistory/master?urlpath=rstudio)
[![.github/workflows/run-on-docker.yaml](https://github.com/benmarwick/systematicsinprehistory/actions/workflows/run-on-docker.yaml/badge.svg)](https://github.com/benmarwick/systematicsinprehistory/actions/workflows/run-on-docker.yaml)

This repository contains the data and code for our paper:

> Authors, (2022). Robert C. Dunnell’s ‘Systematics in Prehistory’ at
> 50. *Evolutionary Human Sciences* <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Authors, (2022). Robert C. Dunnell’s ‘Systematics in Prehistory’ at
> 50. SocArxiv, Accessed 20 Apr 2022. Online at
> <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> Authors, (2022). *Compendium of R code and data for Robert C.
> Dunnell’s ‘Systematics in Prehistory’ at 50*. Accessed 20 Apr 2022.
> Online at <https://doi.org/xxx/xxx>

## Contents

The most important parts of this compendium are:

-   [:dart: \_targets.R](_targets.R): workflow instructions and
    information indicating the order that code needs to be run to
    generate the results. Run `targets::tar_make()` at the R console to
    start the analysis workflow.  
-   [:file_folder: analysis/paper](/analysis/paper): R Markdown source
    document for manuscript, and R script files. Includes code to
    reproduce the figures and tables generated by the analysis. It also
    has a rendered version, `paper.docx`, suitable for reading (the code
    is replaced by figures and tables in this file)
-   [:file_folder: analysis/data](/analysis/data): Data used in the
    analysis.
-   [:file_folder: analysis/figures](/analysis/figures): Plots and other
    illustrations

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/benmarwick/systematicsinprehistory/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses rocker-project.org
Docker images to ensure a consistent and reproducible computational
environment. These Docker images can also be used locally.

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping you can run our code
by followinfg these steps:

1.  open the `.Rproj` file in RStudio  
2.  run `renv::restore()` to download and install the packages required
    to run our code  
3.  run `targets::tar_make()` to run our analysis R code in the
    `analysis` directory in order, the final step of this will be
    knitting the R Markdown document to produce our submitted
    manuscript, `paper.docx`

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
