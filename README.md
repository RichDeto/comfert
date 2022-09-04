# comfert

<!-- badges: start -->

[![CRAN/METACRAN Version](https://www.r-pkg.org/badges/version/comfert)](https://CRAN.R-project.org/package=comfert)
[![CRAN/METACRAN Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/comfert?color=blue)](https://CRAN.R-project.org/package=comfert)
[![CRAN/METACRAN downloads per month](https://cranlogs.r-pkg.org/badges/comfert?color=orange)](https://CRAN.R-project.org/package=comfert)
[![R build status](https://github.com/RichDeto/comfert//workflows/R-CMD-check/badge.svg)](https://github.com/RichDeto/comfert/actions)
<!-- badges: end -->


## Intro

This repository is based on Ciganda, D. & Todd, N. (2021) "Demographic Models of the Reproductive Process: Past, Interlude, and Future". Population Studies. <https://doi.org/10.1080/00324728.2021.1959943>. And try to bring it to a R package, where the main function is `comfert()` where the model itself is implemented.

The function `main_local()` allows you to runs the estimation algorithm in a "local"
computing cluster (without a job scheduler), and the function `main_remote()` manages the computation in a remote (external) cluster running on Slurm.


In the data folder are all the files you need to try this method with all the dataset for Spain example.

## Install

CRAN version 

``` r 
install.packages('comfert') 

library(comfert)
```

Development version

``` r

# install.packages('devtools') 
# If you don´t have devtools package installed

devtools::install_github("RichDeto/comfert")
library(comfert)
```
