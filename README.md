[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/USFWS/escapement/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USFWS/escapement/actions/workflows/R-CMD-check.yaml)

escapement
==========

## Overview

An R package that includes functions for estimating salmon passage and abundances using photo and video count data based on [Deacy et al. 2016](https://peerj.com/articles/2120)., functions for summarizing and plotting results, and the ability to generate a parameterized [rmarkdown](https://rmarkdown.rstudio.com/) report.  

## Installation

The `escapement` package requires several programs to function:  

1. [R](https://cloud.r-project.org/) (>=4.0)  

2. [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (>=40)

3. the `tinyTex` R package. To install it from R:  
      a. `if (!require("tinytex")) install.packages("tinytex")`  
      b. `tinytex::install_tinytex()`

To install and load the escapement R package:

```
if (!require("pak")) install.packages("pak")
pak::pkg_install("USFWS/escapement") 
library(escapement)
```

## Usage

To get started using `escapement`, read the [Getting Started](https://usfws.github.io/escapement/articles/intro.html) article in the [GitHub page](https://usfws.github.io/escapement/index.html).  

## Getting help

You can view a help file in R for each function with `?{function-name}. Contact the project maintainer for additional help with this R package. 

## Contribute

Contact the project maintainer for information about contributing to this repository template. Submit a [GitHub Issue](https://github.com/USFWS/r7-repo-template/issues) to report a bug or request a feature or enhancement.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 1.0 International
License](https://creativecommons.org/licenses/by/1.0/).
