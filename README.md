[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

# escapement

## Overview

An R package that includes functions for estimating salmon passage and abundances using photo and video count data based on [Deacy et al. 2016](https://peerj.com/articles/2120)., functions for summarizing and plotting results, and the ability to generate a parameterized [rmarkdown](https://rmarkdown.rstudio.com/) report.  

## Installation

The `escapement` package requires several programs to function:  

1. [R](https://cloud.r-project.org/) (>=4.0)  

2. [Rtools 40](https://cran.r-project.org/bin/windows/Rtools/)  

3. the `tinyTex` R package. To install it from R:  
      a. `if (!require("tinytex")) install.packages("tinytex")`  
      b. `tinytex::install_tinytex()`

To install and load the escapement R package:  
`if (!require("devtools")) install.packages("devtools")`  
`devtools::install_github("USFWS/escapement", ref = "master", build_vignettes = TRUE)`  
`library(escapement)`  

## Usage

To get started using `escapement`, read the [Getting Started](https://usfws.github.io/escapement/articles/intro.html) article in the [GitHub page](https://usfws.github.io/escapement/index.html).  

## Getting help

You can view a help file in R for each function with `?{function-name}. Contact the project maintainer for additional help with this R package. 

## Contribute

Contact the project maintainer for information about contributing to this repository template. Submit a [GitHub Issue](https://github.com/USFWS/r7-repo-template/issues) to report a bug or request a feature or enhancement.
