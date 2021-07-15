[![Build Status](https://travis-ci.com/USFWS/escapement.png)](https://travis-ci.com/USFWS/escapement)

# escapement
An R package that includes functions for estimating salmon passage and abundances using photo and video count data based on [Deacy et al. 2016](https://peerj.com/articles/2120)., functions for summarizing and plotting results, and the ability to generate a parameterized [rmarkdown](https://rmarkdown.rstudio.com/) report.  

# USFWS Disclaimer
The United States Fish and Wildlife Service (FWS) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. FWS has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by FWS. The FWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by FWS or the United States Government.

# Instructions

The `escapement` package requires several programs to function:  

1. [R](https://cloud.r-project.org/) (>=4.0)  

2. [Rtools 40](https://cran.r-project.org/bin/windows/Rtools/)  

3. the `tinyTex` R package. To install it from R:  
      a. `if (!require("devtools")) install.packages("devtools")`  
      c. `tinytex::install_tinytex()`

To install and load the escapement R package:  
`if (!require("devtools")) install.packages("devtools")`  
`devtools::install_github("USFWS/escapement", ref = "master", build_vignettes = TRUE)`  
`library(escapement)`  

To learn more about the escapement package, read introductory vignette:
`vignette(topic = "intro", package = "escapement")`

To create a report:  
`run_report()` 
