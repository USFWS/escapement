# USFWS Disclaimer
The United States Fish and Wildlife Service (FWS) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. FWS has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by FWS. The FWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by FWS or the United States Government.

# escapement (*alpha*)
An R package that incudes functions for estimating salmon passage and abundances using photo and video count data based on [Deacy et al. 2016](https://peerj.com/articles/2120)., functions for summarizing and plotting results, and the ability to generate a parameterized `rmarkdown` report.  

# Instructions

The `escapement` package requires several programs to function:  
1. [R](https://cloud.r-project.org/) ($\ge$v4.0)
2. [Rtools 40](https://cran.r-project.org/bin/windows/Rtools/)

To install and load the package:  
`if (!require("devtools")) install.packages("devtools")`  
`devtools::install_github("mccrea-cobb/escapement")`
`library(escapement)`

To create a report:  
`run_report()`
