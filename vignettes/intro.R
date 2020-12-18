## ----setup, include = FALSE, message=FALSE------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(escapement)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  if (!require("devtools")) install.packages("devtools")
#  devtools::install_github("mccrea-cobb/escapement", ref = "master", build_vignettes = TRUE)
#  library(escapement)

## -----------------------------------------------------------------------------
dat <- import_format(system.file("extdata", "salmon_counts.csv", package = "escapement"))
head(dat)

## -----------------------------------------------------------------------------
our_models <- model_escapement(dat)

## -----------------------------------------------------------------------------
summary(our_models)

## -----------------------------------------------------------------------------
summary(our_models$models$linear)

## -----------------------------------------------------------------------------
our_models$aic_table

## -----------------------------------------------------------------------------
our_models$top_model

## ---- message=FALSE-----------------------------------------------------------
boots <- boot_escapement(dat, our_models)
summary(boots)

## -----------------------------------------------------------------------------
boots$summary

## ---- fig.height=5, fig.width=6.5---------------------------------------------
p <- explore_plots(dat)
p$photo_counts
p$hourly_counts

## ---- fig.height=4, fig.width=6.5---------------------------------------------
plot_topmodel(our_models)

## ---- fig.height=4, fig.width=6.5---------------------------------------------
p <- plot_boot_escapement(boots)
p$density_estimates

## ---- fig.height=4, fig.width=6.5---------------------------------------------
p$min_escape

## ---- fig.height=4, fig.width=6.5---------------------------------------------
p <- plot_daily(dat, our_models)
p$daily

## ---- fig.height=4, fig.width=6.5---------------------------------------------
p$cumul_daily

## ---- eval=FALSE--------------------------------------------------------------
#  run_report()

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("../inst/rmd/images/report_interface.jpg")

## -----------------------------------------------------------------------------
system.file("rmd", "bibliography.bibtex", package = "escapement")

