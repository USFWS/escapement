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
our_models <- model_escapement(dat)
summary(our_models$models$linear)

## -----------------------------------------------------------------------------
our_models$aic_table

## -----------------------------------------------------------------------------
our_models$top_model

