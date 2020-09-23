## ----
## @knitr model_escapement

#' Model salmon escapement using photo and video counts
#'
#' @param input a directory path to an Rdata file containing formatted salmon count data returned by \code{import_format()}
#' @param saveit whether to save the results as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned results
#'
#' @return a list containing a list of model output (\code{models}), a data frame of associated AIC values (\code{aic_table}), and the top model based on AIC (\code{top_model})
#'
#' @export
#'
#' @import segmented
#' @import AICcmodavg
#'
#' @examples
#' \dontrun{
#' model_escapement(input = "./data/derived/dat.Rdata")
#' }
model_escapement <- function(input = "./data/derived/dat.Rdata",
                             saveit = FALSE,
                             output = "./output/models/models.Rdata"){

  loadRData(input)
  datud <- dat[!is.na(dat$video), ]  # remove rows containing no video data

  # Linear models:
  models <- list()
  models[["linear"]] <- lm(video ~ photo - 1,
                           data = datud)  # Linear model
  models[["polynomial"]] <- lm(video ~ photo + I(photo^2) - 1,
                               data = datud)   # Polynomial model

  # Segmented models
  models[["segmented"]] <- segmented(models$linear,
                                     seg.Z = ~photo,
                                     psi = 0,
                                     control = seg.control(it.max = 0)) # Segmented model
  models[["segmented_polynomial"]] <- segmented(models$polynomial,
                                                seg.Z = ~photo,
                                                psi = 0,
                                                control = seg.control(it.max = 0)) # Segmented polynomial model

  modnames <- c("Linear", "Polynomial", "Segmented", "Segmented polynomial")

  # AICc table:
  aic_table <- AICcmodavg::aictab(cand.set = models,
                                  modnames = modnames,
                                  sort = TRUE)

  # Define the best model based on the lowest AICc
  top_model <- models[[which.min(sapply(1:length(models), function (x) AIC(models[[x]])))]]

  model_results <- list("models" = models,
                        "aic_table" = aic_table,
                        "top_model" = top_model)

  if(saveit == TRUE) {
    save(model_results,
         file = output)
  }
  return(model_results)
}



## ----
## @knitr model_diagnostics

#' Model diagnostics for salmon escapement models from photo and video count data
#'
#' @param models a file directory path to a list of model output returned from \code{model_escapement}.
#' @param output a file directory path to a list of objects model selection results returned by \code{model_selection}
#'
#' @return a list of objects including residual plots, a score test for non-constant error variance (a code{chisqTest} object) returned by \code{car::ncvTest()}, a spread level plot (object of class \code{spreadLevelPlot}) returned by \code{car::spreadLevelPlot()}, influence measures returned by \code{influence.measures()}, and Cook's distances returned by \code{cooks.distance()}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model_diagnostics(models = ./output/models/models.Rdata", output = "path/to/your/model_diagnostics.Rdata")
#' }
model_diagnostics <- function(models = "./output/models/models.Rdata",
                              output = "./output/models/model_diagnostics.Rdata"){

  load(models)

  # Assess normality
  car::qqPlot(models$top_model,
              simulate = TRUE,
              main = "Q-Q Plot")

  residplot <- function(fit,
                        nbreaks = 10) {
    # residplot function (only have to run this part once,
    # then can use the "residplot()" function afterwards)
    z <- rstudent(fit)
    hist(z,
         breaks = nbreaks,
         freq = FALSE,
         xlab = "Studentized Residual",
         main = "Distribution of Errors")
    rug(jitter(z),
        col = "brown")
    curve(dnorm(x,
                mean = mean(z),
                sd = sd(z)),
          add = TRUE,
          col = "blue",
          lwd = 2)
    lines(density(z)$x,
          density(z)$y,
          col = "red",
          lwd = 2,
          lty = 2)
    legend("topright",
           legend = c( "Normal Curve", "Kernel Density Curve"),
           lty = 1:2,
           col = c("blue","red"),
           cex = .7)
  }

  resids <- residplot(models$top_model)

  # Assess constancy of error variance (homoskedasticity)
  ncv_test <- car::ncvTest(models$top_model)
  spread_level <- car::spreadLevelPlot(models$top_model)

  # Influence and leverage tools
  influ_measures <- influence.measures(models$top_model)
  cooks_dist <- cooks.distance(models$top_model)

  model_diagnostics <- list(resids,
                            ncv_test,
                            spread_level,
                            influ_measures,
                            cooks_dist)
  return(model_diagnostics)
}



## ----
## @knitr estimate_escapement

#' Estimate escapement of salmon based on the top model from \code{model_escapement()}
#'
#' @param input a directory path to an Rdata file containing formatted salmon count data returned by \code{import_format()}
#' @param models a file directory path to a list of model output returned from \code{model_escapement}.
#' @param saveit whether to save the results as a RData object
#' @param output a vector of estimated hourly salmon escapement
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' estimate_escapement()
#' }
estimate_escapement <- function(input = "./data/derived/dat.Rdata",
                                models = "./output/models/model_results.Rdata",
                                saveit = FALSE,
                                output) {

  dat <- loadRdata(input)
  model_results <- loadRdata(models)

  if(model_results$aic_table$Modnames[[1]] == "Linear"){
    # Top model (Linear model):
    escapement <- dat$photo * model_results$top_model$coefficients[[1]]   # calculates the fitted number of upward passing fish
  }

  if(model_results$aic_table$Modnames[[1]] == "Polynomial"){
    escapement <- dat$photo * model_results$top_model$coefficients[[1]]   # calculates the fitted number of upward passing fish
  }

  if(model_results$aic_table$Modnames[[1]] == "Segmented"){

  }

  if(model_results$aic_table$Modnames[[1]] == "Segmented polynomial"){

  }

  return(escapement)
}


## ----
## @knitr annual_escapement

#' Estimate annual salmon escapement
#'
#' @param escapement a numeric vector of hourly salmon escapement estimates
#' @param date_time a POSIXct vector of associated dates and times
#'
#' @return a data frame of annual salmon escapement estimates, grouped by year
#'
#' @import tidyverse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' annual_escapement()
#' }
annual_escapement <- function(escapement,
                              date_x,
                              ...){

  dat <- data.frame(date_time == "date",
                    escapement == "escapement")

  ae <- dat %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(sum(escapement, na.rm = T))

  return(ae)
}



## ----
## @knitr daily_passage

#' Estimate daily salmon passage
#'
#' @param escapement a numeric vector of hourly salmon escapement estimates
#' @param date_time a POSIXct vector of associated dates and times
#'
#' @return a data frame of daily salmon escapement estimates, grouped by day
#'
#' @import tidyverse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' daily_passage()
#' }
daily_passage <- function(escapement,
                          date_time){

  dat <- data.frame(date_time == "date",
                    escapement == "escapement")

  daily_passage <- dat %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    summarize(escape_daily = sum(escape))
}



## ----
## @knitr boot_escapement

#' Calculate bootstrapped 95% confidence intervals around salmon escapement estimates
#'
#' @param input
#' @param ... pass additional arguments to/from \code{bootit()} and \code{boot}
#' @return
#'
#' @export
#'
#' @import boot
#' @import tidyverse
#' @import ggpubr
#' @import tictoc
#'
#' @examples
boot_escapement <- function(input = "./data/derived/dat.Rdata") {

  # Load the data
  dat <- loadRdata(input)
  datud <- dat[!is.na(dat$video), ]

  years <- unique(lubridate::year(dat$date))

  e <- lapply(years, function(x) {  # estimate annual mean escapement
    bootit(x, model = "linear")})

  cis <- lapply(e, function(x){  # estimate CIs around annual mean escapements
    boot::boot.ci(x,
                  type = "perc",
                  conf = 0.95) # 95% CI values, using percentile method)
  })

  # Create a data frame of the annual escapement with 95% CIs
  year <- unique(dat$year)
  escapement <- sapply(e, function(x){
    x$t0
  })
  lower_ci <- sapply(cis, function(x){
    x$percent[4]
  })
  upper_ci <- sapply(cis, function(x){
    x$percent[5]
  })

  results <- data.frame(year,
                        lower_ci,
                        upper_ci)
  return(results)
}
