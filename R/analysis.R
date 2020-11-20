## ----
## @knitr model_escapement

#' Model salmon escapement using photo and video counts
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param saveit whether to save the results as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned results
#'
#' @return a list containing a list of model output (\code{models}), a data frame of associated AIC values (\code{aic_table}), and the top model based on AIC (\code{top_model})
#'
#' @export
#'
#' @import segmented
#' @importFrom AICcmodavg aictab
#'
#' @examples
#' \dontrun{
#' model_escapement(input = "./data/derived/dat.Rdata")
#' }
model_escapement <- function(dat,
                             saveit = FALSE,
                             output){

  # Remove rows containing no video and no photo data
  datud <- dat[!is.na(dat$photo) | !is.na(dat$video),]

  # Linear models:
  models <- list()
  models[["linear"]] <- lm(video ~ photo - 1,
                           data = datud)  # Linear model
  models[["polynomial"]] <- lm(video ~ photo + I(photo^2) - 1,
                               data = datud)   # Polynomial model

  # Segmented models
  models[["segmented"]] <- segmented::segmented(models$linear,
                                                seg.Z = ~photo,
                                                psi = 0,
                                                control = seg.control(it.max = 0)) # Segmented model
  models[["segmented_polynomial"]] <- segmented::segmented(models$polynomial,
                                                           seg.Z = ~photo,
                                                           psi = 0,
                                                           control = seg.control(it.max = 0)) # Segmented polynomial model

  modnames <- c("Linear", "Polynomial", "Segmented", "Segmented polynomial")

  # AICc table:
  aic_table <- AICcmodavg::aictab(cand.set = models,
                                  modnames = modnames,
                                  sort = TRUE)

  # Define the best model based on the lowest AICc
  top_model <- models[[which.min(sapply(1:length(models), function (x) stats::AIC(models[[x]])))]]

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
#' @param models a list containing the model output returned from \code{model_escapement}.
#'
#' @return a list of objects including residual plots, a score test for non-constant error variance (a code{chisqTest} object) returned by \code{car::ncvTest()}, a spread level plot (object of class \code{spreadLevelPlot}) returned by \code{car::spreadLevelPlot()}, influence measures returned by \code{influence.measures()}, and Cook's distances returned by \code{cooks.distance()}
#'
#' @import ggplot2
#' @importFrom car ncvTest spreadLevelPlot
#'
#' @examples
#' \dontrun{
#' model_diagnostics(models)
#' }
model_diagnostics <- function(models){

  residplot <- function(fit,
                        nbreaks = 20) {
    # residplot function (only have to run this part once,
    # then can use the "residplot()" function afterwards)
    z <- data.frame(stats::rstudent(models$top_model))

    p <- ggplot2::ggplot(data = z, aes(x=z[[1]], y=..density..)) +
      geom_histogram() +
      geom_density() +
      labs(x = "Studentized Residuals")
    return(p)
  }

  resids <- residplot(models$top_model)

  # Assess constancy of error variance (homoskedasticity)
  ncv_test <- car::ncvTest(models$top_model)
  spread_level <- car::spreadLevelPlot(models$top_model)

  # Influence and leverage tools
  influ_measures <- stats::influence.measures(models$top_model)
  cooks_dist <- stats::cooks.distance(models$top_model)

  model_diagnostics <- list("resids" = resids,
                            "ncv_test" = ncv_test,
                            "spread_level" = spread_level,
                            "influ_measures" = influ_measures,
                            "cooks_dist" = cooks_dist)
  return(model_diagnostics)
}



## ----
## @knitr hourly_passage

#' Estimate hourly passage and cumulative hourly passage of salmon based on the top model from \code{model_escapement()}
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a data frame containing hourly salmon passage and cumulative hourly salmon passage
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by mutate ungroup select
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hourly_passage(dat, models)
#' }
hourly_passage <- function(dat,
                           models) {
  dat$U1.photo <- ifelse(dat$photo < 0, 0, dat$photo)  # adds a new variable (U1.photo) that replaces negative photo counts with zero (for predicting segmented models)
  hourly <- stats::predict(models$top_model, dat)   # calculates the fitted number of upward passing fish
  dat_hourly <- data.frame(dat,
                          passage = hourly)
  dat_hourly <- dat_hourly %>%
    dplyr::group_by(factor(year)) %>%
    dplyr::mutate(cumul_passage = cumsum(ifelse(is.na(passage), 0, passage)) + passage * 0) %>%
    dplyr::ungroup() %>%
    select(-c(photo, video, U1.photo, julian_day, year))

  return(dat_hourly)
}



## ----
## @knitr daily_passage

#' Estimate daily passage and cumulative daily passage of salmon based on the top model from \code{model_escapement()}
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a data frame containing daily salmon passage and cumulative daily salmon passage
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom lubridate year
#'
#' @export
#'
#' @examples
#' \dontrun{
#' daily_passage(dat, models)
#' }
daily_passage <- function(dat,
                           models) {
  dat_hourly <- hourly_passage(dat, models)
  dat_daily <- dat_hourly %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(passage = sum(passage)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::mutate(cumul_passage = cumsum(ifelse(is.na(passage), 0, passage)) + passage * 0) %>%
    dplyr::ungroup() %>%
    select(-year)

  return(dat_daily)
}



## ----
## @knitr escapement

#' Estimate annual salmon escapement
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a data frame of annual salmon escapement estimates, grouped by year
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize ungroup
#' @importFrom lubridate year
#'
#' @export
#'
#' @examples
#' \dontrun{
#' escapement(passage, dat$date)
#' }
escapement <- function(dat,
                       models){

  dat_hourly <- hourly_passage(dat, models)

  ae <- dat_hourly %>%
    dplyr::group_by(lubridate::year(date)) %>%
    dplyr::summarize(escapement = sum(passage, na.rm = T)) %>%
    dplyr::ungroup()

  return(ae)
}



## ----
## @knitr boot_escapement

#' Calculate bootstrapped 95 percent confidence intervals around salmon escapement estimates
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a list containing a summary of the bootstrapped estimates (\code{summary}) and the raw bootstrapped results (\code{raw_boots})
#'
#' @export
#'
#' @importFrom lubridate year
#' @importFrom boot boot.ci
#'
#' @examples
#' \dontrun{
#' boot_escapement(dat, models)
#' }
boot_escapement <- function(dat,
                            models) {

  year <- unique(lubridate::year(dat$date))

  boot_est <- lapply(year, function(x) {  # estimate annual mean escapement
    bootit(year1 = x,
           dat = dat,
           models = models)
  })
  names(boot_est) <- year

  cis <- lapply(boot_est, function(x){  # estimate CIs around annual mean escapements
    boot.ci(x,
            type = "perc",
            conf = 0.95) # 95% CI values, using percentile method)
  })
  names(cis) <- year

  # Create a data frame of the annual escapement with 95% CIs
  escapement <- sapply(boot_est, function(x){
    x$t0
  })
  lower_ci <- sapply(cis, function(x){
    x$percent[4]
  })
  upper_ci <- sapply(cis, function(x){
    x$percent[5]
  })

  summary <- data.frame(year,
                        escapement,
                        lower_ci,
                        upper_ci)
  rownames(summary) <- NULL

  raw_boots <- list("boot_est" = boot_est,
                    "cis" = cis)
  results <- list("summary" = summary,
                  "raw_boots" = raw_boots)
  return(results)
}
