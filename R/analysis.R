## ----
## @knitr model_selection

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
#' @param output a file directory path to a list of objects model selection results returned by \code{model_selection}
#' 
#' @return a list of objects including residual plots, a score test for non-constant error variance (a code{chisqTest} object) returned by \code{car::ncvTest()}, a spread level plot (object of class \code{spreadLevelPlot}) returned by \code{car::spreadLevelPlot()}, influence measures returned by \code{influence.measures()}, and Cook's distances returned by \code{cooks.distance()} 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' model_diagnostics(output = "path/to/your/model_diagnostics.Rdata")
#' }
model_diagnostics <- function(output = "./output/models/model_diagnostics.Rdata"){
  
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

#' Estimate annual escapement of salmon based on the top model from \code{model_escapement()}
#' 
#' @param input_data
#' @param model_results
#' @param saveit
#' @param output
#' 
#' @return 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' estimate_escapement()
#' }
estimate_escapement <- function(input_data = "./data/derived/dat.Rdata",
                                model_results = "./output/models/model_results.Rdata",
                                saveit = FALSE,
                                output) {
  
  dat <- loadRdata(input_data)
  model_results <- loadRdata(model_results)
  
  if(model_results$aic_table$Modnames[[1]] == "Linear"){
    # Top model (Linear model):
    dat$escape <- dat$photo * model_results$top_model$coefficients[[1]]   # calculates the fitted number of upward passing fish
  }
  
  if(model_results$aic_table$Modnames[[1]] == "Polynomial"){
    dat$escape <- dat$photo * model_results$top_model$coefficients[[1]]   # calculates the fitted number of upward passing fish
  }
  
  if(model_results$aic_table$Modnames[[1]] == "Segmented"){
    
  }
  
  if(model_results$aic_table$Modnames[[1]] == "Segmented polynomial"){
    
  }
  
  # total annual escapement
  annual_escapement <- dat %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(sum(escape, na.rm = T))
  
  # summarize salmon passage data by day
  daily_passage <- dat %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    summarize(escape_daily = sum(escape))
  
  plots <- list()
  # Plot estimated daily min. salmon passage 
  plots[["daily_passage"]] <- dat %>%
    group_by(date_x, year) %>%
    summarize(escape = sum(escape)) %>%
    ggplot(aes(date_x, escape)) +
    geom_line() +
    labs(x = "Date", 
         y = "Salmon passage") +
    facet_wrap(. ~ year, 
               scales = "fixed", 
               ncol = 1) +
    scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))
  # ggsave(p.daily.passage, file = "./output/plots/daily_passage.jpg")
  
  # Plots hourly passage within a day
  plots[["hourly_passage"]] <- dat %>%
    mutate(hour = lubridate::hour(date)) %>%
    group_by(hour, year) %>%
    summarize(escape = sum(escape, na.rm = T)) %>%
    ggplot(aes(hour, escape)) +
    geom_line() +
    labs(x="Hour of day", 
         y = "Salmon passage") +
    facet_wrap(. ~ year, 
               scales = "free_y", 
               ncol = 1)
}



## ----
## @knitr boot

#' Calculate bootstrapped 95% confidence intervals for salmon escapement estimates
#'
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
boot_escapement <- function() {
  
  # Load the data
  dat <- loadRdata("./data/derived/dat.Rdata")
  datud <- dat[!is.na(dat$video), ]
  
  bootit <- function(year1) {
    # Subset data by a given year
    dat.year <- dat %>%
      mutate(year = lubridate::year(date)) %>%
      subset(year == year1)
    
    # function to bootstrap
    tot <- function(data, indices, formula, dat) {
      d <- datud[indices, ] # allows boot to select sample 
      fit <- lm(formula, data = d)
      pred.vid <- predict(fit, dat)
      return(sum(pred.vid, na.rm = TRUE))
    } 
    
    # run a bootstrap
    tictoc::tic("Total time")
    message("Bootstrapping...")
    results <- boot::boot(data = datud, 
                          statistic = tot, 
                          R = 10000, 
                          formula = video~photo-1, 
                          dat = dat.year)
    message("Done.")
    tictoc::toc()
    return(results)
  }
  
  years <- unique(lubridate::year(dat$date))
  foo <- lapply(years, function(x) {
    bootit(x)})
  
  
  # Run it:
  results_2015 <- bootit(2015)
  results_2015_ci <- boot.ci(results_2015, 
                             type="perc", 
                             conf = 0.95) # 95% CI values, using percentile method
  results_2016 <- bootit(2016)
  results_2016_ci <- boot.ci(results_2016, 
                             type="perc", 
                             conf = 0.95) # 95% CI values, using percentile method
  results_2017 <- bootit(2017)
  results_2017_ci <- boot.ci(results_2017, 
                             type="perc", 
                             conf = 0.95) # 95% CI values, using percentile method
  results_2018 <- bootit(2018)
  results_2018_ci <- boot.ci(results_2018, 
                             type="perc", 
                             conf = 0.95) # 95% CI values, using percentile method
  
  # Create a data frame of the results
  results <- data.frame("year" = c(2015, 2016, 2017, 2018),
                        "escapement" = c(results_2015$t0, 
                                         results_2016$t0, 
                                         results_2017$t0, 
                                         results_2018$t0),
                        "lower_ci" = c(results_2015_ci$percent[4], 
                                       results_2016_ci$percent[4], 
                                       results_2017_ci$percent[4],
                                       results_2018_ci$percent[4]),
                        "upper_ci" = c(results_2015_ci$percent[5], 
                                       results_2016_ci$percent[5], 
                                       results_2017_ci$percent[5],
                                       results_2017_ci$percent[5])
  )
  # save(results, file = "output/tables/results.RData")
  
  return(results)
}