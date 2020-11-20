
## ----
## @knitr exploratory_plots

#' Create exploratory plot of raw salmon photo counts
#' @description Creates a list of exploratory plots of raw salmon photo counts. This includes a plot of daily photo counts grouped by year, and a plot of hourly photo counts grouped by year.
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#'
#' @return a list of \code{ggplot2} plots
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' explore_plots(dat, saveit = FALSE)
#' }
explore_plots <- function(dat){
  p <- list()

  # Plot the photo data by year
  p[["photo_counts"]] <- dat %>%
    dplyr::mutate(day = as.Date(date)) %>%
    dplyr::group_by(day, year) %>%
    dplyr::summarize(photo = sum(photo)) %>%
    ggplot(aes(day, photo)) +
    geom_line() +
    labs(x = "Date",
         y = "Photo counts") +
    facet_wrap(.~year,
               scales = "free",
               ncol = 1)
  # Hourly counts within a day
  p[["hourly_counts"]] <- dat %>%
    dplyr::mutate(hour = lubridate::hour(date)) %>%
    dplyr::group_by(hour, year) %>%
    dplyr::summarize(photo = sum(photo, na.rm=TRUE)) %>%
    ggplot(aes(hour, photo)) +
    geom_line() +
    facet_wrap(.~year,
               scales = "free",
               ncol = 1)
  return(p)
}



## ----
## @knitr plot_top_model

#' Plots of the top model for estimating salmon escapement from photo and video count data
#'
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a \code{ggplot2} Rdata object
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plot_topmodel(models)
#' }
plot_topmodel <- function(models){

  p <- ggplot(models$top_model, aes(x = photo, y = video)) +
    geom_point() +
    geom_line(aes(x = photo, y = models$top_model$fitted.values)) +
    geom_hline(yintercept = 0,
               size = 0.5,
               linetype = 3) +
    geom_vline(xintercept = 0,
               size = 0.5,
               linetype = 3) +
    labs(y = "Video count",
         x = "Photo count") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_text(size = 16,
                                      lineheight = 0.9),
          axis.title.x = element_text(size = 16,
                                      lineheight = 0.9,
                                      hjust = 0.46),
          axis.text = element_text(size = 14))

  return(p)
}



## ----
## @knitr plot_boot_escapement

#' Plot bootstrapped annual estimates of salmon escapement
#'
#' @param boots a data frame returned by \code{boot_escapement()}
#'
#' @return a \code{ggplot2} Rdata object
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_boot_escapement(boots)
#' }
plot_boot_escapement <- function(boots) {

  # make a data frame of the results
  results <- lapply(boots$raw_boots$boot_est, function(x){
    foo <- x[["t"]]
  })
  results <- data.frame("estimate" = unlist(results))
  year <- rep(unique(boots$summary$year),
              each = boots$raw_boots$boot_est$`2015`$R)
  results$year <- as.factor(year)

  summary <- boots$summary

  # a list to store the plots
  p <- list()

  p[["density_estimates"]] <- ggplot() +
    geom_density(data = results, aes(x = estimate,
                                     group = year,
                                     fill = year),
                 alpha = 0.5) +
    geom_vline(data = summary,
               aes(xintercept = escapement,
                   group = year),
               alpha = 0.2) +
    labs(x = "Bootstrapped values",
         y = "Density") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_text(size = 16,
                                      lineheight = 0.9),
          axis.title.x = element_text(size = 16,
                                      lineheight = 0.9,
                                      hjust = 0.46),
          axis.text = element_text(size = 14))

  p[["min_escape"]] <- ggplot(boots$summary,
                              aes(x = as.factor(year), y = escapement)) +
    geom_errorbar(data = boots$summary,
                  aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.1) +
    geom_point() +
    labs(x = "Year", y = "Est. min. escapement")

  return(p)
}



## ----
## @knitr plot_daily

#' Plot estimated minimum daily salmon escapement by year
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a list containign two \code{ggplot()} objects. \code{daily} is a plot of daily salmon passage by year. \code{cumul_daily} is a plot of cumulative salmon passage by year.
#'
#' @import ggplot2
#' @importFrom lubridate year
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_daily(dat, models)
#' }
plot_daily <- function(dat, models) {
  dat_daily <- daily_passage(dat, models)
  dat_daily$year <- factor(lubridate::year(dat_daily$date))
  dat_daily$julian <- as.numeric(format(dat_daily$date, "%j"))

  p <- list()

  p[["daily"]] <- ggplot(dat_daily, aes(julian, passage)) +
    geom_line() +
    labs(x = "Date",
         y = "Salmon passage") +
    facet_wrap(. ~ year,
               scales = "fixed",
               ncol = 1) +
    scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))

  p[["cumul_daily"]] <- ggplot(dat_daily, aes(x = julian,
                                            y = cumul_passage,
                                            group = year,
                                            color = year)) +
    geom_line() +
    labs(x = "Date",
         y = "Salmon passage") +
    scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))

  return(p)
}


## ----
## @knitr plot_hourly

#' Plot estimate hourly salmon escapement by day
#'
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @return a \code{ggplot()} facetted plot showing
#'
#' @importFrom dplyr mutate group_by summarize
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom lubridate hour
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_hourly(dat, models)
#' }
plot_hourly <- function(dat, models){
  p <- dat %>%
    dplyr::mutate(hour = as.numeric(lubridate::hour(date)),
                  year = factor(dat$year),
                  passage = predict(models$top_model, dat)) %>%
    dplyr::group_by(hour, year) %>%
    dplyr::summarize(passage = sum(passage, na.rm = T)) %>%
    ggplot(aes(x = hour,
               y = passage)) +
    geom_line() +
    labs(x="Hour of day",
         y = "Salmon passage") +
    facet_wrap(. ~ year,
               scales = "free_y",
               ncol = 1)

  return(p)
}
