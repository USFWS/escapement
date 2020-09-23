
## ----
## @knitr exploratory_plots

#' Create exploratory plot of raw salmon photo counts
#' @description Creates a list of exploratory plots of raw salmon photo counts. This includes a plot of daily photo counts grouped by year, and a plot of hourly photo counts grouped by year.
#'
#' @param input a directory path to an Rdata file containing formatted salmon count data returned by \code{import_format()}
#' @param saveit whether to save the returned list of plots as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned list of plots
#'
#' @return a list of \code{ggplot2} plots
#'
#' @export
#'
#' @import tidyverse
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' explore_plots(input = "./data/derived/dat.Rdata", saveit = FALSE)
#' }
explore_plots <- function(input = "./data/derived/dat.Rdata",
                          saveit = FALSE,
                          output){

  dat <- loadRData(input)

  p <- list()
  # Plot the photo data by year:
  p[["photo_counts"]] <- dat %>%
    mutate(day = as.Date(date)) %>%
    group_by(day, year) %>%
    summarize(photo = sum(photo)) %>%
    ggplot(aes(day, photo)) +
    geom_line() +
    labs(x = "Date",
         y = "Photo counts") +
    facet_wrap(.~year,
               scales = "free",
               ncol = 1)
  # save(p.photo.counts, file="./output/plots/plot_photo_counts.Rdata")

  # Hourly counts within a day:
  p[["hourly_counts"]] <- dat %>%
    mutate(hour = lubridate::hour(date)) %>%
    group_by(hour, year) %>%
    summarize(photo = sum(photo, na.rm=TRUE)) %>%
    ggplot(aes(hour, photo)) +
    geom_line() +
    facet_wrap(.~year,
               scales = "free",
               ncol = 1)

  if(saveit == TRUE){
    save(p, file = output)
  }
  return(p)
}



## ----
## @knitr plots

#' Plots of the top model for estimating salmon escapement from photo and video count data
#'
#' @param input a directory path to an Rdata file containing formatted salmon count data returned by \code{import_format()}
#' @param saveit whether to save the returned list of plots as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned \code{ggplot2} Rdata object
#'
#' @return a \code{ggplot2} Rdata object
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plot_topmodel(input = "./data/derived/dat.Rdata", saveit = TRUE, output = "./output/plot_top_model")
#' }
plot_topmodel <- function(input = "./data/derived/dat.Rdata",
                          saveit = TRUE,
                          output = "./output/plot_top_model"){

  dat <- loadRdata(input)
  datud <- dat[!is.na(dat$video), ]  # Remove rows not containing video counts

  # video ~ photo simple linear
  p_bestmodel <- ggplot(data = datud,
                        aes(x = photo,
                            y = video)) +
    geom_point(pch = 16, size = 1) +
    geom_smooth(method = "lm", formula = y ~ x - 1,
                colour = "red",
                linetype = 2,
                size = 0.5,
                se = F,
                fullrange = T) +
    scale_y_continuous(breaks = seq(-50, 1000, 200)) +
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

  if (saveit == TRUE) {
    save(p, file = output)
  }

  return(p_bestmodel)
}



## ----
## @knitr plot_boot_escapement

#' Plot bootstrapped annual estimates of salmon escapement
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' plot_boot_escapement()
#' }
plot_boot_escapement <- function() {

  p <- list()

  # Plot the output:
  p[["boot"]] <- ggplot2::ggplot() +
    geom_density(aes(results_2015$t,
                     fill = "red",
                     alpha = 0.001)) +
    geom_vline(xintercept = results_2015$t0,
               color="red") +
    geom_density(aes(results_2016$t,
                     fill = "orange",
                     alpha = 0.001)) +
    geom_vline(xintercept = results_2016$t0,
               color = "orange") +
    geom_density(aes(results_2017$t,
                     fill = "blue",
                     alpha=0.001)) +
    geom_vline(xintercept = results_2017$t0,
               color = "blue") +
    geom_density(aes(results_2018$t,
                     fill = "yellow",
                     alpha = 0.001)) +
    geom_vline(xintercept = results_2018$t0,
               color = "yellow") +
    labs(x = "Count",
         y = "Bootstrapped values") +
    scale_fill_manual(name = "Year",
                      values = c("red" = "red",
                                 "orange" = "orange",
                                 "blue" = "blue",
                                 "yellow" = "yellow"),
                      labels=c("2018",
                               "2017",
                               "2016",
                               "2015")) +
    theme(legend.position = c(0.6, 0.8)) +
    guides(alpha = F)
  #ggsave("./output/plots/plot_boot.jpg", p)

  p[["min_escape"]] <- ggplot(results,
                              aes(x = as.factor(year), y = escapement)) +
    geom_errorbar(data = results,
                  aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.1) +
    geom_point() +
    labs(x = "Year", y = "Est. min. escapement")

  #ggsave("./output/plots/plot_escapement.jpg", p)
}



## ----
## @knitr plot_daily

#' Plot daily escapement
#'
#' @return
#' @export
#'
#' @examples
plot_daily <- function() {
  plots <- list()
  # Plot estimated daily min. salmon passage
  plots[["daily_passage"]] <- dat %>%
    group_by(date_x, year) %>%
    summarize(escapement = sum(escapement)) %>%
    ggplot(aes(date_x, escapement)) +
    geom_line() +
    labs(x = "Date",
         y = "Salmon passage") +
    facet_wrap(. ~ year,
               scales = "fixed",
               ncol = 1) +
    scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))
  # ggsave(p.daily.passage, file = "./output/plots/daily_passage.jpg")
}


## ----
## @knitr plot_hrly

#' Plot hourly escapement by day
#'
#' @param
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_hrly()
#' }
plot_hrly <- function(){
  # Plots hourly passage within a day
  plots[["hourly_passage"]] <- dat %>%
    mutate(hour = lubridate::hour(date)) %>%
    group_by(hour, year) %>%
    summarize(escapement = sum(escapement, na.rm = T)) %>%
    ggplot(aes(hour, escapement)) +
    geom_line() +
    labs(x="Hour of day",
         y = "Salmon passage") +
    facet_wrap(. ~ year,
               scales = "free_y",
               ncol = 1)
}
