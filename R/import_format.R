## ----
## @knitr import_format

#' Import and format photo and video salmon count data
#'
#' @param input the directory path to a CSV containing dates, and hourly video and photo counts of salmon.
#' @param saveit whether to save the returned data frame as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned data frame
#'
#' @return a data frame of salmon counts formatted for \code{escapement} functions
#'
#' @import tidyverse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import_format("salmon_counts.csv")
#' }
import_format <- function(input,
                          saveit = FALSE,
                          output){
  dat <- read.csv(input)  # Read in the raw data (csv)

  dat$date <- as.POSIXct(as.character(dat$date),
                         format="%m/%d/%Y %H:%M")  # Change date to POSIXct
  dat$date <- round(dat$date, units = "hours") # round to the nearest hour

  dat$photo <- suppressWarnings(as.numeric(dat$photo))
  dat$video <- suppressWarnings(as.numeric(dat$video))
  dat$julian_day <- as.numeric(format(as.Date(dat$date),
                                  "%j"))  # Add a Julian day column
  dat$year <- lubridate::year(dat$date)

  # Option to save the data frame
  if(saveit == TRUE){
    save(dat,
         file = output)
  }

  return(dat)
}
