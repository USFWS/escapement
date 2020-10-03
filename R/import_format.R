## ----
## @knitr import_format

#' Import and format photo and video salmon count data
#'
#' @param input the directory path to a CSV containing dates, and hourly video and photo counts of salmon.
#' @param saveit whether to save the returned data frame as a RData object
#' @param output if \code{saveit = TRUE}, the directory path to where to save the returned data frame
#'
#' @return
#'
#' @export
#'
#' @examples
#' #' \dontrun{
#' import_format(input = "./data/raw/hourraw.csv", saveit = TRUE, output = "./data/derived/dat.Rdata")
#' }
import_format <- function(input,
                          saveit = FALSE,
                          output){

  dat <- read.csv(input)  # Read in the raw data (csv)
  dat$date <- as.POSIXct(as.character(dat$date),
                         format="%m/%d/%Y %H:%M")  # Change date to POSIXct
  dat$photo <- as.numeric(dat$photo)
  dat$video <- as.numeric(dat$video)
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
