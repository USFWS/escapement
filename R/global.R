
#' An internal function to load and rename an Rdata object
#'
#' @param input the directory path to an Rdata object
#'
#' @return a renamed object loaded into R workspace
#'
#' @examples
#' \dontrun{
#' loadRdata("/path/to/my/file.Rdata")
#' }
loadRData <- function(input){
  #loads an RData file, and returns it
  load(input)
  get(ls()[ls() != "input"])
}




#' An internal function used to generate bootstrapped confidence intervals around salmon escapement estimates
#'
#' @param year1 a numeric value indicating the year of data to bootstrap
#' @param model a character value indicating the top model used to estimate escapement
#' @param ... pass additional arguments to boot_escapement()
#'
#' @import tidyverse
#' @import boot
#' @return
#'
#' @examples
bootit <- function(year1,
                   model) {
  # Subset data by a given year
  dat_year <- dat %>%
    mutate(year = lubridate::year(date)) %>%
    subset(year == year1)

  # function to bootstrap
  tot <- function(data, indices, formula, dat) {
    d <- datud[indices, ] # allows boot to select sample
    fit <- lm(formula, data = d)
    pred.vid <- predict(fit, dat)
    return(sum(pred.vid, na.rm = TRUE))
  }

  ## Need an if/then statement here to assign "formula" to the top_model (linear, polynomial, etc)

  if(model == "linear"){
    top_model_formula = "video ~ photo - 1"
  }

  if(model == "polynomial"){
    top_model_formula = "video ~ photo + I(photo^2) - 1"
  }

  if(model == "segmented"){
    top_model_formula = ""
  }

  if(model == "segmented polynomial"){
    top_model_formula = ""
  }


  # run a bootstrap
  tictoc::tic("Total time")
  message(paste("Bootstrapping", unique(dat_year$year), "..."))
  results <- boot::boot(data = datud,
                        statistic = tot,
                        R = 10000,
                        formula = top_model_formula,
                        dat = dat_year)
  message("Done.")
  tictoc::toc()
  return(results)
}
