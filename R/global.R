
#' An internal function to load and rename an Rdata object
#'
#' @param input the directory path to an Rdata object
#'
#' @return a renamed object loaded into R workspace
#'
#' @examples
#' \dontrun{
#' loadRdata("myfile.Rdata")
#' }
loadRData <- function(input){
  #loads an RData file, and returns it
  load(input)
  get(ls()[ls() != "input"])
}




#' An internal function to generate bootstrapped confidence intervals around salmon escapement estimates
#'
#' @param year1 a numeric value indicating the year of data to bootstrap
#' @param dat a data frame containing formatted salmon count data returned by \code{import_format()}
#' @param models a list of model output returned from \code{model_escapement}
#'
#' @import dplyr
#' @import magrittr
#' @importFrom boot boot
#' @import tictoc
#' @importFrom segmented segmented
#'
#' @return bootstrapped estimates
#'
#' @examples
#' \dontrun{
#' bootit(2015)
#' }
bootit <- function(year1, dat, models) {

  # If the top model is segmented, add new variable (U1.photo) that replaces negative photo counts with zero
  if(models$aic_table$Modnames[[1]] == "Segmented" || models$aic_table$Modnames[[1]] == "Segmented polynomial"){
    dat$U1.photo <- ifelse(dat$photo < 0, 0, dat$photo)
  }

  # Remove rows containing no video and no photo data
  datud <- dat[!is.na(dat$photo) | !is.na(dat$video),]

  # Subset data by a given year
  dat_year <- datud %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(year == year1)


  # function to bootstrap (depends on whether it is a segmented model)
  if(models$aic_table$Modnames[[1]] == "Linear" || models$aic_table$Modnames[[1]] == "Polynomial"){
    tot <- function(data, indices, formula, dat) {
      d <- datud[indices, ] # allows boot to select sample
      fit <- lm(formula, data = d)
      pred_vid <- stats::predict(fit, dat)
      return(sum(pred_vid, na.rm = TRUE))
    }
  }
  if(models$aic_table$Modnames[[1]] == "Segmented" || models$aic_table$Modnames[[1]] == "Segmented polynomial"){
    tot <- function(data, indices, formula, dat) {
      d <- datud[indices, ] # allows boot to select sample
      mod <- lm(formula, data = d)
      fit <- segmented::segmented(mod,
                       seg.Z = ~photo,
                       psi = 0,
                       control = seg.control(it.max=0))
      pred_vid <- stats::predict(fit, dat)
      return(sum(pred.vid, na.rm = TRUE))
    }
  }

  # define top model separately for linear and segmented models
  if(models$aic_table$Modnames[[1]] == "Linear" || models$aic_table$Modnames[[1]] == "Segmented"){
    top_model_formula = "video ~ photo - 1"
  }

  if(models$aic_table$Modnames[[1]] == "Polynomial" || models$aic_table$Modnames[[1]] == "Segmented polynomial"){
    top_model_formula = "video ~ photo + I(photo^2) - 1"
  }

  # run a bootstrap
  tictoc::tic("Total time")
  message(paste("Bootstrapping", unique(dat_year$year), "..."))
  results <- boot::boot(data = datud,
                        statistic = tot,
                        R = 1000,
                        formula = top_model_formula,
                        dat = dat_year)
  message("Done.")
  tictoc::toc()

  return(results)
}
