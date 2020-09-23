
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
