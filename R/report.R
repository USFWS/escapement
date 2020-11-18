
## ----
## @knitr annual_report

#' Generate an Rmarkdown report of salmon escapement
#'
#' @param outformat the file format of the report. Default is \code{bookdown::pdf_document2()}.
#' @param outfile a name of the returned file
#' @param outdir a directory path to the folder to save the PDF
#' @param parameters a list of custom parameters or \code{all} to launch a shiny app to select custom parameters
#' @param ... pass additional arguments to \code{rmarkdown::render()}
#'
#' @return an RMarkdown-generated report
#'
#' @import bookdown
#' @import broom
#' @import scales
#' @import dplyr
#' @import magrittr
#' @import kableExtra
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_report()
#' }
run_report <- function(outformat = bookdown::pdf_document2(),
                       outfile = "report",
                       outdir = getwd(),
                       parameters = "ask",
                       ...) {
  rmarkdown::render(input = system.file("rmd", "report.Rmd", package = "escapement"),
                    output_format = outformat,
                    output_file = outfile,
                    output_dir = outdir,
                    params = parameters,
                    ...)
  # browseURL(file.path(paste0(outdir, "/", outfile, filetype)))  # open the report
}
