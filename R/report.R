
## ----
## @knitr annual_report

#' Generate an Rmarkdown report of Alulara salmon escapement
#'
#' @param input_rmd 
#' @param outfile 
#' @param outdir 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' run_report(input_rmd = "akalura_report.rmd", outfile = "akalura_report_2018.pdf", outdir = "./report")
#' }
run_report <- function(input_rmd = "akalura_report.rmd",
                       outfile = "akalura_report_2018.pdf",
                       outdir = "./report") {
  library(rmarkdown)
  rmarkdown::render(input = input_rmd,
                    output_file = outfile,
                    output_dir = outdir)
}