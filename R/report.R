
## ----
## @knitr annual_report

#' Generate an Rmarkdown report of salmon escapement
#'
#' @param outfile a name of the returned PDF file
#' @param outdir a directory path to the folder to save the PDF
#'
#' @return an RMarkdown generated PDF report
#'
#' @import rmarkdown
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_report(outfile = "akalura_report.pdf", outdir = "./report")
#' }
run_report <- function(outfile = "escapement_report.pdf",
                       outdir = getwd(),
                       parameters = "ask") {
  rmarkdown::render(input = system.file("rmd", "report.Rmd", package = "escapement"),
                    output_file = outfile,
                    output_dir = outdir,
                    params = parameters)
}
