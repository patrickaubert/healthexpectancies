#' Runs examples of uses of the package
#'
#' @param example name of the example (default is 'projprevalence' : a Shiny app displaying forecasting of prevalences and DFLE for France)
#'
#' @return launches the app
#'
#' @export runExample
#'
#' @examples
runExample <- function(example = "projprevalence") {

  appDir <- system.file("shiny-examples", example, package = "healthexpectancies")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
