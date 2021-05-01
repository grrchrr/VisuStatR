#' @title visustat_shiny
#' @description With visustat_shiny, you can run all functionalities of the VisuStatR package in a shiny app.
#' @details  To be written...
#' @examples
#'  visustat_shiny()
#' @export
visustat_shiny <- function() {
  appDir <- system.file("app", ".", package = "VisuStatR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
