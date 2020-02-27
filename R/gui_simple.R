#' Starts a simple graphical user interface for selecting data
#'
#' Starts a simple graphical user interface for selecting data, plotting and saving data to disk in the systems default browser.
#'
#'
#' @export
gui_simple <- function(){
  appDir <- system.file("gui_simple", package = "IDA")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
