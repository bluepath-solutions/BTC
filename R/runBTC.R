#' @export
runBTC <- function() {
  appDir <- system.file("BTC", package = "BTC")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}