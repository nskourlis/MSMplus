
#' @title Local deployment of MSMplus application
#' @description Launches MSMplus locally. We advice to click at "Open in a Browser" option
#'  of the window that will open. It works best with the Google Chrome Browser
#' @return OUTPUT_DESCRIPTION
#' @details Details on how to use the MSMplus app can be found in the app platform.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  runMSMplus()
#'  }
#' }
#' @seealso 
#'  \code{\link[shiny]{runApp}}
#' @rdname runMSMplus
#' @export 
#' @importFrom shiny runApp
runMSMplus <- function() {
  appDir <- system.file("Shiny", "MSMplus", package = "MSMplus")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
