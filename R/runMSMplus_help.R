
#' @title Local deployment of MSMplus application- help page
#' @description runMSMplus_help() : Launches the  MSMplus help page locally. We advice to click at "Open in a Browser" option
#'  of the window that will open. It works best with the Google Chrome Browser
#' @return OUTPUT_DESCRIPTION
#' @details Details on how to use the MSMplus application.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  runMSMplus_help()
#'  }
#' }
#' @seealso 
#'  \code{\link[shiny]{runApp}}
#' @rdname runMSMplus
#' @export 
#' @importFrom shiny runApp
runMSMplus_help <- function() {
  appDir <- system.file("Shiny", "tabs", package = "MSMplus")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
