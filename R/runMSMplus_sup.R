
#' @title Local deployment of MSMplus application -supplemetary material
#' @description runMSMplus_sup() : This page is a tutorial on how to build the json/csv input files for the MSMplus app. It also contains examples in Stata and R of 
#' conducting the multi-state analysis and deriving the neccesary input files.
#'  of the window that will open. It works best with the Google Chrome Browser
#' @return OUTPUT_DESCRIPTION
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  runMSMplus_sup()
#'  }
#' }
#' @seealso 
#'  \code{\link[shiny]{runApp}}
#' @rdname runMSMplus
#' @export 
#' @importFrom shiny runApp
runMSMplus_sup <- function() {
  appDir <- system.file("Shiny", "supplementary", package = "MSMplus")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
