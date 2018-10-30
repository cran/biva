#' Start biva
#' @title Launch biva User Interface
#' @return Nothing
#' @description biva() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning business intelligence. Includes example data for testing out a few example analysis.
#' @keywords biva
#' @examples
#' \dontrun{
#' library(shiny)
#' biva()
#' }

biva <- function() {
  
 
 shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "biva"))
Sys.setenv("R_TESTS" = "")
  }
