#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  options(shiny.reactlog = TRUE)
  shiny::runApp(system.file("app", package = "tableOne"))
}


