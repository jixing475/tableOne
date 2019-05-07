# Module UI
  
#' @title   mod_select_ui and mod_select_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_select
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_select_ui <- function(id){
  ns <- NS(id)
  tagList(
  uiOutput(ns("selected"))
  )
}
    
# Module Server
    
#' @rdname mod_select
#' @export
#' @keywords internal
    
mod_select_server <- function(input, output, session){
  ns <- session$ns
  output$selected<- rendUI({
    selectInput(ns("vars"), label = h3("Select Variables"), 
                choices = colnames(data()), multiple = multiple)
  })
}
    
## To be copied in the UI
# mod_select_ui("select_ui_1")
    
## To be copied in the server
# callModule(mod_select_server, "select_ui_1")
 
