# Module UI
  
#' @title   mod_export_word_table_ui and mod_export_word_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom officer read_docx
#' @rdname mod_export_word_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_export_word_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), "Export to word")
  )
}
    
# Module Server
    
#' @rdname mod_export_word_table
#' @export
#' @keywords internal
    
mod_export_word_table_server <- function(input, output, session, data){
  ns <- session$ns
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("TableOne", ".docx", sep = "")
    },
    content = function(file) {
      library(officer)
      library(flextable)
      library(magrittr)
      myft <- 
        data%>% 
        flextable() %>% 
        bold(part = "header") %>% 
        autofit()
      
      officer::read_docx() %>% 
        body_add_flextable(myft) %>% 
        print(target = file)
    }
  )
}
    
## To be copied in the UI
# mod_export_word_table_ui("export_word_table_ui_1")
    
## To be copied in the server
# callModule(mod_export_word_table_server, "export_word_table_ui_1")
 
