# Module UI
  
#' @title   mod_export_word_table_ui and mod_export_word_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
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
      library(ReporteRs)
      library(magrittr)
      #set option
      options( "ReporteRs-fontsize" = 12, 
               "ReporteRs-default-font" = "Times")
      T1_title <-
        pot("Table 1. ", textProperties(color = "black", font.weight = "bold")) +
        "Baseline characteristics of patients in the study"
      
      MyFTable <- 
        data %>%
        # 设置字体
        FlexTable(header.cell.props = cellProperties( background.color = "#DDDDDD"),
                  header.text.props = textBold(color = "black"),
                  add.rownames = FALSE ) %>%
        #设置边界
        setFlexTableBorders(inner.vertical = borderNone(),
                            inner.horizontal = borderNone(),
                            outer.vertical = borderNone(),
                            outer.horizontal = borderProperties( color = "black",style = "solid", width = 2 )) %>% 
        #斑马线
        setZebraStyle(odd = "#FFFFFF", even = "#FFFFFF")
      
      #写入 word
      doc <- docx()
      doc <- addParagraph(doc, T1_title)
      doc <- addFlexTable(doc, MyFTable)
      writeDoc(doc, file)
    }
  )
}
    
## To be copied in the UI
# mod_export_word_table_ui("export_word_table_ui_1")
    
## To be copied in the server
# callModule(mod_export_word_table_server, "export_word_table_ui_1")
 
