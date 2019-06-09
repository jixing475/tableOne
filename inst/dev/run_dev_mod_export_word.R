#inst/dev/run_dev_mod_csv_fileInput.R
.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
library(DT)
if (interactive()){
  ui <- fluidPage(
    mod_export_word_table_ui("jixing")
  )
  server <- function(input, output, session) {
    callModule(mod_export_word_table_server,"jixing", data = head(iris))
  }
  shinyApp(ui, server)
}
