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
    #mod_csv_fileInput("fichier"),
    mod_inputFile_ui("fichier"),
    DTOutput("tableau")
  )
  server <- function(input, output, session) {
    #data <- callModule(mod_csv_file,"fichier")
    data  <- callModule(mod_inputFile_server, "fichier")
    output$tableau <- renderDT({data()})
  }
  shinyApp(ui, server)
}