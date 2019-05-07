.rs.api.documentSaveAll() # closes and saves all open files
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))# detach all  packages
rm(list = ls(all.names = TRUE))# clean environneent
devtools::document('.') # create NAMESPACE and man
devtools::load_all('.') # load package
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)
server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}
shinyApp(ui, server)
}