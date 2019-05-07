#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    # from inst/app/www
    golem_add_external_resources(),
    golem::js(),
    golem::favicon(),
    # List the first level UI elements here 
    fluidPage(
      titlePanel( "TableOne" ), 
      
      sidebarLayout(
        sidebarPanel(
          #mod_csv_fileInput("jixing"),
          mod_inputFile_ui("jixing"),
          #choices_test <- textOutput("cols"),
          #mod_tableOneInput("zhuzhu", choices = colnames(data_csv))
          br(),
          actionButton("mybutton", "Click to update colnames"),
          uiOutput("my_select_UI"),
          actionButton("go", "Click to create TableOne")
          #shinyWidgets::actionBttn("go", "", 
          #                         style = "material-flat",
          #                         color = "primary",
          #                         icon = icon("rocket"))
        ),
        mainPanel(
          #uiOutput("my_table_one_UI")
          DT::DTOutput("my_table_one") %>% shinycssloaders::withSpinner(),
          br(),
          br(),
          mod_export_word_table_ui("tableone_word")
          #uiOutput("colname")
        )
      )
      
    )
  )
}

golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'tableOne')
  )
 
  tagList(
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
