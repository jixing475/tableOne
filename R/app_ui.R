#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    # from inst/app/www
    golem_add_external_resources(),
    golem::js(),
    golem::favicon(),
    # List the first level UI elements here
    fluidPage(theme = "www/bootstrap.css",
              #==== ⭐️ html head ====
              HTML('<nav class="navbar navbar-inverse">
                     <div class="container-fluid">
                     
                     <div class="navbar-header">
                     <button type="button" class="navbar-toggle collapsed" data-toggle="collapse">
                     <span class="sr-only">Toggle navigation</span>
                     <span class="icon-bar"></span>
                     </button>
                     <a class="navbar-brand" href="#">Student Zero</a>
                     </div>
                     
                     <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-2">
                     <ul class="nav navbar-nav">
                     <li class="active"><a href="https://jixing.netlify.com/">Jixing\' blog <span class="sr-only">(current)</span></a></li>
                     </ul>
                     </div>
                     
                     </div>
                     </nav>'),
      headerPanel("Table 1"), 
      
      sidebarLayout(
        # sidebar -------------------------------
        sidebarPanel(width = 3,

            mod_inputFile_ui("jixing"),
            actionButton("mybutton", "Submit"),
            
            uiOutput("my_select_UI"),
            actionButton("go", "Click to create TableOne")
          ),

        # mainPanel --------------------------------------------------
        mainPanel(tabsetPanel(
          #tabPanel("Data upload", value = "A",
          #  DT::DTOutput("data_upload"),
          #  plotOutput("plot_missing")
          #  ),
          
          tabPanel(strong("Table 1"), values = "B", 
            DT::DTOutput("my_table_one") %>%
              shinycssloaders::withSpinner(),
            br(),
            br(),
            mod_export_word_table_ui("tableone_word"),
            br(),
            br(),
            strong("Email: jixing475@163.com")
            )
          
        ))
      )# sidebarLayout
    ) #fluidPage
  ) # tagList
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


