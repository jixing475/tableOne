# Module UI
  
#' @title   mod_inputFile_ui and mod_inputFile_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_inputFile
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_inputFile_ui <- function(id, label = "file"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label)
  )
}
    
# Module Server
    
#' @rdname mod_inputFile
#' @export
#' @keywords internal
    
mod_inputFile_server <- function(input, output, session, stringsAsFactors){
  #ns <- session$ns
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    rio::import(userFile()$datapath)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
    
