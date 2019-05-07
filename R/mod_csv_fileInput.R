#' @title   mod_csv_fileInput and mod_csv_file
#' @description  A shiny Module that imports csv file
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
#' library(shiny)
#' library(DT)
#' if (interactive()){
#' ui <- fluidPage(
#'   mod_csv_fileInput("fichier"),
#' DTOutput("tableau")
#' )
#' server <- function(input, output, session) {
#'   data <- callModule(mod_csv_file,"fichier")
#'   output$tableau <- renderDT({data()})
#' }
#' shinyApp(ui, server)
#' }
#' 
#' 
mod_csv_fileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

#' mod_csv_file server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
#' @rdname mod_csv_fileInput
mod_csv_file <- function(input, output, session, stringsAsFactors=TRUE) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
