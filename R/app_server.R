#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  # Reactive expressions cache their values
  #data <- reactive({
  #  rnorm(input$num)
  #})
  library(tidyverse)
  if(!require(tableone)) {
  install.packages("tableone")
  library(tableone)
  }
  #data  <-  callModule(mod_csv_file, "jixing")
  data <- callModule(mod_inputFile_server, "jixing")
  output$my_select_UI <- renderUI({
    # initial selections
    selections <- c()
    
    # use observe event to notice when the user clicks the button
    # update the selection list. Note the double assignment <<-
    observeEvent(input$mybutton,{
      selections <<- colnames(data())
      shinyWidgets::updatePickerInput(session, "continuousVars", choices = selections, selected = selections)
      shinyWidgets::updatePickerInput(session, "factorsVars", choices = selections)
      shinyWidgets::updatePickerInput(session, "group", choices = selections)
    })
    
    list(
      shinyWidgets::pickerInput("continuousVars", label = h4("Select continuous variables"), 
                  choices = selections, multiple = TRUE),
      br(),
      shinyWidgets::pickerInput("factorsVars", label = h4("Select factor variable"), 
                  choices = selections, multiple = TRUE),
      br(),
      shinyWidgets::pickerInput("group", label = h4("Select group"), 
                  choices = selections, multiple = FALSE)
    )
    
    
  })
  
  #assign('data', data_csv(), envir = .GlobalEnv)
  #colname <- names(data_csv())
  #output$colname <- renderUI({
  #  mod_tableOneInput("zhuzhu", choices = colname())
  #})
  #output$cols <- renderText({colnames(data_csv())})
  #data <- callModule(mod_TableOne_server, "zhuzhu", data_csv())
  #output$table_csv <- DT::renderDT({data()})
  
  observeEvent( input$go , {
    tableOne <- reactive({
      CreateTableOne(
        vars = c(input$continuousVars, input$factorsVars),
        strata = input$group,# Group
        data = data(),
        factorVars = input$factorVars # category variable
      )
    })
    
  group_levels <- data()[[input$group]] %>% as.factor() %>% levels() 
  
  idx <- c()
  for(i in seq_along(group_levels)){
    idx <- tableOne()$ContTable[[i]] %>% as.data.frame() %>% 
      dplyr::pull(skew) %>% 
      abs() %>% 
      `>`(2) %>% # absolutely value is greater than 2
      which() %>% 
      c(idx, .) %>% 
      unique()
  }
  
  idx
  
  non_normal <- rownames(tableOne()$ContTable[[1]])[idx]
  
  ## Just typing the object name will invoke the print.TableOne method
  ## Tests are by oneway.test/t.test for continuous, chisq.test for categorical
  ## Specifying nonnormal variables will show the variables appropriately,
  ## and show nonparametric test p-values (kruskal.test/wilcox.test).
  ## Specify variables in the exact argument to obtain the exact test
  ## (fisher.test) p-values. If a 2-level factor is specified in cramVars
  ## both levels are shown in one row.
  res <- print(
    tableOne(),
    nonnormal = non_normal,# nonparametric test
    #exact = c("status", "stage"), # fisher.test
    #cramVars = "sex", # both levels are shown
    quote = FALSE
  )   
  
  #==== edit results ====
  colnames(res) <-  stringr::str_replace_all(colnames(res), '"', '')
  res <- 
    res %>% 
    as.data.frame() %>% 
    mutate(Variable = stringr::str_replace_all(rownames(res), '"', '')) %>% 
    select(Variable, everything())
  
  output$my_table_one <- DT::renderDataTable({My_DT_table(res)})
  
  table_word_to_export <- select(res, -test)
  callModule(mod_export_word_table_server,"tableone_word", data = table_word_to_export)
  })
  
}




