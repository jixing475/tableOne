# Module UI
  
#' @title   mod_TableOne_ui and mod_TableOne_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_TableOne
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

mod_tableOneInput <- function(id, choices, multiple = TRUE){
  ns <- NS(id)
  tagList(
    selectInput(ns("vars"), label = h3("Select Variables"), 
                choices = choices, multiple = multiple),
    selectInput(ns("factorsVars"), label = h3("Select factor variable"), 
                choices = choices, multiple = multiple),
    selectInput(ns("group"), label = h3("Select group"), 
                choices = choices, multiple = FALSE)
  )
}
    
# Module Server
    
#' @rdname mod_TableOne
#' @export
#' @keywords internal
#' @importFrom tableone CreateTableOne
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate select
mod_TableOne_server <- function(input, output, session, df){
  tableOne <-reactive({
    CreateTableOne(
      vars = input$vars,
      strata = input$group,# Group
      data = df,
      factorVars = input$factorVars # category variable
    )
  })
  
  
  group_levels <- df[[group]] %>% as.factor() %>% levels() 
  
  idx <- c()
  for(i in seq_along(group_levels)){
    idx <- tableOne$ContTable[[i]] %>% as.data.frame() %>% 
      pull(skew) %>% 
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
  return(res)
}
    
## To be copied in the UI
# mod_TableOne_ui("TableOne_ui_1")
    
## To be copied in the server
# callModule(mod_TableOne_server, "TableOne_ui_1")
 



