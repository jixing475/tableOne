#' @title Turn Tableone Objector To Dataframe
#' @description Turn Tableone Objector To Dataframe
#' @param table_one tableone objecter
#' @param non_normal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test).
#' @return dataframe
#'
#' @examples
#' dt <- iris
#' listVar <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#' catVar <- "Species"
#' 
#' table1 <- CreateTableOne(vars = listVar, data = dt, factorVars = catVar)
#' table2 <- CreateTableOne(vars = listVar, data = dt, factorVars = catVar, strata = c("Species"))
#' left_join(tableone_list2df(table1),  tableone_list2df(table2), by = "Variable") %>% 
#'  select(Variable, Overall, everything()) 
#'
#' @importFrom purrr map_if
#' @importFrom purrr partial
#' @importFrom magrittr %>%
#'
#' @rdname tableone_list2df
#' @export


tableone_list2df  <- function(table_one, non_normal=NULL, ...){
  res <- print(
    table_one,
    nonnormal = non_normal,# nonparametric test
    #exact = c("status", "stage"), # fisher.test
    #cramVars = "sex", # both levels are shown
    #showAllLevels = TRUE,
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