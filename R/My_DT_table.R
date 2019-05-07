#' @title My DT table
#' @description return table results for shiny
#' @param data dataframe
#'
#' @return dataframe
#'
#' @examples
#'
#' @importFrom purrr map_if
#' @importFrom purrr partial
#' @importFrom magrittr %>%
#'
#' @rdname My_DT_table
#' @export


My_DT_table <- purrr::partial(DT::datatable,  extensions = c('Buttons', 'FixedColumns', 'Scroller'), 
                         options = list(fixedColumns = TRUE, 
                                        scrollY = 400,
                                        scrollX = TRUE,
                                        scroller = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('colvis','csv','excel'),
                                        columnDefs = list(
                                          list(targets = c(6), visible = FALSE)
                                        )
                                        )#,
                         #filter = 'bottom'
                         )