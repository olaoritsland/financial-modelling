
#' Prep data
#'
#' @param data 
#' @param balance_sheet
#' @param columns_to_remove 
#'
#' @return
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom janitor clean_names
#' @import dplyr
#' @export
#'
#' @examples
prep_data <- function(data, 
                      balance_sheet = FALSE,
                      columns_to_remove = c("Consolidatet statement",
                                            "Start date",
                                            "End date",
                                            "0",
                                            "Currency")) {
  
  # initial prep on whole dataset
  data <- data %>% 
    mutate(row_number = row_number()) %>%
    rename(info = RESULT) %>% 
    mutate(across(everything(), ~tidyr::replace_na(.x, 0)))
  
  
  # identify breakpoints
  breakpoint <- breakpoint(data, "BALANCE")
  breakpoint_max <- breakpoint(data, "Export")
  
  if (balance_sheet) {
    
    breakpoint = breakpoint + 1
    
    data <- data %>% 
      filter(row_number >= breakpoint,
             row_number < breakpoint_max)
    
  }
  else {
    
    data <- data %>% 
      filter(row_number < breakpoint)
    
  }
  
  # prep
  data <- data %>% 
    select(-row_number) %>% 
    tidyr::pivot_longer(!info) %>% 
    filter(!info %in% columns_to_remove) %>% 
    mutate(across(.cols = c(name, value), .fns = as.numeric)) %>%
    distinct(info, name, .keep_all = TRUE) %>% 
    tidyr::pivot_wider(id_cols = "name", 
                       names_from = "info", 
                       values_from = "value") %>% 
    rename(year = name) %>% 
    janitor::clean_names()
  
  return(data)
  
}


#' Breakpoint
#'
#' @param data 
#' @param keyword 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
breakpoint <- function(data, keyword) {
  
  data %>% 
    filter(info == keyword) %>% 
    pull(row_number)
}