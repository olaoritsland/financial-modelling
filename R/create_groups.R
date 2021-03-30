
#' Create groups
#'
#' @param data 
#' @param n_groups 
#' @param select_contains
#'
#' @return
#' @import dplyr
#' @importFrom tidyr picot_longer
#' @export
#'
#' @examples
create_groups <- function(data, n_groups, select_contains) {
  
  data <- data %>% 
    select(year, contains(select_contains)) %>%
    select(-contains("total")) %>% 
    tidyr::pivot_longer(-year)
  
  groups <- data %>% 
    group_by(name) %>% 
    summarise(sum = sum(value, na.rm = TRUE)) %>% 
    filter(sum > 0) %>% 
    arrange(desc(sum)) %>% 
    top_n(n = n_groups - 1) %>% 
    pull(name) %>% 
    unique()
  
  data %>% 
    mutate(group = if_else(name %in% groups, name, "other"))
  
}

  