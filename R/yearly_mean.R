#' Yearly mean
#'
#' @param data 
#' @param var 
#' @param years 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
yearly_mean <- function(data, var, years, base_year) {
  
  data %>% 
    filter(between(year, base_year - years, base_year)) %>% 
    pull({{var}}) %>% 
    mean()
  
}