
#' Unlevered beta
#'
#' @param url 
#' @param industry 
#' @param time_period 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
unlevered_beta <- function(url = "http://www.stern.nyu.edu/~adamodar/pc/datasets/betas.xls", 
                           industry,
                           time_period = "last",
                           ...) {
  
  data <- read_damodaran(url, which = 2, skip = 9, ...) %>% 
    select(industry_name, unlevered_beta, contains("20")) %>% 
    filter(industry_name == industry) %>% 
    rename(x2021 = unlevered_beta) %>% 
    select(-average_2016_21) %>% 
    tidyr::pivot_longer(-industry_name, 
                        names_to = "year", 
                        values_to = "unlevered_beta") %>% 
    mutate(year = stringr::str_remove_all(year, "x") %>% as.numeric) %>% 
    group_by(industry_name) %>% 
    summarise(average_unlevered_beta = mean(unlevered_beta, na.rm = TRUE),
              last_unlevered_beta = last(unlevered_beta))

  
  if (time_period == "last") {
    unlevered_beta <- data %>% 
      pull(last_unlevered_beta)
  }
  else if (time_period == "average") {
    unlevered_beta <- data %>% 
      pull(average_unlevered_beta)
  }
  else {
    stop("time_period must be 'last' or 'average'")
  }
  
  unlevered_beta
  
}

