
#' Estimate market value
#'
#' @param data 
#' @param base_year
#' @param explicit_forecast_period 
#' @param growth_perpetuity 
#'
#' @return
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#'
#' @examples
estimate_market_value <- function(data, base_year, explicit_forecast_period, growth_perpetuity) {
  
  
  final_fcff <- data %>% 
    filter(year == base_year + explicit_forecast_period) %>% 
    pull(fcff)
  
  cost_of_capital <- data %>% 
    filter(year == base_year + explicit_forecast_period) %>% 
    pull(wacc)
  
  sum_discounted_fcff = data %>% filter(year > base_year) %>% pull(fcff_discounted) %>% sum()
  net_debt = data %>% filter(year == base_year) %>% pull(net_debt)
  terminal_value = final_fcff * (1 + growth_perpetuity) / (cost_of_capital - growth_perpetuity)
  terminal_value_discounted = terminal_value / (1 + cost_of_capital)^explicit_forecast_period
  
  enterprise_value = sum_discounted_fcff + terminal_value_discounted
  
  market_value_of_equity = (enterprise_value - net_debt)
  
  tibble::tibble(
    final_fcff = final_fcff,
    wacc = cost_of_capital,
    net_debt = net_debt,
    enterprise_value = enterprise_value,
    market_value_of_equity = market_value_of_equity
  )
  
}