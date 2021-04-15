
#' Estimate FCFF
#'
#' @param data 
#' @param revenue_growth 
#' @param ppe_rate 
#' @param depreciation_rate 
#' @param net_operating_profit_margin 
#' @param nwc_to_revenues 
#' @param explicit_forecast_period
#'
#' @return
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#'
#' @examples
estimate_fcff <- function(data, 
                          revenue_growth, 
                          ppe_rate, 
                          depreciation_rate, 
                          net_operating_profit_margin,
                          nwc_to_revenues,
                          explicit_forecast_period) {
  
  base_year = max(data$year)
  
  terminal_year = base_year + explicit_forecast_period
  
  
  total_operating_income = data %>% 
    filter(year == base_year) %>% 
    pull(total_operating_income) * cumprod(1 + revenue_growth)
  
  
  ppe = total_operating_income * ppe_rate
  
  
  
  df_forecast <- data %>% 
    select(
      year,
      total_operating_income,
      revenue_growth,
      net_operating_profit_margin,
      total_fixed_assets,
      ordinary_depreciation,
      net_working_capital,
      depreciation_rate,
      ppe_rate,
      nwc_to_revenues,
      wacc,
      net_debt
    ) %>% 
    dplyr::bind_rows(
      tibble::tibble(
        year = (base_year + 1):terminal_year,
        total_operating_income = total_operating_income,
        revenue_growth = revenue_growth,
        net_operating_profit_margin = net_operating_profit_margin,
        depreciation_rate = depreciation_rate,
        ppe_rate = ppe_rate,
        nwc_to_revenues = nwc_to_revenues,
        total_fixed_assets = ppe,
        wacc = data %>% filter(year == base_year) %>% pull(wacc),
        net_debt = data %>% filter(year == base_year) %>% pull(net_debt)
      )
    ) %>% 
    mutate(
      nopat = total_operating_income * net_operating_profit_margin,
      ordinary_depreciation = depreciation_rate * total_fixed_assets,
      capex = total_fixed_assets - lag(total_fixed_assets),
      net_working_capital = nwc_to_revenues * total_operating_income,
      increase_net_working_capital = net_working_capital - lag(net_working_capital),
      fcff = nopat + ordinary_depreciation - capex - increase_net_working_capital,
      fcff_discounted = fcff / (1 + wacc)^(year - base_year)
    )
  
  df_forecast
  
}