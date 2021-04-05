
source("script/company_analysis.R")

# input ------------------------------------------------------------------------

base_year = max(df_analysis$year)
explicit_forecast_period = 10
terminal_year = base_year + explicit_forecast_period

revenue_growth = c(0.11, 0.10, 0.085, 0.17, 0.06, 0.05, 0.05, 0.04, 0.04, 0.03)
net_operating_profit_margin = c(0.22, 0.20, 0.19, 0.15, rep(0.14, 6))
depreciation_rate = yearly_mean(df_analysis, "depreciation_rate", years = 6, base_year)
ppe_rate = yearly_mean(df_analysis, "ppe_rate", years = 6, base_year)
nwc_to_revenues = yearly_mean(df_analysis, "nwc_to_revenues", years = 6, base_year)

growth_perpetuity = 0.018






# calculate --------------------------------------------------------------------

total_operating_income = df_analysis %>% 
  filter(year == base_year) %>% 
  pull(total_operating_income) * cumprod(1 + revenue_growth)

ppe = total_operating_income * ppe_rate



df_forecast <- df_analysis %>% 
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
      wacc = df_analysis %>% filter(year == base_year) %>% pull(wacc),
      net_debt = df_analysis %>% filter(year == base_year) %>% pull(net_debt)
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


# Estimate value

final_fcff <- df_forecast %>% 
  filter(year == base_year + explicit_forecast_period) %>% 
  pull(fcff)

cost_of_capital <- df_forecast %>% 
  filter(year == base_year + explicit_forecast_period) %>% 
  pull(wacc)

sum_discounted_fcff = df_forecast %>% filter(year > base_year) %>% pull(fcff_discounted) %>% sum()
net_debt = df_forecast %>% filter(year == base_year) %>% pull(net_debt)
terminal_value = final_fcff * (1 + growth_perpetuity) / (cost_of_capital - growth_perpetuity)
terminal_value_discounted = terminal_value / (1 + cost_of_capital)^explicit_forecast_period

enterprise_value = sum_discounted_fcff + terminal_value_discounted

market_value_of_equity = enterprise_value - net_debt


# TODO: wacc/growth-tabell
# TODO: shares outstanding --> share price