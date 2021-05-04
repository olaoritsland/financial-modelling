
source("script/salmar/company_analysis.R")

# input ------------------------------------------------------------------------

base_year = max(df_analysis$year)
explicit_forecast_period = 10
expected_inflation = 0.02 # inflasjonsmål
growth_real = 0.001

# forecast ---------------------------------------------------------------------

df_forecast <- 
  estimate_fcff(data = df_analysis,
                revenue_growth = c(0.11, 0.10, 0.085, 0.17, 0.06, 0.05, 0.05, 0.04, 0.04, 0.03), 
                net_operating_profit_margin = c(0.22, 0.20, 0.19, 0.15, rep(0.14, 6)),
                ppe_rate = yearly_mean(df_analysis, "ppe_rate", years = 6, base_year),
                depreciation_rate = yearly_mean(df_analysis, "depreciation_rate", years = 6, base_year),
                nwc_to_revenues = yearly_mean(df_analysis, "nwc_to_revenues", years = 6, base_year),
                explicit_forecast_period = explicit_forecast_period)


# Estimate value

market_value <- 
  estimate_market_value(df_forecast, 
                        base_year, 
                        explicit_forecast_period = explicit_forecast_period, 
                        growth_perpetuity = growth_real + expected_inflation)

# Plot equity bridge

plot_equity_bridge(market_value)



# TODO: wacc/growth-tabell
# TODO: shares outstanding --> share price