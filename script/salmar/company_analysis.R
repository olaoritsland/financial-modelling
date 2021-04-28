
# package
devtools::load_all()

# input ------------------------------------------------------------------------

file = "data/salmar.xlsx"
tax_rate = 0.22
market_risk_premium = 0.05
credit_spread = 0.0118
industry = "Farming/Agriculture" # til damodaran

interest_bearing_liabilities <- c("long_term_pension_commitments",
                                  "mortgage_debt_liabilities_to_financial_institutions",
                                  "long_term_group_contribution_liabilities",
                                  "other_long_term_liabilities",
                                  "liabilities_to_financial_institutions")

interest_bearing_assets <- c("bank_deposits_cash_etc",              # keep 10% of ...?
                             "shares_investment_in_subsidiaries",
                             "investments_in_group_companies",
                             "investments_in_shares_and_interests",
                             "bonds_and_other_accounts_receivables",
                             "other_financial_fixed_assets",
                             "bonds",
                             "receivables_to_companies_in_the_same_group",
                             "quoted_investment_shares")

operating_current_assets <- c("total_inventories", "customer_accounts_receivables")

operating_current_liabilities <- c("trade_creditors")


# read raw data ----------------------------------------------------------------

data <- readxl::read_xlsx(file, sheet = "Group accounting", skip = 2) 

data_rf <- risk_free_rate("Norway")

data_skatt <- readr::read_delim("data/skattesats_norge.csv", delim = ";")

# income statement -------------------------------------------------------------

income_statement <- prep_data(data, balance_sheet = FALSE) %>% 
  mutate(
    nopat = operating_profit_loss * (1 - tax_rate),
    operating_margin = operating_profit_loss / total_operating_income
  )



# balance sheet ----------------------------------------------------------------


# reorganise --
balance_sheet = prep_data(data, balance_sheet = TRUE) %>% 
  mutate(
    interest_bearing_liabilities = 
      rowSums(across(.cols = all_of(interest_bearing_liabilities))),
    
    interest_bearing_assets = 
      rowSums(across(.cols = all_of(interest_bearing_assets))),
    
    operating_current_assets = 
      rowSums(across(.cols = all_of(operating_current_assets))),
    
    operating_current_liabilities = 
      rowSums(across(.cols = all_of(operating_current_liabilities))),
    
    net_debt = interest_bearing_liabilities - interest_bearing_assets,
    
    invested_capital = total_equity + net_debt
  )



# measures ---------------------------------------------------------------------

df_analysis <- income_statement %>% 
  select(year,
         total_operating_income, 
         sales_revenue,
         other_operating_income,
         total_operating_expenses,
         cost_of_stocks,
         wages_salaries_and_social_security_expenses,
         other_operating_expenses,
         net_result_profit_for_the_year,
         ordinary_depreciation,
         nopat
  ) %>% 
  left_join(balance_sheet %>% 
              select(year,
                     total_equity,
                     total_assets,
                     total_fixed_assets,
                     interest_bearing_liabilities,
                     interest_bearing_assets,
                     operating_current_assets,
                     operating_current_liabilities,
                     total_inventories,
                     customer_accounts_receivables,
                     trade_creditors,
                     total_current_assets,
                     total_short_term_liabilities,
                     net_debt,
                     invested_capital,
                     goodwill_intangible_assets), 
            by = "year") %>% 
  left_join(data_rf, by = "year") %>% 
  left_join(data_skatt, by = "year") %>% 
  mutate(
    roe = net_result_profit_for_the_year / total_equity, # TODO: vurder snitt eller lag()
    roic = nopat / invested_capital,
    roic_ex_gw = nopat / (invested_capital - goodwill_intangible_assets),
    net_operating_profit_margin = nopat / total_operating_income,
    net_operating_asset_turnover = total_operating_income / invested_capital,
    net_financial_expenses_after_tax = nopat - net_result_profit_for_the_year, # motsatt?
    net_borrowing_costs = net_financial_expenses_after_tax / net_debt,
    financial_leverage = net_debt / total_equity,
    spread = roic - net_borrowing_costs, # TODO: bedre navn: 
    non_operating_return = financial_leverage * spread,
    
    unlevered_beta = unlevered_beta(industry = industry, 
                                    time_period = "last"), # consider parameter
    industry_debt_to_equity = debt_to_equity(industry = industry),
    levered_beta = (1 + industry_debt_to_equity) * unlevered_beta,
    
    cost_of_equity = cost_of_equity(rf = ten_year_gov_bond_rate, 
                                    beta = levered_beta, 
                                    market_risk_premium = market_risk_premium), # TODO historisk (har vært 5 % lenge)
    
    wacc = wacc(debt_to_equity = industry_debt_to_equity,
                cost_of_debt = ten_year_gov_bond_rate + credit_spread,
                cost_of_equity = cost_of_equity,
                tax_rate = tax_rate),  # TODO historisk
    
    # internal benchmark
    eva = (roic - wacc) * invested_capital,
    residual_income = (roe - cost_of_equity) * total_equity,
    
    revenue_growth = total_operating_income / lag(total_operating_income) - 1,
    cost_growth = total_operating_expenses / lag(total_operating_expenses) - 1,
    
    # liquidity
    net_working_capital = operating_current_assets - operating_current_liabilities,
    increase_net_working_capital = net_working_capital - lag(net_working_capital),
    capex = total_fixed_assets - lag(total_fixed_assets) + ordinary_depreciation,
    depreciation_rate = ordinary_depreciation / total_fixed_assets,
    ppe_rate = total_fixed_assets / total_operating_income,
    nwc_to_revenues = net_working_capital / total_operating_income
  )
