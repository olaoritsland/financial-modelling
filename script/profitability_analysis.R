
# package
devtools::load_all()

# input

file = "data/salmar.xlsx"
tax_rate = 0.22

# read raw data ----------------------------------------------------------------

data <- readxl::read_xlsx(file, sheet = "Group accounting", skip = 2) 

# income statement -------------------------------------------------------------

income_statement <- prep_data(data, balance_sheet = FALSE) %>% 
  mutate(nopat = operating_profit_loss * (1 - tax_rate))

# EDA --

theme_set(theme_lab())

# revenues ---
revenues <- create_groups(data = income_statement, 
                          n_groups = 3, 
                          select_contains = c("revenue", "income"))
  
plot_group(revenues)

# costs ---

costs <- create_groups(data = income_statement, 
                       n_groups = 5, 
                       select_contains = c("expense", "cost")) %>% 
  filter(!str_detect(name, "finan"))

plot_group(costs)



# balance sheet ----------------------------------------------------------------

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


# reorganise --
balance_sheet = prep_data(data, balance_sheet = TRUE) %>% 
  mutate(
    interest_bearing_liabilities = 
      rowSums(across(.cols = all_of(interest_bearing_liabilities))),
    
    interest_bearing_assets = 
      rowSums(across(.cols = all_of(interest_bearing_assets))),
    
    net_debt = interest_bearing_assets - interest_bearing_liabilities,
    
    invested_capital = total_equity + net_debt
    )
         



# TODO: plot balance sheet (treemap)

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
         ordinary_result,
         ordinary_depreciation,
         nopat
         ) %>% 
  left_join(balance_sheet %>% 
              select(year,
                     total_equity,
                     interest_bearing_liabilities,
                     interest_bearing_assets,
                     net_debt,
                     invested_capital,
                     goodwill_intangible_assets), 
            by = "year") %>% 
  mutate(
    roe = ordinary_result / lag(total_equity),
    roic = nopat / invested_capital,
    roic_ex_gw = nopat / (invested_capital - goodwill_intangible_assets),
    # TODO: decompose:
    # net_operating_profit_margin = nopat / total_operating_income,
    # net_operating_asset_turnover = total_operating_income / invested_capital
  )

# parameters
unlevered_beta <- unlevered_beta(industry = "Farming/Agriculture", time_period = "last") 
industry_debt_to_equity <- debt_to_equity(industry = "Farming/Agriculture")
levered_beta <- (1 + industry_debt_to_equity) * unlevered_beta

cost_of_equity <- cost_of_equity(rf = 0.01, beta = levered_beta, expected_market_return = 0.06) #TODO E[rm]

wacc <- wacc(debt_to_equity = industry_debt_to_equity,
             cost_of_debt = 0.02, 
             cost_of_equity = cost_of_equity,
             tax_rate = 0.22)

# benchmark --------------------------------------------------------------------

# internal

roe = df_analysis %>% filter(year == 2019) %>% pull(roe)
roic = df_analysis %>% filter(year == 2019) %>% pull(roic)

roe - cost_of_equity
roic - wacc  
