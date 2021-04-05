# solvency and credit risk

source("script/company_analysis.R")



df_analysis %>% 
  
        
  mutate(
    
  # Long-term:
    # 1. does the firm have sound financing structure?
    equity_ratio = total_equity / invested_capital, # Is current equity sufficient to withstand losses equivalent to 2-3 bad years
  
    # 2. is cash from operations sufficient to pay debt on an ongoing basis?
         
    interest_coverage_ratio = nopat / net_financial_expenses_after_tax, # can use CFO TODO: se over nfeat
    net_debt_to_nopat = net_debt / nopat,
    # cfo_to_debt = cfo / net_debt,
    # debt_to_ebitda = total_liabilities / ebitda,
    # debt_to_ebitda = net_debt / ebitda,
    # capex_ratio = cfo / capex
    
    # Are there liquidity reserves for a rainy day?
    # liquidity_reserve_ratio = (financial_assets + unused_lines_of_credit) / total_liabilities,
    # liquidity_current_reserve_ratio = (financial_assets + unused_lines_of_credit) / current_liabilities,
    
    # Is the firm liqudity efficient?
    nwc_to_revenues = net_working_capital / total_operating_income, # TODO remove?
    
  # Short-term:
  
  
  # cash conversion
    inventory_turnover = cost_of_stocks / total_inventories, # TODO: average inventory
    inventory_days_outstanding = 365/inventory_turnover,
    # accounts_receivable_turnover = credit_sales / customer_accounts_receivables,
    # collection_period = 365/accounts_receivable_turnover,
    accounts_payable_turnover = cost_of_stocks / trade_creditors,
    payable_days_outstanding = 365/accounts_payable_turnover,
    # cash_conversion_cycle = inventory_days_outstanding + collection_period - payable_days_outstanding,
    net_cash_position = (total_current_assets - operating_current_assets) - (total_short_term_liabilities - operating_current_liabilities)
    
    ) %>% 
  View()
  
  
  
# plot

plot_nwc(df_analysis)
  

# Other:
# Off-balance items
# Covenants
# non-current financing coverage ratio (1)