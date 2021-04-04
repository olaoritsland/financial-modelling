
# source
source("script/company_analysis.R")

# income statement -------------------------------------------------------------


# revenues ---
revenues <- create_groups(data = income_statement, 
                          n_groups = 3, 
                          select_contains = c("revenue", "income"))
  
plot_group(revenues)

# costs ---

costs <- create_groups(data = income_statement, 
                       n_groups = 5, 
                       select_contains = c("expense", "cost")) %>% 
  filter(!stringr::str_detect(name, "finan"))

plot_group(costs)





# balance sheet ----------------------------------------------------------------










# plot -------------------------------------------------------------------------


plot_roe_decomposed(df_analysis)
plot_value_creation(df_analysis, avkastning = "roic", avkastningskrav = "wacc")
plot_value_creation(df_analysis, avkastning = "roe", avkastningskrav = "cost_of_equity")
plot_growth(df_analysis, no_of_years = 5)

  
  
  


# TODO:
# 
# Inntekts- og kostnadssammensetning fra financial report (pdf)
# Sentimentanalyse baser på financial report
# Dekomponert ROE i treplott (med tall)
# Risikofri rente hvert år for europeisk og amerikansk 10-åring
# Cost of debt (bond rating)
# Markedets risikopremie historisk (og sammenheng mellom E[rm] og MRP)
# Legg inn antagelser/forutsetninger
# Kegg til premium/discounts i avkastningskrav (kontroll/likviditet)
# Funksjon som henter utvikling i råvarepriser som er aktuelle (+ forwardkurve)
# Plot utbytte og eva for å vurdere management
# Plot balansen (treemap)
# Legg inn en andel av cashen som operasjonell (se plot?)
# Legg inn to-års snitt eller lag() av egenkapital for beregning av roe
# NI = net_result_profit_for_the_year vs. ordinary_result vs. før/etter minority interest
# Plot debt to equity (eller flytt til eget script)
# Rename script (vekst, lønnsomhet og kapitalstruktur)
# Legg inn ekstern benchmark på roe og roic (snitt/median av peers)
# Plott (med peers) y = EBIT-margin x = revenue growth (cagr last x years), size = revenues
# Legg sammen flere plottefunksjoner i en fil
# Hensynta off-balance items

