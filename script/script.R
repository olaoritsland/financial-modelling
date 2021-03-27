
# package
  library(tidyverse)

# read raw data

data <- readxl::read_xlsx("data/salmar.xlsx", sheet = "Group accounting", skip = 2) %>% 
  mutate(row_number = row_number()) %>%
  rename(info = RESULT) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )
  


balance_row <- data %>% 
  filter(info == "BALANCE") %>% 
  pull(row_number)

# resultat
resultat <- data %>% 
  filter(row_number < balance_row)

# balanse
balance <- data %>% 
  filter(row_number >= balance_row)

# resultat ----------------------------------------

unwanted_cols <- c("Consolidatet statement", 
                   "Start date",
                   "End date",
                   "0",
                   "Currency")

# long
resultat <- resultat %>% 
  select(-row_number) %>% 
  pivot_longer(!info) %>% 
  filter(!info %in% unwanted_cols) %>% 
  mutate(across(.cols = c(name, value), .fns = as.numeric))


# revenue analysis
revenue_vars <- c("Sales revenue", 	
                  "Rental revenues",
                  "Other operating income",
                  "Total operating income")

revenues <- resultat %>% 
  filter(info %in% revenue_vars) %>% 
  pivot_wider(id = name)
  