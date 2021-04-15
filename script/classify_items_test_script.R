
balance_sheet %>% 
  tidyr::pivot_longer(-year) %>% 
  select(name) %>% 
  distinct() %>% 
  mutate(item_category = 
           case_when(
             
             # debt
             stringr::str_detect(name, "debt") |
               stringr::str_detect(name, "loan") |
               stringr::str_detect(name, "liabilit") ~ "debt",
             
             # equity
             stringr::str_detect(name, "equit") ~ "equity",
             
             # assets
             stringr::str_detect(name, "asset") |
               stringr::str_detect(name, "investment") |
               stringr::str_detect(name, "research") |
               stringr::str_detect(name, "patent") |
               stringr::str_detect(name, "machine") |
               stringr::str_detect(name, "rigs") |
               stringr::str_detect(name, "receivable") |
               stringr::str_detect(name, "materials") |
               stringr::str_detect(name, "goods") |
               stringr::str_detect(name, "inventories") |
               stringr::str_detect(name, "property") ~ "assets",
             
             TRUE ~ NA_character_
             )
  ) %>% 
  View()
