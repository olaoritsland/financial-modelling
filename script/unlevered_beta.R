

# package
devtools::load_all()

# read data
df_beta <- read_damodaran("http://www.stern.nyu.edu/~adamodar/pc/datasets/betas.xls", which = 2, skip = 9) %>% 
  select(industry_name, unlevered_beta, contains("20")) %>% 
  rename(x2021 = unlevered_beta) %>% 
  select(-average_2016_21) %>% 
  tidyr::pivot_longer(-industry_name, names_to = "year", values_to = "unlevered_beta") %>% 
  mutate(year = stringr::str_remove_all(year, "x") %>% as.numeric)
  
df_average_beta <- df_beta %>% 
  group_by(industry_name) %>% 
  summarise(average_unlevered_beta = mean(unlevered_beta, na.rm = TRUE))

# plot beta over time

industries <- df_average_beta %>% 
  filter(stringr::str_detect(industry_name, "Retail")) %>% 
  pull(industry_name) %>% 
  unique()

p <- df_beta %>% 
  filter(industry_name %in% industries) %>% 
  ggplot(aes(x = year, y = unlevered_beta, color = industry_name)) +
  geom_line()

plotly::ggplotly(p, dynamicTicks = TRUE)
