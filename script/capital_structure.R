
# capital structure

devtools::load_all()

df <- read_damodaran("http://www.stern.nyu.edu/~adamodar/pc/datasets/dbtfund.xls", which = 2, skip = 7)



library(tidyverse)



# plot 

df %>% 
  ggplot(aes(x = market_d_e_unadjusted, y = reorder(industry_name, market_d_e_unadjusted))) +
  geom_bar(stat = "identity")
  
