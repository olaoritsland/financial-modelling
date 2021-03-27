# Price and value to book

df <- read_damodaran("http://www.stern.nyu.edu/~adamodar/pc/datasets/pbvdata.xls", skip = 7) %>% 
  mutate_at(c("roe", "pbv"), as.numeric)

# plot

df %>% 
  ggplot(aes(x = roe, y = reorder(industry_name, roe))) +
  geom_bar(stat = "identity")


df %>% 
  ggplot(aes(x = roic, y = reorder(industry_name, roic))) +
  geom_bar(stat = "identity")
