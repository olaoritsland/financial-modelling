
plot_nwc <- function(data) {
  
  data %>% 
    select(year, total_operating_income, net_working_capital, nwc_to_revenues) %>% 
    tidyr::pivot_longer(-year) %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    # scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    facet_grid(name ~ ., scales = "free")
  
}