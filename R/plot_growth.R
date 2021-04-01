
#' Plot growth
#' 
#' TODO: fiks label, legg inn rolling mean (3 eller 5 år) på revenue_growth og nopm
#'
#' @param data 
#'
#' @return
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
plot_growth <- function(data, no_of_years = 3) {
  
  df_plot <- data %>% 
    select(year, contains("growth"), net_operating_profit_margin) %>% 
    tidyr::pivot_longer(-year) %>% 
    mutate(name = as.factor(name))
  
  year_end <- max(df_plot$year)
  
  data_last <- df_plot %>% filter(between(year, year_end - no_of_years + 1, year_end))
  
  data_ends <- data_last %>% 
    group_by(name) %>% 
    summarise(value = mean(value)) %>% 
    mutate(year = year_end)
  
  df_plot %>% 
    ggplot(aes(x = year, y = value, color = name, label = scales::percent(value, accuracy = .1))) +
    scale_y_continuous(labels = scales::percent) +
    geom_smooth(se = FALSE) +
    geom_label(data = data_ends,
               position = position_dodge2(width = 4),
               fontface ="plain",
               # color = "black",
               size = 4
      ) +
    geom_point(data = data_last, size = 3, alpha = .3) +
    geom_line(data = data_last, size = 3, alpha = .1) +
    theme(legend.position = "bottom")
    
  
}