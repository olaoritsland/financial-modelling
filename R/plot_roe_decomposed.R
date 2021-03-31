
#' Plot ROE decomposed
#'
#' @param data 
#'
#' @return
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
plot_roe_decomposed <- function(data) {
  
  
  df_plot <- data %>% 
    select(year, roic, non_operating_return) %>% 
    pivot_longer(-year)
  
  #data_ends <- df_plot %>% filter(year == 2019) %>% arrange(name)
  
  df_plot %>% 
    ggplot(aes(x = year, y = value, fill = name)) +
    geom_bar(alpha = .4, stat = "identity") +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = unique(data$year)) +
    # TODO:
    # geom_text(
    #   aes(label = scales::percent(value)), data = data_ends,
    #   fontface ="plain", color = "black", size = 4
    # ) +
    theme(panel.grid.major.x = element_blank())
  
}