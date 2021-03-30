
#' Plot group
#'
#' @param data 
#'
#' @return
#' @import ggplot2
#' @importFrom scales unit_format
#' @export
#'
#' @examples
plot_group <- function(data) {
  
  data %>% 
    ggplot(aes(x = year, y = value, fill = group)) +
    geom_area(color = "white", alpha = .4) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(expand = c(0, 0), 
                       labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottom")
  
}