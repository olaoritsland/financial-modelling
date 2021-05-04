
#' Plot equity bridge
#'
#' @param data 
#'
#' @return
#' @importFrom plotly plot_ly layout
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @export
#'
#' @examples
plot_equity_bridge <- function(data) {
  
  rank <- tibble::tibble(
    rank = 1:5,
    name = c("sum_discounted_fcff", "terminal_value_discounted",
             "enterprise_value", "net_debt", "market_value_of_equity")
  )
  
  
  plot_data <- data %>% 
    tidyr::pivot_longer(-wacc) %>% 
    select(-wacc) %>% 
    left_join(rank, by = "name") %>% 
    arrange(rank) %>% 
    mutate(
      measure = if_else(name == "market_value_of_equity" |
                          name == "enterprise_value", "total", "relative"),
      name = factor(x = name, levels = unique(name))
    ) 
  
  
  plotly::plot_ly(plot_data, 
                  name = "20", 
                  type = "waterfall", 
                  measure = ~measure,
                  x = ~name, 
                  textposition = "outside", 
                  y = ~value, 
                  text = ~paste0(as.character(round(value/1e6, 1)), " m"),
                  hoverinfo = 'none', 
                  cliponaxis = FALSE,
                  opacity = .7,
                  connector = list(line = list(color= "rgb(63, 63, 63)")),
                  totals = list(marker = list(color = "green",
                                              line = list(color = 'green',
                                                          width = .5)))
  ) %>%
    plotly::layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""),
                   autosize = TRUE,
                   showlegend = FALSE
    )
  
  
}
