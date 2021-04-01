
#' Plot value creation
#'
#' @param data 
#' @param avkastning
#' @param avkastningskrav
#' 
#' @details ROE > ROIC --> belåner operasjonelle eiendeler som gir avkastning 
#'   > finansieringskostnaden (kd)
#'
#' @return
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @export
#'
#' @examples
plot_value_creation <- function(data, avkastning = "roic", avkastningskrav = "wacc") {
  
  data %>% 
    select(year, {avkastning}, {avkastningskrav}) %>% 
    tidyr::pivot_longer(-year) %>% 
    ggplot(aes(x = year, y = value, color = name)) +
    geom_line()
  
}