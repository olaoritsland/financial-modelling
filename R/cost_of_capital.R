
#' WACC
#' 
#' Calculates the weighted average cost of capital
#'
#' @param debt_to_equity 
#' @param cost_of_debt 
#' @param cost_of_equity 
#' @param tax_rate 
#'
#' @return
#' @export
#'
#' @examples
wacc <- function(debt_to_equity, cost_of_debt, cost_of_equity, tax_rate) {
  
  
  debt_ratio = debt_to_equity / (1 + debt_to_equity)
  
  debt_share = cost_of_debt * (1 - tax_rate) * debt_ratio
  
  equity_share = cost_of_equity * (1 - debt_ratio)
  
  debt_share + equity_share
  
}

#' Cost of equity
#'
#' @param rf 
#' @param beta 
#' @param expected_market_return 
#'
#' @return
#' @export
#'
#' @examples
cost_of_equity <- function(rf, beta, expected_market_return) {
  
  rf + beta * (expected_market_return - rf)
  
}