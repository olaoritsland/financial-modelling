
#' Debt-to-equity
#'
#' @param url 
#' @param industry 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
debt_to_equity <- function(url = "http://www.stern.nyu.edu/~adamodar/pc/datasets/dbtfund.xls",
                         industry) {
  
  read_damodaran(url, which = 2, skip = 7) %>% 
    filter(industry_name == industry) %>% 
    pull(market_d_e_adjusted_for_leases)
  
}

  