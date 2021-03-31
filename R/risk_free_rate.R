
#' Risk-free rate
#' 
#' Returns the risk-free rate, define as the 10-year government bond rate, based 
#' on country and year.
#' 
#' TODO: remove warning messages and consider changing to full dataset (faster) in stead of running function on each row
#'
#' @param country
#'
#' @return
#' @importFrom lubridate today
#' @export
#'
#' @examples
risk_free_rate <- function(country = "Norway") {
  
  if (country == "Norway") {
    
  file_name <- "renter_aar.csv"
  
  date <- lubridate::today() %>% format("%m/%d/%Y")
  
  url <- paste0(
    "https://www.norges-bank.no/globalassets/marketdata/stat/en/renter/v2/", 
    file_name, 
    "?v=",
    date,
    "090715&ft=.csv"
  )
  
  readr::read_csv(url) %>% 
    select(DATES, STATSOBL.10Y.EFF) %>% 
    rename(ten_year_gov_bond_rate = STATSOBL.10Y.EFF,
           year = DATES) %>% 
    mutate(ten_year_gov_bond_rate = ten_year_gov_bond_rate / 100)
  
  }
  else {
    stop(paste0("Function does not support ", country))
  }
  
}

