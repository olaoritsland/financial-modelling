
#' Read Damodaran
#'
#' @param url 
#' @param ... 
#'
#' @return
#' @importFrom rio import
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
read_damodaran <- function(url, ...) {
  
  rio::import(file = url, ...) %>% 
    janitor::clean_names() 
  
}