#' @title sums: Generate a synthetic table of sums of numeric columns
#'
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' 
#' @description Get a synthetic table of sums of all numeric columns of a data.frame
#'
#' @param tab data.frame: Input data.frame
#' 
#' @return data.frame
#'
#' @examples sums(mtcars)  # Checking NA values for all columns of mtcars (none)
#'
#' @export
sums <- function(tab){
  
  # compute result df 
  sums_r_tab <- data.frame(t(data.frame(lapply(tab %>% ungroup %>% select(where(is.numeric)), function(x) sum(x, na.rm = T)))))
  
  # reshape result df for lisibility 
  colnames(sums_r_tab)[1] <- "sum"
  sums_r_tab <- tibble::rownames_to_column(sums_r_tab, var = "column")
}