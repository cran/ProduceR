#' @title Missing: Generate a synthetic table of missing values for all columns of a data.frame
#'
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' @importFrom stats setNames
#' @importFrom stats complete.cases
#' 
#' @description Get a synthetic table of missing values for all columns of a data.frame
#'
#' @param df data.frame: Input data.frame
#' @param values column: Variable (~weight) to measure the number of missing values (otherwise, count of rows)
#' @param view boolean: automatic opening of generated tables
#'
#' @return data.frame
#'
#' @examples miss(base_eu_2025, view = FALSE)  # Checking NA values for all columns of base_eu_2025
#'
#' @export
miss <- function(df, values = NULL, view = T) {

  if (!is.null(groups(df))) df <- df %>% ungroup()
  
  # Table listing NAs per variable: raw
  if (is.null(values)) {
    stat <- df %>% summarise_all(~ sum(is.na(.)))
  } else {
    stat <- df %>% summarise(across(everything(), ~ sum(.data[[values]][is.na(.)], na.rm = TRUE)))
  }

  # Detailed view of missing values
  if (view == TRUE) {
    miss_xmpl <- df[!complete.cases(df), ] %>% head(17)
    View(miss_xmpl)
  }

  # Table listing NAs per variable: final form
  # Note: To handle cases where df is a reference to a remote table and not an R table: added collect() instruction
  miss_r_tab <- tibble::rownames_to_column(setNames(data.frame(t(stat %>% collect())), c("Missing")), var = "Variable")
  if (view == TRUE) View(miss_r_tab)
  return(miss_r_tab)
}
