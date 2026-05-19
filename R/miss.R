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
#' @param view boolean: Display a glimpse of cases with NA values
#'
#' @return data.frame
#'
#' @examples miss(mtcars)  # Checking NA values for all columns of mtcars (none)
#'
#' @export
miss <- function(df, values = NULL, view = FALSE) {

  if (!is.null(groups(df))) df <- df %>% ungroup()
  
  # Table listing NAs per variable: raw
  if (is.null(values)) {
    stat <- df %>% summarise_all(~ sum(is.na(.)))
  } else {
    stat <- df %>% summarise(across(everything(), ~ sum(.data[[values]][is.na(.)], na.rm = TRUE)))
  }

  # Detailed view of missing values
  if (view == TRUE) {
    glimpse(df[!complete.cases(df), ])
  }

  # Table listing NAs per variable: final form
  # Note: To handle cases where df is a reference to a remote table and not an R table: added collect() instruction
  return(tibble::rownames_to_column(setNames(data.frame(t(stat %>% collect())), c("Missing")), var = "Variable"))
}
