#' @title TAC-based Outlier Control (TOC)
#'
#' @description Generalized detection of outlier values in a database, based on contingency tables (tac)
#'
#' @import dplyr
#'
#' @param df1 Input data.frame (to compare with df2)
#' @param df2 Input data.frame (to compare with df1)
#' @param values Vector of columns that serve as measures (amounts, counts, etc.)
#' @param a Allowed absolute variation
#' @param r Allowed relative variation
#' @param sample_rate Sampling rate, if df is a remote table
#' @param num_but_discrete Numeric variables to be treated as discrete modal variables. If 'all', all numeric variables are treated as discrete modal variables.
#'
#' @return data.frame
#' @export
toc <- function(df1, df2, values = NULL, a = 10, r = 0.34, sample_rate = 0.01, num_but_discrete = 'NULL') {
  
  dfname1 <- deparse(substitute(df1))
  dfname2 <- deparse(substitute(df2))

  # This program only accepts a single quantity/amount variable
  if(length(values) > 1) stop("The input values vector has multiple elements, which is not accepted by this function.")

  # Both data.frames must have the same structure
  col1 <- colnames(df1)
  col2 <- colnames(df2)
  if(!identical(col1, col2)) warning("Different structure between the two tables. Comparison will be made on common columns.")
  common_cols <- intersect(col1, col2)
  df1 <- df1 %>% select(all_of(common_cols))
  df2 <- df2 %>% select(all_of(common_cols))

  # Generalized contingency tables for both data.frames
  tac1 <- tac(df1, values, sample_rate, num_but_discrete)
  tac2 <- tac(df2, values, sample_rate, num_but_discrete)

  # Names of the values on which detection is based
  if(is.null(values)) {
    value <- "Freq"
  } else {
    value <- paste0("sum_", values[[1]])
  }
  value.x <- value %+% ".x"
  value.y <- value %+% ".y"

  # Outlier value detection
  tt <- full_join(tac1, tac2, by = c("column", "format", "modality"), na_matches = "na")
  tt$score <- mapply(toc_score, tt[[value.x]], tt[[value.y]], a)
  tt$score <- ifelse(abs(tt$score) > r, tt$score, 0)
  tt$format <- NULL
  tt$abscore <- abs(score)

  # Conclusion
  return(tt %>% arrange(column, desc(abscore)) %>% select(all_of(c("column", "modality", value.x, value.y, "score"))) %>%
           rename(!!dfname1 := value.x, !!dfname2 := value.y))
}

#' @title Scoring significativity of difference between two values x and y
#' @description Difference score between x and y (0 = no significant difference, >0 = presence of significant difference)
#' @param x (num) First value to compare
#' @param y (num) Second value to compare
#' @param a (num) Absolute difference threshold below which all differences are considered normal
#' @return numeric
#' @examples toc_score(15, 1500, a = 500) # 1.91: significant difference
#' @examples toc_score(1432, 1501, a = 100) # 0: non-significant difference
#'
#' @export
toc_score <- function(x, y, a) {
  logistic <- function(x) 1 / (1 + exp(-x))
  x <- ifelse(is.na(x), 0, x)
  y <- ifelse(is.na(y), 0, y)
  if      (x + a < y)  { logistic(y/(x + a) - 1)}
  else if (x - a <= y & y <= x + a)  { 0 }
  else if (y < x - a)  {-logistic(x/(y + a) - 1)}
}
