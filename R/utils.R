utils::globalVariables(c(":=", "Freq", "RAN_UNI_CNS", "cardinal", "chi2", "column", "expected_independence",
                         "indicator", "modality", "modality_margin_col_mod", "modality_margin_criterion", "nb_clefs", "score", "abscore",
                         "nb_lignes", "overall_margin"))

#' @title short for `paste0()`
#' @param a string
#' @param b string
#' @return string
#' @export
`%+%` <- function(a, b) paste0(a, b)


#' @title coltypes()
#' @description Create vector of df's column types. Similar to colnames(), but with column types instead of names.
#' @param df data.frame
#' @return vector
#' @export
coltypes <- function(df) sapply(df, typeof)


#' @title is.POSIXct
#' @description Returns TRUE or FALSE depending on whether its argument is of POSIXct type or not
#' @param x object
#' @return TRUE/FALSE
#' @export
is.POSIXct <- function(x) inherits(x, "POSIXct")

#' @title is.POSIXlt
#' @description Returns TRUE or FALSE depending on whether its argument is of POSIXlt type or not
#' @param x object
#' @return TRUE/FALSE
#' @export
is.POSIXlt <- function(x) inherits(x, "POSIXlt")

#' @title is.POSIXt
#' @description Returns TRUE or FALSE depending on whether its argument is of POSIXxt type or not
#' @param x object
#' @return TRUE/FALSE
#' @export
is.POSIXt  <- function(x) inherits(x, "POSIXt")

#' @title is.Date
#' @description Returns TRUE or FALSE depending on whether its argument is of Date type or not
#' @param x object
#' @return TRUE/FALSE
#' @export
is.Date    <- function(x) inherits(x, "Date")


#' @title get_recursion_depth
#' @description get recursion depth of a list
#' @param x : input list
#' @param depth : depth of x in another list (1 if x in a list. 2 if x is in a list of lists. Etc.)
#' @return integer
#' @export
get_recursion_depth <- function(x, depth = 0) {
  if (!is.list(x)) {
    if(depth == 0) stop("Your object in input of `get_recursion_depth()` is not a list.")
    return(depth)
  } else {
    max_depth <- depth
    for (item in x) {
      item_depth <- get_recursion_depth(item, depth + 1)
      max_depth <- max(max_depth, item_depth)
    }
    return(max_depth)
  }
}
