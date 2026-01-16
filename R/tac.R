#' @title Computes a contingency table (tac) of all columns in a dataframe for control purposes
#'
#' @description Contingency table (tac) of all columns in a dataframe for control purposes
#'
#' @import dplyr
#'
#' @param df Input data.frame
#' @param values Vector of columns that serve as measures (amounts, counts, etc.)
#' @param sample_rate Sampling rate, if df is a remote table
#' @param num_but_discrete Vector of names of numeric columns with discrete modalities (not continuous)
#' @param strates Vector of column names by which to stratify the contingency tables
#'
#' @return data.frame
#'
#' @examples
#' tab <- tac(iris) # calculate column frequencies
#'
#' @export
tac <- function(df, values = NULL, sample_rate = 0.01, num_but_discrete = 'NULL', strates = NULL) {
  
  if(nrow(df) == 0) stop("The input table for the tac() function has no observations")

  # (1) Create an empty data.frame to store results
  tac_stock <- data.frame(modality = character(), Freq = integer(), column = character(), format = character())

  # (2) Collect remote table, if df is a remote table instead of an R data.frame
  if (identical(class(df), c("tbl_Oracle", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))) {
    if(sample_rate != 1) {
      df <- df %>% mutate(RAN_UNI_CNS = sql("MOD(ROWNUM, 1000)") / 1000) %>% filter(RAN_UNI_CNS < sample_rate) %>% collect()
    } else {
      df <- df %>% collect()
    }
  }

  # (3) Loop through each column in the data.frame
  for (col_name in colnames(df)) {
    # If factor column, for technical reasons we have to convert it to a character column (= the factor labels)
    # If numeric, keep only sign (positive, negative, zero or NA) to avoid too many modalities
    if(is.factor(df[[col_name]]) | col_name %in% num_but_discrete | n_distinct(df[[col_name]]) < 85) {
      df <- df %>% mutate(modality := as.character(.data[[col_name]]))
    } else if(is.POSIXt(df[[col_name]]) | is.Date(df[[col_name]])) {
      df <- df %>% mutate(modality := format(.data[[col_name]], "%Y"))
    } else if(is.numeric(df[[col_name]]) | is.double(df[[col_name]])) {
      df <- df %>% mutate(modality := as.character(sign(.data[[col_name]])))
    } else {
      df <- df %>% mutate(modality := as.character(.data[[col_name]]))
    }

    if(n_distinct(df$modality) > 85 & !(col_name %in% num_but_discrete) & num_but_discrete[[1]] != 'all') {
      df <- df %>% mutate(modality := ifelse(is.na(modality), "NA", "ID"))
    }

    # Result data.frame (tac_...) for the column
    tac_column <- get_tac_column(df, col_name, values, strates)

    # Iteration
    tac_stock <- bind_rows(tac_stock, tac_column)
  }

  # (4) Select & arrange: to manage column and row order
  return(tac_stock %>% arrange(column, modality) %>% relocate(column, modality, format, Freq))
}

#' Contingency table for column `col_name` in data.frame `df
#'
#' @param col_name string : name of column to which generate the contingency table
#' @inheritParams tac
get_tac_column <- function(df, col_name, values, strates) {
  if(is.null(values)) {
    return(df %>% group_by(across(all_of(c("modality", strates)))) %>%
             summarise(Freq = n()) %>%
             mutate(column = col_name, format = typeof(df[[col_name]])) %>%
             collect())
  } else {
    return(df %>% group_by(across(all_of(c("modality", strates)))) %>%
             summarise(Freq = n(), across(all_of(values), ~ round(sum(., na.rm = TRUE), 2), .names = "sum_{.col}")) %>%
             mutate(column = col_name, format = typeof(df[[col_name]])) %>%
             relocate(column, format, modality) %>%
             collect())
  }
}

