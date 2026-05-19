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
  tac_stock <- data.frame(modality = character(), freq = integer(), column = character(), format = character())

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
    
    # Compute `modality` - which is a appropriate transformation of column values
    
      # Identifier column (> 85 distinct values) : keep filling in (Y/N)
      if(length(unique(df[[col_name]])) > 85 & !(col_name %in% num_but_discrete) & num_but_discrete[[1]] != 'all') {
        
        col_typology = 'identifier'
        df <- df %>% mutate(modality = ifelse(is.na(modality), "not filled in (NA)", "filled in"))
        
      # Date or datetime column : keep year 
      } else if(is.POSIXt(df[[col_name]]) | is.Date(df[[col_name]])) {
        
        col_typology = 'date'
        df <- df %>% mutate(modality := format(.data[[col_name]], "%Y"))

      # Numeric value : keep sign (positive, negative, zero or NA)
      } else if(is.numeric(df[[col_name]]) | is.double(df[[col_name]])) {
        
        col_typology <- 'quantitive'
        df <- df %>% mutate(modality := case_when(is.null(.data[[col_name]]) ~ "NULL", 
                                                  is.na(  .data[[col_name]]) ~ "NA", 
                                                  .data[[col_name]]  < 0 ~ "negative", 
                                                  .data[[col_name]] == 0 ~ "equal to 0", 
                                                  .data[[col_name]]  > 0 ~ "positive" 
        ))

      # Other cases such as character, boolean, etc, with < 85 distinct values : keep whole value (levels)
      } else {
        
        col_typology <- 'levels'
        df <- df %>% mutate(modality := as.character(.data[[col_name]]))
        
      }

    # Result data.frame (tac_...) for the column
    tac_column <- get_tac_column(df, col_name, values, strates) %>%
      mutate(col_typology = col_typology)

    # Iteration
    tac_stock <- bind_rows(tac_stock, tac_column)
  }

  # (4) Select & arrange: to manage column and row order
  return(tac_stock %>% arrange(column, modality) %>% relocate(column, format, col_typology, modality))
  
}

#' Contingency table for column `col_name` in data.frame `df`
#'
#' @param col_name string : name of column to which generate the contingency table
#' @inheritParams tac
get_tac_column <- function(df, col_name, values, strates) {
  
  # Compute number of rows
  if(is.null(values)) {
    return(df %>% group_by(across(all_of(c("modality", strates)))) %>%
             summarise(freq = n()) %>%
             mutate(column = col_name, format = typeof(df[[col_name]])) %>%
             collect())
  
  # Compute sum of values
  } else {
    return(df %>% group_by(across(all_of(c("modality", strates)))) %>%
             summarise(freq = n(), across(all_of(values), ~ round(sum(., na.rm = TRUE), 2), .names = "sum_{.col}")) %>%
             mutate(column = col_name, format = typeof(df[[col_name]])) %>%
             collect())
  }
}

