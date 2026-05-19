#' @title Find modalities related to a criterion
#' 
#' @import dplyr
#' @importFrom rlang parse_expr
#' 
#' @param df data.frame
#' @param criterion character string: criterion that spots target rows
#' 
#' @return data.frame
#' 
#' @export
chi2_find <- function(df, criterion) {
  
  result <-
    
  # Cross table with `indicator` for all columns in the table
    bind_rows(
      df %>% filter(  !!rlang::parse_expr(criterion) ) %>% tac() %>% mutate(indicator = TRUE),
      df %>% filter(!(!!rlang::parse_expr(criterion))) %>% tac() %>% mutate(indicator = FALSE)
    ) %>%
    
  # Preparation: calculate margins
    
    # overall margin (each column cross has exactly this total)
    group_by(column) %>%
    mutate(overall_margin = sum(freq, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # margin for indicator modalities (TRUE or FALSE)
    group_by(indicator) %>%
    mutate(modality_margin_criterion = sum(freq, na.rm = TRUE) / n_distinct(column)) %>%
    ungroup() %>%
    
    # margin for modalities of different columns
    group_by(column, modality) %>%
    mutate(modality_margin_col_mod = sum(freq, na.rm = TRUE)) %>%
    ungroup() %>%
    
  # Indicators of correlation between modalities 1 and 2
    mutate(
      expected_independence = modality_margin_criterion * modality_margin_col_mod / overall_margin,
      chi2 = round((freq - expected_independence) / expected_independence * (freq - expected_independence), 2),
      sign = ifelse(freq < expected_independence, '-', '+'),
      modality_among_criterion = round(freq / modality_margin_criterion, 2),
      criterion_among_modality = round(freq / modality_margin_col_mod  , 2),
      modality_among_whole     = round(modality_margin_col_mod   / overall_margin, 2),
      criterion_among_whole    = round(modality_margin_criterion / overall_margin, 2)
    ) %>%
    
    # Final table: the strongest correlations with TRUE are of interest, not with FALSE
    filter(indicator == TRUE, sign == '+') %>%
    arrange(desc(indicator), desc(chi2)) %>%
    select(-indicator, -sign, -col_typology, -freq, -modality_margin_criterion, -overall_margin, -expected_independence, -format) %>%
    mutate(criterion = criterion) %>%
    relocate(chi2, criterion, column, modality, modality_margin_col_mod) %>%
    rename(freq_modality = modality_margin_col_mod) 
}

#' @example stat <- chi2_find(iris, "Petal.Width == 1.0")
