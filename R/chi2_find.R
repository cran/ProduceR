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
  
  tac_FALSE <- df %>% filter(!(!!rlang::parse_expr(criterion))) %>% tac() %>% mutate(is.criterion = FALSE)
  # forcer le type identifiant, qui peut être difféent dans tac_TRUE pour des raisons d'effectif !
  list_ident <- unlist(tac_FALSE %>% filter(col_typology == 'identifier') %>% select(column) %>% distinct)
  
  tac_TRUE  <- df %>% filter(  !!rlang::parse_expr(criterion) ) %>% tac(force_identifier = list_ident) %>% mutate(is.criterion = TRUE)
  
  result <-
    
  # Cross table with `is.criterion` for all columns in the table
    bind_rows(tac_FALSE, tac_TRUE) %>%
    
  # Preparation: calculate margins
    
    # overall margin (each column cross has exactly this total)
    group_by(column) %>%
    mutate(overall_margin = sum(freq, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # margin for is.criterion modalities (TRUE or FALSE)
    group_by(is.criterion) %>%
    mutate(criterion_margin = sum(freq, na.rm = TRUE) / n_distinct(column)) %>%
    ungroup() %>%
    
    # margin for modalities of different columns
    group_by(column, modality) %>%
    mutate(modality_margin = sum(freq, na.rm = TRUE)) %>%
    ungroup() %>%
    
  # Indicators of correlation between modalities 1 and 2
    mutate(
      expected_independence = criterion_margin * modality_margin / overall_margin,
      chi2 = round((freq - expected_independence) / expected_independence * (freq - expected_independence), 2),
      sign = ifelse(freq < expected_independence, '-', '+'),
      modality_among_criterion = round(freq / criterion_margin, 2),
      criterion_among_modality = round(freq / modality_margin  , 2),
      modality_among_whole     = round(modality_margin   / overall_margin, 2),
      criterion_among_whole    = round(criterion_margin / overall_margin, 2)
    ) %>%
    
    # Final table: the strongest correlations with TRUE are of interest, not with FALSE
    filter(is.criterion == TRUE, sign == '+') %>%
    arrange(desc(is.criterion), desc(chi2)) %>%
    mutate(`criterion` = criterion) %>%
    select(-is.criterion, -sign, -col_typology, -freq, -criterion_margin, -overall_margin, -expected_independence, -format) %>%
    relocate(chi2, `criterion`, column, modality, modality_margin, modality_among_criterion, modality_among_whole, criterion_among_modality, criterion_among_whole) %>%
    rename(freq_modality = modality_margin)
}

#' @example stat <- chi2_find(iris, "Petal.Width == 1.0")
