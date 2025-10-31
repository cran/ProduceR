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
      df %>% filter(!!rlang::parse_expr(criterion)) %>% tac() %>% mutate(indicator = TRUE),
      df %>% filter(!(!!rlang::parse_expr(criterion))) %>% tac() %>% mutate(indicator = FALSE)
    ) %>%
    # Preparation: calculate margins
    # overall margin (each column cross has exactly this total)
    group_by(column) %>%
    mutate(overall_margin = sum(Freq, na.rm = TRUE)) %>%
    ungroup() %>%
    # margin for indicator modalities (TRUE or FALSE)
    group_by(indicator) %>%
    mutate(modality_margin_criterion = sum(Freq, na.rm = TRUE) / n_distinct(column)) %>%
    ungroup() %>%
    # margin for modalities of different columns
    group_by(column, modality) %>%
    mutate(modality_margin_col_mod = sum(Freq, na.rm = TRUE)) %>%
    ungroup() %>%
    # Indicators of correlation between modalities 1 and 2
    mutate(
      expected_independence = modality_margin_criterion * modality_margin_col_mod / overall_margin,
      chi2 = (Freq - expected_independence) / expected_independence * (Freq - expected_independence),
      sign = ifelse(Freq < expected_independence, '-', '+'),
      modality_part_among_criterion = Freq / modality_margin_criterion,
      modality_part_among_whole = modality_margin_col_mod / overall_margin,
      TRUE_part_among_modality = Freq / modality_margin_col_mod,
      TRUE_part_among_whole = modality_margin_criterion / overall_margin
    ) %>%
    # Final table: the strongest correlations with TRUE are of interest, not with FALSE
    select(-modality_margin_col_mod, -modality_margin_criterion, -overall_margin, -expected_independence, -format) %>%
    arrange(desc(indicator), desc(chi2)) %>%
    relocate(.before = c("indicator", "column", "modality"))
}

#' @example stat <- chi2_find(iris, "Petal.Width == 1.0")
