# VR 202306: Add option to restart with a new partition, but without restarting everything. For both functions.
# VR 202312: Does not work if count_what is a vector of columns (only single column)
#' @title Analysis of the cardinality of a key/identifier in a table
#'
#' @description Creates multiple result tables.
#' The term "n-plicate" is used to generalize the notion of duplicate: a n_plicate can be a duplicate, a triplicate, etc.
#'
#' @import dplyr
#' @importFrom utils View
#' @importFrom utils head
#' 
#' @param tab Either an R dataframe or a reference to a remote table ("remote table")
#' @param keyby (character vector) names of the column(s) considered as keys
#' @param count_what (character vector) defines what to count by key (by *keyby*).
#' 'rows' to count distinct rows, otherwise the name of the columns whose distinct values are to be counted
#' @param partition (character vector) names of the columns by which to break down the analysis
#' @param view automatic opening of generated tables
#'
#' @return A set of dataframes in the global environment.
#' * nup_r_tab: table of n-plicate counts
#' * nup_xpl_r: table of n-plicate examples
#' * nup_exZ_r: table of examples of (n-plicates with value 0)
#' * nup_r_tab_part: table of n-plicate counts broken down by the modalities of the `partition` columns
#'
#' @examples 
#' # Check if "name" is a unique key of the starwars table (yes !)
#' dup(dplyr::starwars, keyby = "name", view = FALSE)
#' 
#' # Check if "key" is a unique key of the basic table (no !)
#' basic <- data.frame("key"   = c("a", "b", "c", "d", NA, "a", "e", "f"), 
#'                     "value" = c(112, 117, 317,  NA,  0,  17, 117, 112))
#' dup(basic, keyby = "key", view = FALSE)
#' 
#' @export
dup <- function(tab, keyby, count_what = "rows", partition = NULL, view = TRUE) {
  
  # (I) Intermediate table with the same structure as the input table, but with an additional column on the right giving the number of n-plicates
  # -----------------------------------------------------------------------------------------------------------------
  
  if (count_what != "rows") {
    nup_i_all <- tab %>%
      group_by(across(all_of(!!keyby))) %>%
      mutate(cardinal = n_distinct({{count_what}})) %>%
      ungroup() # Alas, n_distinct({{count_what}}) only accepts a single column, I tried in vain with across(all_of(count_what))
  } else {
    nup_i_all <- tab %>%
      group_by(across(all_of(!!keyby))) %>%
      mutate(cardinal = n()) %>%
      ungroup()
  }

  # (II) Result tables
  # -----------------------------------------------------------------------------------------------------------------
  
  # Result 1: table of n-plicate counts (nup_r_tab = Results table of n_plicates)
  
    nup_r_tab <- nup_i_all %>%
      group_by(cardinal) %>%
      summarise(nb_lignes = n()) %>%
      mutate(nb_clefs = nb_lignes / cardinal) %>%
      arrange(cardinal) %>%
      collect()
    
    if (count_what != 'rows') nup_r_tab <- nup_r_tab %>% rename(nb_val_distinct = nb_lignes)
    
    if (view) View(nup_r_tab)
    
  # Result 2: examples of n-plicates
    
    if (!(tally(nup_r_tab) == 1 && sum(nup_r_tab$cardinal) == 1)) # if there are duplicates
    {
      nup_xpl_r <- nup_i_all %>%
        filter(cardinal > 1) %>%
        head(527) %>%
        arrange(across(all_of(!!keyby))) %>%
        collect()
      
      if (view) View(nup_xpl_r)
    }
  
  # Result 3: examples of zero-plons (rows whose key is NA)
  
    nup_exZ_r <- nup_i_all %>% filter(is.na(.data[[keyby]])) %>% head(510) %>% collect()

    if (view) if(nrow(nup_exZ_r) > 0) View(nup_exZ_r)
  
  # Result 4: modalities of the partition column when there are n-plicates
  
    if (!is.null(partition)) {
      nup_r_tab_part <- nup_i_all %>%
        group_by(!!sym(partition), cardinal) %>%
        summarise(nb_clefs = n_distinct(across(all_of(!!keyby))), nb_lignes = n()) %>%
        arrange(!!sym(partition), cardinal) %>%
        collect()
      
      if (view) View(nup_r_tab_part) # VR issue: partition is not in nup_i_all
    }
    
}