# FUNCTIONS FOR DATA MANIPULATION


# Count the number of entries in a vector [expands length()]
len <- function(
  vec, 
  na.rm = FALSE,   # Exclude NAs from count?
  unique = FALSE   # Count only unique values?
){
  if (na.rm) vec <- vec[!is.na(vec)] 
  if (unique) x <- unique(x)
  length(vec)
}

# CANT REMEMBER EXACT FUNCTIONALITY -- REQUIRES REVISION
transpose_df <- function(df, names_from){
  if (!is.na(names_from)) {
    names <- df[, names_from][[1]]
    df <- df |> 
      dplyr::select(-all_of(names_from)) |> 
      t()
    colnames(df) <- names
  }
  df
}