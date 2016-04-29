#' Keyword combinations
#'
#' This function helps you generate combinations and sequences of words, to
#' obtain meaningful keywords for your campaigns. It assumes you already have a
#' data frame with different words for each element of search
#'
#' @param df  data frame containing different word variations in each column
#' @param cols character vector of names of the columns that you want to
#' combine. The order is important here
#' @param sep Optional separator of the final keywords generated
#'
#' @return data fram with a column for each keywrod template
#' @export
#'
#' @examples
#' kw_combinations(mtcars, c("cyl", "mpg"))
kw_combinations <- function(df, cols, sep = " ") {
  if (!all(cols %in% names(df))){
    stop("make sure you select columns from the data frame")
  }
  index <- vector()
  for (i in seq_along(cols)) {
    idx <- which(cols[i] == names(df))
    index <- c(index, idx)
    index
  }
  columns <- names(df)[index]
  new_column <- paste(cols, collapse = "_")
  df <- df %>% tidyr::unite_(col = new_column, sep = sep, remove = F, columns)
  df
}
