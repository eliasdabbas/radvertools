#' Words in text
#'
#' Analyze the weighted frequency of words in a certain text with metrics.
#' Get hidden patterns in the text by extracting and aggregating metrics about words
#' in your text and comparing it to the original metric.
#'
#' useful for:
#' - keyword / search term analysis
#' - tweets analysis
#' - articles traffic (from titles or URLs)
#' - Facebook statuses
#' - any other social media text with metrics
#'
#' @param df a data frame with a character and a numeric vector columns
#'
#' @return a data frame of metrics by word along side the original metrics
#' @export
#'
#' @examples
#'
words_in_text <- function(df){

  names(df) <- c("text", "metric")
  df$metric <- tidyr::extract_numeric(df$metric)
  df <- df %>% dplyr::arrange(desc(metric))

  dfsep <- df
  dfsep$length <-   stringr::str_count(string = dfsep$text, pattern = ' ') + 1

  dfsep <- dfsep %>%
    tidyr::separate(col = text, into = 1:max(dfsep$length), sep = ' ', remove = FALSE)

  dfmelt <- dfsep %>% tidyr::gather(order, word, -c(text, length, metric))
  dfmelt <- dfmelt %>% dplyr::filter(!is.na(word))

  dffinal <- dfmelt %>% dplyr::group_by(word) %>%
    dplyr::summarise(metric = sum(metric, na.rm = TRUE)) %>%
    dplyr::arrange(desc(metric))

  dffinal$cum_perc <- round(cumsum(dffinal$metric / sum(dffinal$metric)), digits = 2)
  dffinal$perc <- round(dffinal$metric / sum(dffinal$metric), digits = 3)
  dffinal$keyword <- df$text[1:nrow(dffinal)]
  dffinal$original_metric <- df$metric[1:nrow(dffinal)]
  dffinal$wordle <- paste(dffinal$word, dffinal$metric, sep = ':')
  assign("textdf", dffinal, envir = globalenv())
  View(dffinal)
}
