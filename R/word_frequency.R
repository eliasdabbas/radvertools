#' Word frequency
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
#' word_frequency(boxoffice)
word_frequency <- function(df, sep = " "){

  names(df) <- c("text", "metric")
  df$metric <- tidyr::extract_numeric(df$metric)
  df <- df[order(df$metric,decreasing = TRUE), ]
  originaldf <- df
  df <- df %>% dplyr::filter(!is.na(text))
  df$length <-   stringr::str_count(string = df$text, pattern = ' ') + 1
  df <- df %>%
    tidyr::separate(col = text, into = 1:max(df$length), sep = sep, remove = FALSE)

  df <- df %>% tidyr::gather(order, word, -c(text, length, metric))
  df <- df %>% dplyr::filter(!is.na(word))
  df$word <- tolower(df$word)
  df <- df %>% dplyr::group_by(word) %>%
    dplyr::summarise(abs_freq = n(),   wtd_freq = sum(metric, na.rm = TRUE)) %>%
    dplyr::arrange(desc(wtd_freq))

  df$cum_perc <- round(cumsum(df$wtd_freq / sum(df$wtd_freq)), digits = 2)
  df$perc <- round(df$wtd_freq / sum(df$wtd_freq), digits = 3)
  df$text <- originaldf$text[1:nrow(df)]
  df$original_metric <- originaldf$metric[1:nrow(df)]

  assign("textdf", df, envir = globalenv())
  View(df)
}

#' The boxoffice revenues of the top 100 movies of all time
#'
#' A dataset of the top 100 grossing movies of all time, as of April
#' 2016.
#'
#' @format A data frame with 101 rows and 2 variables:
#' \describe{
#'    \item{Movie_Title}{Name of the movie}
#'    \item{Lifetime.Gross}{Lifetime revnue in US$}
#' }
#' @source \url{http://www.boxofficemojo.com/alltime/domestic.htm}
"boxoffice"























