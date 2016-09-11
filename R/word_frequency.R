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
#' @param sep the separator of words, defaults to space, but can be modified if needed
#' @param rm_words words to remove, defaults to English stopwords from the \code{tm}
#' package
#'
#' @return a data frame of metrics by word along side the original metrics
#' @export
#'
#' @examples
#' word_frequency(boxoffice[c("title", "lifetime_gross")])
word_frequency <- function(df, sep = " ", rm_words = stopwords_en){

  names(df) <- c("text", "metric")
  df$metric <- as.numeric(gsub("[^0-9.-]+", "", as.character(df$metric)))
  df <- df[order(df$metric,decreasing = TRUE), ]
  originaldf <- df
  df <- df %>% dplyr::filter(!is.na(text))
  df$length <-   stringr::str_count(string = df$text, pattern = ' ') + 1
  df <- df %>%
    tidyr::separate(col = text, into = as.character(1:max(df$length)), sep = sep, remove = FALSE)

  df <- df %>% tidyr::gather(order, word, -c(text, length, metric))
  df <- df %>% dplyr::filter(!is.na(word))
  df$word <- tolower(df$word)
  df <- df %>% dplyr::filter(!word %in% rm_words)
  df <- df %>% dplyr::group_by(word) %>%
    dplyr::summarise(abs_freq = n(),   wtd_freq = sum(metric, na.rm = TRUE)) %>%
    dplyr::arrange(desc(wtd_freq))

  df$cum_perc <- round(cumsum(df$wtd_freq / sum(df$wtd_freq)), digits = 2)
  df$perc <- round(df$wtd_freq / sum(df$wtd_freq), digits = 3)
  df$text <- originaldf$text[1:nrow(df)]
  df$original_metric <- originaldf$metric[1:nrow(df)]
  df
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
