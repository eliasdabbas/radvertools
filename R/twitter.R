#' twtr_get_hashtags
#'
#' Extract hash tags and analyze them
#'
#' @param \code{x} a vector of tweets
#'
#' @export
#'
#' @return a list of data frames about the hash tags
twtr_get_hashtags <- function(x) {
  hashtags        <- tolower(stringr::str_extract_all(x, "(^#\\w+| #\\w+)", TRUE))
  colnames(hashtags) <- paste0("hash_", 1:ncol(hashtags))
  top_hashtags   <- as.data.frame(sort(table(hashtags), decreasing = TRUE),
                                  stringsAsFactors = F)
  top_hashtags   <- subset(top_hashtags, hashtags != "")
  hash_count      <- data.frame(hash_count = stringr::str_count(x, "(^#\\w+| #\\w+)"))
  hash_freq      <- as.data.frame(table(hash_count))
  hashtag_summary <- data.frame(tweets = length(x),
                                hashtag_count = sum(hash_count$hash_count),
                                unique_hashtags = nrow(top_hashtags),
                                hash_per_tweet = round(sum(hash_count$hash_count) / length(x), digits = 2))
  list(hashtags = hashtags, top_hashtags = top_hashtags,
       hash_count = hash_count, hash_freq = hash_freq,
       hashtag_summary = hashtag_summary)
}
#' twtr_get_mentions
#'
#' Extract mentions and analyze them
#'
#' @param \code{x} a vector of tweets
#'
#' @export
#' @return a list of data frames about the mentions
twtr_get_mentions <- function(x) {
  mentions        <- tolower(stringr::str_extract_all(x, "(^@\\w+| @\\w+|^\\.@\\w+)", TRUE))
  colnames(mentions) <- paste0("mention_", 1:ncol(mentions))
  top_mentions   <- as.data.frame(sort(table(mentions), decreasing = TRUE),
                                  stringsAsFactors = F)
  top_mentions   <- subset(top_mentions, mentions != "")
  mention_count      <- data.frame(mention_count = stringr::str_count(x, "(^@\\w+| @\\w+|^\\.@\\w+)"))
  mention_freq      <- as.data.frame(table(mention_count))
  mention_summary <- data.frame(tweets = length(x),
                                mention_count = sum(mention_count$mention_count),
                                unique_mentions = nrow(top_mentions),
                                mention_per_tweet = round(sum(mention_count$mention_count) / length(x), digits = 2))
  list(mentions = mentions, top_mentions = top_mentions,
       mention_count = mention_count, mention_freq = mention_freq,
       mention_summary = mention_summary)
}
twtr_get_word <- function(x, word = "word", exact = TRUE) {
  x <- tolower(x)
  word <- tolower(word)

  if (exact) {
    wordregex <- paste0("(^", word, " | ", word, " | ", word, "$)")
  }else {
    wordregex <- word
  }
  words        <- stringr::str_extract_all(x, wordregex, TRUE)
  colnames(words) <- paste0("word_", 1:ncol(words))
  top_words   <- as.data.frame(sort(table(words), decreasing = TRUE),
                               stringsAsFactors = F)
  top_words   <- subset(top_words, words != "")
  word_count      <- data.frame(word_count = stringr::str_count(x, wordregex))
  word_freq      <- as.data.frame(table(word_count))
  word_summary <- data.frame(tweets = length(x),
                             word_count = sum(word_count$word_count),
                             unique_words = nrow(top_words),
                             word_per_tweet = round(sum(word_count$word_count) / length(x), digits = 2))
  list(words = words, top_words = top_words,
       word_count = word_count, word_freq = word_freq,
       word_summary = word_summary)
}

