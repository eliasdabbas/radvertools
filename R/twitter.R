#' Extract and analyze hashtags from tweets
#'
#' Get the hashtags from a vector of tweets, together with summaries about the
#' usage of those hashtags; frequency, occurrences, percentages, and more
#'
#' @param x a vector of tweets
#'
#' @export
#'
#' @return a list of data frames about the hash tags
#' \enumerate{
#'   \item \code{hashtags}: a matrix of all hashtags used, each row corresponding to a
#'   tweet.
#'   \item \code{top_hashtags}: a data.frame of the top hashtags, showing the frequency
#'   of usage, and sorted in descending order.
#'   \item \code{hash_count}: a data.frame with one column, where each row shows the
#'   number of hashtags used in the corresponding tweet.
#'   \item \code{hash_freq}: a data.frame showing the number of tweets where one hashtag
#'   was used, where two hashtags were used, etc
#'   \item \code{hashtag_summary}: a summary data.frame shwoing the number of tweets,
#'   total hashtag count, unique hashtags, and hashtags per tweet
#' }
#' @examples
#'
#' tweets <- c("this is my first #tweet with #two hashtags and one @mention",
#' "my second tweet has no hashtags and no mentions",
#' "my third #tweet #has #the most hashtags and @mentions @mention")
#' twtr_get_hashtags(tweets)
twtr_get_hashtags <- function(x) {
  hashtags        <- tolower(stringr::str_extract_all(x, "#\\S+", TRUE))
  colnames(hashtags) <- paste0("hash_", 1:ncol(hashtags))
  top_hashtags   <- as.data.frame(sort(table(hashtags), decreasing = TRUE),
                                  stringsAsFactors = F)
  top_hashtags   <- subset(top_hashtags, hashtags != "")
  hash_count      <- data.frame(hash_count = stringr::str_count(x, "#\\S+"))
  hash_freq      <- as.data.frame(table(hash_count))
  hashtag_summary <- data.frame(tweets = length(x),
                                hashtag_count = sum(hash_count$hash_count),
                                unique_hashtags = nrow(top_hashtags),
                                hash_per_tweet = round(sum(hash_count$hash_count) / length(x), digits = 2))
  list(hashtags = hashtags, top_hashtags = top_hashtags,
       hash_count = hash_count, hash_freq = hash_freq,
       hashtag_summary = hashtag_summary)
}
#' Extract and analyze mentions from tweets
#'
#' Get the mentions from a vector of tweets, together with summaries about the
#' usage of those mentions; frequency, occurrences, percentages, and more
#'
#' @param x a vector of tweets
#'
#' @export
#'
#' @return a list of data frames about the hash tags
#' \enumerate{
#'   \item \code{mentions}: a matrix of all mentions used, each row corresponding to a
#'   tweet.
#'   \item \code{top_mentions}: a data.frame of the top mentions, showing the frequency
#'   of usage, and sorted in descending order.
#'   \item \code{mention_count}: a data.frame with one column, where each row shows the
#'   number of mentions used in the corresponding tweet.
#'   \item \code{mention_freq}: a data.frame showing the number of tweets where one mention
#'   was used, where two mentions were used, etc
#'   \item \code{mention_summary}: a summary data.frame shwoing the number of tweets,
#'   total mention count, unique mentions, and mentions per tweet
#' }
#' @examples
#'
#' tweets <- c("this is my first #tweet with #two hashtags and one @mention",
#' "my second tweet has no hashtags and no mentions",
#' "my third #tweet #has #the most hashtags and @mentions @mention")
#' twtr_get_mentions(tweets)
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
#' Extract and analyze words from tweets
#'
#' Get a word from a vector of tweets, together with summaries about the
#' usage of this word; frequency, occurrences, percentages, and more
#'
#' @param x a vector of tweets
#' @param words a character vector of one or more words
#' @param exact logical, defaults to TRUE, do you want the \code{words}
#' to be exactly matched as complete words or do you want anny occurence of
#' this sequence of characters?
#'
#' @export
#'
#' @return a list of data frames about the word
#' \enumerate{
#'   \item \code{words}: a matrix of occurences of the word, each row corresponding to a
#'   tweet.
#'   \item \code{top_words} a data frame of the most frequently used words, sorted
#'   \item \code{word_count}: a data.frame with one column, where each row shows the
#'   number of times the word was used in the corresponding tweet.
#'   \item \code{word_freq}: a data.frame showing the number of tweets where the word
#'   was used once, where the word was used twice, etc
#'   \item \code{word_summary}: a summary data.frame shwoing the number of tweets,
#'   total word count, unique words, and words per tweet
#' }
#' @examples
#'
#' tweets <- c("this is my first #tweet with #two hashtags and one @mention",
#' "my second tweet has no hashtags and no mentions",
#' "my third #tweet #has #the most hashtags and @mentions @mention")
#' twtr_get_word(tweets, word = "hashtags", exact = TRUE)
twtr_get_words <- function(x, words, exact = TRUE) {
  x <- tolower(x)
  words <- tolower(words)

  if (exact) {
    wordregex <- paste0("\\b", paste0(words, collapse = "\\b|\\b"), "\\b")
  }else {
    wordregex <- paste0(words, collapse = "|")
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
                             words_per_tweet = round(sum(word_count$word_count) / length(x), digits = 2))
  list(words = words, top_words = top_words,
       word_count = word_count, word_freq = word_freq,
       word_summary = word_summary)
}
