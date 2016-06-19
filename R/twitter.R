twtr_get_hashtags <- function(x) {
  hash_idx <- stringr::str_detect(x, "#\\w+")
  hash_perc <- mean(hash_idx)
  hash_perc <- data.frame(hash_perc = hash_perc)
  hash_count <- stringr::str_count(x, "#\\w+")
  hash_count <- data.frame(hash_count = hash_count)
  hash_table <- table(hash_count) %>% as.data.frame()
  hashtags <- stringr::str_extract_all(x, "#\\w+", TRUE)
  colnames(hashtags) <- paste0("hash_", 1:ncol(hashtags))
  top_hashtags <- sort(table(hashtags), decreasing = T) %>%
    as.data.frame()
  top_hashtags <- top_hashtags[-1, ]
  top_hashtags$percentage <- top_hashtags$Freq / length(x)

  list(hash_perc = hash_perc, hash_count = hash_count,
       hash_table = hash_table, hashtags = hashtags,
       top_hashtags = top_hashtags)
}

twtr_get_mentions <- function(x) {
  mention_idx <- stringr::str_detect(x, "(^@\\w+| @\\w+)")
  mention_perc <- mean(mention_idx)
  mention_perc <- data.frame(mention_perc = mention_perc)
  mention_count <- stringr::str_count(x, "(^@\\w+| @\\w+)")
  mention_count <- data.frame(mention_count = mention_count)
  mention_table <- table(mention_count) %>% as.data.frame()
  mentions <- stringr::str_extract_all(x, "(^@\\w+| @\\w+)", TRUE)
  colnames(mentions) <- paste0("mention_", 1:ncol(mentions))
  top_mentions <- sort(table(mentions), decreasing = T) %>%
    as.data.frame()
  top_mentions <- top_mentions[-1, ]
  top_mentions$percentage <- top_mentions$Freq / length(x)
  list(mention_perc = mention_perc, mention_count = mention_count,
       mention_table = mention_table, mentions = mentions,
       top_mentions = top_mentions)
}
