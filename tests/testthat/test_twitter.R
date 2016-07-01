library(advertools)
context("twitter")

# Objects ----------------------------------------------------------------------

tweets <- c("this is my first #tweet with #two hashtags and one @mention",
            "my second tweet has no hashtags and no mentions",
            "my third #tweet #has #the most hashtags and @mentions @mention")

tweet_plain_text <- c("this tweet has no hashtags or mentions",
                      "neither does this one",
                      "but this one, it also has nothing! ")

tweets_hash <- twtr_get_hashtags(tweets)
tweets_mention <- twtr_get_mentions(tweets)
tweets_words <- twtr_get_words(tweets, c("hashtags", "and"), exact = T)
tweets_words_notexact <- twtr_get_words(tweets, c("mention", "has"), exact = F)

# Tests ------------------------------------------------------------------------

test_that("tweets have correct number of hashtags & mentions",{
  expect_equal(sum(tweets_hash$hash_count$hash_count), 5)
  expect_equal(sum(tweets_mention$mention_count$mention_count), 3)
  expect_equal(sum(tweets_words$word_count$word_count), 6)
  expect_equal(sum(tweets_words_notexact$word_count$word_count), 9)
})

test_that("tweets & mentions have correct dims", {
  expect_equal(dim(tweets_hash$hashtags), c(3,3))
  expect_equal(dim(tweets_mention$mentions), c(3,2))
  expect_equal(dim(tweets_words$words), c(3,2))
  expect_equal(dim(tweets_words_notexact$words), c(3,4))
})

test_that("3rd and 4th hashtag correctly identified", {
  expect_equivalent(tweets_hash$hashtags[1,1], "#tweet")
  expect_equivalent(tweets_hash$hashtags[3,2], "#has")
  expect_equivalent(tweets_words$words[2,2], "and")
  expect_equivalent(tweets_words_notexact$words[1,2], "mention")
})

test_that("tweets with no hashtags or mentions produce an error", {
  expect_error(twtr_get_hashtags(tweet_plain_text))
  expect_error(twtr_get_mentions(tweet_plain_text))
  expect_error(twtr_get_words(tweet_plain_text))
})



