library(advertools)
context("word_frequency")

# Objects ----------------------------------------------------------------------

keywords <- data.frame(
  words = c("the credit cards", "best of credit cards", "credit card application", "personal loan"),
  clicks = c(100, 120, 110, 130)
)
word_freq <- word_frequency(keywords)

# Tests ------------------------------------------------------------------------

test_that("various character values are correctly in place", {
  expect_equivalent(word_freq$word[1], "credit")
  expect_equivalent(word_freq$word[7], "card")
  expect_equivalent(word_freq$text[1], as.factor("personal loan"))
  expect_equivalent(word_freq$text[4], as.factor("the credit cards"))
})

test_that("various numeric values are correctly in place", {
  expect_equal(word_freq$abs_freq[1], 3)
  expect_equal(word_freq$abs_freq[7], 1)
  expect_equal(word_freq$perc[1], 0.287)
  expect_equal(word_freq$perc[7], 0.096)
})
