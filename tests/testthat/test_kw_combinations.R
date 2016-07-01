library(advertools)
context("kw_combinations")
kw_df <- data.frame(kw_1 = c("word", "thing", "something"),
                         kw_2 = c("this", "that", "them"),
                         kw_3 = c("good", "morning", "friend"))

kw_df_combo <- kw_combinations(df = kw_df, cols = c("kw_1", "kw_2"))


test_that("kw_combinations adds a new column", {
  expect_equal(
    ncol(kw_combinations(kw_df, c("kw_1", "kw_2"))) - ncol(kw_df),
    1)
})

test_that("kw_combinations produces the right output", {
  expect_equal(kw_df_combo[1,1], "word this")
})

test_that("error is given on selecting non-existing columns", {
  expect_error(kw_combinations(kw_df, cols = c("col1", "col2")))
})
