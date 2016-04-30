library(advertools)
context("kw_match_type")

test_that("kw_exact wraps brackets",{
  expect_equal(kw_exact("exact match"), "[exact match]")
  expect_equal(kw_exact("'[-exact match]'"), "[exact match]")
})

test_that("kw_phrase wraps parentheses",{
  expect_equal(kw_phrase("[phrase match]"), '"phrase match"')
})

test_that("kw_broad removes punctuation", {
  expect_equal(kw_broad("[-keyword]"), "keyword")
  expect_equal(kw_broad("keyword"), "keyword")
})

test_that("ke_modified_broad adds + before all words", {
  expect_equal(kw_modified_broad("keyword one"), "+keyword +one")
  expect_equal(kw_modified_broad("keyword"), "+keyword")
})

test_that("kw_negative adds a - before a keyword", {
  expect_equal(kw_negative("keyword"), "-keyword")
  expect_equal(kw_negative("two words"), "-two words")
})

test_that("kw_negative_exact adds brackets and -", {
  expect_equal(kw_negative_exact("keyword"), "-[keyword]")
  expect_equal(kw_negative_exact("two words"), "-[two words]")
})

test_that("kw_negative_phrase adds quotations and -", {
  expect_equal(kw_negative_phrase("keyword"), "-\"keyword\"")
  expect_equal(kw_negative_phrase("two words"), "-\"two words\"")
})










