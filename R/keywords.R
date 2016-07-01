#' Broad match keywords
#'
#' This is useful in case you already have keywords as phrase, or exact for
#' example and want to return to broad. Another useful thing about this function is
#' that it serves as an intermediary state between transforming a keyword from phrase to exact
#' for example, it makes sure that you don't end up with something like ["keyword"].
#'
#' @param x a keyword or a vector of keywords
#'
#' @return keywords without ", [, ], +, or -
#' @export
#' @keywords AdWords, Bing Ads, SEM, online marketing
#' @examples
#' iris$exact <- kw_exact(iris$Species)
#' iris$broad <- kw_broad(iris$exact)
#' mtcars$phrase <- kw_phrase(rownames(mtcars))
#' mtcars$broad <- kw_broad(mtcars$phrase)
kw_broad <- function(x){
  modifiers <- paste('"', "\\[", "\\]", "\\+", "\\-", "'", sep  = "|")
  gsub(pattern = modifiers, replacement = "", x = x)
}

#' Exact match keywords
#'
#' Wraps square brackets around a character vector to turn them into exact-match
#' keywords. It takes care of removing any other match characters like +, -, or ".
#'
#' @param x a keyword or a vector of keywords
#'
#' @return keywords in exact match
#' @export
#'
#' @examples
#' mtcars$exact <- kw_exact(rownames(mtcars))
#' keywords <- c("movie tickets", "movies", "best movies")
#' kw_exact(keywords)
kw_exact <- function(x){
  paste0("[", kw_broad(x), "]")
}

#' Phrase match keywords
#'
#' Wraps quotation marks around a character vector to turn them into phrase-match
#' keywords. It takes care of removing any other match characters like +, -, or [
#' if they exist.
#'
#' @param x a keyword or a vector of keywords
#'
#' @return keywords in phrase match
#' @export
#'
#' @examples
#' mtcars$phrase <- kw_phrase(rownames(mtcars))
#' keywords <- c("movie tickets", "movies", "best movies")
#' kw_phrase(keywords)
kw_phrase <- function(x){
  paste0('"', kw_broad(x), '"')
}


#' Negative match keywords
#'
#' Adds a minus sign to each word in a character vector to turn them into negative match
#' keywords. It takes care of removing any other match characters like +, [, or ".
#'
#' @param x a keyword or a vector of keywords
#'
#' @return negative match keywords
#' @export
#'
#' @examples
#' mtcars$negative <- kw_negative(rownames(mtcars))
#' keywords <- c("movie tickets", "movies", "best movies")
#' kw_negative(keywords)
kw_negative <- function(x){
  paste0("-", kw_broad(x))
}


#' Negative phrase match keywords
#'
#' Wraps a minus sign as well as quotation marks around a character vector to turn them into
#' the "embedded" negative phrase match keywords.
#' It takes care of removing any other match characters like +, -, or ".
#'
#' @param x a keyword or a vector of keywords
#'
#' @return negative phrase match keywords
#' @export
#'
#' @examples
#' mtcars$negative_phrase <- kw_negative_phrase(rownames(mtcars))
#' keywords <- c("movie tickets", "movies", "best movies")
#' kw_negative_phrase(keywords)
kw_negative_phrase <- function(x){
  paste0('-"', kw_broad(x), '"')
}

#' Negative exact match keywords
#'
#' Wraps a minus sign as well as square brackets around a character vector to turn them into
#'the "embedded" negative exact match keywords.
#'It takes care of removing any other match characters like +, -, or ".
#'
#' @param x a keyword or a vector of keywords
#'
#' @return negative exact match keywords
#' @export
#'
#' @examples
#' mtcars$negative_exact <- kw_negative_exact(rownames(mtcars))
#' keywords <- c("movie tickets", "movies", "best movies")
#' kw_negative_exact(keywords)
kw_negative_exact <- function(x){
  paste0('-[', kw_broad(x), ']')
}

#' Modified broad match keywords
#'
#' Add a plus sign before each word of the keyword / keyphrase.
#' Note that sometimes it's better to only have one or two words with the plus sign
#' next to them.
#'
#' @param x a keyword or a vector of keywords
#' @param words add words if you want to have a + sign only next to them
#' @return modified broad match keywords
#' @export
#'
#' @examples
#' keywords <- c("chocolate stores", "buy chocolate online", "best chocolate")
#' kw_modified_broad(keywords)
#' @family keyword functions
kw_modified_broad <- function(x, words = "") {
  if(words != "") {
    x <- tolower(x)
    words <- tolower(words)
    replacement <- paste0("+", words)
    pattern <- paste0("\\b", words, "\\b")
    x <- gsub(pattern = pattern, replacement = replacement, x = x)
    x
  }else {
    x <- gsub(pattern = "\\s", replacement = " +", x = x)
    x <- paste0("+", x)
    x
  }
}
