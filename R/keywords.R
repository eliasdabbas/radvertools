#'Broad match keywords
#'
#'This is useful in case you already have keywords as phrase, or exact for
#'example and want to return to broad. Another useful thing about this function is
#'that it serves as an intermediary state between transforming a keyword from phrase to exact
#'for example, it makes sure that you don't end up with something like ["keyword"].
#'@section Examples:
#'
#'\code{iris$exact <- kw_exact(iris$Species)}
#'
#'\code{iris$broad <- kw_broad(iris$exact)}
#'
#'\code{mtcars$phrase <- kw_phrase(rownames(mtcars))}
#'
#'\code{mtcars$broad <- kw_broad(mtcars$phrase)}
#'
#'

kw_broad <- function(x){
  modifiers <- paste('"', "\\[", "\\]", "\\+", "\\-", sep  = "|")
  gsub(pattern = modifiers, replacement = "", x = x)
}
