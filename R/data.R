#' Boxoffice revenues of 14,196 US movies
#'
#' A dataset of 14k movies produced since 1920, together with
#' alltime boxoffice revenues, as of June 2016.
#'
#' @format A data.frame with 14,196 observations and 5 variables
#' \describe{
#'   \item{rank}{ranking, based on alltime revenue}
#'   \item{title}{the name of the movie}
#'   \item{studio}{the production studio}
#'   \item{lifetime_gross}{lifetime boxoffice revenue}
#'   \item{year}{year of production}
#' }
#' @source \href{http://www.boxofficemojo.com/alltime/domestic.htm}{Boxoffice Mojo}
"boxoffice"
#' English stopwords
#'
#' 174 words frequenty used English words for removal in text mining,
#' and based on the \code{tm} package
#'
#' @source \link[tm]{stopwords}
#'
"stopwords_en"
#' Arabic stopwords
#'
#' 107 words frequenty used Arabic words for removal in text mining,
#' and based on the list adapted from ranks.nl
#'
#' @source \href{http://www.ranks.nl/stopwords/arabic}{Ranks.nl}
"stopwords_ar"



