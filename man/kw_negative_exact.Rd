% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keywords.R
\name{kw_negative_exact}
\alias{kw_negative_exact}
\title{Negative exact match keywords}
\usage{
kw_negative_exact(x)
}
\arguments{
\item{x}{a keyword or a vector of keywords}
}
\value{
negative exact match keywords
}
\description{
Wraps a minus sign as well as square brackets around a character vector to turn them into
the "embedded" negative exact match keywords.
It takes care of removing any other match characters like +, -, or ".
}
\examples{
mtcars$negative_exact <- kw_negative_exact(rownames(mtcars))
keywords <- c("movie tickets", "movies", "best movies")
kw_negative_exact(keywords)
}

