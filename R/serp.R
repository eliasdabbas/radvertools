#' Get the search engine results pages.
#'
#' Get the top ten resuls of any query you want, along with several
#' paramters of the search query; the location of the user, language,
#' and the Google domain where you want the query to be run.
#'
#' You will need to create an account at
#' \href{https://console.developers.google.com/}{Google's Developers Console}.
#' You also need to create a \href{https://cse.google.com/}{custom search engine}.
#' Then you will need to supply you API key from the developer console, as well
#' as your custom search engine ID (referred to as "cx" by Google).
#'
#' @param query the keyword you want to research
#' @param cse_id your google custom search engine ID
#' @param api_key your API key
#' @param lang a two-letter representation of the user langugae
#' @param user_loc location of the user given with the two-letter ISO code
#' @param goog_domain the domain of Google which you want it to search in
#' Simply enter a keyword, with optional parameters, and get the page of search
#' results returned as a data.frame.
#'
#' @export
#'
#' @return A data.frame of the ten search results, as they are ranked,
#' together with the supplied parameters; query, language, location, Google domain,
#' and the time the query was run. Each of those parameters has its own column to
#' aid later in any analysis you want to make.
#'
#' @examples
#'
#' \dontrun{
#' serp_google("credit card", cse_id = "YOUR_CUSTOM_SEARCH_ENGINE_ID",
#' api_key = "YOUR_API_KEY", lang = "en", user_loc = "jp", goog_domain = "co.jp")
#' }
serp_google <- function(query, cse_id, api_key, lang = "en",
                        user_loc = "us", goog_domain = "com") {
  query <- gsub(" ", "+", query)
  base_url <- "https://www.googleapis.com/customsearch/v1?"
  request_url <-  paste0(base_url, "key=", api_key, "&cx=", cse_id,
                         "&hl=", lang, "&gl=", user_loc, "&googlehost=", goog_domain,
                         "&q=", query)
  results <- httr::content(httr::GET(request_url))
  query <- gsub("\\+", " ", query)
  serp_completeDF <- data.frame()
  for(i in seq_along(results$items)) {
    serpDF <- data.frame(
      query = query,
      title = results$items[[i]]$title,
      snippet = results$items[[i]]$snippet,
      disp_link = results$items[[i]]$displayLink,
      dest_link = results$items[[i]]$link
    )
    serp_completeDF <- rbind(serp_completeDF, serpDF)
  }
  serp_completeDF$time <- Sys.time()
  serp_completeDF$rank <- 1:nrow(serp_completeDF)
  serp_completeDF$domain <- goog_domain
  serp_completeDF$lang <- lang
  serp_completeDF$loc <- user_loc
  serp_completeDF$disp_link <- gsub("^www\\.", "", serp_completeDF$disp_link)
  serp_completeDF
}

