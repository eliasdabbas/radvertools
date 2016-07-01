#' Search Ads - Google
#'
#' Analyze the competition or use it for explaring who is advertising for what.
#' Enter a keyword or a vector of keywords, specify the Google domain
#' (com, co.jp, etc), and get a data frame showing keywords, ads, and the advertisers
#' who are bidding on this keyword at this particular moment.
#' This is valuable in seeing how you fare compared to competitors. It also helps you
#' figure out strategies of other advertisers and whether they are consistent, changing
#' strategies, specific, aggressive, etc.
#'
#' For long ongoing campaigns, ideally you can run a report for your top keywords a few
#' times a day, and bind the data in one large data frame. This can augment your campaign
#' data in interesting ways and can give you additional insights.
#'
#' @param keywords a keyword or a character vector
#' @param domains the Google domain you wish to check
#'
#' @return data frame with ad text with some metadata
#' @export
#'
#' @examples
#' search_ads_google(c("flights to new york", "cheap flights", "nyc airline tickets"))
search_ads_google <- function(keywords, domains = "com"){

  # Construct URLs from keywords --------------------------------------------------

  keywords <- gsub(" ", "+", x = keywords)
  pagedf <- expand.grid(keywords, domains)
  names(pagedf) <- c("keywords", "domains")
  pagedf$page <- paste0("https://google.",pagedf$domains, "/search?q=", pagedf$keywords)
  google_adsDF <- data.frame(keyword = character(),headlines = character(),
                             adtext = character(), dispurl = character(),
                             domain = character())
  for (i in 1:nrow(pagedf)){
    page_html <- xml2::read_html(pagedf$page[i])

    headlines <- page_html %>% rvest::html_nodes(".ads-ad > h3 a") %>% rvest::html_text()
    adtext <- page_html %>% rvest::html_nodes(".ads-creative") %>% rvest::html_text()
    dispurl <- page_html %>% rvest::html_nodes(".ads-visurl cite") %>% rvest::html_text()
    keyword <- pagedf$keywords[i]
    domain <- pagedf$domains[i]
    if(length(headlines) == 0) {
      google_adsDF <- google_adsDF %>% tibble::add_row(keyword = keyword, headlines = "no ads",
                                                       adtext = "no ads", dispurl = "no ads", domain = domain)
    } else {
      df <- cbind(as.data.frame(keyword), as.data.frame(headlines),
                  as.data.frame(adtext), as.data.frame(dispurl),
                  as.data.frame(domain))

      google_adsDF <- rbind(google_adsDF, df)
    }
  }

  google_adsDF$keyword <- gsub("\\+", " ", x = google_adsDF$keyword)
  google_adsDF$rank <- 1:nrow(google_adsDF)
  google_adsDF <-  google_adsDF %>% dplyr::group_by(keyword, domain) %>% dplyr::mutate(position = rank(rank))
  google_adsDF$rank <- NULL
  google_adsDF$time <- Sys.time()
  google_adsDF$dispurl <-  gsub(pattern = "^www\\.", replacement = "", x = google_adsDF$dispurl)
  google_adsDF
}
