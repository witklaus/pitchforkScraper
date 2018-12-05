##### PITCHFORK REVIEWS SCRAPER v. 0.1.0 #####
### For the time being the last page with reviews is 1735. Keeping
### in mind that nowadays the number of reviews on Pitchfork 
### increments rapidly (25 reviews per week: 4 per weekday and
### an additional one on Sunday) it is vital to ensure that the 
### range of pages to be scraped from is correctly provided.
### As of December 5, 2018, this is the last page with reviews:
### https://pitchfork.com/reviews/albums/?page=1735
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

### INITIALIZATION
batch <- 1:1735
links <- sprintf("https://pitchfork.com/reviews/albums/?page=%s", 
                   as.character(batch))

get_urls <- function(links){
  urls <- list()
  for (page in 1:length(links)){
    urls[[page]] <- read_html(links[page])
    Sys.sleep(sample(10, 1) * 0.5)
  }
  urls
}

urls <- get_urls(links)

get_reviews <- function(url){
  temp <- list()
  for (page in 1:length(n)){
    temp[[page]] <- urls[[page]] %>% 
      html_nodes(".review") %>% 
      as.character() %>%
      str_extract('/reviews/albums/.*?/') %>%
      str_c("https://pitchfork.com", .)
    Sys.sleep(sample(10, 1) * 0.5)
  }
  unlist(temp)
}

get_urls <- function(links){
  urls <- list()
  for (page in 1:length(links)){
    urls[[page]] <- read_html(links[page])
    Sys.sleep(sample(10, 1) * 0.5)
  }
  urls
}

reviews <- get_reviews(urls) %>% map(read_html)
nodes <- c(".single-album-tombstone__artist-links",
           ".single-album-tombstone__review-title",
           ".score", ".single-album-tombstone__meta",
           ".authors-detail__display-name",
           ".authors-detail__title", ".genre-list__item",
           "p", ".pub-date")

get_data <- function(reviews){
  data <- data.frame(matrix(NA, length(reviews), length(nodes)))
  for (review in 1:length(reviews)){
    for (node in 1:length(nodes)){
      if (length(reviews[[review]] %>% 
                 html_nodes(nodes[node]) %>% 
                 html_text()) != 0){
        data[review, node] <- reviews[[review]] %>% 
          html_nodes(nodes[node]) %>%
          html_text() %>% str_c(collapse = " // ")
      } else {
        data[review, node] <- NA
      }
    }
  }
  names(data) <- c("Artist", "Title", "Score", "Label", "Reviewer",
                   "Status", "Genre", "Review", "Date")
  data
}

pitchfork_data <- get_data(reviews)
proper_data <- pitchfork_data %>% filter(!str_detect(pitchfork_data[, 1], " // "))
duplicates <- pitchfork_data %>% filter(str_detect(pitchfork_data[, 1], " // "))

process_data <- function(reviews){
  count <- vector("numeric", length(nrow(reviews)))
  df <- data.frame(matrix(NA, 0, 9))
  for (review in 1:nrow(reviews)){
    count[review] <- reviews[review, 1] %>% str_count(" // ") + 1
    data <- reviews[review, 1:4] %>% str_split(" // ")
    c5 <- reviews[review, 5] %>% rep(times = count[review]) %>% list()
    c6 <- reviews[review, 6] %>% rep(times = count[review]) %>% list()
    c7 <- reviews[review, 7] %>% rep(times = count[review]) %>% list()
    c8 <- reviews[review, 8] %>% rep(times = count[review]) %>% list()
    c9 <- reviews[review, 9] %>% rep(times = count[review]) %>% list()
    new <- data.frame(c(data, c5, c6, c7, c8, c9))
    names(new) <- c("Artist", "Title", "Score", "Label", "Reviewer",
                    "Status", "Genre", "Review", "Date")
    set <- rbind(df, new)
    df <- set
  }
  df
}

split_data <- process_data(duplicates)
pitchfork <- rbind(proper_data, split_data) %>% 
  separate(col = Label, 
           into = c("Label", "Release"), 
           sep = " • ")

#BNM - Best New Music, BNR - Best New Reissue
pitchfork_data[, "BNM"] <- as.integer(as.character(pitchfork_data[,"Review"]) %>% 
                                        str_sub(1, 25) %>% 
                                        str_detect("Best new music"))
pitchfork_data[, "BNR"] <- as.integer(as.character(pitchfork_data[,"Review"]) %>% 
                                        str_sub(1, 25) %>% 
                                        str_detect("Best new reissue"))