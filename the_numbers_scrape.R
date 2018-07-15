library('rvest')
library('dplyr')
library('stringi')
library('omdbapi')
library('Hmisc')
library('stringr')
library('tidyverse')

##########################
#The Numbers scrape - retrieving movie info from thenumbers.com
#########################
#Returns: A data frame containing movie titles, release date, production budget, domestic gross, and worldwide gross
###########################


#Scrape Raw HTML from page
search_results <- read_html(search_url)

#pull all movie titles and their release dates for the first 5000 movies on the site
movieName_and_releasDate <- lapply(paste0('https://www.the-numbers.com/movie/budgets/all/', seq(1,5501, 100)),
                                   function(url){
                                     url %>% read_html() %>% 
                                       html_nodes("td a") %>% 
                                       html_text() %>%
                                       gsub('[\r\n\t]', '', .)
                                     
                                   })

#pull production budget, domestic gross, and worldwide gross for the first 5000 movies on the site
gross_and_budget <- lapply(paste0('https://www.the-numbers.com/movie/budgets/all/', seq(1,5501, 100)),
                           function(url){
                             url %>% read_html() %>% 
                               html_nodes(".data") %>% 
                               html_text() %>%
                               gsub('[\r\n\t]', '', .)
                             
                           })
#unlist to a vector, convert vector to dataset, assign col names
vector1 <- unlist(movieName_and_releasDate)
m1 <- matrix(vector1, ncol = 2, byrow = TRUE)
d1 <- as.data.frame(m1, stringsAsFactors = FALSE)
colnames(d1) <- 
  c('release_date',
    'movie_name')

#unlist to a vector, convert vector to dataset, assign col names
vector2 <- unlist(gross_and_budget)
m2 <- matrix(vector2, ncol = 4, byrow = TRUE)
d2 <- as.data.frame(m2, stringsAsFactors = FALSE)
colnames(d2) <-
  c('num',
    'production_budget',
    'domestic_gross',
    'worldwide_gross')
theNumbersData <- cbind(d1, d2)

#get rid of useless col
theNumbersData <- theNumbersData[-3]

#export dataset as csv
write.csv(theNumbersData, "theNumbersData.csv")


