library('rvest')
library('dplyr')
library('stringi')
library('omdbapi')
library('Hmisc')
library('stringr')
library('tidyverse')
#########################
#Data base imdb title comparison
#####################

#figure out which movies are not already in the database
for ( i in 1:nrow(theNumbersData)){
    if (theNumbersData$movie_name[i] %in% db_imdb_titles$title){
      theNumbersData <- theNumbersData[-i, ]
    }
}


##create list of movie names with dashes to help construct url
list <- gsub(' ', '-',theNumbersData$movie_name )

urls <- for (i in length(list)){
  urls <- paste0("https://www.the-numbers.com/movie/", gsub(' ',list[i], list)) 
}

get_movie_details <-  lapply(urls, function(url){
                       try(
                             url %>% read_html() %>% 
                             html_nodes("#summary table tr td ") %>% 
                             html_text() %>%
                             gsub('[\r\n\t]', '', .)
                           )})

read_html("https://www.the-numbers.com/movie/20-Feet-From-Stardom#tab=summary") %>% 
html_nodes("#summary table") %>% 
html_text() %>%
gsub('[\r\n\t]', '', .)


#############################
###IMDB SCRAPE - retirieving movie info from IMDB.com
#############################
#Args
#Returns
#A data frame containing one row per movie and nine columns including: duration in minutes,
#MPAA rating, genre(s), director(S),IMDB rating, actor(S)'/director(S)' facebook likes,and full cast

##OMDB api key
omdbKey <-  'f5b8f99e'
omdb_api_key(force = FALSE)

#retrieve imdb ID for each movie scraped from theNumbers
get_movieID <- function(movie_title) {
  search_return <- search_by_title(movie_title, api_key = omdbKey)
  if(length(search_return$imdbID) >= 1 && !is.na(search_return)){
    id <- (search_return$imdbID[1])
  }else{
    id <- NA
  }
 
  return(id)
}

#charcter list for movieID's
movieID <- character()

#loop over all the movie names and retrieve each imdb id
for (i in length(movieID):5553) {
  movieID[i] <- get_movieID(theNumbersData$movie_name[i])
  #Sys.sleep(5)
}

## removing all NA's from the list to make it easier to process IDs
movieID <- movieID[!is.na(movieID)]

#create imdb url for 5282 movies
for (i in 1:length(movieID)){
  imdbURL <- paste0("https://www.imdb.com/title/",gsub('%20',movieID[i],movieID))
}

#save all imdb urls as csv
write.csv(imdbURL, "movie_imdb_url.csv")



#### Using a login
login_url <- 'https://www.amazon.com/ap/signin?_encoding=UTF8&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.pape.max_auth_age=0&ie=UTF8&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.assoc_handle=amzn_lwa_na&marketPlaceId=ATVPDKIKX0DER&arb=048cefea-d298-4124-98df-7064b4656edb&language=en_US&openid.return_to=https%3A%2F%2Fna.account.amazon.com%2Fap%2Foa%3FmarketPlaceId%3DATVPDKIKX0DER%26arb%3D048cefea-d298-4124-98df-7064b4656edb%26language%3Den_US&enableGlobalAccountCreation=1&metricIdentifier=amzn1.application.eb539eb1b9fb4de2953354ec9ed2e379&signedMetricIdentifier=fLsotU64%2FnKAtrbZ2LjdFmdwR3SEUemHOZ5T2deI500%3D'
session <- html_session(login_url)

form <- html_form(read_html(login_url))[[1]]

filled_form <- set_values(form,
                          email = "tmel825296@gmail.com",
                          password = "TMel825296")

submit_form(session, filled_form)

  read_html(jump_to(session, "https://pro.imdb.com/title/tt3606756/?ref_=hm_reel_bo_p03"))%>% 
  html_nodes('ranking_graph_container.a-section')%>%
  html_text()
  #jsonlite::fromJSON(.) 
 # gsub('[\r\n\t]', '', .)
  
