library('rvest')
library('dplyr')
library('stringi')
library('omdbapi')
library('Hmisc')
library('tidyverse')
library('plyr')
library('taRifx')

#############################
###IMDB SCRAPE - retirieving movie info from IMDB.com
#############################
#Args
#Returns
#A data frame containing one row per movie and nine columns including: duration in minutes,
#MPAA rating, genre(s), director(S),IMDB rating, actor(S)'/director(S)' facebook likes,and full cast

##OMDB api key
omdbKey <-  'e23d3bab'
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
for (i in 1:nrow(theNumbersData)) {
  movieID[i] <- get_movieID(theNumbersData$Title[i])
}
## removing all NA's from the list to make it easier to process IDs
movieID <- movieID[!is.na(movieID)]


#create imdb url for 5282 movies
for (i in 1:length(movieID)){
  imdbURL <- paste0("https://www.imdb.com/title/",gsub('%20',movieID[i],movieID))
}

#save all imdb urls as csv
write.csv(imdbURL, "movie_imdb_url.csv")


##get movie details 

get_movie_data <- function(url){
  
  html <- read_html(url)
  
  #title of movie
  title <- html%>%
    html_nodes(xpath = '//div[@class="title_wrapper"]/h1/text()') %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #length of movie in minutes
  length <- html%>%
    html_nodes("#title-overview-widget > div.vital > div.title_block > div > div.titleBar > div.title_wrapper > div > time") %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #find MPAA
  mpaa <- html%>%
    html_nodes("#titleStoryLine > div:nth-child(12) > span:nth-child(2)") %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)%>%
    strsplit(mpaa, split = " ", fixed = TRUE)%>%
    unlist(mpaa)
  
  mpaa <- mpaa[2]
  
  #Find genere(s)
  generes <- html%>%
    html_nodes('.subtext .itemprop') %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  
  #find director(S)
  directors <- html%>%
    html_nodes('.credit_summary_item:nth-child(2) .itemprop') %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #find top iactor(s)
  actors <- html%>%
    html_nodes(  '.credit_summary_item:nth-child(4) .itemprop') %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #number of votes
  numVotes <- html%>%
    html_nodes("a .small") %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #find imdb score
  imdbScore <- html%>%
    html_nodes("strong span") %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  #find metascore
  metascore <- html%>%
    html_nodes("div.titleReviewBar > div:nth-child(1) > a > div") %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  
  #find aspect ratio
  aspectRatio <- html%>%
    html_nodes(xpath = '//h4[contains(text(), "Aspect Ratio:")]/following-sibling::node()/descendant-or-self::text()') %>%
    html_text()%>%
    gsub('[\r\n\t]', '', .)
  
  
  if (is_empty(mpaa)) {   # Some movies don't have a MPAA rating
    mpaa <- "UNRATED"
  }
  if (is_empty(title)) {   # Some movies don't have a MPAA rating
    title <- NA
  }
  if (is_empty(generes)) {   # case where scraper fails
    generes <- NA
  }
  if (is_empty(directors)) {   # case where scraper fails
    directors <- NA
  }
  if (is_empty(actors)) {   # case where scraper fails
    actors <- NA
  }
  if (is_empty(numVotes)) {   # case where scraper fails
    numVotes <- NA
  }
  if (is_empty(imdbScore)) {   # case where scraper fails
    imdbScore <- NA
  }
  if (is_empty(metascore)) {  # case where scraper fails
    metascore <- NA
  }
  if (is_empty(aspectRatio)) {   # case where scraper fails
    aspectRatio <- NA
  }
  if (is_empty(length)) {   # case where scraper fails
    length <- NA
  }
  
  
  
  data.frame(title[1],
             toString(generes), 
             toString(actors), 
             toString(directors),
             length, 
             mpaa, 
             imdbScore,
             metascore,
             numVotes, 
             aspectRatio)
}

#get movie data for each moive url in imdbURL
datalist <- list()
for (i in 1:length(imdbURL)) {
  datalist[[i]]<- get_movie_data(imdbURL[i])
}


#Convert list of dataframes to one dataframe
df<- ldply(datalist, data.frame)

#get rid of duplicates
dfClean <- unique(df)

#convert title to a string and then get rid of extra white space
dfClean$title <- as.character(dfClean$title)
trim.trailing <- function (x) sub("\\s+$", "", x)
for (i in 1:nrow(dfClean)) {
  dfClean$title[i]<- trim.trailing(dfClean$title[i])
}


#add on financial details from theNumbers Data
theNumbersData <- theNumbersData[, c(2, 1, 3, 4, 5)]
df_final <- merge(dfClean, theNumbersData, by = c("title"))

#rename columns
colnames(df_final) <- c("title", "genere(s)",
                        "actors", "directors", 
                        "length", "mpaa", 
                        "imdbScore", "metaScore",
                        "numVotes", "aspectRatio",
                        "releaseDate", "productionBudget", 
                        "domesticGross", "worldWideGross")

