library('rvest')
library('dplyr')
library('stringi')
library('omdbapi')
library('Hmisc')
library('tidyverse')
library('plyr')
library('taRifx')

#############################
###IMDB SCRAPER - retirieving movie info from IMDB.com
#############################
#Returns:
#A data frame containing one row per movie and nine columns including: duration in minutes,
#MPAA rating, genre(s), director(S),IMDB rating, actor(S)'/director(S)' facebook likes,and full cast


##get movie details
get_movie_data <- function(url) {
  html <- read_html(url)
  
  #title of movie
  title <- tryCatch({
    html %>%
      html_nodes(xpath = '//div[@class="title_wrapper"]/h1/text()') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  
  #length of movie in minutes
  length <- tryCatch(
    html %>%
      html_nodes('#title-overview-widget > div.vital > div.title_block > div > div.titleBar > div.title_wrapper > div > time'
      ) %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  ,error = function(cond)
  {
    NA
  })
  
  #find MPAA
  mpaa <- tryCatch({
    html %>%
      html_nodes(xpath = '//meta[@itemprop="contentRating"]/following-sibling::node()/descendant-or-self::text()') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .) %>%
      strsplit(mpaa, split = " ", fixed = TRUE) %>%
      unlist(mpaa)
  }, error = function(cond)
  {
    NA
  })

  
  #Find genere(s)
  generes <- tryCatch({
    html %>%
      html_nodes('.subtext .itemprop') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  
  #find director(S)
  directors <- tryCatch({
    html %>%
      html_nodes('.credit_summary_item:nth-child(2) .itemprop') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  #find top iactor(s)
  actors <- tryCatch({
    html %>%
      html_nodes('.credit_summary_item:nth-child(4) .itemprop') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  #number of votes
  num_votes <- tryCatch({
    html %>%
      html_nodes("a .small") %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  #find imdb score
  imdb_score <- tryCatch({
    html %>%
      html_nodes("strong span") %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  popularity <- tryCatch({
    html %>%
      html_nodes("div.plot_summary_wrapper > div.titleReviewBar > div:nth-child(5) > div.titleReviewBarSubItem") %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  
  #find metascore
  metascore <- tryCatch({
    html %>%
      html_nodes("div.titleReviewBar > div:nth-child(1) > a > div") %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })
  
  
  
  critic_reviews <- tryCatch({
    html%>%
      html_nodes(xpath = '//span/a[contains(@href, "externalreviews")]/text()') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  }, error = function(cond)
  {
    NA
  })

  #find aspect ratio
  aspect_ratio <- tryCatch({
    html %>%
      html_nodes(xpath = '//h4[contains(text(), "Aspect Ratio:")]/following-sibling::node()/descendant-or-self::text()') %>%
      html_text() %>%
      gsub('[\r\n\t]', '', .)
  } , error=function(cond)
  {
    NA
  })
  

  data <- data.frame(
    title[1],
    toString(generes),
    toString(actors),
    toString(directors),
    length,
    mpaa[1],
    imdb_score,
    metascore,
    popularity,
    critic_reviews,
    num_votes,
    aspect_ratio
  )
  
  return(data)
}

#get movie data for each moive url in imdbURL
for (i in 1:nrow(imdbURL)) {
  if (i == 1){
    final_data <- get_movie_data(imdbURL$imdbURL[i])
  }else {
    tryCatch({
      final_data <- rbind.fill(final_data, get_movie_data(imdbURL$imdbURL[i]))},error=function(cond)
    {
      NA
    })
  }
  cat(paste0('\nFinished: ', i, ". Starting next loop on " , i+1, sep =' '))
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

