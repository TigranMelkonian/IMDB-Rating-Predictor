#Scraping The-numbers.com to gather anticipated movies
library('stringr')
library('rvest')
library('dplyr')
#devtools::install_github("hrbrmstr/omdbapi")
library('omdbapi')

# a <- test %>%
  #   # html_nodes("#single_content .primary") %>%
#   # html_node("h11") %>%
#   # html_nodes("h6") %>%
#   # html_nodes("ul") %>%
#   # html_nodes("li") %>%
#   html_nodes("div") %>%
#   html_text()

add_movie_year <- function(upcoming_movie_data){
  date_split <- upcoming_movie_data %>%
    separate(Release.Date, into = c("date","year"),sep = ", ")
  date_split$Release.Date <- upcoming_movie_data$Release.Date
  return(date_split)
}

get_anticipatedMovies <- function(){
  anticipated_movies <- read_html('http://www.the-numbers.com/movies/most-anticipated') %>%
    html_nodes("#col2mid") %>%
    html_text()
  splits <- strsplit(anticipated_movies,split = "\n\t") #split strings, 2 should be the title
  movies <- sapply(splits,function(x) x[2]) #Pull all of the second sub-elements
  return(movies)
}

get_releaseSchedule <- function(){
  release_schedule <- read_html('http://www.the-numbers.com/movies/release-schedule') %>%
    html_nodes("table") %>%
    html_table()
  return(as.data.frame(release_schedule))
}

get_movieID <- function(movie_title){
  search_return <- search_by_title(movie_title)
  if(length(search_return[search_return$Year %in% "2017","imdbID"]) == 1){
    id <- search_return[search_return$Year %in% "2017","imdbID"]
  } else {
    id <- NULL
  }
  return(id)
}

get_movieActors <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    get_actors(find_by_title(movie_title,year_of_release = year_of_release))
  }, error=function(cond)
  {
    NA
  })
}

get_movieActors_id <- function(movie_id){
  temp_actors <- tryCatch({
    get_actors(find_by_id(movie_id))
  }, error=function(cond)
  {
    NA
  })
}

get_movieWriters <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    get_writers(find_by_title(movie_title,year_of_release = year_of_release))
  }, error=function(cond)
  {
    NA
  })
}

get_movieWriters_id <- function(movie_id){
  temp_writers <- tryCatch({
    get_writers(find_by_id(movie_id))
  }, error=function(cond)
  {
    NA
  })
}

get_movieDirectors <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    get_directors(find_by_title(movie_title,year_of_release = year_of_release))
  }, error=function(cond)
  {
    NA
  })
}

get_movieDirectors_id <- function(movie_id){
  temp_directors <- tryCatch({
    get_directors(find_by_id(movie_id))
  }, error=function(cond)
  {
    NA
  })
}

get_movieGenres <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    get_genres(find_by_title(movie_title,year_of_release = year_of_release))
  }, error=function(cond)
  {
    NA
  })
}

get_movieGenres_id <- function(movie_id){
  temp_genres <- tryCatch({
    get_genres(find_by_id(movie_id))
  }, error=function(cond)
  {
    NA
  })
}

get_movieCountries <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    get_countries(find_by_title(movie_title,year_of_release = year_of_release))
  }, error=function(cond)
  {
    NA
  })
}

get_movieCountries_id <- function(movie_id){
  temp_countries <- tryCatch({
    get_countries(find_by_id(movie_id))
  }, error=function(cond)
  {
    NA
  })
}

get_movieData <- function(movie_title,year_of_release = NULL){
  movie_title <- gsub(" \\(.*","",c(movie_title)) #remove anything in parentheses
  temp_actors <- tryCatch({
    find_by_title(movie_title,year_of_release = year_of_release)
  }, error=function(cond)
  {
    NA
  })
}

get_movieData_id <- function(movie_id){
  temp_data <- tryCatch({
    find_by_id(movie_id)
  }, error=function(cond)
  {
    NA
  })
}

build_actor_url <- function(actor_id){
  #each ID has 7 digits in the URL
  actor_id <- paste0('nm',paste(rep("0",7-nchar(actor_id)),collapse = ""),as.character(actor_id))
  paste0('http://www.imdb.com/name/',actor_id)
}

get_actor_movieData <- function(url){

  html_raw <- read_html(url)
  
  actor_name <- html_raw %>%
    html_nodes("span.itemprop") %>%
    html_text()
  
  acting_role_data <- html_raw %>%
    html_nodes(".article #filmography .filmo-category-section") %>%
    html_text()
  
  if(length(acting_role_data)>0){
    full_data <- strsplit(acting_role_data[1],split = '\n\n\n\n')
  } else {
    full_data <- NA
    acting_role_data <- NA
  }

  acting_role_headings <- html_raw %>%
    html_nodes("#jumpto") %>%
    html_text()
  
  #If they only have one type of role, there will be no 'jump to' box. Hopefully this will account for all others.
  if(length(acting_role_headings) < 1){
    temp <- html_raw %>%
      html_nodes("#filmography a") %>%
      html_text()
    
    if(length(temp)>0){
      acting_role_headings <- temp[1]
    } else {
      acting_role_headings <- NA
    }
  } 

  column_headings <- gsub("\n","",acting_role_headings)
  column_headings <- gsub("Jump to:","",column_headings)
  column_headings <- gsub(" ","",column_headings)
  column_headings <- unlist(strsplit(column_headings, "|", fixed=TRUE))
  
  return(data.frame(actor = actor_name[1],
                    heading = column_headings,
                    raw_data = as.character(acting_role_data)))
}

organize_actor_movieData <- function(celeb_df){
  for(j in 1:length(celeb_df$heading)){
    string <- as.character(celeb_df$raw_data[j])
    clean_data <- unlist(strsplit(string,'\n\n\n\n'))
    clean_data <- strsplit(clean_data,'\n\n')
    for(i in 1:length(clean_data)){
      #Remove empty cells
      clean_data[[i]] <- clean_data[[i]][!clean_data[[i]] %in% ""]
      temp_data <- data.frame(heading = celeb_df$heading[j],
                              date = clean_data[[i]][1],
                              title = clean_data[[i]][2],
                              stringsAsFactors = F
      )
      #Changing columns to character
      temp_data$date <- as.character(temp_data$date)
      temp_data$title <- as.character(temp_data$title)
      temp_data$heading <- as.character(temp_data$heading)
      
      #removing unnecessary characters
      temp_data$date <- gsub('\n',' ',temp_data$date)
      temp_data$title <- gsub('\n',' ',temp_data$title)
      temp_data$date <- gsub("(^\\s+)|(\\s+$)", "", temp_data$date)
      temp_data$title <- gsub("(^\\s+)|(\\s+$)", "", temp_data$title)
      
      if(i == 1 & j == 1){
        final_data <- temp_data
      } else {
        final_data <- rbind(final_data,temp_data)
      }
    }
  }
  
  #Removing the '/I' from the date in this list. It will come up when there are mutliple movies with the same name. I would rather exclude and deal with it.
  final_data$date <- gsub("\\/.*","",final_data$date)
  
  #Changing actress to actor, for filtering later
  final_data[final_data == 'Actress'] <- 'Actor'
  
  #Removing the show all episodes lines that also don't have a title
  final_data <- final_data[!is.na(final_data$title),]
  
  return(final_data)
}
