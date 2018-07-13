# IMDB-Rating-Predictor
Implementing Random Forest model and multiple regression to predict movie ratings and better gauge the greatness of a movie without relying on critics or our own intuition.

I am using both R libraries called "rvest" and "omdbapi" to scrape metadata for approximatly 5000 movies from imdb.com and theNumbers.com. I have saved all titles in a csv and have constructed each movie's imdb url to be able to access each movie's respective IMDB page. I am currently in the process of sending HTTP requests to each movie page using the newly constructed urls, rvest, and omdb.
