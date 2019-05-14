.libPaths(c( "C:/Users/Oleh Kravets/Documents/R/win-library/3.5", "C:/Program Files/R/R-3.5.3/library"))
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

setwd("D:/bd1")

movie_body_counts <- read.csv ( 'filmdeathcounts.csv')

#expolirng the structure of dataset
head(movie_body_counts)
str(movie_body_counts)

#creating body_per_min variable
movie_body_counts$body_per_min <- movie_body_counts$Body_Count/movie_body_counts$Length_Minutes

#creating Body_Count Histogram
ggplot(movie_body_counts, aes(x=Body_Count)) + geom_histogram(bins=20, color="grey", fill="lightblue")

#top 10 films with biggest Body_Count 
movie_body_counts %>%
  top_n(n = 10, Body_Count) %>% arrange(desc(Body_Count))

#top 10 films by body_per_min
movie_body_counts %>%
  top_n(n = 10, body_per_min) %>% arrange(desc(body_per_min))

#creating IMBd rating histogram
ggplot(movie_body_counts, aes(x=IMDB_Rating)) + geom_histogram(bins=10, color="grey", fill="lightblue")

#calculation mean and sd of the IMDb rating values
imdb_mean <- movie_body_counts %>% summarise(mean(IMDB_Rating))
imdb_sd <- movie_body_counts %>% summarise(sd(IMDB_Rating))
#imdb mean
imdb_mean
#imdb standart deviation
imdb_sd

#generatinï random simulation using mean and sd 
set.seed(900)
imdb_simulation <- rnorm(n=nrow(movie_body_counts), as.numeric(imdb_mean), as.numeric(imdb_sd))
movie_body_counts$imdb_simulation <- imdb_simulation

#creating histogram for the simulation
ggplot(movie_body_counts, aes(x=imdb_simulation)) + geom_histogram(bins=10, color="grey", fill="lightblue")

#creating qq plot for the simulation
ggplot(movie_body_counts, aes(sample = imdb_simulation)) + stat_qq()

#comparing it to qq plot of the original rating
ggplot(movie_body_counts, aes(sample = IMDB_Rating)) + stat_qq()
