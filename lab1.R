#.libPaths(c( "C:/Users/Oleh Kravets/Documents/R/win-library/3.5", "C:/Program Files/R/R-3.5.3/library"))
#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)

setwd("D:/bd1")

flats <- read.csv("flats1.csv", stringsAsFactors=FALSE)
class(flats)
str(flats)

#exs1
dim(flats)
head (flats, 6) 
head (flats, 15) 
tail(flats, 6)
names(flats)

#ex2
count(flats, Square)

#ex3
#variables
str(flats) 

#how many cities
flats %>%
  count(City) %>%
    arrange(n)

#three-bedroom in Odessa
flats%>%
  filter(Room == 3) %>%
    filter(City != "Odessa") %>% count(Square) %>%
      arrange(desc(n))

#meadian one-room Lviv
flats %>%
  filter(Room == 1) %>%
  filter(City == "Lviv") %>% group_by(City) %>%
         summarise(mean=median(Square))

#price dispersion two-room Lviv
flats%>%
  filter(Room == 2)%>%
    filter(City == "Lviv")%>% group_by(City) %>%
      #dispersion = standart deviation ^ 2
      summarize(dispersion=sd(Price)*sd(Price))


#ex4
#box-like price to rooms
p <- ggplot(flats, aes(x=Room, y=Price)) + ylab('Ціна') + xlab('Кімнати') +
  geom_boxplot()
  p + coord_flip()
p

#scattered price of the total area
library(ggplot2)
ggplot(flats, aes(x=Square, y= Price)) + ylab('Ціна') + xlab('Площа, м.кв.') +
  geom_point()

#histogram price of flats
ggplot(flats, aes(x=Price))  + ylab('Кількість') + xlab('Ціна') +
  geom_histogram(breaks=seq(0, 12250000, by = 1000000),
                 fill="red", col="grey")



