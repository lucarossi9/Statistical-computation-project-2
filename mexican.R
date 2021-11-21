require(jsonlite)
require(h2o)
library(ggplot2)
library(feather)
library(tidyverse)
library(stringr)
library(lubridate)
library('wordcloud')
library(tidytext) 
library(DT)      
library(igraph) #  graphs
library(ggraph)
# Reading the data
business <- fromJSON(sprintf("[%s]", paste(readLines('yelp_academic_dataset_business.json'), collapse=",")))
business_mex <- read.csv2("business.csv", sep = ",")
american <- read.csv2("american.csv", sep = ",")
nobell <- read.csv2("notacobell.csv", sep = ",")
# Analysis of shared features for Mexican restaurants
categories = str_split(business_mex$categories,",")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")
categories$Name <- gsub('\\s+', '', categories$Name)
data <- categories %>%
  group_by(Name) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))
data <- data[!duplicated(data),]
data <- data[-c(1, 2, 3), ]
data <- data[c(1:10),]
ggplot(data, aes(x= reorder(Name, count), y=count)) +
  geom_bar(stat='identity',colour="white", fill ="orange") +
  geom_text(aes(x = Name, y = 1, label = paste0("(",count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count') +
  coord_flip() + 
  theme_bw()
# Geographical analysis of mexican restaurants
cities = business_mex$city
cities = as.data.frame(cities)
colnames(cities) = c("Name")
cities$Name <- gsub('\\s+', '', cities$Name)
data <- cities %>%
  group_by(Name) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))
data <- data[!duplicated(data),]
data <- data[c(1:10),]
ggplot(data, aes(x= reorder(Name, count), y=count)) +
  geom_bar(stat='identity',colour="white", fill ="yellow2") +
  geom_text(aes(x = Name, y = 1, label = paste0("(",count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'City', y = 'Count') +
  coord_flip() + 
  theme_bw()
# Distribution of rating
stars = business_mex$stars
stars = as.data.frame(stars)
colnames(stars) = c("St")
stars$St <- gsub('\\s+', '', stars$St)
data <- stars %>%
  group_by(St) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))
data <- data[!duplicated(data),]
ggplot(data, aes(x= St, y=count)) +
  geom_bar(stat='identity',colour="black", fill ="blue3", alpha=0.5) +
  labs(x = 'Stars', y = 'Count') +
  theme_bw()
# Testing differences 
group1 <- business$stars
group2 <- as.numeric(as.character(business_mex$stars))
wilcox.test(group1, group2)
t.test(group1, group2)
mean(group1)
mean(group2)
# Compare mexican restaurants with american
group1 <- as.numeric(as.character(american$stars))
group2 <- as.numeric(as.character(business_mex$stars))
wilcox.test(group1, group2)
t.test(group1, group2)
mean(group1)
mean(group2)
# Distribution of rating for mexican vs american
stars = american$stars
stars = as.data.frame(stars)
colnames(stars) = c("St")
stars$St <- gsub('\\s+', '', stars$St)
data <- stars %>%
  group_by(St) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))
data <- data[!duplicated(data),]
ggplot(data, aes(x= St, y=count)) +
  geom_bar(stat='identity',colour="black", fill ="blue3", alpha=0.5) +
  labs(x = 'Stars', y = 'Count') +
  theme_bw()

nf <- layout( matrix(c(1,2), ncol=2) )
hist(group1 , breaks=10 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="ratings" , main="")
boxplot(group1 , ylab="rating" , col=rgb(0.8,0.8,0.3,0.5) , las=2)
nf <- layout( matrix(c(1,2), ncol=2) )
hist(group2 , breaks=10 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="ratings" , main="")
boxplot(group2 , ylab="rating" , col=rgb(0.8,0.8,0.3,0.5) , las=2)
# Analysis of different categories
categories = str_split(business$categories,",")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")
categories$Name <- gsub('\\s+', '', categories$Name)
data <- categories %>%
  group_by(Name) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))
data <- data[!duplicated(data),]
# Analysis without taco bell
group1 <- as.numeric(as.character(american$stars))
group2 <- as.numeric(as.character(nobell$stars_x))
wilcox.test(group1, group2)
t.test(group1, group2)
m1 <- mean(group1)
m2 <- mean(group2)
nf <- layout( matrix(c(1,2), ncol=2) )
hist(group1 , breaks=10 , border=F , col=rgb(0.1,0.1,0.3,0.5) , xlab="ratings" , main="", freq = FALSE)
abline(v = m1, col = "black", lwd = 2)
hist(group2 , breaks=10 , border=F , col=rgb(0.1,0.1,0.3,0.5) , xlab="ratings" , main="", freq = FALSE)
abline(v = m2, col = "black", lwd = 2)

library(stringr)
library(dplyr)


