# VISUALIZATION
require(jsonlite)
library(ggplot2)

business <- fromJSON(sprintf("[%s]", paste(readLines('yelp_academic_dataset_business.json'), collapse=",")))

# Relationship between number of reviews and average rating
data = business[business$review_count < 2000,]
ggplot(data, aes(stars, review_count)) +
  geom_jitter(aes(color = stars)) +
  scale_y_continuous("Number of reviews")
# restaurants with many reviews plot distribution
data = business[business$review_count > 2000,]
ggplot(data, aes(x=stars))+
  geom_histogram(color = "black", fill = "grey",binwidth=0.5, alpha=0.8)
# general histogram of stars
ggplot(business, aes(x=stars))+
  geom_histogram(color = "black", fill = "blue",binwidth=0.5, alpha=0.5)+
  geom_vline(aes(xintercept = mean(stars)),col='black', linetype = 2, size=1)
# geographic plot
# Remove states with very few observations
data <- business[!business$state %in% c("NY", "DC", "KY", "KS"),]
state_data <- data %>%
  group_by(state) %>%
  summarise(stars = mean(stars, na.rm = TRUE)) %>%
  arrange(stars) %>%
  mutate(state = factor(state, levels = .$state))
library(dplyr)
library(tidyr)  
ggplot(state_data, aes(state, stars)) +
  geom_bar(stat = "identity") +
  coord_flip()
#ggplot(state_data, aes(stars, state)) +
#  geom_point()
# Price
attributes <- business$attributes
business$price <- attributes$RestaurantsPriceRange2
data <- business[!is.na(business$price),]
data <- data[data$price %in% c(1, 2 ,3, 4),]
ggplot(data, aes(x=factor(price), y=stars, color=factor(price))) + 
  geom_boxplot()+ labs( y = "Rating", x = "Price Range", fill = "Price Range") + theme(legend.position="none")
# Noise
attributes$noise <- as.factor(attributes$NoiseLevel)
business$noise <- attributes$NoiseLevel
data <- business[!is.na(business$noise),]
data <- data[data$noise %in% c("u'average'", "u'loud'" , "u'quiet'" , "u'very_loud'"),]
ggplot(data, aes(x=factor(noise), y=stars, color=factor(noise))) + 
  geom_boxplot()+ labs( y = "Rating", x = "Price Range", fill = "Price Range") + theme(legend.position="none")
# Outdoor
business$a <- attributes$BikeParking
business$rating <- as.factor(round(business$stars))
data <- business[business$a %in% c("True","False"),]
data <- data %>%
  group_by(rating) %>% 
  mutate(count = n())

#ggdotchart(data, x = "rating", y ="count",
#           color = "a", palette = "jco", size = 3, 
#           add = "segment", 
#           add.params = list(color = "lightgray", size = 1.5),
#           position = position_dodge(0.3),
#           ggtheme = theme_pubclean()
#)
