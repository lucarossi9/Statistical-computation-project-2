require(jsonlite)
require(randomForest)
require(h2o)
library(ggplot2)

business <- fromJSON(sprintf("[%s]", paste(readLines('yelp_academic_dataset_business.json'), collapse=",")))
attributes <- business$attributes
attributes$rating <- business$star
business$tserv <- attributes$RestaurantsTableService
attributes <- attributes[-c(35:39)]
attributes_bool <- attributes[-c(2, 4, 13, 14, 16, 17, 20, 21, 26, 28, 29, 32, 34)]
attributes_bool$rating <- attributes$rating
attributes_bool$city <- business$city
attributes_bool$count <- business$review_count
p = c(1:20)
r = c(1:20)
m = c(1:20)
# t-test and Wilcox 
for (i in c(1:20)){
  business$i <- attributes_bool[, i]
  group1 <- subset(business, business$i == "True")
  group2 <- subset(business, business$i == "False")
  p[i] <- t.test(group1$stars, group2$stars)$p.value
  r[i] <- wilcox.test(group1$stars, group2$stars)$p.value
  m[i] <- mean(group1$stars) - mean(group2$stars)
}

business = business[-c(12:39)]
# Iterate over all possible attrributes and add them to the business data
names <- colnames(attributes_bool)
n <- length(names)
for (i in c(1:n)){
  business[names[i]] <- attributes_bool[names[i]]
}

# Credit card
data <- business[business$BusinessAcceptsCreditCards %in% c("True","False"),]
ggplot(data, aes(x=stars, color=BusinessAcceptsCreditCards))+
  geom_histogram(aes(y=2*(..density..)/sum(..density..)),binwidth=0.5, alpha=0.8)+
  facet_wrap(~BusinessAcceptsCreditCards, nrow=2)
ggplot(data, aes(x = BusinessAcceptsCreditCards, y = stars)) + geom_boxplot()

# Reservation
data <- business[business$RestaurantsReservations %in% c("True","False"),]
ggplot(data, aes(x=stars, color=RestaurantsReservations))+
  geom_histogram(aes(y=2*(..density..)/sum(..density..)),binwidth=0.5, alpha=0.8)+
  facet_wrap(~RestaurantsReservations, nrow=2)
# Reservation
data <- business[business$WheelchairAccessible %in% c("True","False"),]
ggplot(data, aes(x=stars, color=WheelchairAccessible))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~WheelchairAccessible, nrow=2)
# Outdoor
data <- business[business$OutdoorSeating %in% c("True","False"),]
ggplot(data, aes(x=stars, color=OutdoorSeating))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~OutdoorSeating, nrow=2)
ggplot(data, aes(x = OutdoorSeating, y = stars)) + geom_boxplot()

# Happy hour
data <- business[business$HappyHour %in% c("True","False"),]
ggplot(data, aes(x=stars, color=HappyHour))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~HappyHour, nrow=2)
# Bitcoin
data <- business[business$BusinessAcceptsBitcoin %in% c("True","False"),]
ggplot(data, aes(x=stars, color=BusinessAcceptsBitcoin))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~BusinessAcceptsBitcoin, nrow=2)
#Dancing
data <- business[business$GoodForDancing %in% c("True","False"),]
ggplot(data, aes(x=stars, color=GoodForDancing))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~GoodForDancing, nrow=2)
# Dogs
data <- business[business$DogsAllowed %in% c("True","False"),]
ggplot(data, aes(x=stars, color=DogsAllowed))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~DogsAllowed, nrow=2)
ggplot(data, aes(x = DogsAllowed, y = stars)) + geom_boxplot()
# Bike
data <- business[business$BikeParking %in% c("True","False"),]
ggplot(data, aes(x=stars, color=BikeParking))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~BikeParking, nrow=2)

data <- business[business$CoatCheck %in% c("True","False"),]
ggplot(data, aes(x=stars, color=CoatCheck))+
  geom_histogram(aes(y=0.5*..density..),binwidth=0.5, alpha=0.8)+
  facet_wrap(~CoatCheck, nrow=2)
# Variable importance
#install.packages('vip')
#install.packages('xgboost')
#install.packages('ranger')
#install.packages('rpart')
library(xgboost)
library(ranger)
library(rpart)

features <- business[-c(1:8)]
features <- features[-c(18:24)]
features <- features[-c(12:16)]
features <- features[-c(2, 3, 13, 14)]
features <- na.omit(features)
rf <- ranger(stars~., data = features, importance = "impurity")
bst <- xgboost(
  data = data.matrix(subset(features, select = -stars)),
  label = features$stars, 
  objective = "reg:linear",
  nrounds = 100, 
  max_depth = 5, 
  eta = 0.3,
  verbose = 0  # suppress printing
)

vi_rf <- rf$variable.importance
barplot(vi_rf, horiz = TRUE, las = 1)
vi_bst <- xgb.importance(model = bst)
xgb.ggplot.importance(vi_bst)
vi(rf)
library(vip)
p1 <- vip(rf, width = 0.5, aesthetics = list(fill = "blue3"))   # RF
p1


