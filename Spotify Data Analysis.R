library(psych)
library(skimr)
library(ggplot2)
library(GGally)
library(regclass)
library(tidyverse)
library(ggcorrplot)
library(dplyr)
library(randomForest)

# Data exploring
spotify = read.csv('data.csv')
head(spotify)
names(spotify)
skim(spotify)

# Boxplot for variables with range from 0 to 1
boxplot(spotify [, c(1, 3, 5, 8, 10, 16, 18)])

# Boxplot for unique variables
scatt(spotify [, 4])

# Table for categorical variable 
table(spotify$explicit)
table(spotify$mode)

# Histogram
hist(spotify$popularity, ylab = 'Songs Frequency', xlab = 'Popularity', main = 'Popularity Histogram')

hist(spotify$key, ylab = 'Songs Frequency', xlab = 'Key', main = 'Key Histogram')

ggplot(spotify, aes(x=key)) + geom_histogram()


## Cleaning data
skim(spotify)
names(spotify)
# release_date, as professor Oscar asked, is not good for any model because some observations contains full date,
# While others contains only the year. Therefore, we can use the variable year instead. 

# Variable id is not needed in the model 
# Variable name does not change anything, so we do not need it in the model.

spotify.drop = c('release_date', 'id', 'name', 'year')
spotify1 = spotify[ ,!(names(spotify) %in% spotify.drop)] 
dim(spotify1)

# Checking Null and duplicate values 
is.null(ad)
# There is no missing values in this data.

dim(spotify[duplicated(spotify$id),])[1]
# There is no duplicated values.

# Numeric Variables 
spotify_numeric = spotify[,-c(2, 7, 9, 12, 13, 15)]
names(spotify_numeric)

## Analyzing/Modeling
# Linear Regression Model
spotify_lm = lm(popularity ~ ., data = spotify_numeric)
summary(spotify_lm)
predict(spotify_numeric, interval = "confidence")

# According to Linear Regression Model, there is no relationship between duration_ms and popularity.
# because duration_ms' p-value is higher than 0.05. Therefore, We are removing it from the model.
spotify_numeric = spotify_numeric[,-3]
drop1(spotify_lm, test = 'F') # Checking if there's any variable not useful to the model. 
names(spotify_numeric)
head(spotify_numeric)

## Correlation graph
ggcorr(spotify_numeric, method = c('pairwise.complete.obs', 'pearson'), 
       label = TRUE,
       nname = 'Pearson correlation',
       legend.position = c(0.3, 0.5))
abs(cor(spotify_numeric$popularity, spotify_numeric$acousticness))
abs(cor(spotify_numeric$popularity, spotify_numeric$danceability))
abs(cor(spotify_numeric$popularity, spotify_numeric$energy))
abs(cor(spotify_numeric$popularity, spotify_numeric$instrumentalness))
abs(cor(spotify_numeric$popularity, spotify_numeric$liveness))
abs(cor(spotify_numeric$popularity, spotify_numeric$loudness))
abs(cor(spotify_numeric$popularity, spotify_numeric$speechiness))
abs(cor(spotify_numeric$popularity, spotify_numeric$tempo))
abs(cor(spotify_numeric$popularity, spotify_numeric$valence))
abs(cor(spotify_numeric$popularity, spotify_numeric$year))
abs(cor(spotify_numeric$popularity, spotify_numeric$explicit))


## Checking the most popular artist in the data
## Trying to measure artists by their average of popularity.
unique(popu_artist$artists) # There are 32375
popu_artist = subset(spotify, select = c(popularity, artists)) # Subset  data in singers and their popularity
head(popu_artist)


unique(popu_artist$artists) # How many singers in the data.

mean_popu = popu_artist %>% group_by(artists) %>% summarise_at(vars(popularity),funs(mean(.,na.rm=FALSE))) # Mean of each song popularity 
order = sort(mean_popu$popularity, decreasing = TRUE, na.last = TRUE) # Order 
head(mean_popu, 20)
barplot(table(mean_popu$artists)) # Trying to make a barplot or histogram with a order from the highest to the lowest results. 


## Randon Forest model 
# Train (20%) Test (80%)
n = dim(spotify_numeric)[1]
train_index = sample(1:n, round(0.8*n))

train = spotify_numeric[train_index, ]
test = spotify_numeric[-train_index, ]


# Defining the number of trees 
n_trees = c(100, 300, 500, 1000)

# Creating variables to storage results
## Storing the results
RF_results = expand.grid(tree = n_trees)
RF_results$RMSE = NA
RF_results$MAE = NA
RF_results  


# Computing the performance of the different number of trees

for(i in 1:length(n_trees)){
  
  ## Fitting the random forest
  RF = randomForest(popularity ~ acousticness + danceability + duraction_ms + energy + explicit + 
                      instrumentalness + liveness + loudness + speechiness + tempo + valence,
                      data = train, n_tree = n_trees[i])
  
  ## Predicting on test dara
  RF_preds = predict(RF, newdata = test)
  
  ## Computing and store RSME and MAE
  RF_results$RMSE[i] = sqrt(mean((RF_preds - test$popularity)^2))
  RF_results$MAE[i] = mean(abs(RF_preds - test$popularity))
}
RF_results # Identifying how many trees to use

mean(RF_preds)
mean(test$Price)
min(RF_results$RMSE, RF_results$MAE)


new_data = data.frame(valence = seq(0, 1.0, len = 10)) # Valence of 0 provides the highest predicted popularity (40.975).
new_data$acousticness = 0.49 # Acousticness of 0.3 provides the highest predicted popularity (34.163). 
new_data$danceability = 0.53 # Danceability of 0.9 provides the highest predicted popularity (40.833).
new_data$duration_ms = 231400 # Duration_ms of 679907ms provides the highest predicted popularity (24.846).
new_data$energy = 0.48 # Energy of 1.0 provides the highest predicted popularity (31.942).
new_data$explicit = 0.08 # Explicit >= 0.6 provides the highest predicted popularity (44.265).
new_data$instrumentalness = 0.16 # Instrumentalness of 0.1 provides the highest predicted popularity (27.764).
new_data$liveness = 0.20 # Liveness of 0.1 provides the highest predicted popularity (32.621).
new_data$loudness = -11.37 # Loudness of -3.111111 provides the highest predicted popularity (41.095).
new_data$speechiness = 0.09 # Speechiness of 0.3 provides the highest predicted popularity (29.759).
new_data$tempo = 116.95 # Tempo of <= 27.11 provides the highest predicted popularity (32.294).

RF_md = randomForest(popularity ~ acousticness + danceability + duration_ms + energy + explicit + 
                    instrumentalness + liveness + loudness + speechiness + tempo + valence,
                  data = train, ntree = 100)
predict(RF_md, newdata = new_data)

summary(spotify_numeric)

# Checking what is going to be the popularity prediction using the best numbers.
perfect_data = data.frame(acousticness = 0.3) 
perfect_data$danceability = 0.9
perfect_data$duration_ms = 679907
perfect_data$energy = 1.0
perfect_data$explicit = 0.6 
perfect_data$instrumentalness = 0.1
perfect_data$liveness = 0.1
perfect_data$loudness = -3.11 
perfect_data$speechiness = 0.3
perfect_data$tempo = 90 
perfect_data$valence = 0.0 

RF_md = randomForest(popularity ~ acousticness + danceability + duration_ms + energy + explicit + 
                       instrumentalness + liveness + loudness + speechiness + tempo + valence,
                     data = train, ntree = 100)
predict(RF_md, newdata = perfect_data) 
# Here, we are simulating a song containing the same data from perfect_data. 
# The predicted popularity was 47.31.



 # Gradient boosting model
# Defining the number of trees and depths
n_trees = c(100, 300)
n_depth = c(2, 3)

# Creating all combinations between trees and depths
parameter_grid = expand.grid(Number_of_Trees = n_trees, Depth = n_depth)

# Creating variables to storage results
## Storing the results
gbm_results = expand.grid(Number_of_Trees = n_trees, Depth = n_depth)
gbm_results$RMSE = NA
gbm_results$MAE = NA
gbm_results  

library(gbm)
library(caret)

# Computing the performance of the different number of trees

for(i in 1:dim(parameter_grid)[1]){
  
  ## Fitting the random forest
  gbm_md = gbm(popularity ~ ., 
               data = train, 
               n.trees = parameter_grid$Number_of_Trees[i], 
               interaction.depth = parameter_grid$Depth[i],
               distribution = 'gaussian')
  
  ## Predicting on test dara
  gbm_preds = predict(gbm_md, newdata = test)
  
  ## Computing and store RSME and MAE
  gbm_results$RMSE[i] = sqrt(mean((gbm_preds - test$Price)^2))
  gbm_results$MAE[i] = mean(abs(gbm_preds - test$Price))
}
gbm_results
min(gbm_results$RMSE)
min(gbm_results$MAE)

mean(gbm_preds)
mean(test$Price)
 
  


