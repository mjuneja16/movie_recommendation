#load required libraries
library(plyr)

#import text files: userData, movieData, trainData and testData

#column names for user dataset
cols <- c("user_id", "age", "gender", "occupation", "zip_code")
colnames(userData) <- cols

#column names for movie dataset
cols <- c("movie_id", "movie_title", "release_date", "video_release_date", "IMDb_URL", 
          "unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", 
          "Documentary", "Drama", "Fantasy", "Film_Noir", "Horror", "Musical", 
          "Mystery", "Romance", "Sci_Fi","Thriller", "War", "Western")
colnames(movieData) <- cols

#movieData<-userProfile[-944,]
#movieData<-userProfile[,-5]

#column names for train dataset
cols <- c("user_id", "movie_id", "rating", "timestamp") 
colnames(trainData) <- cols

#column names for test dataset
cols <- c("user_id", "movie_id", "rating", "timestamp") 
colnames(testData) <- cols

#writing the training and test data to comma separated file
write.csv(testData, file = "testData.csv")
write.csv(trainData, file = "trainData.csv")

fullTrain <- merge(trainData,userData, by = "user_id")
fullTrain <- merge(fullTrain,movieData, by = "movie_id")

fullTest <- merge(testData,userData, by = "user_id")
fullTest <- merge(fullTest,movieData, by = "movie_id")

#writing the full training dataset to comma separated file
write.csv(fullTrain, file = "fullTrain.csv")

#writing the full test dataset to comma separated file
write.csv(fullTest, file = "fullTest.csv")

#drop timestamp, zip_code, IMDb_URL, video_release_date from fullTrain
subTrain <- subset(fullTrain, select = -c(timestamp, zip_code,IMDb_URL, 
                                          video_release_date))

#check if the table is as expected
str(subTrain)
head(subTrain)
tail(subTrain)

#fix dates field
subTrain$release_date <- as.Date(subTrain$release_date, "%d-%b-%Y")

#remove duplicate entries for user_id and movie
subTrain <- unique(subTrain)

#function to create a single genre field in the dataset 
#assigns "multiple" genre to the Genre field if the movie has more than one genre
createSingleGenreField <- function(df){
  #remove variables to make looping easier
  tempData <- subset(df, select = -c(user_id, age, gender, occupation,
                                     movie_id, movie_title, rating, release_date))  
  count <- 0
  #unknown genre is default
  genre <- "unknown" 
  
  #some movies may have multiple ratings from same user
  #check if there are muliple rows in the dataframe df
  #set tempData to only have one row

    if(nrow(df) > 1){
    tempData <- head(tempData, n = 1)
  }
  
  for (i in names(tempData)){
    if(tempData[i] == 1){
      count <- count + 1
      genre <- i
    }
  }
  
  if(count > 1){
    genre <- "multiple"
  }
  
  names(genre) <- "genre"
  return(genre)
}

genreData <- ddply(subTrain, ~user_id + movie_title, createSingleGenreField)

subTrainSingle <- merge(genreData, subTrain)

#create subset
subTrainSingle <- subset(subTrainSingle, select = 
                             c(user_id, movie_title, rating, genre,
                               release_date, age, gender, occupation))

#write the table as csv file
write.csv(subTrainSingle, "unifiedMovieLensData.csv", row.names = FALSE)

#function to create multiple rows for movies with multiple genres
createMultipleGenreField <- function(df){
  tempData <- subset(df, select = -c(user_id, age, gender, occupation,
                                     movie_id, movie_title, rating, release_date))  
  genreName <- data.frame()
  
  #some movies may have multiple ratings from same user
  #check if there are muliple rows in the dataframe df
  #set tempData to only have one row
  
  if(nrow(df) > 1){
    #set tempDat to only have one row
    tempData <- head(tempData, n = 1)
  }
  
  for (i in names(tempData)){
    if(tempData[i] == 1){
      genreName<- rbind(genreName,i)
    }
    genreName<- rbind(genreName, NA)
  }
  
  names(genreName) <- "genre"
  return(genreName)
}

#create multiple rows based on the number of genres
subTrainMultiple <- ddply(subTrain, ~movie_title , createMultipleGenreField)

#remove all NA
subTrainMultiple <- na.omit(subTrainMultiple)

#clean and remerge
subTrainMultiple <- merge(subTrainMultiple, ddply(subTrain, ~movie_title + user_id, 
                                                summarize, release_date))
subTrainMultiple <- merge(subTrainMultiple, subTrain)

#removing data that have no genre at all
subTrainMultiple <- subTrainMultiple[subTrainMultiple$genre != "1",]

subTrainMultiple <- subset(subTrainMultiple, 
                          select = c(user_id, movie_title, genre, rating,
                                     release_date, age, gender, occupation) )

write.csv(subTrainMultiple, "unifiedMovieLensDataMultiple.csv", row.names = FALSE)
