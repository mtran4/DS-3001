---
  title: "Lab11"
author: "Maggie Tran"
date: "11/3/2021"
output: html_document
---
  
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(caret)

# read in data and create dataframe (df)
df <- read_csv("C:\\Users\\mktra\\Desktop\\DS-3001\\week-11-reinforcement-lab\\data-frame.csv")

# make scatter plot comparing new features
ggplot(df,aes(x=cora,y=corc,color=won))+geom_point()

df$won <- as.character(df$won)
df$won <- recode(df$won,
                 'TRUE' = 1,
                 'FALSE' = 0)
View(df)



# KNN using Caret
set.seed(1981)

scaled_df <- as.data.frame(scale(df[c(4,6,7)], center = TRUE, scale = TRUE)) 
scaled_df$won <- df$won
View(df)
View(scaled_df)

df_sample <- sample(2, nrow(scaled_df), replace=TRUE, prob=c(0.67, 0.33))
df_training_car <- scaled_df[df_sample==1, 1:4]  
df_test_car <- scaled_df[df_sample==2, 1:4]
View(df_training_car)

trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3) # generic control to pass back into the knn mode using the cross validation method. 

df_knn <- train(won~.,
                data = df_training_car,
                method="knn",
                tuneLength=10,
                trControl= trctrl,#cv method above, will select the optimal K
                preProcess="scale") #already did this but helpful reference

df_knn#take a look

varImp(df_knn)#gives us variable importance on a range of 0 to 100

df_pred <- predict(df_knn, df_test_car)

df_pred #gives a character predicted value for each row.

confusionMatrix(df_pred, df_test_car$won)

table(df_test_car$won)



# Normalize data
# Build our own normalizer, min-max scaler function.
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# Before we can apply this we need to make sure our data classes are correct. 
(column_index <- tibble(colnames(df)))

# Convert columns to factors
df[,c(1,2,3)] <- lapply(df[,c(1,2,3)], as.factor)

# Now we can move forward in normalizing the numeric values, create a index based on numeric columns: 
abc <- names(select_if(df, is.numeric))# select function to find the numeric variables 
# Use lapply to normalize the numeric values 
df[abc] <- as_tibble(lapply(df[abc], normalize))



# Run k-means
clust_df = df[,c(4,5,6,7)]

# Run an algorithm with 2 centers
set.seed(1)
kmeans_obj_df = kmeans(clust_df, centers=2, algorithm="Lloyd")
kmeans_obj_df
head(kmeans_obj_df)

df_clusters = as.factor(kmeans_obj_df$cluster)
ggplot(df, aes(x = cora,
               y = corc,
               color = won,
               shape = df_clusters)) +
  geom_point(size = 6) +
  ggtitle("Cora vs Corc") +
  xlab("Cora") +
  ylab("Corc") +
  scale_shape_manual(name = "Cluster",
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  theme_light()