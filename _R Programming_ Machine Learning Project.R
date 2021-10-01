

#Exploratory Data Analysis

#Importing Dataset Library
library(datasets) 


#Importing Iris Dataset
data(iris) 


#Viewing Iris Data
View(iris) 


#Viewing First 4 Tables
head(iris, 4)


#Viewing Last 4 Tables
tail(iris, 4) 


#Summarizing the Total Data
summary(iris) 


#Summarizing the Data with Respect to a Column
summary(iris$Sepal.Length) 


#Checking if Data has any Null Values
sum(is.null(iris)) 


#Data Visualization
plot(iris)
plot(iris, col="red")



#Scatter Plot
plot(iris$Sepal.Width, iris$Sepal.Length, col="red",
     xlab="sepal width", ylab="sepal length")



#Histogram
hist(iris$Sepal.Width, col = "Red")



#Installing Skimr Library
install.packages("skimr")


#Importing Skimr Library
library(skimr) 


#Skimming Iris Data
skim(iris) 



#Skimming Iris Data with Respect to a Column
iris %>%
  dplyr::group_by(Species) %>%
  skim()


#Developing Machine Learning Model
install.packages("caret")


#Importing Packages for machine learning algorithms
library(caret) 

data(iris)

sum(is.null(iris))


#Reproducing Random Numbers
set.seed(100) 



# Partitioning 80 to 20 the Training Dataset
TrainingIndex <- createDataPartition(iris$Species, p=0.8,list=FALSE)
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

View(TrainingSet)
View(TestingSet)


# SVM model 
#Build Training Model
Model <- train(Species ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(degree=1, scale=1,C=1))



#Build Cross Validation model wih 10 folds
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)



# Apply model for prediction
Model.training <- predict(Model, TrainingSet) # Apply model to make prediction on Training Set
Model.testing <- predict(Model, TestingSet) # Apply model to make prediction on Testing Set
Model.cv <-predict(Model.cv, TrainingSet)



# Applying Confusion Matrix which is a Metric System to measure the Accuracy
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)


#Printing the Confusion Matrix
print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)


#Used to Show the Importance of Variables
Importance <- varImp(Model)
plot(Importance)
